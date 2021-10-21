!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_vfei_mod.F90
!
! !DESCRIPTION: Module HCOX\_VFEI\_Mod.F90 is a HEMCO extension to use
!  VFEI biomass burning emissions, read from ascii tables. This module
!  reads the daily data tables and emits the emissions according to the
!  information in this file.
!\\
!\\
! !INTERFACE:
!
MODULE HCOX_VFEI_Mod
!
! !USES:
!
  USE HCO_Error_MOD
  USE HCO_Diagn_MOD
  USE HCOX_TOOLS_MOD
  USE HCOX_State_MOD, ONLY : Ext_State
  USE HCO_State_MOD,  ONLY : HCO_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCOX_VFEI_Init
  PUBLIC :: HCOX_VFEI_Run
  PUBLIC :: HCOX_VFEI_Final
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: ReadVFEI
  PRIVATE :: EmitVFEI
!
! !REMARKS:
!  TBD
!
! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  TYPE :: MyInst
   INTEGER                         :: Instance
   INTEGER                         :: ExtNr     = -1   ! Extension number
   INTEGER                         :: CatNr     = -1   ! Category number 
   INTEGER                         :: nSpc      =  0   ! # of species
   INTEGER                         :: nFire     =  0   ! # of fires in buffer
   INTEGER,  ALLOCATABLE           :: SpcIDs(:)        ! HEMCO species IDs
   CHARACTER(LEN=15), ALLOCATABLE  :: SpcName(:)       ! HEMCO species name 
   REAL(sp), ALLOCATABLE           :: SpcScl(:)        ! Species scale factors
   REAL(sp), ALLOCATABLE           :: SpcMW(:)         ! Species molecular weight 
   CHARACTER(LEN=15), ALLOCATABLE  :: VFEIname(:,:)    ! Species name(s) in VFEI table 
   REAL(sp), ALLOCATABLE           :: FireEmis(:,:)    ! Emissions for each fire (rows) and species (columns)
   INTEGER,  ALLOCATABLE           :: FireIdx(:)       ! Lon grid index
   INTEGER,  ALLOCATABLE           :: FireJdx(:)       ! Lat grid index
   CHARACTER(LEN=255)              :: FileName         ! VFEI file name
   INTEGER                         :: YmdOnFile = -1   ! Date of file currently in record 
   CHARACTER(LEN=255)              :: VFEISource       ! VFEI  data source
   CHARACTER(LEN=61), ALLOCATABLE  :: SpcScalFldNme(:) ! Names of scale factor fields
   TYPE(MyInst), POINTER           :: NextInst => NULL()
  END TYPE MyInst

  ! Pointer to instances
  TYPE(MyInst), POINTER            :: AllInst => NULL()

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_VFEI_Run
!
! !DESCRIPTION: Subroutine HCOX\_VFEI\_Run is the driver routine
! for the HEMCO VFEI extension.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_VFEI_Run( ExtState, HcoState, RC )
!
! !USES:
!
    USE HCO_FluxArr_Mod,  ONLY : HCO_EmisAdd
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER       :: ExtState    ! Module options
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER       :: HcoState    ! Hemco state
    INTEGER,         INTENT(INOUT) :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER               :: N
    LOGICAL               :: ERR

    ! Strings
    CHARACTER(LEN=255)    :: ErrMsg, ThisLoc

    ! Arrays
    REAL(sp)              :: iFlx(HcoState%NX,HcoState%NY,HcoState%NZ)

    ! Pointers
    TYPE(MyInst), POINTER :: Inst

    !=================================================================
    ! HCOX_VFEI_RUN begins here!
    !=================================================================

    ! Assume success
    RC = HCO_SUCCESS

    ! Sanity check: return if extension not turned on
    IF ( ExtState%VFEI <= 0 ) RETURN

    ! Enter
    CALL HCO_Enter( HcoState%Config%Err, 'HCOX_VFEI_Run (hcox_vfei_mod.F90)', RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Define strings for error messgaes
    ErrMsg = ''
    ThisLoc = ' -> in HCOX_VFEI_Run (in module HEMCO/Extensions/hcox_vfei_mod.F90)'

    ! Get instance
    Inst => NULL()
    CALL InstGet( ExtState%VFEI, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       WRITE( ErrMsg, * ) 'Cannot find VFEI instance Nr. ', ExtState%VFEI
       CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC, ThisLoc )
       RETURN
    ENDIF

    !----------------------------------------------
    ! Read/update the VFEI data
    ! (will be done only if this is a new day)
    !----------------------------------------------
    CALL ReadVFEI( HcoState, ExtState, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       ErrMsg = 'Error encountered in "ReadVFEI"!'
       CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC, ThisLoc )
       RETURN
    ENDIF

    !=======================================================================
    ! Compute VFEI emissions for non dry-run simulations
    ! (Skip for GEOS-Chem dry-run or HEMCO-standalone dry-run)
    !=======================================================================
    IF ( .not. HcoState%Options%IsDryRun ) THEN

       ! Add emissions to emission arrays & diagnostics
       DO N = 1, Inst%nSpc

          ! Emit volcanos into iflx array [kg/m2/s]
          CALL EmitVFEI( HcoState, ExtState, Inst, N, iFlx, RC )
          IF ( RC /= HCO_SUCCESS ) THEN
             ErrMsg = 'Error encountered in "EmitVFEI"!'
             CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC, ThisLoc )
             RETURN
          ENDIF

          ! Apply user-defined scaling (if any) for this species
          CALL HCOX_Scale( HcoState, iFlx, TRIM(Inst%SpcScalFldNme(N)), RC )
          IF ( RC /= HCO_SUCCESS ) THEN
             ErrMsg = 'Error encountered in "HCOX_Scale (degassing)"!'
             CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC, ThisLoc )
             RETURN
          ENDIF

          ! Add emissions into the HEMCO state
          CALL HCO_EmisAdd( HcoState, iFlx, Inst%SpcIDs(N), &
                            RC, ExtNr=Inst%ExtNr )
          IF ( RC /= HCO_SUCCESS ) THEN
             ErrMsg = 'Error encountered in "HCO_EmisAdd"'
             CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC, ThisLoc )
             RETURN
          ENDIF
       ENDDO !N
    ENDIF

    !=======================================================================
    ! Exit
    !=======================================================================

    ! Cleanup
    Inst => NULL()

    ! Return w/ success
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_VFEI_Run
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_VFEI_Init
!
! !DESCRIPTION: Subroutine HCOX\_VFEI\_Init initializes the HEMCO
! VFEI extension.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_VFEI_Init( HcoState, ExtName, ExtState, RC )
!
! !USES:
!
    USE HCO_ExtList_Mod,    ONLY : GetExtNr
    USE HCO_ExtList_Mod,    ONLY : GetExtOpt
    USE HCO_ExtList_Mod,    ONLY : GetExtSpcVal
    USE HCO_STATE_MOD,      ONLY : HCO_GetExtHcoID
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN   ) :: ExtName    ! Extension name
    TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER       :: HcoState   ! Hemco state
    INTEGER,          INTENT(INOUT) :: RC

! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(MyInst), POINTER          :: Inst
    REAL(sp)                       :: ValSp
    INTEGER                        :: ExtNr, N, Dum
    LOGICAL                        :: FOUND, IsAero 
    CHARACTER(LEN=31), ALLOCATABLE :: SpcNames(:)
    CHARACTER(LEN=15)              :: iname(5)
    CHARACTER(LEN=255)             :: MSG, Str

    !=================================================================
    ! HCOX_VFEI_INIT begins here!
    !=================================================================

    ! Extension Nr.
    ExtNr = GetExtNr( HcoState%Config%ExtList, TRIM(ExtName) )
    IF ( ExtNr <= 0 ) THEN
       MSG = 'The VFEI extension is turned off.'
       CALL HCO_MSG( HcoState%Config%Err,  MSG )
       RETURN
    ENDIF

    ! Enter
    CALL HCO_Enter( HcoState%Config%Err,                                     &
                    'HCOX_VFEI_Init (hcox_vfei_mod.F90)', RC          )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Create VFEI instance for this simulation
    Inst => NULL()
    CALL InstCreate( ExtNr, ExtState%VFEI, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_Error( HcoState%Config%Err,                                  &
                      'Cannot create VFEI instance', RC                  )
       RETURN
    ENDIF

    ! Get species IDs.
    CALL HCO_GetExtHcoID( HcoState, ExtNr,     Inst%SpcIDs,                  &
                          SpcNames, Inst%nSpc, RC                           )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! There must be at least one species
    IF ( Inst%nSpc == 0 ) THEN
       CALL HCO_Error( HcoState%Config%Err,                                  &
                      'No VFEI species specified', RC                    )
       RETURN
    ENDIF

    ! Determine scale factor to be applied to each species. This is 1.00
    ! by default, but can be set in the HEMCO configuration file via setting
    ! Scaling_<SpcName>.
    CALL GetExtSpcVal( HcoState%Config, ExtNr,  Inst%nSpc,   SpcNames,       &
                       'Scaling',       1.0_sp, Inst%SpcScl, RC             )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Get species mask fields
    CALL GetExtSpcVal( HcoState%Config,    ExtNr,        Inst%nSpc,          &
                       SpcNames,           'ScaleField', HCOX_NOSCALE,       &
                       Inst%SpcScalFldNme, RC                               )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Pass name and molecular weights for all species
    ALLOCATE(Inst%SpcName(Inst%nSpc))
    ALLOCATE(Inst%SpcMW(Inst%nSpc))
    DO N = 1, Inst%nSpc
       Inst%SpcName(N) = TRIM(ADJUSTL(SpcNames(N))) 
       Inst%SpcMW(N)   = HcoState%Spc(Inst%SpcIDs(N))%MW_g
    ENDDO

    ! Map HEMCO species to VFEI species
    ALLOCATE(Inst%VFEIname(Inst%nSpc,5))
    Inst%VFEIname(:,:) = ''
    DO N = 1, Inst%nSpc
       iname(:) = ''
       SELECT CASE ( TRIM(ADJUSTL(SpcNames(N))) )
          CASE ( 'BENZ' )
             iname(1) = 'BENZENE'
          CASE ( 'TOLU' )
             iname(1) = 'TOLUENE'
          CASE ( 'XYLE' )
             iname(1) = 'XYLENE'
          CASE ( 'OCPI','OCPO','POG1','POG2' )
             iname(1) = 'OC'
          CASE ( 'BCPI','BCPO' )
             iname(1) = 'BC'
          CASE ( 'NAP','SOAP' )
             iname(1) = 'CO'
          CASE ( 'CH2O' )
             iname(1) = 'HCHO'
          CASE ( 'ACET' )
             iname(1) = 'ACETONE'
          CASE ( 'PRPE' )
             iname(1) = 'C3H6'
             iname(2) = 'BUTENES'
             iname(3) = 'PENTENES'
             iname(4) = 'HEXENES'
             iname(5) = 'OCTENES'
          CASE ( 'ALK4' )
             iname(1) = 'BUTANES'
             iname(2) = 'PENTANES'
             iname(3) = 'HEXANES'
             iname(4) = 'HEPTANES'
          CASE ( 'MTPO', 'MONX' )
             iname(1) = 'TERPENES'
          CASE ( 'MOH' )
             iname(1) = 'METHANOL'
          CASE ( 'EOH' )
             iname(1) = 'ETHANOL'
          CASE ( 'GLYX' )
             iname(1) = 'GLYOXAL'
          CASE ( 'ALD2' )
             iname(1) = 'CH3CHO'
          CASE ( 'MGLY' )
             iname(1) = 'CH3COCHO'
          CASE ( 'ACTA' )
             iname(1) = 'CH3COOH'
          CASE ( 'HNO2' )
             iname(1) = 'HONO'
          CASE ( 'HCl'  )
             iname(1) = 'HCL'  
          CASE DEFAULT
             iname(1) = TRIM(ADJUSTL(SpcNames(N)))
       END SELECT
       Inst%VFEIname(N,:) = iname(:)
    ENDDO

    ! Adjust scale factor. The emissions in the VFEI txt files are stored in 
    ! units of moles/day for gases and kg/day for aerosols. Add conversion
    ! factor here to go to kg/s. The emissions will be normalized by area
    ! when processing them (so that they are in kg/m2/s)
    DO N = 1, Inst%nSpc
       ! Is this an aerosol or a gas?
       SELECT CASE ( TRIM(ADJUSTL(Inst%VFEIname(N,1))) )
          CASE ( 'OC', 'BC','PM2_5','PM2.5','TPM' )
             IsAero = .TRUE.
          CASE DEFAULT
             IsAero = .FALSE.
       END SELECT   
       ! Convert from moles/day to kg/day
       IF ( .NOT. IsAero ) THEN
          Inst%SpcScl(N) = Inst%SpcScl(N) * Inst%SpcMW(N) / 1000.0_sp
       ENDIF
       ! Convert from kg/day to kg/s
       Inst%SpcScl(N) = Inst%SpcScl(N) / 86400.0_sp
    ENDDO

    ! Get location of volcano table. This must be provided.
    CALL GetExtOpt( HcoState%Config, ExtNr, 'VFEI_Table',                 &
                    OptValChar=Inst%FileName, FOUND=FOUND, RC=RC            )

    IF ( RC /= HCO_SUCCESS .OR. .NOT. FOUND ) THEN
       MSG = 'Cannot read VFEI table file name. Please provide '       // &
             'the VFEI table as a setting to the VFEI extension. '  // &
             'The name of this setting must be `VFEI_Table`.'
       CALL HCO_Error( HcoState%Config%Err, MSG, RC )
       RETURN
    ENDIF

    ! See if eruptive and degassing hierarchies are given
    Inst%CatNr = 1
    CALL GetExtOpt( HcoState%Config, ExtNr,       'Category', &
                    OptValInt=Dum,   FOUND=FOUND, RC=RC        )
    IF ( RC /= HCO_SUCCESS ) RETURN
    IF ( FOUND ) Inst%CatNr = Dum

    ! Verbose mode
    IF ( HcoState%amIRoot ) THEN
       MSG = 'Use emissions extension `VFEI`:'
       CALL HCO_MSG( HcoState%Config%Err,  MSG )

       MSG = ' - use the following species (Name, HcoID, Species names in VFEI table):'
       CALL HCO_MSG( HcoState%Config%Err, MSG)
       DO N = 1, Inst%nSpc
          WRITE(MSG,*) TRIM(Inst%SpcName(N)), ', ', Inst%SpcIDs(N), ', ', Inst%VFEIname(N,:)
          CALL HCO_MSG( HcoState%Config%Err, MSG)
          WRITE(MSG,*) 'Apply scale factor: ', Inst%SpcScl(N)
          CALL HCO_MSG( HcoState%Config%Err, MSG)
          WRITE(MSG,*) 'Apply scale field: ', TRIM(Inst%SpcScalFldNme(N))
          CALL HCO_MSG( HcoState%Config%Err, MSG)
       ENDDO
       WRITE(MSG,*) ' - Emit emissions as category ', Inst%CatNr
       CALL HCO_MSG( HcoState%Config%Err,  MSG )
    ENDIF

    ! Cleanup
    Inst => NULL()
    IF ( ALLOCATED(SpcNames) ) DEALLOCATE(SpcNames)

    CALL HCO_Leave( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_VFEI_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_VFEI_Final
!
! !DESCRIPTION: Subroutine HCOX\_VFEI\_Final finalizes the HEMCO
!  VFEI extension.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_VFEI_Final( ExtState )
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER :: ExtState   ! Module options
!
! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    !=================================================================
    ! HCOX_VFEI_FINAL begins here!
    !=================================================================
    CALL InstRemove( ExtState%VFEI )

  END SUBROUTINE HCOX_VFEI_Final
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ReadVFEI
!
! !DESCRIPTION: Subroutine ReadVFEI reads the VFEI emissions table of the
!  current day.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ReadVFEI( HcoState, ExtState, Inst, RC )
!
! !USES:
!
    USE HCO_CharTools_Mod
    USE HCO_CharPak_Mod,    ONLY : StrSplit
    USE HCO_inquireMod,     ONLY : findfreeLun
    USE HCO_CLOCK_MOD,      ONLY : HcoClock_NewDay
    USE HCO_CLOCK_MOD,      ONLY : HcoClock_Get
    USE HCO_GeoTools_MOD,   ONLY : HCO_GetHorzIJIndex
    USE HCO_EXTLIST_MOD,    ONLY : HCO_GetOpt
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER       :: HcoState   ! Hemco state
    TYPE(MyInst),     POINTER       :: Inst
    INTEGER,          INTENT(INOUT) :: RC

! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER               :: ThisYMD
    INTEGER               :: YYYY, MM, DD
    INTEGER               :: I, J, N, M, LUN, IOS, AS
    INTEGER               :: nFire, nCol, nHeader
    INTEGER               :: LATCOL, LONCOL
    REAL(sp)              :: Dum(60)
    REAL(hp), ALLOCATABLE :: FireLon(:)      ! VFEI longitude [deg E]
    REAL(hp), ALLOCATABLE :: FireLat(:)      ! VFEI latitude  [deg N]
    REAL(sp), ALLOCATABLE :: FireArr(:,:)    ! Array with original fire data
    LOGICAL               :: FileExists, EOF
    LOGICAL               :: DoUpdate
    CHARACTER(LEN=255)    :: ThisFile
    CHARACTER(LEN=2047)   :: ThisLine
    CHARACTER(LEN=255)    :: MSG,      FileMsg
    CHARACTER(LEN=255)    :: LOC = 'ReadVFEI (hcox_vfei_mod.F90)'
    CHARACTER(LEN=15)     :: iname, HEADER(60)
    CHARACTER(LEN=255)    :: SUBSTR(255)

    !=================================================================
    ! ReadVFEI begins here!
    !=================================================================

    ! Do only if it's a new day...
    ! Get current year, month, day
    CALL HcoClock_Get ( HcoState%Clock, cYYYY=YYYY, cMM=MM, cDD=DD, RC=RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

#if defined( MODEL_GEOS )
       ! Error trap: skip leap days
       IF ( MM == 2 .AND. DD > 28 ) DD = 28
#endif

    ! Compare current day against day on file
    ThisYMD  = YYYY*10000 + MM*100+ DD
    DoUpdate = ThisYMD /= Inst%YmdOnFile 

    IF ( DoUpdate ) THEN

       ! Get file name
       ThisFile = Inst%FileName
       CALL HCO_CharParse( HcoState%Config, ThisFile, YYYY, MM, DD, 0, 0, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN

       !--------------------------------------------------------------------
       ! In dry-run mode, print file path to dryrun log and exit.
       ! Otherwise, print file path to the HEMCO log file and continue.
       !--------------------------------------------------------------------

       ! Test if the file exists
       INQUIRE( FILE=TRIM( ThisFile ), EXIST=FileExists )

       ! Create a display string based on whether or not the file is found
       IF ( FileExists ) THEN
          FileMsg = 'HEMCO (VFEI): Opening'
       ELSE
          FileMsg = 'HEMCO (VFEI): REQUIRED FILE NOT FOUND'
       ENDIF

       ! Write file status to stdout and the HEMCO log
       IF ( Hcostate%amIRoot ) THEN
          WRITE( 6,   300 ) TRIM( FileMsg ), TRIM( ThisFile )
          WRITE( MSG, 300 ) TRIM( FileMsg ), TRIM( ThisFile )
          CALL HCO_MSG( HcoState%Config%Err, MSG )
 300      FORMAT( a, ' ', a )
       ENDIF

       ! For dry-run simulations, return to calling program.
       ! For regular simulations, throw an error if we can't find the file.
       IF ( HcoState%Options%IsDryRun ) THEN
          RETURN
       ELSE
          IF ( .not. FileExists ) THEN
             WRITE( MSG, 300 ) TRIM( FileMsg ), TRIM( ThisFile )
             CALL HCO_ERROR( HcoState%Config%Err, MSG, RC )
             RETURN
          ENDIF
       ENDIF

       !--------------------------------------------------------------------
       ! Read data from file
       !--------------------------------------------------------------------

       ! Open file
       LUN = findFreeLun()
       OPEN ( LUN, FILE=TRIM(ThisFile), STATUS='OLD', IOSTAT=IOS )
       IF ( IOS /= 0 ) THEN
          MSG = 'Error reading ' // TRIM(ThisFile)
          CALL HCO_ERROR( HcoState%Config%Err,  MSG, RC, THISLOC=LOC )
          RETURN
       ENDIF

       ! Get number of fire records
       nFire = 0
       DO
          CALL GetNextLine( LUN, ThisLine, EOF, RC )
          IF ( RC /= HCO_SUCCESS ) RETURN
          IF ( EOF ) EXIT

          ! Skip any entries that contain '::'
          IF ( INDEX( TRIM(ThisLine), '::') > 0 ) CYCLE

          ! If we make it to here, this is a valid entry
          nFire = nFire + 1
       ENDDO

       ! First line is header, skip
       nFire = nFire - 1

       ! Verbose
       IF ( HCO_IsVerb(HcoState%Config%Err,2) ) THEN
          WRITE(MSG,*) 'Number of fires: ', nFire
          CALL HCO_MSG( HcoState%Config%Err, MSG)
       ENDIF

       ! Allocate arrays
       IF ( nFire > 0 ) THEN
          ! Eventually deallocate previously allocated data
          IF ( ALLOCATED(Inst%FireIdx) ) DEALLOCATE(Inst%FireIdx)
          IF ( ALLOCATED(Inst%FireJdx) ) DEALLOCATE(Inst%FireJdx)
          IF ( ALLOCATED(Inst%FireEmis)) DEALLOCATE(Inst%FireEmis)

          ALLOCATE(     FireLon(nFire),    &
                        FireLat(nFire),    &
                        FireArr(nFire,60), &
                   Inst%FireIdx(nFire),    &
                   Inst%FireJdx(nFire),    &
                   Inst%FireEmis(nFire,Inst%nSpc), &
                   STAT=AS )
          IF ( AS /= 0 ) THEN
             CALL HCO_ERROR ( HcoState%Config%Err, &
                              'VFEI allocation error', RC, THISLOC=LOC )
             RETURN
          ENDIF
               FireLon  = 0.0_hp
               FireLat  = 0.0_hp
               FireArr  = 0.0_sp
          Inst%FireEmis = 0.0_sp

       ELSE
          WRITE(MSG,*) 'No VFEI data found for year/mm/dd: ', YYYY, MM, DD
          CALL HCO_WARNING(HcoState%Config%Err,MSG,RC,WARNLEV=1,THISLOC=LOC)
       ENDIF

       ! Now read records
       IF ( nFire > 0 ) THEN
          REWIND( LUN )
          N = 0
          DO
             CALL GetNextLine( LUN, ThisLine, EOF, RC )
             IF ( RC /= HCO_SUCCESS ) RETURN
             IF ( EOF ) EXIT

             ! Write this data into the following vector element
             N = N + 1
             IF ( N > ( nFire + 1 ) ) THEN
                WRITE(MSG,*) 'N exceeds nFire: ', N, nFire, &
                             ' - This error occurred when reading ', &
                             TRIM(ThisFile), '. This line: ', TRIM(ThisLine)
                CALL HCO_ERROR ( HcoState%Config%Err, MSG, RC, THISLOC = LOC )
                RETURN
             ENDIF

             ! Parse header and find lat/lon columns
             IF ( N == 1 ) THEN
                HEADER(:) = ''
                LATCOL = -1
                LONCOL = -1
                CALL STRSPLIT( TRIM(ThisLine), ',', SUBSTR, nHeader )
                DO I=1,nHeader
                   HEADER(I) = TRIM(ADJUSTL(SUBSTR(I)))
                   IF ( TRIM(HEADER(I))=='LATI'  ) LATCOL = I
                   IF ( TRIM(HEADER(I))=='LONGI' ) LONCOL = I
                ENDDO          
                IF ( LATCOL <= 0 .OR. LONCOL <= 0 ) THEN
                   WRITE(MSG,*) 'LATI or LONGI not found in header of file ',TRIM(ThisFile)
                   CALL HCO_ERROR ( HcoState%Config%Err, MSG, RC, THISLOC = LOC )
                   RETURN
                ENDIF
                CYCLE
             ENDIF

             ! If not header, parse emissions data and assign to emissions array
             IF ( N > 1 ) THEN
                CALL HCO_CharSplit( TRIM(ThisLine), ',', &
                                    HCO_GetOpt(HcoState%Config%ExtList,'Wildcard'), &
                                    Dum, nCol, RC )
                IF ( RC /= HCO_SUCCESS ) RETURN

                ! pass to array
                FireArr(N,1:nCol) = Dum(1:nCol)
             ENDIF
          ENDDO

          ! At this point, we should have read exactly nFire + 1 entries!
          IF ( N /= nFire+1 ) THEN
             WRITE(MSG,*) 'N /= nFire: ', N, nFire, &
                          ' - This error occurred when reading ', TRIM(ThisFile)
             CALL HCO_ERROR ( HcoState%Config%Err, MSG, RC, THISLOC = LOC )
             RETURN
          ENDIF

       ENDIF

       ! All done reading data.
       CLOSE ( LUN )

       ! Parse original data 
       IF ( nFire > 0 ) THEN
          ! Get latitude and longitude index for each fire
          FireLon(:) = FireArr(:,LONCOL)
          FireLat(:) = FireArr(:,LATCOL)
          CALL HCO_GetHorzIJIndex( HcoState, nFire, FireLon, &
                                   FireLat, Inst%FireIdx, Inst%FireJdx, RC )
          IF ( RC /= HCO_SUCCESS ) RETURN
          
          ! Get fire emissions for each HEMCO species.
          DO N = 1, Inst%nSpc
             ! Find VREI names for each HEMCO species. This can be more than one
             DO J = 1, 5
                iname = TRIM(ADJUSTL(Inst%VFEIname(N,J)))
                IF ( iname == '' ) CYCLE
                I = -1
                DO M=1,60
                   IF ( TRIM(ADJUSTL(HEADER(M)))==iname ) THEN
                      I = M
                      EXIT
                   ENDIF
                ENDDO 
                IF ( I <= 0 ) THEN
                   WRITE(MSG,*) 'Species not found in header ', TRIM(iname)
                   CALL HCO_WARNING ( HcoState%Config%Err, MSG, RC, THISLOC = LOC )
                ELSE
                   Inst%FireEmis(:,N) = Inst%FireEmis(:,N) + FireArr(:,I)
                ENDIF
             ENDDO
          ENDDO
       ENDIF

       ! Save # of volcanoes in archive
       Inst%nFire = nFire

       ! Update date for file on record
       Inst%YmdOnFile = ThisYMD 
    ENDIF ! new day

    ! Cleanup
    IF ( ALLOCATED(FireLon) ) DEALLOCATE(FireLon)
    IF ( ALLOCATED(FireLat) ) DEALLOCATE(FireLat)
    IF ( ALLOCATED(FireArr) ) DEALLOCATE(FireArr)

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ReadVFEI
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: EmitVFEI
!
! !DESCRIPTION: Subroutine EmitVFEI emits the emissions of a given species
!  into a 3D array. For now, the emissions are evenly distributed within the 
!  lowest 5500m 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE EmitVFEI( HcoState, ExtState, Inst, iSpc, iFlx, RC )
!
! !USES:
!
    USE HCO_CLOCK_MOD,      ONLY : HcoClock_Get
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER       :: HcoState   ! Hemco state
    TYPE(MyInst),     POINTER       :: Inst
    INTEGER,          INTENT(IN)    :: iSpc
    INTEGER,          INTENT(INOUT) :: RC
!
! !OUTPUT PARAMETERS:
!
    REAL(sp),         INTENT(  OUT) :: iFlx(HcoState%NX,HcoState%NY,HcoState%NZ)
!
! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER             :: I, J, L, N, HH, MN, hhmmss
    REAL(sp)            :: TotEmis, PlumeHgt, iPBL, PlumeHgtPBL, PlumeHgtFT
    REAL(sp)            :: TotEmisPBL, TotEmisFT
    REAL(sp)            :: z1, z2, zBot, zTop
    REAL(sp)            :: tmp1, tmp2, Frac
    REAL(sp), PARAMETER :: zTopFT = 5500.0_sp
    CHARACTER(LEN=255)  :: MSG
    CHARACTER(LEN=255)  :: LOC = 'EmitVFEI (hcox_vfei_mod.F90)'

    !=================================================================
    ! EmitVFEI begins here!
    !=================================================================

    ! Init
    iFlx(:,:,:) = 0.0_sp

    ! Make sure all required grid quantities are defined
    IF ( .NOT. ASSOCIATED(HcoState%Grid%AREA_M2%Val) ) THEN
       CALL HCO_ERROR ( HcoState%Config%Err, &
                       'Grid box areas not defined', RC, THISLOC=LOC )
       RETURN
    ENDIF
    IF ( .NOT. ASSOCIATED(HcoState%Grid%PBLHEIGHT%Val) ) THEN
       CALL HCO_ERROR ( HcoState%Config%Err, &
                       'PBL heights not defined', RC, THISLOC=LOC )
       RETURN
    ENDIF
    IF ( .NOT. ASSOCIATED(HcoState%Grid%BXHEIGHT_M%Val) ) THEN
       CALL HCO_ERROR ( HcoState%Config%Err, &
                       'Grid box heights not defined', RC, THISLOC=LOC )
       RETURN
    ENDIF
    IF ( .NOT. ASSOCIATED(HcoState%Grid%ZSFC%Val) ) THEN
       CALL HCO_ERROR ( HcoState%Config%Err, &
                       'Surface heights not defined', RC, THISLOC=LOC )
       RETURN
    ENDIF

    ! Get current hour, minute and save as hhmmss
    CALL HcoClock_Get ( HcoState%Clock, cH=HH, cM=MN, RC=RC )
    IF ( RC /= HCO_SUCCESS ) RETURN
    hhmmss = HH*10000 + MN*100

    ! Do for every fire
    IF ( Inst%nFire > 0 ) THEN
       DO N = 1, Inst%nFire

          ! Grid box index for this fire
          I = Inst%FireIdx(N)
          J = Inst%FireJdx(N)

          ! Skip if outside of domain
          IF( I < 1 .OR. J < 1 ) CYCLE

          ! Get total emitted molec/day/m2 or kg/day/m2 
          TotEmis = Inst%FireEmis(N,iSpc) / HcoState%Grid%AREA_M2%Val(I,J)
          IF ( TotEmis == 0.0_sp ) CYCLE

          ! Apply scale factor to convert to kg/m2/s
          TotEmis = TotEmis * Inst%SpcScl(iSpc)

          ! Get emission heights
          iPBL = HcoState%Grid%PBLHEIGHT%Val(I,J)
          zBot = HcoState%Grid%ZSFC%Val(I,J)
          zTop = max(zTopFT,iPBL)

          ! Plume heights
          PlumeHgtPBL = iPBL - zBot
          PlumeHgtFT  = zTop - iPBL 

          ! Emission amounts for PBL and FT
          ! Default: 65% PBL, 35% FT
          TotEmisPBL = 0.65_sp * TotEmis
          TotEmisFT  = 0.35_sp * TotEmis 
          ! If PBL is zero, emit everything into free troposphere 
          IF ( PlumeHgtPBL <= 0.0_sp ) THEN
             TotEmisPBL = 0.0_sp
             TotEmisFT  = TotEmis 
          ENDIF 
          ! If PBL is higher than 5500m, emit everything into PBL
          IF ( PlumeHgtFT <= 0.0_sp ) THEN 
             TotEmisPBL = TotEmis
             TotEmisFT  = 0.0_sp
          ENDIF

          ! Current height
          z1 = zBot 
        
          ! Distribute emissions into emission arrays. The fire plume
          ! ranges from zBot to zTop, with 65% emitted into the PBL and 35% emitted above
          DO L = 1, HcoState%NZ

             ! Get top height of this box
             z2 = z1 + HcoState%Grid%BXHEIGHT_M%Val(I,J,L)

             ! Skip if the plume bottom is above this grid box top
             IF ( zBot >= z2 ) THEN
                z1 = z2
                CYCLE
             ENDIF

             ! If the plume top is below this grid box bottom, we can exit
             ! since there will be no more emissions to distribute.
             IF ( z1 > zTop ) EXIT

             ! If we make it to here, the fire plume is at least partly
             ! within this level. Determine the fraction of the plume that
             ! is within heights z1 to z2.

             ! First check for fraction of box in PBL
             IF ( z1 < iPBL .AND. TotEmisPBL > 0.0_sp ) THEN
                tmp1 = MAX(z1,zBot)  ! this layer's plume bottom
                tmp2 = MIN(z2,iPBL)  ! this layer's plume top
                ! Special case that zTop is heigher than the highest level: make
                ! sure that all emissions are going to be used.
                IF ( ( L == HcoState%NZ ) .AND. ( iPBL > z2 ) ) THEN
                   tmp2 = iPBL
                ENDIF
                IF ( PlumeHgtPBL <= 0.0_sp ) THEN
                   Frac = 1.0_sp
                ELSE
                   Frac = (tmp2-tmp1) / PlumeHgtPBL
                ENDIF
                iFlx(I,J,L) = iFlx(I,J,L) + ( Frac * TotEmisPBL )
             ENDIF

             ! Check for fraction of box in FT
             IF ( z2 > iPBL ) THEN
                tmp1 = MAX(z1,iPBL)  ! this layer's plume bottom
                tmp2 = MIN(z2,zTop)  ! this layer's plume top
                IF ( ( L == HcoState%NZ ) .AND. ( zTop > z2 ) ) THEN
                   tmp2 = zTop
                ENDIF
                IF ( PlumeHgtFT == 0.0_sp ) THEN
                   Frac = 1.0_sp
                ELSE
                   Frac = (tmp2-tmp1) / PlumeHgtFT  
                ENDIF
                iFlx(I,J,L) = iFlx(I,J,L) + ( Frac * TotEmisFT )
             ENDIF

             ! The top height is the new bottom
             z1 = z2
          ENDDO
       ENDDO
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE EmitVFEI
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InstGet
!
! !DESCRIPTION: Subroutine InstGet returns a poiner to the desired instance.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE InstGet( Instance, Inst, RC, PrevInst )
!
! !INPUT PARAMETERS:
!
    INTEGER                             :: Instance
    TYPE(MyInst),     POINTER           :: Inst
    INTEGER                             :: RC
    TYPE(MyInst),     POINTER, OPTIONAL :: PrevInst
!
! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MyInst),     POINTER    :: PrvInst

    !=================================================================
    ! InstGet begins here!
    !=================================================================

    ! Get instance. Also archive previous instance.
    PrvInst => NULL()
    Inst    => AllInst
    DO WHILE ( ASSOCIATED(Inst) )
       IF ( Inst%Instance == Instance ) EXIT
       PrvInst => Inst
       Inst    => Inst%NextInst
    END DO
    IF ( .NOT. ASSOCIATED( Inst ) ) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF

    ! Pass output arguments
    IF ( PRESENT(PrevInst) ) PrevInst => PrvInst

    ! Cleanup & Return
    PrvInst => NULL()
    RC = HCO_SUCCESS

  END SUBROUTINE InstGet
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InstCreate
!
! !DESCRIPTION: Subroutine InstCreate creates a new instance.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE InstCreate( ExtNr, Instance, Inst, RC )
!
! !INPUT PARAMETERS:
!
    INTEGER,       INTENT(IN)       :: ExtNr
!
! !OUTPUT PARAMETERS:
!
    INTEGER,       INTENT(  OUT)    :: Instance
    TYPE(MyInst),  POINTER          :: Inst
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,       INTENT(INOUT)    :: RC
!
! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MyInst), POINTER          :: TmpInst
    INTEGER                        :: nnInst

    !=================================================================
    ! InstCreate begins here!
    !=================================================================

    ! ----------------------------------------------------------------
    ! Generic instance initialization
    ! ----------------------------------------------------------------

    ! Initialize
    Inst => NULL()

    ! Get number of already existing instances
    TmpInst => AllInst
    nnInst = 0
    DO WHILE ( ASSOCIATED(TmpInst) )
       nnInst  =  nnInst + 1
       TmpInst => TmpInst%NextInst
    END DO

    ! Create new instance
    ALLOCATE(Inst)
    Inst%Instance  = nnInst + 1
    Inst%ExtNr     = ExtNr
    Inst%YmdOnFile = -1

    ! Attach to instance list
    Inst%NextInst => AllInst
    AllInst       => Inst

    ! Update output instance
    Instance = Inst%Instance

    ! ----------------------------------------------------------------
    ! Type specific initialization statements follow below
    ! ----------------------------------------------------------------

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE InstCreate
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InstRemove
!
! !DESCRIPTION: Subroutine InstRemove creates a new instance.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE InstRemove( Instance )
!
! !INPUT PARAMETERS:
!
    INTEGER :: Instance
!
! !REVISION HISTORY:
!  15 Oct 2021 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER                     :: RC
    TYPE(MyInst), POINTER       :: PrevInst
    TYPE(MyInst), POINTER       :: Inst

    !=================================================================
    ! InstRemove begins here!
    !=================================================================

    ! Init
    PrevInst => NULL()
    Inst     => NULL()

    ! Get instance. Also archive previous instance.
    CALL InstGet ( Instance, Inst, RC, PrevInst=PrevInst )

    ! Instance-specific deallocation
    IF ( ASSOCIATED(Inst) ) THEN
       IF ( ALLOCATED(Inst%SpcName      ) ) DEALLOCATE ( Inst%SpcName       )
       IF ( ALLOCATED(Inst%SpcIDs       ) ) DEALLOCATE ( Inst%SpcIDs        )
       IF ( ALLOCATED(Inst%SpcScl       ) ) DEALLOCATE ( Inst%SpcScl        )
       IF ( ALLOCATED(Inst%SpcMW        ) ) DEALLOCATE ( Inst%SpcMW         )
       IF ( ALLOCATED(Inst%VFEIname     ) ) DEALLOCATE ( Inst%VFEIname      )
       IF ( ALLOCATED(Inst%FireEmis     ) ) DEALLOCATE ( Inst%FireEmis      )
       IF ( ALLOCATED(Inst%FireIdx      ) ) DEALLOCATE ( Inst%FireIdx       )
       IF ( ALLOCATED(Inst%FireJdx      ) ) DEALLOCATE ( Inst%FireJdx       )
       IF ( ALLOCATED(Inst%SpcScalFldNme) ) DEALLOCATE ( Inst%SpcScalFldNme )

       ! Pop off instance from list
       IF ( ASSOCIATED(PrevInst) ) THEN
          PrevInst%NextInst => Inst%NextInst
       ELSE
          AllInst => Inst%NextInst
       ENDIF
       DEALLOCATE(Inst)
       Inst => NULL()
    ENDIF

   END SUBROUTINE InstRemove
!EOC
END MODULE HCOX_VFEI_Mod
