!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_gfas_mod.F90
!
! !DESCRIPTION: Module HCOX\_GFAS\_MOD contains routines to calculate
! GFAS biomass-burning emissions with a 3D vertical injection profile.
!
! A single reference species (default CO, configurable) provides the 3D
! vertical structure of emissions. For every other GFAS species, a 2D
! surface emission is read and redistributed vertically using the
! column-normalized fraction of the reference 3D field.
!
! The reference species itself is emitted using its native 3D field
! (pass-through); no re-multiplication is performed.
!
! !INTERFACE:
!
MODULE HCOX_GFAS_MOD
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_DIAGN_MOD
  USE HCOX_TOOLS_MOD
  USE HCO_STATE_MOD,  ONLY : HCO_State
  USE HCOX_State_MOD, ONLY : Ext_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCOX_GFAS_Init
  PUBLIC :: HCOX_GFAS_Run
  PUBLIC :: HCOX_GFAS_Final
!
! !REMARKS:
!  HEMCO config (extension switches section):
!
!    112    GFAS                       : on    CO/CO2/NO/NH3/SO2/OC/BC
!        --> Vertical Profile Species :       CO
!
!
!  Notes:
!  - The reference species needs ONLY the GFAS_<ref>_3D entry.
!  - Every species listed in the extension settings line MUST have a
!    matching GFAS_<SpcName>_2D entry; otherwise the run errors out.
!  - GFAS data on its native 36 vertical levels is auto-mapped to model
!    levels 1-36 by HEMCO's I/O (HEMCO/src/Core/hco_interp_mod.F90).
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !PRIVATE TYPES:
!
  TYPE :: MyInst
   INTEGER                    :: Instance
   INTEGER                    :: ExtNr           ! Extension number

   ! Reference species (whose 3D defines the vertical profile)
   CHARACTER(LEN=31)          :: RefSpcName      ! Name of reference species
   INTEGER                    :: RefIdx          ! Index of ref in SpcNames, -1 if not listed

   ! Species list (for output)
   INTEGER                    :: nSpc            ! Number of listed species
   CHARACTER(LEN=31), POINTER :: SpcNames(:)      => NULL()
   INTEGER,           POINTER :: HcoIDs(:)        => NULL()
   REAL(sp),          POINTER :: SpcScal(:)       => NULL()
   CHARACTER(LEN=61), POINTER :: SpcScalFldNme(:) => NULL()

   ! Working arrays
   REAL(hp),          POINTER :: Ref3D(:,:,:)     => NULL()  ! Reference 3D field
   REAL(hp),          POINTER :: Frac3D(:,:,:)    => NULL()  ! Vertical fraction (sums to 1 per column)
   REAL(hp),          POINTER :: Spc2D(:,:)       => NULL()  ! Scratch 2D for non-ref species

   TYPE(MyInst),      POINTER :: NextInst => NULL()
  END TYPE MyInst

  ! Linked list of instances
  TYPE(MyInst), POINTER       :: AllInst => NULL()

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_GFAS_Run
!
! !DESCRIPTION: Subroutine HCOX\_GFAS\_Run computes 3D biomass-burning
! emissions for each listed species by applying the column-normalized
! vertical profile of the reference 3D field to each species' 2D field.
!
! !INTERFACE:
!
  SUBROUTINE HCOX_GFAS_Run( ExtState, HcoState, RC )
!
! !USES:
!
    USE HCO_Calc_Mod,     ONLY : HCO_EvalFld
    USE HCO_FluxArr_MOD,  ONLY : HCO_EmisAdd
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER        :: HcoState   ! HEMCO state
    TYPE(Ext_State), POINTER        :: ExtState   ! Module options
    INTEGER,         INTENT(INOUT)  :: RC         ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                 :: I, J, L, N
    REAL(hp)                :: ColSum
    REAL(hp), PARAMETER     :: EPS = TINY(1.0_hp)
    CHARACTER(LEN=63)       :: FieldName
    CHARACTER(LEN=255)      :: MSG, LOC
    LOGICAL                 :: FOUND
    REAL(hp), ALLOCATABLE   :: SpcArr3D(:,:,:)
    REAL(hp), ALLOCATABLE   :: MaskFld(:,:)
    INTEGER                 :: AS

    TYPE(MyInst), POINTER   :: Inst

    !=================================================================
    ! HCOX_GFAS_Run begins here
    !=================================================================
    LOC = 'HCOX_GFAS_Run (HCOX_GFAS_MOD.F90)'

    ! Return if extension disabled
    IF ( ExtState%GFAS <= 0 ) RETURN

    ! Enter
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Get instance
    Inst => NULL()
    CALL InstGet ( ExtState%GFAS, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       WRITE(MSG,*) 'Cannot find GFAS instance Nr. ', ExtState%GFAS
       CALL HCO_ERROR(MSG,RC)
       RETURN
    ENDIF

    !-----------------------------------------------------------------
    ! Step 1: Read the reference 3D field (hard error if missing)
    !-----------------------------------------------------------------
    FieldName = 'GFAS_' // TRIM(Inst%RefSpcName) // '_3D'

    Inst%Ref3D = 0.0_hp
    FOUND = .FALSE.
    CALL HCO_EvalFld( HcoState, TRIM(FieldName), Inst%Ref3D, RC, FOUND=FOUND )
    IF ( RC /= HCO_SUCCESS .OR. .NOT. FOUND ) THEN
       WRITE(MSG,*) 'GFAS reference 3D field "' // TRIM(FieldName) //          &
                    '" not found in HEMCO emissions list (ExtNr=',             &
                    Inst%ExtNr, '). Please declare it in the base-emissions ', &
                    'section of HEMCO_Config.rc with SrcDim=xyz, or change ',  &
                    'the "Vertical Profile Species" option.'
       CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    !-----------------------------------------------------------------
    ! Step 2: Compute column-normalized vertical fraction
    !         frac(i,j,k) = Ref3D(i,j,k) / sum_k Ref3D(i,j,k)
    !-----------------------------------------------------------------
    Inst%Frac3D = 0.0_hp

!$OMP PARALLEL DO                                            &
!$OMP DEFAULT( SHARED )                                      &
!$OMP PRIVATE( I, J, L, ColSum )                             &
!$OMP SCHEDULE( DYNAMIC, 8 )                                 &
!$OMP COLLAPSE( 2 )
    DO J = 1, HcoState%NY
    DO I = 1, HcoState%NX
       ColSum = 0.0_hp
       DO L = 1, HcoState%NZ
          ColSum = ColSum + Inst%Ref3D(I,J,L)
       ENDDO
       IF ( ColSum > EPS ) THEN
          DO L = 1, HcoState%NZ
             Inst%Frac3D(I,J,L) = Inst%Ref3D(I,J,L) / ColSum
          ENDDO
       ENDIF
    ENDDO
    ENDDO
!$OMP END PARALLEL DO

    !-----------------------------------------------------------------
    ! Step 3: Allocate per-call 3D work array
    !-----------------------------------------------------------------
    ALLOCATE( SpcArr3D(HcoState%NX, HcoState%NY, HcoState%NZ), STAT=AS )
    IF ( AS /= 0 ) THEN
       CALL HCO_ERROR( 'Cannot allocate SpcArr3D', RC, THISLOC=LOC )
       RETURN
    ENDIF

    !-----------------------------------------------------------------
    ! Step 4: For each species, build 3D emission and pass to HEMCO
    !-----------------------------------------------------------------
    DO N = 1, Inst%nSpc

       ! Skip unknown species
       IF ( Inst%HcoIDs(N) < 0 ) CYCLE

       IF ( N == Inst%RefIdx ) THEN

          !-----------------------------------------------------------
          ! Reference species: pass through the native 3D field
          !-----------------------------------------------------------
          SpcArr3D = Inst%Ref3D

       ELSE

          !-----------------------------------------------------------
          ! Other species: read 2D field(s) and multiply by frac.
          ! Special case: PRPE has two source fields in GFAS
          ! (hialkenes + c3h6); read both and sum them. All other
          ! species follow the standard GFAS_<SpcName>_2D convention.
          !-----------------------------------------------------------
          Inst%Spc2D = 0.0_hp

          IF ( TRIM(Inst%SpcNames(N)) == 'PRPE' ) THEN

             ! --- PRPE special case: sum GFAS_PRPE1_2D + GFAS_PRPE2_2D ---
             CALL Read2DField( HcoState, 'GFAS_PRPE1_2D',                       &
                               Inst%Spc2D, RC, AddTo=.FALSE.,                   &
                               ExtNr=Inst%ExtNr, SpcName='PRPE' )
             IF ( RC /= HCO_SUCCESS ) THEN
                IF ( ALLOCATED(SpcArr3D) ) DEALLOCATE(SpcArr3D)
                RETURN
             ENDIF
             CALL Read2DField( HcoState, 'GFAS_PRPE2_2D',                       &
                               Inst%Spc2D, RC, AddTo=.TRUE.,                    &
                               ExtNr=Inst%ExtNr, SpcName='PRPE' )
             IF ( RC /= HCO_SUCCESS ) THEN
                IF ( ALLOCATED(SpcArr3D) ) DEALLOCATE(SpcArr3D)
                RETURN
             ENDIF

          ELSE

             ! --- Standard case: single GFAS_<SpcName>_2D field ---
             FieldName = 'GFAS_' // TRIM(Inst%SpcNames(N)) // '_2D'
             CALL Read2DField( HcoState, TRIM(FieldName), Inst%Spc2D, RC,       &
                               AddTo=.FALSE., ExtNr=Inst%ExtNr,                 &
                               SpcName=TRIM(Inst%SpcNames(N)) )
             IF ( RC /= HCO_SUCCESS ) THEN
                IF ( ALLOCATED(SpcArr3D) ) DEALLOCATE(SpcArr3D)
                RETURN
             ENDIF

          ENDIF

!$OMP PARALLEL DO                                            &
!$OMP DEFAULT( SHARED )                                      &
!$OMP PRIVATE( I, J, L )                                     &
!$OMP SCHEDULE( DYNAMIC, 8 )                                 &
!$OMP COLLAPSE( 3 )
          DO L = 1, HcoState%NZ
            DO J = 1, HcoState%NY
              DO I = 1, HcoState%NX
                SpcArr3D(I,J,L) = Inst%Spc2D(I,J) * Inst%Frac3D(I,J,L)
              ENDDO
            ENDDO
          ENDDO
!$OMP END PARALLEL DO

       ENDIF

       ! Apply per-species scaling factor
       IF ( Inst%SpcScal(N) /= 1.0_sp ) THEN
          SpcArr3D = SpcArr3D * Inst%SpcScal(N)
       ENDIF

       ! Apply per-species mask field (manual application; HCOX_SCALE_3D
       ! in hcox_tools_mod.F90 collapses level 1 across all levels,
       ! which would corrupt our genuine 3D distribution).
       IF ( TRIM(Inst%SpcScalFldNme(N)) /= TRIM(HCOX_NOSCALE) ) THEN
          ALLOCATE( MaskFld(HcoState%NX, HcoState%NY), STAT=AS )
          IF ( AS /= 0 ) THEN
             CALL HCO_ERROR( 'Cannot allocate MaskFld', RC, THISLOC=LOC )
             RETURN
          ENDIF
          MaskFld = 0.0_hp
          CALL HCO_EvalFld( HcoState, TRIM(Inst%SpcScalFldNme(N)),              &
                            MaskFld, RC )
          IF ( RC /= HCO_SUCCESS ) THEN
             WRITE(MSG,*) 'Cannot evaluate GFAS mask field "',                  &
                          TRIM(Inst%SpcScalFldNme(N)), '" for species "',      &
                          TRIM(Inst%SpcNames(N)), '"'
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             IF ( ALLOCATED(MaskFld)  ) DEALLOCATE(MaskFld)
             IF ( ALLOCATED(SpcArr3D) ) DEALLOCATE(SpcArr3D)
             RETURN
          ENDIF
          DO L = 1, HcoState%NZ
             SpcArr3D(:,:,L) = SpcArr3D(:,:,L) * MaskFld(:,:)
          ENDDO
          DEALLOCATE(MaskFld)
       ENDIF

       ! Add to HEMCO emissions
       CALL HCO_EmisAdd( HcoState, SpcArr3D, Inst%HcoIDs(N), RC,                &
                         ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          MSG = 'HCO_EmisAdd error: ' //                                        &
                TRIM(HcoState%Spc(Inst%HcoIDs(N))%SpcName)
          CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
          IF ( ALLOCATED(SpcArr3D) ) DEALLOCATE(SpcArr3D)
          RETURN
       ENDIF

    ENDDO !N

    IF ( ALLOCATED(SpcArr3D) ) DEALLOCATE(SpcArr3D)

    ! Cleanup
    Inst => NULL()

    ! Leave w/ success
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_GFAS_Run
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_GFAS_Init
!
! !DESCRIPTION: Subroutine HCOX\_GFAS\_Init initializes the GFAS extension.
!
! !INTERFACE:
!
  SUBROUTINE HCOX_GFAS_Init( HcoState, ExtName, ExtState, RC )
!
! !USES:
!
    USE HCO_STATE_MOD,    ONLY : HCO_GetExtHcoID
    USE HCO_ExtList_Mod,  ONLY : GetExtNr, GetExtOpt
    USE HCO_ExtList_Mod,  ONLY : GetExtSpcVal
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN   )  :: ExtName     ! Extension name
    TYPE(Ext_State),  POINTER        :: ExtState    ! Options object
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState    ! HEMCO state object
    INTEGER,          INTENT(INOUT)  :: RC          ! Return status
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: ExtNr, AS, N
    INTEGER,           ALLOCATABLE :: HcoIDs(:)
    CHARACTER(LEN=31), ALLOCATABLE :: SpcNames(:)
    CHARACTER(LEN=61), ALLOCATABLE :: SpcScalFldNme(:)
    REAL(sp),          ALLOCATABLE :: SpcScal(:)
    CHARACTER(LEN=255)             :: MSG, LOC
    CHARACTER(LEN=31)              :: RefName
    LOGICAL                        :: FOUND
    TYPE(MyInst),      POINTER     :: Inst

    !=================================================================
    ! HCOX_GFAS_Init begins here
    !=================================================================
    LOC = 'HCOX_GFAS_Init (HCOX_GFAS_MOD.F90)'

    ! Extension Nr (returns 0 if extension is not in config -> not enabled)
    ExtNr = GetExtNr( HcoState%Config%ExtList, TRIM(ExtName) )
    IF ( ExtNr <= 0 ) RETURN

    ! Enter
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR enter GFAS', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Create instance
    Inst => NULL()
    CALL InstCreate ( ExtNr, ExtState%GFAS, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR ( 'Cannot create GFAS instance', RC, THISLOC=LOC )
       RETURN
    ENDIF

    !-----------------------------------------------------------------
    ! Read reference species name (default: CO)
    !-----------------------------------------------------------------
    RefName = ''
    CALL GetExtOpt( HcoState%Config, ExtNr, 'Vertical Profile Species',         &
                    OptValChar=RefName, FOUND=FOUND, RC=RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error reading Vertical Profile Species option', RC,    &
                       THISLOC=LOC )
       RETURN
    ENDIF
    IF ( .NOT. FOUND .OR. LEN_TRIM(RefName) == 0 ) THEN
       RefName = 'CO'
    ENDIF
    Inst%RefSpcName = ADJUSTL(RefName)

    !-----------------------------------------------------------------
    ! Read species list (HEMCO species IDs and names)
    !-----------------------------------------------------------------
    CALL HCO_GetExtHcoID( HcoState, Inst%ExtNr, HcoIDs, SpcNames, Inst%nSpc,    &
                          RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Error in HCO_GetExtHcoID (GFAS)', RC, THISLOC=LOC )
        RETURN
    ENDIF
    IF ( Inst%nSpc == 0 ) THEN
       MSG = 'No GFAS species specified in HEMCO configuration file'
       CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    ALLOCATE( Inst%HcoIDs(Inst%nSpc), Inst%SpcNames(Inst%nSpc), STAT=AS )
    IF ( AS /= 0 ) THEN
       CALL HCO_ERROR( 'Cannot allocate HcoIDs/SpcNames', RC, THISLOC=LOC )
       RETURN
    ENDIF
    Inst%HcoIDs   = HcoIDs
    Inst%SpcNames = SpcNames
    DEALLOCATE( HcoIDs, SpcNames )

    !-----------------------------------------------------------------
    ! Read per-species scaling factors and mask field names
    !-----------------------------------------------------------------
    CALL GetExtSpcVal( HcoState%Config, Inst%ExtNr, Inst%nSpc,                  &
                       Inst%SpcNames, 'Scaling', 1.0_sp, SpcScal, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Error reading GFAS Scaling_* values', RC,             &
                        THISLOC=LOC )
        RETURN
    ENDIF

    CALL GetExtSpcVal( HcoState%Config, Inst%ExtNr, Inst%nSpc,                  &
                       Inst%SpcNames, 'ScaleField', HCOX_NOSCALE,              &
                       SpcScalFldNme, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Error reading GFAS ScaleField_* values', RC,          &
                        THISLOC=LOC )
        RETURN
    ENDIF

    ALLOCATE( Inst%SpcScal(Inst%nSpc), Inst%SpcScalFldNme(Inst%nSpc),           &
              STAT=AS )
    IF ( AS /= 0 ) THEN
       CALL HCO_ERROR( 'Cannot allocate SpcScal/SpcScalFldNme', RC,            &
                       THISLOC=LOC )
       RETURN
    ENDIF
    Inst%SpcScal       = SpcScal
    Inst%SpcScalFldNme = SpcScalFldNme
    DEALLOCATE( SpcScal, SpcScalFldNme )

    !-----------------------------------------------------------------
    ! Resolve the reference species index in the species list
    ! (RefIdx = -1 if the reference is not in the emitted list,
    !  which is allowed: its 3D is still read for the profile)
    !-----------------------------------------------------------------
    Inst%RefIdx = -1
    DO N = 1, Inst%nSpc
       IF ( TRIM(Inst%SpcNames(N)) == TRIM(Inst%RefSpcName) ) THEN
          Inst%RefIdx = N
          EXIT
       ENDIF
    ENDDO

    !-----------------------------------------------------------------
    ! Allocate work arrays
    !-----------------------------------------------------------------
    ALLOCATE( Inst%Ref3D (HcoState%NX, HcoState%NY, HcoState%NZ),              &
              Inst%Frac3D(HcoState%NX, HcoState%NY, HcoState%NZ),              &
              Inst%Spc2D (HcoState%NX, HcoState%NY),                           &
              STAT=AS )
    IF ( AS /= 0 ) THEN
       CALL HCO_ERROR( 'Cannot allocate GFAS work arrays', RC, THISLOC=LOC )
       RETURN
    ENDIF
    Inst%Ref3D  = 0.0_hp
    Inst%Frac3D = 0.0_hp
    Inst%Spc2D  = 0.0_hp

    !-----------------------------------------------------------------
    ! Verbose log
    !-----------------------------------------------------------------
    IF ( HcoState%amIRoot ) THEN

       MSG = 'Using HEMCO extension: GFAS (3D biomass-burning emissions)'
       CALL HCO_Msg( MSG, sep1='-', LUN=HcoState%Config%hcoLogLUN )

       WRITE(MSG,*) '   - Vertical profile species : ', TRIM(Inst%RefSpcName)
       CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN )
       IF ( Inst%RefIdx > 0 ) THEN
          WRITE(MSG,*) '     (also emitted as model species, idx=',            &
                       Inst%RefIdx, ')'
          CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN )
       ELSE
          MSG = '     (reference profile only; not in emitted species list)'
          CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN )
       ENDIF
       WRITE(MSG,*) '   - Number of species        : ', Inst%nSpc
       CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN )
       MSG = '   - Species (Name : HcoID : Scaling : ScaleField):'
       CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN )
       DO N = 1, Inst%nSpc
          WRITE(MSG,'(A,A,A,I5,A,F8.3,A,A)')                                   &
             '     ', TRIM(Inst%SpcNames(N)), ' : ', Inst%HcoIDs(N),           &
             ' : ', Inst%SpcScal(N), ' : ', TRIM(Inst%SpcScalFldNme(N))
          CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN )
       ENDDO
    ENDIF

    ! Cleanup
    Inst => NULL()

    ! Return w/ success
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_GFAS_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_GFAS_Final
!
! !DESCRIPTION: Subroutine HCOX\_GFAS\_Final finalizes the GFAS extension.
!
! !INTERFACE:
!
  SUBROUTINE HCOX_GFAS_Final( ExtState )
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    CALL InstRemove( ExtState%GFAS )

  END SUBROUTINE HCOX_GFAS_Final
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read2DField
!
! !DESCRIPTION: Helper to read a 2D HEMCO field by name, with a clear
! debug message on failure. If AddTo is .TRUE., the read value is added
! to Arr2D; otherwise Arr2D is overwritten with the read value.
!
! !INTERFACE:
!
  SUBROUTINE Read2DField( HcoState, FieldName, Arr2D, RC, AddTo,                &
                          ExtNr, SpcName )
!
! !USES:
!
    USE HCO_Calc_Mod, ONLY : HCO_EvalFld
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN   ) :: FieldName
    LOGICAL,          INTENT(IN   ) :: AddTo
    INTEGER,          INTENT(IN   ) :: ExtNr
    CHARACTER(LEN=*), INTENT(IN   ) :: SpcName
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER       :: HcoState
    REAL(hp),         INTENT(INOUT) :: Arr2D(:,:)
    INTEGER,          INTENT(INOUT) :: RC
!EOP
!------------------------------------------------------------------------------
!BOC
    REAL(hp), ALLOCATABLE :: Tmp2D(:,:)
    LOGICAL               :: FOUND
    INTEGER               :: AS
    CHARACTER(LEN=255)    :: MSG, LOC

    LOC = 'Read2DField (HCOX_GFAS_MOD.F90)'

    ALLOCATE( Tmp2D(SIZE(Arr2D,1), SIZE(Arr2D,2)), STAT=AS )
    IF ( AS /= 0 ) THEN
       CALL HCO_ERROR( 'Cannot allocate Tmp2D in Read2DField', RC,             &
                       THISLOC=LOC )
       RETURN
    ENDIF
    Tmp2D = 0.0_hp

    FOUND = .FALSE.
    CALL HCO_EvalFld( HcoState, TRIM(FieldName), Tmp2D, RC, FOUND=FOUND )
    IF ( RC /= HCO_SUCCESS .OR. .NOT. FOUND ) THEN
       WRITE(MSG,*) 'GFAS 2D field "' // TRIM(FieldName) //                    &
                    '" for species "' // TRIM(SpcName) //                      &
                    '" not found in HEMCO emissions list (ExtNr=',             &
                    ExtNr, '). Please add a base-emissions line for this ',    &
                    'field, or remove the species from the GFAS species list.'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       IF ( ALLOCATED(Tmp2D) ) DEALLOCATE(Tmp2D)
       RETURN
    ENDIF

    IF ( AddTo ) THEN
       Arr2D = Arr2D + Tmp2D
    ELSE
       Arr2D = Tmp2D
    ENDIF

    DEALLOCATE(Tmp2D)
    RC = HCO_SUCCESS

  END SUBROUTINE Read2DField
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InstGet
!
! !DESCRIPTION: Subroutine InstGet returns a pointer to the desired instance.
!
! !INTERFACE:
!
  SUBROUTINE InstGet ( Instance, Inst, RC, PrevInst )
!
! !INPUT PARAMETERS:
!
    INTEGER                             :: Instance
    TYPE(MyInst),     POINTER           :: Inst
    INTEGER                             :: RC
    TYPE(MyInst),     POINTER, OPTIONAL :: PrevInst
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MyInst), POINTER  :: PrvInst

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

    IF ( PRESENT(PrevInst) ) PrevInst => PrvInst

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
!
! !INTERFACE:
!
  SUBROUTINE InstCreate ( ExtNr, Instance, Inst, RC )
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
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MyInst), POINTER  :: TmpInst
    INTEGER                :: nnInst

    Inst => NULL()

    TmpInst => AllInst
    nnInst = 0
    DO WHILE ( ASSOCIATED(TmpInst) )
       nnInst  =  nnInst + 1
       TmpInst => TmpInst%NextInst
    END DO

    ALLOCATE(Inst)
    Inst%Instance = nnInst + 1
    Inst%ExtNr    = ExtNr

    Inst%NextInst => AllInst
    AllInst       => Inst

    Instance = Inst%Instance

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
! !DESCRIPTION: Subroutine InstRemove deallocates and removes an instance.
!
! !INTERFACE:
!
  SUBROUTINE InstRemove ( Instance )
!
! !INPUT PARAMETERS:
!
    INTEGER                         :: Instance
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER                     :: RC
    TYPE(MyInst), POINTER       :: PrevInst
    TYPE(MyInst), POINTER       :: Inst

    PrevInst => NULL()
    Inst     => NULL()

    CALL InstGet ( Instance, Inst, RC, PrevInst=PrevInst )

    IF ( ASSOCIATED(Inst) ) THEN

       ! Deallocate fields before removing from list
       IF ( ASSOCIATED(Inst%SpcNames     ) ) DEALLOCATE(Inst%SpcNames     )
       IF ( ASSOCIATED(Inst%HcoIDs       ) ) DEALLOCATE(Inst%HcoIDs       )
       IF ( ASSOCIATED(Inst%SpcScal      ) ) DEALLOCATE(Inst%SpcScal      )
       IF ( ASSOCIATED(Inst%SpcScalFldNme) ) DEALLOCATE(Inst%SpcScalFldNme)
       IF ( ASSOCIATED(Inst%Ref3D        ) ) DEALLOCATE(Inst%Ref3D        )
       IF ( ASSOCIATED(Inst%Frac3D       ) ) DEALLOCATE(Inst%Frac3D       )
       IF ( ASSOCIATED(Inst%Spc2D        ) ) DEALLOCATE(Inst%Spc2D        )

       Inst%SpcNames      => NULL()
       Inst%HcoIDs        => NULL()
       Inst%SpcScal       => NULL()
       Inst%SpcScalFldNme => NULL()
       Inst%Ref3D         => NULL()
       Inst%Frac3D        => NULL()
       Inst%Spc2D         => NULL()

       ! Pop off the list
       IF ( ASSOCIATED(PrevInst) ) THEN
          PrevInst%NextInst => Inst%NextInst
       ELSE
          AllInst => Inst%NextInst
       ENDIF
       DEALLOCATE(Inst)
       Inst => NULL()
    ENDIF

    PrevInst => NULL()
    Inst     => NULL()

  END SUBROUTINE InstRemove
!EOC

END MODULE HCOX_GFAS_MOD
