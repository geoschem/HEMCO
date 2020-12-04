!------------------------------------------------------------------------------ 
!                   Harmonized Emissions Component (HEMCO)                    ! 
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_interface_common
!
! !DESCRIPTION: Module HCO\_Interface\_Common defines common utilities for interfacing
!  with HEMCO from other models. The common toolbox should be present for all
!  models interacting with HEMCO.
!\\
!\\
! !INTERFACE:
!
MODULE HCO_Interface_Common
!
! !USES:
!
  USE HCO_Error_Mod
  USE HCO_State_Mod,  ONLY: HCO_State
  USE HCOX_State_Mod, ONLY: Ext_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: SetHcoTime
  PUBLIC  :: GetHcoVal
  PUBLIC  :: GetHcoDiagn
#ifdef FALSE
  PUBLIC  :: CALC_EMS_SF_ADJ
#endif
!
! !REMARKS:
!  These utilities were mostly migrated from GEOS-Chem HCO_Interface_Mod.
!  All functions now accept as input the HEMCO state (HcoState) as there may
!  be multiple instances and all variables should not be inferred.
!
! !REVISION HISTORY:
!  12 Mar 2020 - H.P. Lin    - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !PRIVATE MODULE VARIABLES:
!

!
! !DEFINED PARAMETERS:
!

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                    Harmonized Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetHcoTime
!
! !DESCRIPTION: SUBROUTINE SetHcoTime sets the current simulation
! datetime in HcoState.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE SetHcoTime( HcoState,  ExtState,   year,   month,  &
                         day,       dayOfYr,    hour,   minute, &
                         second,    IsEmisTime, RC             )
!
! !USES:
!
    USE HCO_CLOCK_MOD, ONLY : HcoClock_Set
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER       :: HcoState    ! HEMCO state object
    TYPE(Ext_State), POINTER       :: ExtState    ! HEMCO extensions state object
    INTEGER,         INTENT(IN)    :: year        ! UTC year 
    INTEGER,         INTENT(IN)    :: month       ! UTC month
    INTEGER,         INTENT(IN)    :: day         ! UTC day
    INTEGER,         INTENT(IN)    :: dayOfYr     ! UTC day of year
    INTEGER,         INTENT(IN)    :: hour        ! UTC hour
    INTEGER,         INTENT(IN)    :: minute      ! UTC minute
    INTEGER,         INTENT(IN)    :: second      ! UTC second
    LOGICAL,         INTENT(IN)    :: IsEmisTime  ! Time for emissions?
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(INOUT) :: RC
!
! !REVISION HISTORY:
!  23 Oct 2012 - C. Keller - Initial Version
!  12 Jan 2015 - C. Keller - Added argument TimeForEmis
!  12 Mar 2020 - H.P. Lin  - Changed to accept external arguments, renamed to
!                            be consistent with GIGC\_Chunk\_Run argument order
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    !=================================================================
    ! SetHcoTime begins here
    !=================================================================

    CALL HcoClock_Set ( HcoState, year, month, day, hour, minute, &
                        second, dayOfYr, IsEmisTime=IsEmisTime, RC=RC )

  END SUBROUTINE SetHcoTime
!EOC
!------------------------------------------------------------------------------
!                    Harmonized Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: GetHcoVal
!
! !DESCRIPTION: Subroutine GetHcoVal is a wrapper routine to return an
! emission (kg/m2/s) or deposition (1/s) value from the HEMCO state object
! for a given species at position I, J, L.
! A value of zero is returned if no HEMCO species is defined for the given
! tracer, and the output parameter Found is set to false.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GetHcoVal ( HcoState, ExtState, &
                         HcoID, I, J, L, Found, Emis, Dep )
!
! !USES:
!
!
! !INPUT ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState    ! HEMCO state object
    TYPE(Ext_State),    POINTER        :: ExtState    ! HEMCO extensions state object
    INTEGER,            INTENT(IN   )  :: HcoID       ! HEMCO tracer ID
    INTEGER,            INTENT(IN   )  :: I, J, L     ! Position
!
! !OUTPUT ARGUMENTS:
!
    LOGICAL,            INTENT(  OUT)  :: Found       ! Was this tracer ID found?
    REAL(hp), OPTIONAL, INTENT(  OUT)  :: Emis        ! Emissions  [kg/m2/s]
    REAL(hp), OPTIONAL, INTENT(  OUT)  :: Dep         ! Deposition [1/s]
!
! !REMARKS:
!  Tracer ID passed is now always HcoID. This is because GEOS-Chem uses the same
!  HcoID = TrcID. If your model is not, please do a mapping internally in the
!  interface.
!
!  Note that HEMCO expects the grid to be 3-D in IJL indices. If your model (e.g. CAM)
!  stores data in 2-D columns (K, I) where I is a chunked set of columns,
!  one dummy dimension needs to be added. Refer to ESCOMP/HEMCO_CESM (hplin, 3/12/20)
!
! !REVISION HISTORY:
!  20 Oct 2014 - C. Keller - Initial Version
!  12 Dec 2014 - M. Yannetti - Changed real(dp) to real(hp)
!  12 Mar 2020 - H.P. Lin  - Tracer IDs are always HcoID. See remarks.
!EOP
!------------------------------------------------------------------------------
!BOC
    !=================================================================
    ! GetHcoVal begins here
    !=================================================================

    ! Init
    FOUND = .FALSE.
    IF ( PRESENT(Emis) ) Emis = 0.0_hp
    IF ( PRESENT(Dep ) ) Dep  = 0.0_hp

    ! If HEMCO species exists, get value from HEMCO state
    IF ( HcoID > 0 ) THEN
       IF ( PRESENT(Emis) ) THEN
          IF ( ASSOCIATED(HcoState%Spc(HcoID)%Emis%Val) ) THEN
             Emis  = HcoState%Spc(HcoID)%Emis%Val(I,J,L)
             FOUND = .TRUE.
          ENDIF
       ENDIF
       IF ( PRESENT(Dep) ) THEN
          IF ( ASSOCIATED(HcoState%Spc(HcoID)%Depv%Val) ) THEN
             Dep   = HcoState%Spc(HcoID)%Depv%Val(I,J)
             FOUND = .TRUE.
          ENDIF
       ENDIF
    ENDIF

  END SUBROUTINE GetHcoVal
!EOC
!------------------------------------------------------------------------------
!                    Harmonized Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: GetHcoDiagn
!
! !DESCRIPTION: Subroutine GetHcoDiagn is a convenience wrapper routine to
!  get a HEMCO diagnostics from an external model.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GetHcoDiagn ( HcoState,       ExtState,  DiagnName, &
                           StopIfNotFound, RC,        Ptr2D,     &
                           Ptr3D,          COL,       AutoFill  )
!
! !USES:
!
    USE HCO_TYPES_MOD,      ONLY : DiagnCont
    USE HCO_DIAGN_MOD
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER               :: HcoState       ! HEMCO state object
    TYPE(Ext_State), POINTER               :: ExtState       ! HEMCO extensions state object
    CHARACTER(LEN=*), INTENT(IN)           :: DiagnName      ! Name of diagnostics
    LOGICAL,          INTENT(IN)           :: StopIfNotFound ! Stop if diagnostics
                                                             ! does not exist?
    INTEGER,          INTENT(IN), OPTIONAL :: COL            ! Collection Nr.
    INTEGER,          INTENT(IN), OPTIONAL :: AutoFill       ! AutoFill diagnostics only?
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)        :: RC             ! Error return code
!
! !OUTPUT PARAMETERS:
!
    REAL(sp),         POINTER, OPTIONAL    :: Ptr2D(:,:)     ! Pointer to 2D data
    REAL(sp),         POINTER, OPTIONAL    :: Ptr3D(:,:,:)   ! Pointer to 3D data
!
! !REMARKS:
!
! !REVISION HISTORY:
!  24 Sep 2014 - C. Keller   - Initial version
!  12 Mar 2020 - H.P. Lin    - Moved to HEMCO. Uses HEMCO errors.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                   :: FLAG, LevIDx, PS, AF
    TYPE(DiagnCont), POINTER  :: DgnCont  => NULL()

    ! Strings
    CHARACTER(LEN=255) :: ErrMsg
    CHARACTER(LEN=255) :: ThisLoc

    !=======================================================================
    ! GetHcoDiagn begins here
    !=======================================================================

    ! Initialize
    RC      = HCO_SUCCESS

    ! For error handling
    ErrMsg  = ''

    ! Set collection number
    PS = HcoState%Diagn%HcoDiagnIDManual
    IF ( PRESENT(COL) ) PS = COL

    ! Set AutoFill flag
    AF = -1
    IF ( PRESENT(AutoFill) ) AF = AutoFill

    ! Get diagnostics by name. Search all diagnostics, i.e. both AutoFill
    ! and manually filled diagnostics. Also include those with a manual
    ! output interval.
    CALL Diagn_Get( HcoState, .FALSE., DgnCont, FLAG, RC, &
                    cName=TRIM(DiagnName), AutoFill=AF, COL=PS )

    ! Trap potential errors
    IF ( RC /= HCO_SUCCESS ) THEN
       ErrMsg = 'Error in getting diagnostics: ' // TRIM(DiagnName)
       CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC )
       RETURN
    ENDIF

    IF ( (FLAG /= HCO_SUCCESS) .AND. StopIfNotFound ) THEN
       ErrMsg = 'Cannot get diagnostics for this time stamp: ' //    &
                 TRIM(DiagnName)
       CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC )
       RETURN
    ENDIF

    ! Pass data to output pointer (only if diagnostics defined):
    IF ( FLAG == HCO_SUCCESS ) THEN

       ! 2D pointer
       IF ( PRESENT(Ptr2D) ) THEN

          ! Pass 2D data
          IF ( ASSOCIATED(DgnCont%Arr2D%Val) ) THEN
             Ptr2D => DgnCont%Arr2D%Val

          ! Pass 3D data. Get level index from diagnostics (if set)
          ELSEIF ( ASSOCIATED(DgnCont%Arr3D%Val) ) THEN
             LevIDx = DgnCont%LevIdx
             IF ( LevIdx < 1 ) LevIdx = 1
             Ptr2D => DgnCont%Arr3D%Val(:,:,LevIDx)

          ! Error if no 2D or 3D data available
          ELSE
             ErrMsg = 'no data defined: '// TRIM(DiagnName)
             CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC )
             RETURN
          ENDIF

       ! 3D pointer: must point to 3D data
       ELSEIF ( PRESENT(Ptr3D) ) THEN
          IF ( ASSOCIATED(DgnCont%Arr3D%Val) ) THEN
             Ptr3D => DgnCont%Arr3D%Val
          ELSE
             ErrMsg = 'no 3D data defined: '// TRIM(DiagnName)
             CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC )
             RETURN
          ENDIF

       ! Error otherwise
       ELSE
          ErrMsg = 'Please define output data pointer: ' // TRIM(DiagnName)
          CALL HCO_Error( HcoState%Config%Err, ErrMsg, RC )
          RETURN
       ENDIF
    ENDIF

    ! Free pointer
    DgnCont  => NULL()

    ! Leave with success
    RC = HCO_SUCCESS

  END SUBROUTINE GetHcoDiagn

#ifdef FALSE ! the adjoint-relevant code in here has been moved to 
             ! Hco_CalcEmis in #ifdef ADJOINT blocks so I'm disabling 
             ! this for now
!
! !DESCRIPTION: Subroutine CALC_EMS_SF_ADJ stores adjoint sensitvities to 
! emissions categories
!\\
!\\
! !INTERFACE
!
  SUBROUTINE CALC_EMS_SF_ADJ(SpcId, DT, Input_Opt, State_Chm, State_Diag, RC)
!
! !USES:
!
    USE Input_Opt_Mod,   ONLY : OptInput
    USE State_Chm_Mod,   ONLY : ChmState
    USE State_Diag_Mod,  ONLY : DgnState
    USE HCO_STATE_MOD,    ONLY : HCO_State
    USE HCO_ARR_MOD,      ONLY : HCO_ArrAssert
    USE HCO_DATACONT_MOD, ONLY : ListCont_NextCont
    USE HCO_FILEDATA_MOD, ONLY : FileData_ArrIsDefined
    USE HCO_Calc_Mod,     ONLY : GET_CURRENT_EMISSIONS_ADJ
    USE HCO_Scale_Mod,    ONLY : HCO_ScaleArr
    USE HCO_Diagn_Mod
    USE HCO_Types_Mod


! !INPUT PARAMETERS:
!
    INTEGER,          INTENT(IN   )           :: spcId        ! Which species
    TYPE(OptInput),   INTENT(IN   )           :: Input_Opt    ! Input opts
    REAL(fp),         INTENT(IN   )           :: DT           ! Time step [s]
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ChmState),   INTENT(INOUT)           :: State_Chm    ! Chemistry state 
    TYPE(DgnState),   INTENT(INOUT)           :: State_Diag   ! Diags State
    INTEGER,          INTENT(INOUT)           :: RC           ! Success/Failure
!
! !REVISION HISTORY: 
!  18 May 2020 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Working pointers: list and data container
    TYPE(ListCont), POINTER :: Lct
    TYPE(DataCont), POINTER :: Dct

    ! Temporary emissions arrays
    REAL(hp), TARGET        :: SpcFlx( HcoState%NX, &
                                       HcoState%NY, &
                                       HcoState%NZ   )
    REAL(hp), TARGET        :: CatFlx( HcoState%NX, &
                                       HcoState%NY, &
                                       HcoState%NZ   )
    REAL(hp), TARGET        :: TmpFlx( HcoState%NX, &
                                       HcoState%NY, &
                                       HcoState%NZ   )
    REAL(hp)                :: Mask  ( HcoState%NX, &
                                       HcoState%NY, &
                                       HcoState%NZ   )
    REAL(hp)                :: HirFlx( HcoState%NX, &
                                       HcoState%NY, &
                                       HcoState%NZ   )
    REAL(hp)                :: HirMsk( HcoState%NX, &
                                       HcoState%NY, &
                                       HcoState%NZ   )

    ! Integers
    INTEGER             :: ThisSpc, PrevSpc ! current and previous species ID
    INTEGER             :: ThisCat, PrevCat ! current and previous category 
    INTEGER             :: ThisHir, PrevHir ! current and previous hierarchy 
    INTEGER             :: SpcMin,  SpcMax  ! range of species to be considered 
    INTEGER             :: CatMin,  CatMax  ! range of categories to be considered 
    INTEGER             :: ExtNr            ! Extension Nr to be used 
    INTEGER             :: nI, nJ, nL 
    INTEGER             :: nnSpec, FLAG

    LOGICAL             :: Found, DoDiagn, EOL, UpdateCat, UseConc

    ! For error handling & verbose mode
    CHARACTER(LEN=255)  :: MSG

    ! testing / debugging
    integer :: ix,iy

    !=================================================================
    ! CALC_EMS_SF_ADJ begins here!
    !=================================================================
    ! Initialize
    Lct => NULL()
    Dct => NULL()


    !-----------------------------------------------------------------
    ! Initialize variables 
    !-----------------------------------------------------------------

    ! Initialize
    SpcFlx(:,:,:)    = 0.0_hp
    CatFlx(:,:,:)    = 0.0_hp
    HirFlx(:,:,:)    = 0.0_hp
    HirMsk(:,:,:)    = 0.0_hp
    PrevSpc          = -1
    PrevHir          = -1
    PrevCat          = -1
    nnSpec           = 0

    ! Pass emission grid dimensions
    nI = HcoState%NX
    nJ = HcoState%NY
    nL = HcoState%NZ

    ! Pass calculation options
    SpcMin  = HcoState%Options%SpcMin        !Lower species ID
    SpcMax  = HcoState%Options%SpcMax        !Upper species ID
    CatMin  = HcoState%Options%CatMin        !Lower emission category
    CatMax  = HcoState%Options%CatMax        !Upper emission category
    ExtNr   = HcoState%Options%ExtNr         !Extension number
    DoDiagn = HcoState%Options%AutoFillDiagn !Write AutoFill diagnostics?

    ! I am not sure what do about units here, so set this to false for now
    UseConc = .false.

    ! Enter routine 
    CALL HCO_ENTER (HcoState%Config%Err,'CALC_EMS_SF_ADJ (HCO_INTERFACE_MOD.F90)', RC )
    IF(RC /= HCO_SUCCESS) RETURN

    ! Verbose mode 
    IF ( HCO_IsVerb(HcoState%Config%Err,2) ) THEN
       WRITE (MSG, *) 'Run HEMCO calculation w/ following options:'
       CALL HCO_MSG ( HcoState%Config%Err, MSG )
       WRITE (MSG, "(A20,I5)")    'Extension number:', ExtNr 
       CALL HCO_MSG ( HcoState%Config%Err, MSG )
       WRITE (MSG, "(A20,I5,I5)") 'Tracer range:', SpcMin, SpcMax
       CALL HCO_MSG ( HcoState%Config%Err, MSG )
       WRITE (MSG, "(A20,I5,I5)") 'Category range:', CatMin, CatMax
       CALL HCO_MSG ( HcoState%Config%Err, MSG )
       WRITE (MSG, *) 'Auto diagnostics: ', DoDiagn
       CALL HCO_MSG ( HcoState%Config%Err, MSG )
    ENDIF

    !=================================================================
    ! Walk through all containers of EmisList and determine the
    ! emissions for all containers that qualify for calculation.
    ! The containers in EmisList are sorted by species, category and 
    ! hierarchy. This enables a straightforward, piece-by-piece 
    ! assembly of the final emission array (start with lowest
    ! hierarchy emissions, then overwrite piece-by-piece with higher
    ! hierarchy values).
    !=================================================================

    ! Point to the head of the emissions linked list
    EOL = .FALSE. ! End of list
    Lct => NULL()
    CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG ) 

    ! Do until end of EmisList (==> loop over all emission containers) 
    DO
       ! Have we reached the end of the list? 
       IF ( FLAG /= HCO_SUCCESS ) THEN 
          EOL = .TRUE.
       ELSE
          EOL = .FALSE.
       ENDIF

       ! ------------------------------------------------------------
       ! Select container and update all working variables & arrays.
       ! ------------------------------------------------------------
       IF ( .NOT. EOL ) THEN

          ! Dct is the current data container 
          Dct => Lct%Dct
          if ( HCO_IsVerb(HcoState%Config%Err,2) ) THEN
             WRITE(MSG,*) ' CALC_EMS_SF_ADJ processing container ', trim(Dct%cName)
             CALL HCO_MSG(HcoState%Config%Err, MSG)
          endif
          
          ! Check if this is a base field 
          IF ( Dct%DctType /= HCO_DCTTYPE_BASE ) THEN
             CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG )
             CYCLE
          ENDIF

          ! Sanity check: Make sure this container holds data.
          ! 'Empty' containers are possible if the simulation time
          ! is outside of the specified data time range and time
          ! slice cycling is deactivated (CycleFlag > 1). 
          IF( .NOT. FileData_ArrIsDefined(Lct%Dct%Dta) ) THEN
             CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG )
             CYCLE
          ENDIF

          ! Check if this is the specified extension number
          IF ( Dct%ExtNr /= ExtNr ) THEN 
             CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG )
             CYCLE
          ENDIF

          ! Advance to next container if the species ID is outside 
          ! the specified species range (SpcMin - SpcMax). Consider 
          ! all species above SpcMin if SpcMax is negative!
          IF( (  Dct%HcoID < SpcMin                     ) .OR. &
              ( (Dct%HcoID > SpcMax) .AND. (SpcMax > 0) ) ) THEN
             CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG )
             CYCLE
          ENDIF

          ! Advance to next emission field if the emission category of 
          ! the current container is outside of the specified species 
          ! range (CatMin - CatMax). Consider all categories above CatMin
          ! if CatMax is negative!
          IF( (  Dct%Cat < CatMin                     ) .OR. &
              ( (Dct%Cat > CatMax) .AND. (CatMax > 0) ) ) THEN
             CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG )
             CYCLE
          ENDIF

          ! Check if this container holds data in the desired unit format,
          ! i.e. concentration data if UseConc is enabled, emission data
          ! otherwise.
          IF ( UseConc .NEQV. Dct%Dta%IsConc ) THEN
             CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG )
             CYCLE
          ENDIF 

          ! Check if this is our species
          IF ( Dct%HcoID /= SpcId ) THEN
             CALL ListCont_NextCont ( HcoState%EmisList, Lct, FLAG )
             CYCLE
          ENDIF 

          ! Update working variables
          ThisSpc = Dct%HcoID
          ThisCat = Dct%Cat
          ThisHir = Dct%Hier

       ! If end of list, use dummy values for ThisSpc, ThisCat and ThisHir
       ! to make sure that emissions are added to HEMCO in the section
       ! below!
       ELSE
          ThisSpc = -1
          ThisCat = -1
          ThisHir = -1
       ENDIF  

       !--------------------------------------------------------------------
       ! Before computing emissions of current data container make sure that
       ! emissions of previous container are properly archived.
       !--------------------------------------------------------------------

       ! Add emissions on hierarchy level to the category flux array. Do
       ! this only if this is a new species, a new category or a new 
       ! hierarchy level.
       ! Note: no need to add to diagnostics because hierarchy level 
       ! diagnostics are filled right after computing the emissions of
       ! a given data container (towards the end of the DO loop).
       IF ( (ThisHir /= PrevHir) .OR. &
            (ThisSpc /= PrevSpc) .OR. &
            (ThisCat /= PrevCat)        ) THEN

          ! Add hierarchy level emissions to category array over the
          ! covered regions.
          CatFlx = ( (1.0_hp - HirMsk) * CatFlx ) + HirFlx

          ! Reset
          HirFlx = 0.0_hp
          HirMsk = 0.0_hp
       ENDIF

       !--------------------------------------------------------------------
       ! If this is a new species or category, pass the previously collected 
       ! emissions to the species array. Update diagnostics at category level.
       ! Skip this step for first species, i.e. if PrevSpc is still -1. 
       !--------------------------------------------------------------------
       UpdateCat = .FALSE.
       IF ( ThisCat /= PrevCat ) UpdateCat = .TRUE.
       IF ( ThisSpc /= PrevSpc ) UpdateCat = .TRUE.
       IF ( PrevCat <= 0 .OR. PrevSpc <= 0 ) UpdateCat = .FALSE.
       IF ( UpdateCat ) THEN

          ! CatFlx holds the emissions for this category. Pass this to 
          ! the species array SpcFlx.
          SpcFlx(:,:,:) = SpcFlx(:,:,:) + CatFlx(:,:,:)

          IF (Input_Opt%IS_FD_SPOT_THIS_PET) THEN
             WRITE(*,*) PrevCat, ' CatFlx(IFD,JFD) = ', &
                  SUM(CatFlx(Input_Opt%IFD,Input_Opt%JFD,:))
          ENDIF

          ! verbose 
          IF ( HCO_IsVerb(HcoState%Config%Err,3) ) THEN
             WRITE(MSG,*) 'Added category emissions to species array: '
             CALL HCO_MSG(HcoState%Config%Err,MSG)
             WRITE(MSG,*) 'Species       : ', PrevSpc
             CALL HCO_MSG(HcoState%Config%Err,MSG)
             WRITE(MSG,*) 'Category      : ', PrevCat
             CALL HCO_MSG(HcoState%Config%Err,MSG)
             WRITE(MSG,*) 'Cat. emissions: ', SUM(CatFlx) 
             CALL HCO_MSG(HcoState%Config%Err,MSG)
             WRITE(MSG,*) 'Spc. emissions: ', SUM(SpcFlx) 
             CALL HCO_MSG(HcoState%Config%Err,MSG)
          ENDIF
          CatFlx(:,:,:) = DT * State_Chm%SpeciesAdj(:,:,:, SpcId) * CatFlx(:,:,:) 
          ! verbose 
          IF ( HCO_IsVerb(HcoState%Config%Err,3) ) THEN
             WRITE(MSG,*) 'Cat. emissions: ', SUM(CatFlx) 
             CALL HCO_MSG(HcoState%Config%Err,MSG)
          ENDIF
          IF (Input_Opt%IS_FD_SPOT_THIS_PET) THEN
             WRITE(*,*) PrevCat, ' CatFlxAdj(IFD,JFD) = ', &
                  SUM(CatFlx(Input_Opt%IFD,Input_Opt%JFD,:))
          ENDIF

          ! Add category emissions to diagnostics at category level
          ! (only if defined in the diagnostics list).
          IF ( Diagn_AutoFillLevelDefined(HcoState%Diagn,3) .AND. DoDiagn ) THEN 
             CALL Diagn_Update( HcoState, ExtNr=ExtNr,   &
                                Cat=PrevCat, Hier=-1,  HcoID=PrevSpc, &
                                AutoFill=1,  Array3D=CatFlx,          &
                                COL=HcoState%Diagn%HcoDiagnIDAdjoint, RC=RC ) 
             IF ( RC /= HCO_SUCCESS ) RETURN
          ENDIF

          ! Reset CatFlx array and the previously used hierarchy 
          ! ==> Emission hierarchies are only important within the 
          ! same category, hence always start over at lowest hierarchy
          ! when entering a new category.
          CatFlx(:,:,:)  = 0.0_hp
          PrevHir        = -1
       ENDIF

       !--------------------------------------------------------------------
       ! If this is a new species, pass previously calculated emissions
       ! to the final emissions array in HcoState. 
       ! Update diagnostics at extension number level. 
       ! Don't do before first emission calculation, i.e. if PrevSpc 
       ! is still the initialized value of -1!
       !--------------------------------------------------------------------
       IF ( ThisSpc /= PrevSpc .AND. PrevSpc > 0 ) THEN

          IF (Input_Opt%IS_FD_SPOT_THIS_PET) THEN
             WRITE(*,*) PrevCat, ' SpcFlx(IFD,JFD) = ', &
                  SUM(SpcFlx(Input_Opt%IFD,Input_Opt%JFD,:))
          ENDIF
          ! testing only
          IF ( HCO_IsVerb(HcoState%Config%Err,3) ) THEN
             WRITE(MSG,*) 'Added total emissions to output array: '
             CALL HCO_MSG(HcoState%Config%Err,MSG)
             WRITE(MSG,*) 'Species: ', PrevSpc
             CALL HCO_MSG(HcoState%Config%Err,MSG)
             WRITE(MSG,*) 'SpcFlx : ', SUM(SpcFlx)
             CALL HCO_MSG(HcoState%Config%Err,MSG)
          ENDIF

          ! Add to diagnostics at extension number level. 
          ! The same diagnostics may be updated multiple times during 
          ! the same time step, continuously adding emissions to it.
          IF ( Diagn_AutoFillLevelDefined(HcoState%Diagn,2) .AND. DoDiagn ) THEN 
             CALL Diagn_Update(HcoState,  ExtNr=ExtNr,  &
                               Cat=-1,    Hier=-1,  HcoID=PrevSpc, &
                               AutoFill=1,Array3D=SpcFlx,          &
                               COL=HcoState%Diagn%HcoDiagnIDAdjoint, RC=RC ) 
             IF ( RC /= HCO_SUCCESS ) RETURN
          ENDIF

          ! Reset arrays and previous hierarchy. 
          SpcFlx(:,:,:)  =  0.0_hp
          PrevCat        =  -1
          PrevHir        =  -1
       ENDIF

       !--------------------------------------------------------------------
       ! Exit DO loop here if end of list
       !--------------------------------------------------------------------
       IF ( EOL ) EXIT

       !--------------------------------------------------------------------
       ! Update/archive information on species level if needed
       !--------------------------------------------------------------------
       IF ( ThisSpc /= PrevSpc .AND. ThisSpc > 0 ) THEN

          ! Update number of species for which emissions have been
          ! calculated. 
          nnSpec = nnSpec + 1

          ! verbose mode
          IF ( HCO_IsVerb(HcoState%Config%Err,2) ) THEN
             WRITE(MSG,*) 'Calculating emissions for species ', &
                           TRIM(HcoState%Spc(ThisSpc)%SpcName)
             CALL HCO_MSG( HcoState%Config%Err, MSG, SEP1='-', SEP2='-' )
          ENDIF
       ENDIF
       
       ! verbose mode
       IF ( HCO_IsVerb(HcoState%Config%Err,2) ) THEN
          WRITE(MSG,*) 'Dct has  ', &
               Dct%Dta%SpaceDim, ' dimensions.'
          CALL HCO_MSG( HcoState%Config%Err, MSG, SEP1='-', SEP2='-' )
       ENDIF
       !--------------------------------------------------------------------
       ! Get current emissions and write into TmpFlx array. The array Mask
       ! denotes all valid grid boxes for this inventory.
       !--------------------------------------------------------------------
       TmpFlx(:,:,:) = 0.0_hp 
       CALL GET_CURRENT_EMISSIONS_ADJ( HcoState, Dct,    & 
                                   nI, nJ, nL, TmpFlx,   Mask, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN

       ! Eventually add universal scale factor
       CALL HCO_ScaleArr( HcoState, ThisSpc, TmpFlx, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN

       ! Check for negative values according to the corresponding setting
       ! in the configuration file: 2 means allow negative values, 1 means
       ! set to zero and prompt a warning, else return with error.
       IF ( HcoState%Options%NegFlag /= 2 ) THEN

          IF ( ANY(TmpFlx < 0.0_hp) ) THEN

             ! Set to zero and prompt warning
             IF ( HcoState%Options%NegFlag == 1 ) THEN
                WHERE ( TmpFlx < 0.0_hp ) TmpFlx = 0.0_hp
                MSG = 'Negative emissions set to zero: '// TRIM(Dct%cName)
                CALL HCO_WARNING( HcoState%Config%Err, MSG, RC )

             ! Return with error
             ELSE
                MSG = 'Negative emissions in: '// TRIM(Dct%cName) // '. ' // &
                'To allow negatives, edit settings in the configuration file.'
                CALL HCO_ERROR( HcoState%Config%Err, MSG, RC )
                RETURN
             ENDIF
          ENDIF
       ENDIF

       ! ------------------------------------------------------------
       ! Collect all emissions of the same category (and species) on
       ! the hierarchy level into array HirFlx. HirMsk contains the
       ! combined covered region. That is, if there are two regional
       ! inventories with the same hierarchy HirMsk will cover both
       ! of these regions.
       ! The specified field hierarchies determine whether the
       ! temporary emissions are added (if hierarchy is the same
       ! as the previously used hierarchy), or if they overwrite the 
       ! previous values in HirFlx (if hierarchy is higher than the 
       ! previous hierarchy).
       ! ------------------------------------------------------------

       ! Add emissions to the hierarchy array HirFlx if this hierarchy
       ! is the same as previous hierarchy
       IF ( ThisHir == PrevHir ) THEN
          HirFlx = HirFlx + TmpFlx
          HirMsk = HirMsk + Mask

          ! Make sure mask values do not exceed 1.0 
          WHERE(HirMsk > 1.0 ) HirMsk = 1.0

       ! If hierarchy is larger than those of the previously used
       ! fields, overwrite HirFlx with new values. 
       ELSE

          HirFlx = TmpFlx
          HirMsk = Mask

       ENDIF

       ! Update diagnostics at hierarchy level. Make sure that only 
       ! positive values are used.
       ! The same diagnostics may be updated multiple times 
       ! during the same time step, continuously adding
       ! emissions to it.
       ! Now remove PosOnly flag. TmpFlx is initialized to zero, so it's 
       ! ok to keep negative values (ckeller, 7/12/15).
       IF ( Diagn_AutoFillLevelDefined(HcoState%Diagn,4) .AND. DoDiagn ) THEN 
          CALL Diagn_Update( HcoState,       ExtNr=ExtNr,   &
                             Cat=ThisCat,Hier=ThisHir,   HcoID=ThisSpc, &
                             !AutoFill=1, Array3D=TmpFlx, PosOnly=.TRUE.,&
                             AutoFill=1, Array3D=TmpFlx, &
                             COL=-1, RC=RC ) 
          IF ( RC /= HCO_SUCCESS ) RETURN
       ENDIF

       ! Update previously used species, category and hierarchy
       PrevSpc = ThisSpc
       PrevCat = ThisCat
       PrevHir = ThisHir
 
       ! Advance to next emission container
       CALL ListCont_NextCont( HcoState%EmisList, Lct, FLAG )

    ENDDO ! Loop over EmisList

    ! Make sure internal pointers are nullified 
    Lct    => NULL()
    Dct    => NULL()

    ! Leave w/ success
    CALL HCO_LEAVE ( HcoState%Config%Err, RC )

  END SUBROUTINE CALC_EMS_SF_ADJ
#endif
!EOC
END MODULE HCO_Interface_Common
