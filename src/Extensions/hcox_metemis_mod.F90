!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_metemis_mod.F90
!
! !DESCRIPTION: Module HCOX\_METEMIS\_MOD contains routines to
! compute mobile souce emissions based on inputs from the U.S. EPA MOVES 
! model and associated 2-meter temperature dependence look-up table. 
! This code is adapted from the MetEmis codes in U.S. EPA CMAQ Version 5.3.1.
!\\
!\\
!Mobile emissions from the on-road and off-network (e.g., vehicle start-up, 
! running exhaust, brake–tire wear, hot soak, and extended idling) are sensitive 
! to temperature and humidity due to various factors, including (1) cold engine 
! starts that enhance emissions at lower ambient temperatures due to incomplete 
! fuel combustion, (2) evaporative losses of volatile organic compounds (VOCs) 
! due to expansion and contraction caused by ambient diurnal temperature variations, 
! (3) enhanced running emissions at higher ambient temperatures, (4) atmospheric 
! moisture suppression of high combustion temperatures that lower nitrogen oxide 
! emissions at higher humidity, and (5) indirect increased emissions from air 
! conditioning at higher ambient temperatures (Choi et al., 2010; Iodice and 
! Senatore, 2014; Lindhjem et al., 2004; Mellios et al., 2019; U.S. EPA, 2015). 
! McDonald et al. (2018) found that NOx emissions from the National Emissions 
! Inventory (NEI) estimated from the U.S. EPA's MOVES are underestimated, 
! leading to a failure regarding the prediction of high ozone days (8 h max ozone >70 ppb; McDonald et al., 2018).

! The dependency of mobile emissions on local meteorology can vary by vehicle type 
! (light duty, heavy duty, truck and bus), fuel type (gasoline, diesel, hybrid, and electric), 
! road type (interstate, freeway, and local roads), process (vehicle start-up, running exhaust, 
! brake–tire wear, hot soak, and extended idling), vehicle speed for on-road vehicles, 
! and hour of the day for off-network vehicles, as well as by pollutants such as CO, 
! NOX, SO2, NH3, VOCs, and particulate matter (PM). Figure 1 shows the dependency of the 
! MOVES emission factors of CO, NOx, VOCs, and PM2.5 from gasoline-fueled vehicles on 
! ambient temperature from the on-road and off-network vehicles, respectively. All 
! pollutant emissions vary with the temperature, particularly under lower speeds. 
! The CO, VOCs, and NOx emissions increase with temperature, while the opposite 
! relationship is suggested between PM2.5 emissions and temperature, implying the complexity 
! of meteorology impacts on different pollutant emissions. For off-network emissions from 
! gasoline-fueled vehicles, CO, NOx, and PM2.5 show negative correlations with temperature, 
! while the VOCs exhibit a nonlinear response to the temperature variation. The largest 
! meteorology dependency occurs in the daytime when emissions are the greatest. 
! A further, detailed meteorology dependency of MOVES emission factors on local meteorology can be found in Choi et al. (2010).

! This module can dynamically estimate meteorology-induced hourly gridded 
! on-road mobile emissions using simulated meteorology 
! without any computational burden to the modeling system.
!\\
!\\
! The MetEmis onroad look-up-tables (LUT) are be provided in netCDF format.

!\\
! References:
! \begin{itemize}
! Baek, B. H., Coats, C., Ma, S., Wang, C.-T., Li, Y., Xing, J., Tong, D., 
! Kim, S., and Woo, J.-H.: Dynamic Meteorology-induced Emissions Coupler (MetEmis) 
! development in the Community Multiscale Air Quality (CMAQ): CMAQ-MetEmis, 
! Geosci. Model Dev., 16, 4659–4676, https://doi.org/10.5194/gmd-16-4659-2023, 2023.
! \end{itemize}

!
!\\
!\\
! !INTERFACE:
!
MODULE HCOX_MetEmis_MOD
! !USES:
!
  USE HCO_Error_MOD
  USE HCO_Diagn_MOD
  USE HCO_State_MOD,  ONLY : HCO_State
  USE HCOX_State_MOD, ONLY : Ext_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: HCOX_MetEmis_Run
  PUBLIC  :: HCOX_MetEmis_Init
  PUBLIC  :: HCOX_MetEmis_Final
!
! !PRIVATE MEMBER FUNCTIONS:
!
!
! !REMARKS:
!  Adapted from the code in CMAQv5.3.1
!
! !REVISION HISTORY:
!  11 Mar 2025 - P.C. Campbell   - Initial NO only version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:

  ! Number of values for each variable in the provided input look-up table
  ! CONUS MetEmis Tables with  25 temperature bins
   INTEGER, PARAMETER ::  nT=25     !25 Temperature Bins in degrees F

  ! Now place all module variables in a lderived type object (for a linked
  ! list) so that we can have one instance per node in an MPI environment.
  TYPE :: MyInst

     ! Scalars
     INTEGER               :: Instance
     INTEGER               :: ExtNr
     INTEGER               :: IDTNO

     ! Arrays

     ! Reference temperature values of variables in the MetEmis look-up tables
     REAL*4                :: Tlev(nT)

     TYPE(MyInst), POINTER :: NextInst => NULL()
  END TYPE MyInst

  ! Pointer to instances
  TYPE(MyInst), POINTER    :: AllInst => NULL()

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_MetEmis_Run
!
! !DESCRIPTION: Subroutine HCOX\_MetEmis\_Run is the driver routine to
! calculate MetEmis emissions for the current time step. Emissions in
! [kg/m2/s] are added to the emissions array.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_MetEmis_Run( ExtState, HcoState, RC )
!
! !USES:
!
    USE HCO_Calc_Mod, ONLY : HCO_CalcEmis
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER       :: ExtState    ! External data fields
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER       :: HcoState    ! HEMCO State object
    INTEGER,         INTENT(INOUT) :: RC          ! Success or failure?

! !REVISION HISTORY:
!  11 Mar 2025 - P. C. Campbell   - Initial NO Version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL               :: DefScaleEmis
    CHARACTER(LEN=255)    :: MSG, LOC
    TYPE(MyInst), POINTER :: Inst

    !=================================================================
    ! HCOX_METEMIS_RUN begins here!
    !=================================================================
    LOC = 'HCOX_METEMIS_RUN (HCOX_METEMIS_MOD.F90)'

    ! Return if extension disabled
    IF ( ExtState%MetEmis <= 0 ) RETURN

    ! Enter
    CALL HCO_ENTER(HcoState%Config%Err, LOC, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Get local instance
    Inst => NULL()
    CALL InstGet ( ExtState%MetEmis, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       WRITE(MSG,*) 'Cannot find MetEmis instance Nr. ', ExtState%MetEmis
       CALL HCO_ERROR(MSG,RC)
       RETURN
    ENDIF

    ! ----------------------------------------------------------------
    ! Use HEMCO core routines to get MetEmis emissions table
    ! ----------------------------------------------------------------

    ! Prepare HEMCO core run (Hco_CalcEmis):
    ! --> Set tracer and category range + extension number.
    ! Note: Set species min and max to the full range of species.
    ! For the MetEmis extension, emission fields of only one species
    ! should be defined currently. Hco_CalcEmis will exit w/ error if this is
    ! not the case.
    HcoState%Options%SpcMin =  1
    HcoState%Options%SpcMax = -1
    HcoState%Options%CatMin =  1
    HcoState%Options%CatMax = -1
    HcoState%Options%ExtNr  = Inst%ExtNr

     CALL Calc_MetEmis( ExtState, HcoState, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 2', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Leave w/ success
    Inst => NULL()
    CALL HCO_Leave( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_MetEmis_Run
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calc_MetEmis
!
! !DESCRIPTION: Subroutine Calc_MetEmis performs linear interpolation of 
! temperature binned onroad emissions for every grid box based on 
! HEMCO model state near-surface temperature (e.g., 2-meter temperature)
! and writes the resulting emission rates into State\_Chm%NomixS.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Calc_MetEmis( ExtState, HcoState, Inst, RC )
!
! !USES:
!
    USE HCO_Types_Mod,    ONLY : DiagnCont
    USE HCO_FluxArr_mod,  ONLY : HCO_EmisAdd
    USE HCO_Clock_Mod,    ONLY : HcoClock_First

! !INPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER        :: ExtState           ! External data
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER        :: HcoState           ! HEMCO State obj
    TYPE(MyInst),    POINTER        :: Inst               ! Local instance
    INTEGER,         INTENT(INOUT)  :: RC                 ! Success or failure
!
! !REVISION HISTORY:
!  19 Mar 2025 - P. C. Campbell   - Initial Version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                  :: I, J
    LOGICAL                  :: ERR
    LOGICAL                  :: FILLED
    LOGICAL                  :: FIRST
    LOGICAL                  :: DefScaleEmis
    CHARACTER(LEN=255)       :: MSG, LOC
    CHARACTER(LEN=1)         :: CHAR1

    ! Arrays
    REAL(hp), TARGET         :: FLUXNO  (HcoState%NX,HcoState%NY)

    ! Pointers
    REAL(hp), POINTER        :: Arr2D(:,:)

    ! For diagnostics
    REAL(hp), TARGET         :: DIAGN   (HcoState%NX,HcoState%NY,1)
    LOGICAL, SAVE            :: DODIAGN = .FALSE.
    CHARACTER(LEN=31)        :: DiagnName
    TYPE(DiagnCont), POINTER :: TmpCnt

    !MetEmis Diag Update
    REAL(dp)                 :: TEMP_NO

    !=================================================================
    ! MetEmis begins here!
    !=================================================================
    LOC = 'MetEmis (HCOX_METEMIS_MOD.F90)'

    ! Enter
    CALL HCO_ENTER(HcoState%Config%Err, LOC, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 3', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Leave here if none of the tracers defined
     IF ( Inst%IDTNO <= 0) THEN  !Starting with only NO right now
       RC = HCO_SUCCESS
       RETURN
    ENDIF

    ! Nullify
    Arr2D  => NULL()
    TmpCnt => NULL()

    ! ------------------------------------------------------------------
    ! First call: check for diagnostics to write and fill restart values
    ! ------------------------------------------------------------------
    FIRST = HcoClock_First( HcoState%Clock, .TRUE. )

    IF ( FIRST ) THEN
       IF ( .NOT. DoDiagn ) THEN
          DiagnName = 'METEMIS_NO'
          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
                                DiagnName, 0, DoDiagn, TmpCnt )
          TmpCnt => NULL()
       ENDIF

    ENDIF

    IF ( DoDiagn ) DIAGN(:,:,:) = 0.0_hp

    ! Error check
    ERR = .FALSE.

    ! Initialize
    FLUXNO       = 0.0_hp

    DO J = 1, HcoState%NY
    DO I = 1, HcoState%NX

       TEMP_NO    = 0.0_hp
    
       !---------------------------------------------------------------------
       ! MetEmis lookup table for emissions based on temperature 
       ! (P.C. Campbell, 03/19/2025)
       !---------------------------------------------------------------------
       CALL METEMIS_LUT( ExtState,  HcoState,  Inst,      I,                  &
                         J,         RC,        TEMP_NO )
       IF ( RC /= HCO_SUCCESS ) THEN
          ERR = .TRUE.; EXIT
       ENDIF

!       !---------------------------------------------------------------------
!       ! Calculate NO emissions
!       !---------------------------------------------------------------------
       IF ( Inst%IDTNO > 0 ) THEN
!
!           ! Unit: kg/m2/s
           FLUXNO(I,J) = TEMP_NO
       ENDIF
!
       !---------------------------------------------------------------------
       ! Eventually write out into diagnostics array
       !---------------------------------------------------------------------
       IF ( DoDiagn ) THEN
           DIAGN(I,J,1) =  FLUXNO(I,J)
       ENDIF

    ENDDO !I
    ENDDO !J

    ! Error check
    IF ( ERR ) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF


    !=======================================================================
    ! PASS TO HEMCO STATE AND UPDATE DIAGNOSTICS
    !=======================================================================

    ! Turn off emission scaling. We don't want the computed fluxes to be
    ! scaled any more. If a uniform scale factor is defined for NO, it
    ! has been applied to the ship NO emissions already (ckeller, 5/11/17).
    DefScaleEmis               = HcoState%Options%ScaleEmis
    HcoState%Options%ScaleEmis = .FALSE.

    ! NO
    IF ( Inst%IDTNO > 0 ) THEN

       ! Add flux to emission array
       CALL HCO_EmisAdd( HcoState, FLUXNO, Inst%IDTNO, &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXNO', RC )
          RETURN
       ENDIF
    ENDIF

    ! Eventually update manual diagnostics
    IF ( DoDiagn ) THEN
        DiagnName =  'MetEmis_NO'
       Arr2D     => DIAGN(:,:,1)
       CALL Diagn_Update( HcoState, ExtNr=Inst%ExtNr, &
                          cName=TRIM(DiagnName), Array2D=Arr2D, RC=RC)
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 5', RC, THISLOC=LOC )
           RETURN
       ENDIF

    ENDIF

    ! Reset option ScaleEmis to default value
    HcoState%Options%ScaleEmis = DefScaleEmis

    ! Return w/ success
    CALL HCO_LEAVE( HcoState%Config%Err,RC )

  END SUBROUTINE Calc_MetEmis
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_MetEmis_Init
!
! !DESCRIPTION: Subroutine HcoX\_MetEmis\_Init initializes the HEMCO
! METEMIS extension.
!\\
!\\
! !INTERFACE:
!
 SUBROUTINE HCOX_MetEmis_Init( HcoState, ExtName, ExtState, RC )
!
! !USES:

   USE HCO_Chartools_Mod, ONLY : HCO_CharParse
   USE HCO_State_MOD,     ONLY : HCO_GetHcoID
   USE HCO_State_MOD,     ONLY : HCO_GetExtHcoID
   USE HCO_ExtList_Mod,   ONLY : GetExtNr
   USE HCO_ExtList_Mod,   ONLY : GetExtOpt
   USE HCO_Restart_Mod,   ONLY : HCO_RestartDefine
!
! !INPUT PARAMETERS:
!
   CHARACTER(LEN=*), INTENT(IN   )  :: ExtName       ! Extension name
   TYPE(Ext_State),  POINTER        :: ExtState      ! Module options
!
! !INPUT/OUTPUT PARAMETERS:
!
   TYPE(HCO_State),  POINTER        :: HcoState      ! HEMCO state object
   INTEGER,          INTENT(INOUT)  :: RC            ! Success or failure?
!
! !REVISION HISTORY:
!  03 24 2025 - P. C. Campbell   - Initial Version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
   INTEGER                        :: ExtNr, I, NN, tmpID, nSpc
   INTEGER                        :: YYYY, MM, DD
   INTEGER,           ALLOCATABLE :: HcoIDs(:)
   CHARACTER(LEN=31), ALLOCATABLE :: SpcNames(:)
   CHARACTER(LEN=31)              :: Dummy
   CHARACTER(LEN=31)              :: DiagnName
   CHARACTER(LEN=255)             :: MSG, LOC
   CHARACTER(LEN= 1)              :: CHAR1
   LOGICAL                        :: FOUND
   TYPE(MyInst), POINTER          :: Inst

   !========================================================================
   ! HCOX_METEMIS_INIT begins here!
   !========================================================================
   LOC = 'HCOX_METEMIS_INIT (HCOX_METEMIS_MOD.F90)'

   ! Assume success
   RC = HCO_SUCCESS

   ! Extension Nr.
   ExtNr = GetExtNr( HcoState%Config%ExtList, TRIM(ExtName) )
   IF ( ExtNr <= 0 ) RETURN

   ! Enter
   CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
   IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'ERROR 10', RC, THISLOC=LOC )
       RETURN
   ENDIF

   ! Create local instance
   Inst => NULL()
   CALL InstCreate( ExtNr, ExtState%MetEmis, Inst, RC                       )
   IF ( RC /= HCO_SUCCESS ) THEN
      CALL HCO_ERROR(                                   &
                      'Cannot create MetEmis instance', RC                  )
      RETURN
   ENDIF

   !========================================================================
   ! Skip the following for GEOS-Chem dry-run or HEMCO-standalone dry-run
   !========================================================================
   IF ( .not. HcoState%Options%IsDryRun ) THEN

      !---------------------------------------------------------------------
      ! Initialize fields of Inst object for safety's sake (bmy, 10/17/18)
      !---------------------------------------------------------------------
      Inst%IDTNO         = -1
      Inst%Tlev          =  0.0e0

      !------------------------------------------------------------------------
      ! Get species IDs
      !------------------------------------------------------------------------

      ! Get HEMCO species IDs
      CALL HCO_GetExtHcoID( HcoState, ExtNr, HcoIDs, SpcNames, nSpc, RC )
      IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'ERROR 11', RC, THISLOC=LOC )
          RETURN
      ENDIF

      ! Check for NO
      DO I = 1, nSpc
         SELECT CASE ( TRIM(SpcNames(I)) )
            CASE ( "NO" )
               Inst%IDTNO = HcoIDs(I)
            CASE DEFAULT
               ! leave empty
         END SELECT
      ENDDO

      ! Verbose mode
      IF ( HcoState%amIRoot ) THEN

         ! Write the name of the extension regardless of the verbose setting
         msg = 'Using HEMCO extension: MetEmis (met adjusted emissions)'
         CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN, sep1='-' ) ! with separator

      ENDIF

   ENDIF

   !========================================================================
   ! Exit if this is a GEOS-Chem dry-run or HEMCO-standalone dry-run
   !========================================================================
   IF ( HcoState%Options%IsDryRun ) THEN
      Inst => NULL()
      CALL HCO_LEAVE( HcoState%Config%Err,RC )
      RETURN
   ENDIF

   !========================================================================
   ! Continue initializing METEMIS for regular simulations
   !========================================================================
   !three digit suffix pertains to temperature bins in degrees fahrenheit
   ExtState%T2M%DoUse                      = .TRUE.
   ExtState%QV2M%DoUse                     = .TRUE.
   ExtState%MEmisNO_OR_000%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_005%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_010%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_015%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_020%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_025%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_030%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_035%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_040%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_045%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_050%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_055%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_060%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_065%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_070%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_075%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_080%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_085%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_090%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_095%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_100%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_105%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_110%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_115%DoUse           = .TRUE.
   ExtState%MEmisNO_OR_120%DoUse           = .TRUE.

   !------------------------------------------------------------------------
   ! Leave w/ success
   !------------------------------------------------------------------------
   IF ( ALLOCATED(HcoIDs  ) ) DEALLOCATE(HcoIDs  )
   IF ( ALLOCATED(SpcNames) ) DEALLOCATE(SpcNames)
   Inst => NULL()
   CALL HCO_LEAVE( HcoState%Config%Err,RC )

 END SUBROUTINE HCOX_MetEmis_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_MetEmis_Final
!
! !DESCRIPTION: Subroutine HcoX\_MetEmis\_Final finalizes the HEMCO
! METEMIS extension.
!\\
!\\
! !INTERFACE:
!
 SUBROUTINE HCOX_MetEmis_Final( HcoState, ExtState, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER        :: HcoState      ! HEMCO State obj
    TYPE(Ext_State), POINTER        :: ExtState      ! Module options
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  25 Mar 2025 - P. C. Campbell - Initial Version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!

   !=================================================================
   ! HCOX_METEMIS_FINAL begins here!
   !=================================================================
    CALL InstRemove( ExtState%MetEmis )

   RC = HCO_SUCCESS

 END SUBROUTINE HCOX_MetEmis_Final
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: INTERPOL_LINWEIGHTS
!
! !DESCRIPTION:  Subroutine INTERPOL\_LINWEIGHTS finds the array elements and
!      weights for piecewise 1-D linear interpolation. The input array of NODES
!      must be in monotonic ascending order. (C. Holmes 3/27/2014)
!
!      If Y is an array containing values of a function evaluated at the points
!      given in NODES, then its interpolated value at the point VALUESIN will be
!      Y(VALUEIN) =  Y(INDICES(1)) * WEIGHTS(1) +
!                    Y(INDICES(2)) * WEIGHTS(2)
!
!      This subroutine finds indices of consecutive nodes that bracket VALUEIN and
!      weights such that
!      VALUEIN = NODES(INDICES(1))   * WEIGHTS(1)     +
!                NODES(INDICES(1)+1) * (1-WEIGHTS(1))
!
!      For convenience, the returned values of INDICES and WEIGHTS are 2-element
!      arrays, where
!          INDICES(2) = INDICES(1)+1 and
!          WEIGHTS(2) = 1 - WEIGHTS(1)
!\\
!\\
! !INTERFACE:
!
 SUBROUTINE INTERPOL_LINWEIGHTS( NODES, VALUEIN, INDICES, WEIGHTS )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
   REAL*4,INTENT(IN)   :: NODES(:), VALUEIN
!
! !OUTPUT PARAMETERS:
!
   ! These arrays are always 2 elements each, but declaring
   ! as deferred shape avoids array temporaries
   INTEGER,INTENT(OUT) :: INDICES(:)
   REAL*4, INTENT(OUT) :: WEIGHTS(:)
!
! !REVISION HISTORY:
!  03 Jun 2013 - C. Holmes      - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
   INTEGER :: I
   REAL*8  :: VALUE

   !=================================================================
   ! INTERPOL_LINWEIGHTS begins here!
   !=================================================================

   ! If larger than largest in LUT, assign largest level values
   VALUE = MIN( VALUEIN, MAXVAL( NODES ) )

   ! If smaller, assign smallest level value
   !GanLuo+VALUE = MAX( VALUE,   MINVAL( NODES ) )
   VALUE = MAX( VALUE,   MINVAL( NODES )*1.d0 )

   ! Initialize
   INDICES = (/ 1, 1 /)

   ! Loop over interpolation nodes until we find the largest node value
   ! that is less than the desired value
   DO I=1, SIZE(NODES)
      INDICES(1) = I
      IF ( VALUE <= NODES(I+1) ) EXIT
   END DO

   ! The next node
   INDICES(2) = INDICES(1) + 1

   ! Weights for the corresponding node indices
   WEIGHTS(1) = ( NODES(I+1) - VALUE ) / ( NODES(I+1) - NODES(I) )
   WEIGHTS(2) = 1.0 - WEIGHTS(1)

 END SUBROUTINE INTERPOL_LINWEIGHTS
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: metemis_lut
!
! !DESCRIPTION:  Subroutine METEMIS_LUT returns NO emissions 
! based on temperature LUT (TEMPNO), Values are taken taken from a
! lookup table using piecewise linear interpolation. The look-up table is derived
! from the EPA MOVES model involving work by (Baek et al. 2023; 
! https://doi.org/10.5194/gmd-16-4659-2023)
!
! The lookup table uses 1 input variable:
!     TEMP   : model temperature, K
!\\
! !INTERFACE:
!
 SUBROUTINE METEMIS_LUT( ExtState,  HcoState, Inst, &
                         I, J, RC,  TEMPNO )
!
! !USES:
!
   USE HCO_STATE_MOD,        ONLY : HCO_State
   USE HCOX_STATE_MOD,       ONLY : Ext_State
!
! !INPUT PARAMETERS:
!
   TYPE(Ext_State), POINTER    :: ExtState
   TYPE(HCO_State), POINTER    :: HcoState
   TYPE(MyInst),    POINTER    :: Inst
   INTEGER, INTENT(IN)         :: I, J      ! Grid indices
!
! !OUTPUT PARAMETERS:
!
   REAL*8, INTENT(OUT)           :: TEMPNO  ! Temp dependent NO emissions, kg/m2/s

! !INPUT/OUTPUT PARAMETERS:
!
   INTEGER, INTENT(INOUT)        :: RC      ! Return code
!
! !REVISION HISTORY:
!     Mar 2025 - P.C. Campbell - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
   INTEGER                    :: I1
   REAL(sp)                   :: TEMPNO_TMP
   REAL(sp)                   :: WEIGHT
   REAL(sp)                   :: TAIR
   REAL(sp)                   :: QAIR,QMOL,A,B
   REAL(sp)                   :: NOXGAS,NOXDIS,NOXCORR

   ! Interpolation variables, indices, and weights
   REAL(sp), DIMENSION(1)     :: VARS
   INTEGER,  DIMENSION(1,2)   :: INDX
   REAL(sp), DIMENSION(1,2)   :: WTS

   CHARACTER(LEN=255)         :: MSG
   CHARACTER(LEN=255)         :: LOC = 'METEMIS_LUT'

   !=================================================================
   ! METEMIS_LUT begins here!
   !=================================================================

   !MetEmis Temperature Bins (Degrees Fahrenheit)
   Inst%Tlev = (/ 0.0e0, 5.0e0, 10.0e0, 15.0e0, 20.0e0, 25.0e0, 30.0e0, &
                  35.0e0, 40.0e0,  45.0e0,  50.0e0,  55.0e0,  60.0e0,  &
                  65.0e0, 70.0e0,  75.0e0,  80.0e0,  85.0e0,  90.0e0,  &
                  95.0e0, 100.0e0, 105.0e0, 110.0e0, 115.0e0, 120.0e0 /)



   !Get 2-m air temperature, K
   TAIR = ExtState%T2M%Arr%Val(I,J)
   !Get 2-m air specific humidity, kg/kg
   QAIR = ExtState%QV2M%Arr%Val(I,J)
   
   !========================================================================
   ! Load all variables into a single array
   !========================================================================

   ! Air Temperature, K --> Fahrenheit for MetEmis consistency
   VARS(1) = (TAIR - 273.15)*1.8 + 32.0

   !========================================================================
   ! Find the indices of nodes and their corresponding weights for the
   ! interpolation
   !========================================================================

   ! Temperature:
   CALL INTERPOL_LINWEIGHTS( Inst%Tlev, VARS(1), INDX(1,:), WTS(1,:) )

   !========================================================================
   ! Piecewise linear interpolation
   !========================================================================

   ! Initialize
   TEMPNO = 0.0d0

  ! Loop over temperature bins
   DO I1=1,2
      SELECT CASE ( NINT( Inst%Tlev(INDX(1,I1)) ) )
         CASE (  0 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_000%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE (  5 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_005%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 10 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_010%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 15 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_015%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 20 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_020%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 25 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_025%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 30 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_030%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 35 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_035%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 40 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_040%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 45 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_045%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 50 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_050%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 55 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_055%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 60 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_060%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 65 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_065%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 70 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_070%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 75 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_075%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 80 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_080%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 85 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_085%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 90 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_090%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 95 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_095%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 100 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_100%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 105 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_105%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 110 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_110%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 115 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_115%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE ( 120 )
            TEMPNO_TMP  =  ExtState%MEmisNO_OR_120%Arr%Val(I,J)
            WEIGHT      = WTS(1,I1)
         CASE DEFAULT
             MSG = 'LUT error: Temperature interpolation error!'
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
      END SELECT
         
         !-----------------------------------
         ! Final interpolated values
         !-----------------------------------
         ! Weighted sum of TempNO from the LUT
         TEMPNO = TEMPNO + TEMPNO_TMP * WEIGHT

   END DO

   !Calculate humidity correction For NOx
   QAIR = QAIR*1000.0   !convert from kg water/kg dry air to g/kg
   A = MIN( QAIR, 17.71 )
   B = MAX( 3.0, A )
   NOXGAS = 1.0 - 0.0329 * ( B - 10.71 )   ! NOx humidity correction for Gasoline fuel
   QMOL = QAIR * 0.001607524  ! convert from g of water/kg of dry air to moles of water/moles of dry air
   A = MIN( QMOL, 0.035 )
   B = MAX( 0.002, A )
   NOXDIS = 1.0 / ( 9.953 * B  + 0.832 )  ! Nox humidity correction for Diesel fuel

   !::: These NOXGAS and NOXDIS correction factors can be multiplied with the 
   !estimated emissions between temperature bins to reflect the impact of humidity up to 20% (+/-)

   !For now since we do not yet separate NOx Gas vs. Diesel fuels in MetEmis tables...
   !  I arbitrarily take average of two factors..
   NOXCORR = ( NOXGAS + NOXDIS )/2.0

   !Apply humidity correction for NOx
   TEMPNO = NOXCORR * TEMPNO

   ! Return w/ success
   RC = HCO_SUCCESS

 END SUBROUTINE METEMIS_LUT
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
  SUBROUTINE InstGet ( Instance, Inst, RC, PrevInst )
!
! !INPUT PARAMETERS:
!
    INTEGER                             :: Instance
    TYPE(MyInst),     POINTER           :: Inst
    INTEGER                             :: RC
    TYPE(MyInst),     POINTER, OPTIONAL :: PrevInst
!
! !REVISION HISTORY:
!  18 Feb 2016 - C. Keller   - Initial version
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
!
! !REVISION HISTORY:
!  18 Feb 2016 - C. Keller   - Initial version
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
    Inst%Instance = nnInst + 1
    Inst%ExtNr    = ExtNr

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
  SUBROUTINE InstRemove ( Instance )
!
! !INPUT PARAMETERS:
!
    INTEGER                         :: Instance
!
! !REVISION HISTORY:
!  18 Feb 2016 - C. Keller   - Initial version
!  25  Mar 2025 - P. C. Campbell - Edited for MetEmis
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

       !---------------------------------------------------------------------
       ! Deallocate fields of Inst before popping off from the list
       ! in order to avoid memory leaks (Bob Yantosca (17 Aug 2022)
       ! Edited for MetEmis
       !---------------------------------------------------------------------

       !---------------------------------------------------------------------
       ! Pop off instance from list
       !---------------------------------------------------------------------
       IF ( ASSOCIATED(PrevInst) ) THEN
          PrevInst%NextInst => Inst%NextInst
       ELSE
          AllInst => Inst%NextInst
       ENDIF
       DEALLOCATE(Inst)
    ENDIF

    ! Free pointers before exiting
    PrevInst => NULL()
    Inst     => NULL()

   END SUBROUTINE InstRemove
!EOC
END MODULE HCOX_MetEmis_mod
