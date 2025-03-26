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
! on-road mobile emissions within the CMAQ, using simulated meteorology 
! without any computational burden to the modeling system.
!\\
!\\
! The MetEmis onroad look-up-table (LUT) can be provided in netCDF format.

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
!
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

  ! Number of values for each variable in the look-up table
  INTEGER, PARAMETER ::  nT=26  !25 + 1 Temperature Bins

  ! Now place all module variables in a lderived type object (for a linked
  ! list) so that we can have one instance per node in an MPI environment.
  TYPE :: MyInst

     ! Scalars
     INTEGER               :: Instance
     INTEGER               :: ExtNr
     INTEGER               :: IDTNO

     ! Arrays
     REAL(hp), POINTER     :: MetEmisNO(:,:,:)

     ! Reference values of variables in the MetEmis look-up tables
     REAL*4                :: Tlev(nT)

     ! Look-up tables currently used in CMAQv5.3.1
     ! Described by Baek et al. 2023, now includes effects of temperature
     ! Last three digits in LUT names indicate temperature in Fahrenheit
     REAL(sp), POINTER     :: NO_LUT000(:,:,:,:)   !Temp=0. F
     REAL(sp), POINTER     :: NO_LUT005(:,:,:,:)   !Temp=5. F
     REAL(sp), POINTER     :: NO_LUT010(:,:,:,:)   !Temp=10. F
     REAL(sp), POINTER     :: NO_LUT015(:,:,:,:)   !Temp=15. F
     REAL(sp), POINTER     :: NO_LUT020(:,:,:,:)   !Temp=20. F
     REAL(sp), POINTER     :: NO_LUT025(:,:,:,:)   !Temp=25. F
     REAL(sp), POINTER     :: NO_LUT030(:,:,:,:)   !Temp=30. F
     REAL(sp), POINTER     :: NO_LUT035(:,:,:,:)   !Temp=35. F
     REAL(sp), POINTER     :: NO_LUT040(:,:,:,:)   !Temp=40. F
     REAL(sp), POINTER     :: NO_LUT045(:,:,:,:)   !Temp=45. F
     REAL(sp), POINTER     :: NO_LUT050(:,:,:,:)   !Temp=50. F
     REAL(sp), POINTER     :: NO_LUT055(:,:,:,:)   !Temp=55. F
     REAL(sp), POINTER     :: NO_LUT060(:,:,:,:)   !Temp=60. F
     REAL(sp), POINTER     :: NO_LUT065(:,:,:,:)   !Temp=65. F
     REAL(sp), POINTER     :: NO_LUT070(:,:,:,:)   !Temp=70. F
     REAL(sp), POINTER     :: NO_LUT075(:,:,:,:)   !Temp=75. F
     REAL(sp), POINTER     :: NO_LUT080(:,:,:,:)   !Temp=80. F
     REAL(sp), POINTER     :: NO_LUT085(:,:,:,:)   !Temp=85. F
     REAL(sp), POINTER     :: NO_LUT090(:,:,:,:)   !Temp=90. F
     REAL(sp), POINTER     :: NO_LUT095(:,:,:,:)   !Temp=95. F
     REAL(sp), POINTER     :: NO_LUT100(:,:,:,:)   !Temp=100. F
     REAL(sp), POINTER     :: NO_LUT105(:,:,:,:)   !Temp=105. F
     REAL(sp), POINTER     :: NO_LUT110(:,:,:,:)   !Temp=110. F
     REAL(sp), POINTER     :: NO_LUT115(:,:,:,:)   !Temp=115. F
     REAL(sp), POINTER     :: NO_LUT120(:,:,:,:)   !Temp=120. F
     REAL(sp), POINTER     :: NO_LUT125(:,:,:,:)   !Temp=125. F

     ! Location and type of MetEmis look up table data
     CHARACTER(LEN=255)    :: FileName
!     LOGICAL               :: IsNc

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
! [kg/m2/s] are added to the emissions array of the passed
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
!  11 Mar 2025 - P. C. Campbell   - Initial Version
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

    ! --> Define array to write emissions into.
    Inst%MetEmisNO                    = 0.0d0
    HcoState%Options%AutoFillDiagn    = .FALSE.
    HcoState%Options%FillBuffer       =  .TRUE.
    HcoState%Buffer3D%Val             => Inst%MetEmisNO

    ! Get MetEmis NO emissions from table as core emissions and 
    ! write them into the MetEmisNO array [kg/m2/s].
    CALL HCO_CalcEmis( HcoState, .FALSE., RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 1', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Reset settings to standard
    HcoState%Buffer3D%Val          => NULL()
    HcoState%Options%FillBuffer    = .FALSE.
    HcoState%Options%ExtNr         = 0
    HcoState%Options%AutoFillDiagn = .TRUE.

    ! Calculate MetEmis interpolation based on NO emissions lookup table and 
    ! model 2-m temperature and add these values to the respective emission
    ! arrays.
    CALL Calc_MetEmis( ExtState, Inst%MetEmisNO, HcoState, Inst, RC )
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
! temperature binned onroad NO emissions for every grid box based on 
! HEMCO model state near-surface temperature (e.g., 2-meter temperature)
! and writes the resulting NO emission rates into State\_Chm%NomixS.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Calc_MetEmis( ExtState, MetEmisNO, HcoState, Inst, RC )
!
! !USES:
!
    USE HCO_Types_Mod,    ONLY : DiagnCont
    USE HCO_FluxArr_mod,  ONLY : HCO_EmisAdd
!    USE HCO_FluxArr_mod,  ONLY : HCO_DepvAdd
    USE HCO_Clock_Mod,    ONLY : HcoClock_First
!    USE HCO_Calc_Mod,     ONLY : HCO_CheckDepv
!    USE HCO_GeoTools_Mod, ONLY : HCO_GetSUNCOS
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER        :: ExtState           ! External data
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL(hp),        INTENT(INOUT)  :: MetEmisNO(:,:,:)   ! Emissions
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
    INTEGER                  :: I, J, L
    LOGICAL                  :: ERR
    LOGICAL                  :: FILLED
    LOGICAL                  :: FIRST
    LOGICAL                  :: DefScaleEmis
    CHARACTER(LEN=255)       :: MSG, LOC
    CHARACTER(LEN=1)         :: CHAR1

    ! Arrays
    REAL(hp), TARGET         :: FLUXNO  (HcoState%NX,HcoState%NY)
!    REAL(hp), TARGET         :: FLUXNO2 (HcoState%NX,HcoState%NY)
!    REAL(hp), TARGET         :: FLUXHNO3(HcoState%NX,HcoState%NY)
!    REAL(hp), TARGET         :: FLUXO3  (HcoState%NX,HcoState%NY)
!%%% Comment out unused code
!%%%!    REAL(hp), TARGET         :: DEPO3   (HcoState%NX,HcoState%NY)
!%%%!    REAL(hp), TARGET         :: DEPHNO3 (HcoState%NX,HcoState%NY)

    ! Pointers
    REAL(hp), POINTER        :: Arr2D(:,:)

    ! For diagnostics
    REAL(hp), TARGET         :: DIAGN   (HcoState%NX,HcoState%NY,1)
    LOGICAL, SAVE            :: DODIAGN = .FALSE.
    CHARACTER(LEN=31)        :: DiagnName
    TYPE(DiagnCont), POINTER :: TmpCnt

    ! Paranox update
!    REAL(dp)                 :: SHIP_FNOx, SHIP_DNOx, SHIP_OPE, SHIP_MOE
!    REAL(dp)                 :: FNO_NOx
!    REAL(hp)                 :: iMass
!    REAL(hp)                 :: ExpVal

    !MetEmis Diag Update
    REAL(dp)                 :: TEMP_NO

!%%% Comment out debug code
!%%%!    ! testing only
!%%%!    REAL*8             :: FRAC, TOTPRES, DELTPRES
!%%%!    INTEGER            :: TOP
!%%%!    integer            :: ix, jx
!%%%!    logical, parameter :: add2hemco = .true.

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
!    IF ( Inst%IDTNO <= 0 .AND. Inst%IDTO3 <= 0 .AND. Inst%IDTHNO3 <= 0 ) THEN
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
       ! See if we have to write out manual diagnostics
!       IF ( .NOT. DoDiagn ) THEN
!          DiagnName = 'PARANOX_NOXFRAC_REMAINING'
!          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
!                                DiagnName, 0, DoDiagn, TmpCnt )
!          TmpCnt => NULL()
!       ENDIF
!       IF ( .NOT. DoDiagn ) THEN
!          DiagnName = 'PARANOX_O3_PRODUCTION'
!          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
!                                DiagnName, 0, DoDiagn, TmpCnt )
!          TmpCnt => NULL()
!       ENDIF
!       IF ( .NOT. DoDiagn ) THEN
!          DiagnName = 'PARANOX_NO_PRODUCTION'
!          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
!                                DiagnName, 0, DoDiagn, TmpCnt )
!          TmpCnt => NULL()
!       ENDIF
!       IF ( .NOT. DoDiagn ) THEN
!          DiagnName = 'PARANOX_TOTAL_SHIPNOX'
!          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
!                                DiagnName, 0, DoDiagn, TmpCnt )
!          TmpCnt => NULL()
!       ENDIF
!       IF ( .NOT. DoDiagn ) THEN
!          DiagnName = 'PARANOX_OPE'
!          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
!                                DiagnName, 0, DoDiagn, TmpCnt )
!          TmpCnt => NULL()
!       ENDIF
       IF ( .NOT. DoDiagn ) THEN
          DiagnName = 'METEMIS_TEMP_NO'
          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
                                DiagnName, 0, DoDiagn, TmpCnt )
          TmpCnt => NULL()
       ENDIF

    ENDIF

    IF ( DoDiagn ) DIAGN(:,:,:) = 0.0_hp

    ! ------------------------------------------------------------------
    ! Update SC5
    ! ------------------------------------------------------------------
    ! SC5 holds the SUNCOS values of 5 hours ago.
!    CALL HCO_getSUNCOS( HcoState, Inst%SC5, -5, RC )
!    IF ( RC /= HCO_SUCCESS ) THEN
!        CALL HCO_ERROR( 'ERROR 4', RC, THISLOC=LOC )
!        RETURN
!    ENDIF

    ! Error check
    ERR = .FALSE.

    ! Initialize
    FLUXNO       = 0.0_hp
!    FLUXNO2      = 0.0_hp
!    FLUXHNO3     = 0.0_hp
!    FLUXO3       = 0.0_hp

    ! Deposition fluxes
!    Inst%DEPO3   = 0.0_sp
!    Inst%DEPHNO3 = 0.0_sp

!%%% Comment out debug code
!%%%!    ! Debug
!%%%!    print*, '### In EVOLVE_PLUME:'
!%%%!    print*, '### JOH: ',  SUM   ( ExtState%JOH%Arr%Val ),  &
!%%%!                          MAXVAL( ExtState%JOH%Arr%Val )
!%%%!    print*, '### JNO2: ', SUM   ( ExtState%JNO2%Arr%Val ),  &
!%%%!                          MAXVAL( ExtState%JNO2%Arr%Val )
!%%%!    print*, '### SC5 : ', SUM   ( SC5 ), MAXVAL(SC5)
!%%%!    print*, '### EMIS: ', SUM   ( SHIPNOEMIS(:,:,1) ), MAXVAL(SHIPNOEMIS(:,:,1))

    ! Loop over all grid boxes
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Note: there seems to be a problem with the OMP loop in that
    ! the species concentrations (O3molec, NOmolec, NO2molec)
    ! differ slightly in a few grid boxes. Don't know exactly what
    ! is going on here, but uncomment for now! Needs more
    ! evaluation and testing.
    !
    ! Now use #if defined( 0 ) to block of this code (bmy, 6/6/14)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!$OMP PARALLEL DO                                                   &
    !!!$OMP DEFAULT( SHARED )                                             &
    !!!$OMP PRIVATE( I, J, L,   MSG, iFlx, iMass,    TMP                ) &
    !!!$OMP PRIVATE( SHIP_FNOx, SHIP_DNOx, SHIP_OPE, SHIP_MOE, FNO_NOx  ) &
    !!!$OMP SCHEDULE( DYNAMIC )
    DO J = 1, HcoState%NY
    DO I = 1, HcoState%NX

       ! Zero private variables for safety's sake
!       FNO_NOx   = 0.0_dp
!       iFlx      = 0.0_hp
!       iMass     = 0.0_hp
!       SHIP_FNOx = 0.0_dp
!       SHIP_DNOx = 0.0_dp
!       SHIP_OPE  = 0.0_dp
!       SHIP_MOE  = 0.0_dp
!       TMP       = 0.0_hp
       TEMP_NO    = 0.0_hp
    
       !---------------------------------------------------------------------
       ! Skip if no MetEmis NO emissions in this grid box
       !---------------------------------------------------------------------
       IF ( .not. ( MetEmisNO(I,J,1) > 0.0_hp ) ) CYCLE

       !---------------------------------------------------------------------
       ! MetEmis lookup table for NO emiss based on temperature 
       ! TBD:  Include effects of humidity on different fuel types
       ! (P.C. Campbell, 03/19/2025)
       !---------------------------------------------------------------------
       CALL METEMIS_LUT( ExtState,  HcoState,  Inst,      I,                  &
                         J,         RC,        TEMP_NO )
       IF ( RC /= HCO_SUCCESS ) THEN
          ERR = .TRUE.; EXIT
       ENDIF

!%%% Comment out debug code
!%%%!       ! for debugging only
!%%%!       if(I==3.and.J==35)then
!%%%!          write(*,*) 'PARANOX ship emissions @',I,J
!%%%!          write(*,*) 'Emis [kg/m2/s]: ', ShipNoEmis(I,J,1)
!%%%!          write(*,*) 'SHIP_FNOx: ', SHIP_FNOx
!%%%!          write(*,*) 'SHIP_DNOx: ', SHIP_DNOx
!%%%!          write(*,*) 'SHIP_OPE : ', SHIP_OPE
!%%%!          write(*,*) 'SHIP_MOE : ', SHIP_MOE
!%%%!       endif


       !---------------------------------------------------------------------
       ! Split the ship NOx emission into NO and NO2
       ! following the ambient ratio
       ! Now check for zero ambient air concentrations. In this
       ! case, arbitrarily emit everything as NO (ckeller, 04/10/15).
       !
       ! Bug fix: Make sure NO and NO2 are both positive
       ! See https://github.com/geoschem/geos-chem/issues/380
       !  -- Bob Yantosca (24 Jul 2020)
       !---------------------------------------------------------------------
!       FNO_NOx = 1.0_hp
!       IF ( ExtState%NO%Arr%Val (I,J,1) > 0.0_hp .and.                       &
!            ExtState%NO2%Arr%Val(I,J,1) > 0.0_hp       ) THEN
!          FNO_NOx = (   ExtState%NO%Arr%Val(I,J,1)  / Inst%MW_NO    )        &
!                  / ( ( ExtState%NO%Arr%Val(I,J,1)  / Inst%MW_NO  )          &
!                  +   ( ExtState%NO2%Arr%Val(I,J,1) / Inst%MW_NO2 ) )
!       ENDIF
!
!       !---------------------------------------------------------------------
!       ! Calculate NO emissions
!       !---------------------------------------------------------------------
       IF ( Inst%IDTNO > 0 ) THEN
!
!           ! Of the total ship NOx, the fraction SHIP_FNOx
!           ! survives after plume dilution and chemistry.
!           ! FNO_NOx is the ratio of NO / NOx.
!           ! Unit: kg/m2/s
!           FLUXNO(I,J) = ShipNoEmis(I,J,1) * SHIP_FNOx * FNO_NOx
           FLUXNO(I,J) = TEMP_NO
       ENDIF
!
!       !---------------------------------------------------------------------
!       ! Calculate NO2 emissions
!       !---------------------------------------------------------------------
!       IF ( Inst%IDTNO2 > 0 ) THEN
!
!           ! NO2 emissions complement NO emissions, so that total NOx
!           ! emissions are preserved.
!           FLUXNO2(I,J) = ShipNoEmis(I,J,1)                                  &
!                        * SHIP_FNOx                                          &
!                        * ( 1.0d0 - FNO_NOx          )                       &
!                        * ( Inst%MW_NO2 / Inst%MW_NO )
!       ENDIF
!
!       !---------------------------------------------------------------------
!       ! Calculate HNO3 emissions
!       !---------------------------------------------------------------------
!       IF ( Inst%IDTHNO3 > 0 ) THEN
!
!          ! Of the total ship NOx, the fraction 1-SHIP_FNOx-SHIP_DNOx
!          ! is converted to HNO3 during plume dilution and chemistry.
!          ! Unit: kg/m2/s
!          FLUXHNO3(I,J) = ShipNoEmis(I,J,1)                                  &
!                        * ( 1d0 - SHIP_FNOx - SHIP_DNOx )                    &
!                        * ( Inst%MW_HNO3 / Inst%MW_NO   )
!       ENDIF
!
!       !--------------------------------------------------------------------
!       ! NOy deposition (as HNO3) from the sub-grid plume.
!       ! The calculated deposition flux is in kg/m2/s, which has to be
!       ! converted to 1/s. The species mass is either the species mass in
!       ! the first grid box or of the entire PBL column, depending on the
!       ! HEMCO setting 'PBL_DRYDEP'.
!       !
!       ! As of 4/10/15, exchange loss rates in original units of kg/m2/s.
!       ! (ckeller)
!       !--------------------------------------------------------------------
!       IF ( (Inst%IDTHNO3 > 0) .AND. (SHIP_DNOx > 0.0_dp) ) THEN
!
!          ! Deposition flux in kg/m2/s.
!          Inst%DEPHNO3(I,J) = ShipNoEmis(I,J,1)                              &
!                            * SHIP_DNOx                                      &
!                            * ( Inst%MW_HNO3 / Inst%MW_NO )
!
!!%%% Comment out unused code
!!%%%!          iFlx = ShipNoEmis(I,J,1) * SHIP_DNOx * ( MW_HNO3 / MW_NO )
!!%%%!
!!%%%!          ! Get mass of species. This can either be the total PBL
!!%%%!          ! column mass or the first layer only, depending on the
!!%%%!          ! HEMCO setting.
!!%%%!          iMass = ExtState%HNO3%Arr%Val(I,J,1) &
!!%%%!                * ExtState%FRAC_OF_PBL%Arr%Val(I,J,1)
!!%%%!          IF ( HcoState%Options%PBL_DRYDEP ) THEN
!!%%%!             DO L = 1, HcoState%NZ
!!%%%!                IF ( ExtState%FRAC_OF_PBL%Arr%Val(I,J,L) == 0.0_hp ) EXIT
!!%%%!                iMass = iMass + ( ExtState%HNO3%Arr%Val(I,J,L) *       &
!!%%%!                                  ExtState%FRAC_OF_PBL%Arr%Val(I,J,L) )
!!%%%!             ENDDO
!!%%%!          ENDIF
!!%%%!
!!%%%!          ! Calculate deposition velocity (1/s) from flux
!!%%%!          ! Now avoid div-zero error (ckeller, 11/10/2014).
!!%%%!          IF ( iMass > TINY(1.0_hp) ) THEN
!!%%%!             TMP = ABS(iFlx) * HcoState%Grid%AREA_M2%Val(I,J)
!!%%%!
!!%%%!             ! Check if it's safe to do division
!!%%%!             IF ( (EXPONENT(TMP)-EXPONENT(iMass)) < MAXEXPONENT(TMP) ) THEN
!!%%%!                DEPHNO3(I,J) = TMP / iMass
!!%%%!             ENDIF
!!%%%!
!!%%%!             ! Check deposition velocity
!!%%%!             CALL HCO_CheckDepv( HcoState, DEPHNO3(I,J), RC )
!!%%%!          ENDIF
!
!       ENDIF
!
!       !---------------------------------------------------------------------
!       ! Calculate O3 emissions
!       !---------------------------------------------------------------------
!       IF ( Inst%IDTO3 > 0 ) THEN
!
!          ! Of the total ship NOx, the fraction
!          ! (1-FRACTION_NOX)*INT_OPE is converted to O3 during
!          ! plume dilution and chemistry.
!          ! Unit: kg/m2/s
!          iFlx = ShipNoEmis(I,J,1)                                           &
!               * ( 1d0 - SHIP_FNOx         )                                 &
!               * SHIP_OPE                                                    &
!               * ( Inst%MW_O3 / Inst%MW_NO )
!
!          ! For positive fluxes, add to emission flux array
!          IF ( iFlx >= 0.0_hp ) THEN
!             FLUXO3(I,J) = iFlx
!
!          ! For negative fluxes, calculate deposition velocity based
!          ! on current surface O3 concentration and pass to deposition
!          ! array. See comment on dry dep calculation of HNO3 above.
!          ! As of 4/10/15, exchange loss rates in original units of kg/m2/s.
!          ! (ckeller)
!          ELSE
!
!             ! Deposition flux in kg/m2/s.
!             ! Make sure ozone deposition flux is positive (ckeller, 3/29/16).
!             !DEPO3(I,J) = iFlx
!             Inst%DEPO3(I,J) = ABS(iFlx)
!
!!%%% Comment out unused code
!!%%%!             ! Get mass of species. This can either be the total PBL
!!%%%!             ! column mass or the first layer only, depending on the
!!%%%!             ! HEMCO setting.
!!%%%!             iMass = ExtState%O3%Arr%Val(I,J,1) &
!!%%%!                   * ExtState%FRAC_OF_PBL%Arr%Val(I,J,1)
!!%%%!             IF ( HcoState%Options%PBL_DRYDEP ) THEN
!!%%%!                DO L = 1, HcoState%NZ
!!%%%!                   IF ( ExtState%FRAC_OF_PBL%Arr%Val(I,J,L) == 0.0_hp ) EXIT
!!%%%!                   iMass = iMass + ( ExtState%O3%Arr%Val(I,J,L) *       &
!!%%%!                                     ExtState%FRAC_OF_PBL%Arr%Val(I,J,L) )
!!%%%!                ENDDO
!!%%%!             ENDIF
!!%%%!
!!%%%!             ! Calculate deposition velocity (1/s) from flux
!!%%%!             ! Now avoid div-zero error (ckeller, 11/10/2014).
!!%%%!             IF ( iMass > TINY(1.0_hp) ) THEN
!!%%%!                TMP = ABS(iFlx) * HcoState%Grid%AREA_M2%Val(I,J)
!!%%%!
!!%%%!                ! Check if it's safe to do division
!!%%%!                IF ( (EXPONENT(TMP)-EXPONENT(iMass)) < MAXEXPONENT(TMP) ) THEN
!!%%%!                   DEPO3(I,J) = TMP / iMass
!!%%%!                ENDIF
!!%%%!
!!%%%!                ! Check deposition velocity
!!%%%!                CALL HCO_CheckDepv( HcoState, DEPO3(I,J), RC )
!!%%%!             ENDIF
!
!          ENDIF
!       ENDIF

       !---------------------------------------------------------------------
       ! Eventually write out into diagnostics array
       !---------------------------------------------------------------------
       IF ( DoDiagn ) THEN
!          DIAGN(I,J,1) = SHIP_FNOx
!          DIAGN(I,J,2) = SHIP_OPE
!          DIAGN(I,J,3) = FLUXO3(I,J)
!          DIAGN(I,J,4) = ShipNoEmis(I,J,1)
!          DIAGN(I,J,5) = FLUXNO(I,J)
           DIAGN(I,J,1) =  FLUXNO(I,J)
       ENDIF

       !---------------------------------------------------------------------
       ! Reset ship NO emissions to zero. Will be refilled on next
       ! emission step!
       !---------------------------------------------------------------------
!       ShipNoEmis(I,J,1) = 0.0d0

    ENDDO !I
    ENDDO !J
    !!!$OMP END PARALLEL DO

    ! Error check
    IF ( ERR ) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF

!------------------------------------------------------------------------
!    ! Debug
!    print*, '### In EVOLVE_PLUME (B):'
!    print*, '### DIAG 1: ',  SUM   ( DIAGN(:,:,1) ),  &
!                             MAXVAL( DIAGN(:,:,1) )
!    print*, '### DIAG 2: ',  SUM   ( DIAGN(:,:,2) ),  &
!                             MAXVAL( DIAGN(:,:,2) )
!    print*, '### DIAG 3: ',  SUM   ( DIAGN(:,:,3) ),  &
!                             MAXVAL( DIAGN(:,:,3) )
!    print*, '### DIAG 4: ',  SUM   ( DIAGN(:,:,4) ),  &
!                             MAXVAL( DIAGN(:,:,4) )
!    print*, '### DIAG 5: ',  SUM   ( DIAGN(:,:,5) ),  &
!                             MAXVAL( DIAGN(:,:,5) )
!------------------------------------------------------------------------

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

    ! NO2
!    IF ( Inst%IDTNO2 > 0 ) THEN
!
!       ! Add flux to emission array
!       CALL HCO_EmisAdd( HcoState, FLUXNO2, Inst%IDTNO2, &
!                         RC,       ExtNr=Inst%ExtNr )
!    ENDIF
!
!    ! HNO3
!    IF ( Inst%IDTHNO3 > 0 ) THEN
!
!       ! Add flux to emission array
!       CALL HCO_EmisAdd( HcoState, FLUXHNO3, Inst%IDTHNO3, &
!                         RC,       ExtNr=Inst%ExtNr )
!    ENDIF
!
!    ! O3
!    IF ( Inst%IDTO3 > 0 ) THEN
!
!       ! Add flux to emission array (kg/m2/s)
!       CALL HCO_EmisAdd( HcoState, FLUXO3, Inst%IDTO3, &
!                         RC,       ExtNr=Inst%ExtNr )
!    ENDIF


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

!       DiagnName =  'PARANOX_NOXFRAC_REMAINING'
!       Arr2D     => DIAGN(:,:,1)
!       CALL Diagn_Update( HcoState, ExtNr=Inst%ExtNr, &
!                          cName=TRIM(DiagnName), Array2D=Arr2D, RC=RC)
!       IF ( RC /= HCO_SUCCESS ) THEN
!           CALL HCO_ERROR( 'ERROR 5', RC, THISLOC=LOC )
!           RETURN
!       ENDIF
!       Arr2D => NULL()
!
!       DiagnName =  'PARANOX_OPE'
!       Arr2D     => DIAGN(:,:,2)
!       CALL Diagn_Update( HcoState, ExtNr=Inst%ExtNr, &
!                          cName=TRIM(DiagnName), Array2D=Arr2D, RC=RC)
!       IF ( RC /= HCO_SUCCESS ) THEN
!           CALL HCO_ERROR( 'ERROR 6', RC, THISLOC=LOC )
!           RETURN
!       ENDIF
!       Arr2D => NULL()
!
!       DiagnName =  'PARANOX_O3_PRODUCTION'
!       Arr2D     => DIAGN(:,:,3)
!       CALL Diagn_Update( HcoState, ExtNr=Inst%ExtNr, &
!                          cName=TRIM(DiagnName), Array2D=Arr2D, RC=RC)
!       IF ( RC /= HCO_SUCCESS ) THEN
!           CALL HCO_ERROR( 'ERROR 7', RC, THISLOC=LOC )
!           RETURN
!       ENDIF
!       Arr2D => NULL()
!
!       DiagnName =  'PARANOX_TOTAL_SHIPNOX'
!       Arr2D     => DIAGN(:,:,4)
!       CALL Diagn_Update( HcoState, ExtNr=Inst%ExtNr, &
!                          cName=TRIM(DiagnName), Array2D=Arr2D, RC=RC)
!       IF ( RC /= HCO_SUCCESS ) THEN
!           CALL HCO_ERROR( 'ERROR 8', RC, THISLOC=LOC )
!           RETURN
!       ENDIF
!       Arr2D => NULL()
!
!       DiagnName =  'PARANOX_NO_PRODUCTION'
!       Arr2D     => DIAGN(:,:,5)
!       CALL Diagn_Update( HcoState, ExtNr=Inst%ExtNr, &
!                          cName=TRIM(DiagnName), Array2D=Arr2D, RC=RC)
!       IF ( RC /= HCO_SUCCESS ) THEN
!           CALL HCO_ERROR( 'ERROR 9', RC, THISLOC=LOC )
!           RETURN
!       ENDIF
!       Arr2D => NULL()
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
!
   USE HCO_Chartools_Mod, ONLY : HCO_CharParse
   USE HCO_State_MOD,     ONLY : HCO_GetHcoID
   USE HCO_State_MOD,     ONLY : HCO_GetExtHcoID
   USE HCO_ExtList_Mod,   ONLY : GetExtNr
   USE HCO_ExtList_Mod,   ONLY : GetExtOpt
   USE HCO_Restart_Mod,   ONLY : HCO_RestartDefine
!   USE ParaNOx_Util_Mod,  ONLY : Read_ParaNOx_LUT
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
!      Inst%IDTNO2        = -1
!      Inst%IDTO3         = -1
!      Inst%IDTHNO3       = -1
!      Inst%MW_O3         =  0.0d0
!      Inst%MW_NO         =  0.0d0
!      Inst%MW_NO2        =  0.0d0
!      Inst%MW_HNO3       =  0.0d0
!      Inst%MW_AIR        =  0.0d0
      Inst%Tlev          =  0.0e0
!      Inst%JNO2lev       =  0.0e0
!      Inst%O3lev         =  0.0e0
!      Inst%SEA0lev       =  0.0e0
!      Inst%SEA5lev       =  0.0e0
!      Inst%JRATIOlev     =  0.0e0
!      Inst%NOXlev        =  0.0e0
!      Inst%WSlev         =  0.0e0
       Inst%MetEmisNO     => NULL()
!      Inst%SC5           => NULL()
!      Inst%DEPO3         => NULL()
!      Inst%DEPHNO3       => NULL()
!      Inst%FRACNOX_LUT02 => NULL()
!      Inst%FRACNOX_LUT06 => NULL()
!      Inst%FRACNOX_LUT10 => NULL()
!      Inst%FRACNOX_LUT14 => NULL()
!      Inst%FRACNOX_LUT18 => NULL()
!      Inst%OPE_LUT02     => NULL()
!      Inst%OPE_LUT06     => NULL()
!      Inst%OPE_LUT10     => NULL()
!      Inst%OPE_LUT14     => NULL()
!      Inst%OPE_LUT18     => NULL()
!      Inst%MOE_LUT02     => NULL()
!      Inst%MOE_LUT06     => NULL()
!      Inst%MOE_LUT10     => NULL()
!      Inst%MOE_LUT14     => NULL()
!      Inst%MOE_LUT18     => NULL()
!      Inst%DNOX_LUT02    => NULL()
!      Inst%DNOX_LUT06    => NULL()
!      Inst%DNOX_LUT10    => NULL()
!      Inst%DNOX_LUT14    => NULL()
!      Inst%DNOX_LUT18    => NULL()
      Inst%NO_LUT000     => NULL()
      Inst%NO_LUT005     => NULL()
      Inst%NO_LUT010     => NULL()
      Inst%NO_LUT015     => NULL()
      Inst%NO_LUT020     => NULL()
      Inst%NO_LUT025     => NULL()
      Inst%NO_LUT030     => NULL()
      Inst%NO_LUT035     => NULL()
      Inst%NO_LUT040     => NULL()
      Inst%NO_LUT045     => NULL()
      Inst%NO_LUT050     => NULL()
      Inst%NO_LUT055     => NULL()
      Inst%NO_LUT060     => NULL()
      Inst%NO_LUT065     => NULL()
      Inst%NO_LUT070     => NULL()
      Inst%NO_LUT075     => NULL()
      Inst%NO_LUT080     => NULL()
      Inst%NO_LUT085     => NULL()
      Inst%NO_LUT090     => NULL()
      Inst%NO_LUT095     => NULL()
      Inst%NO_LUT100     => NULL()
      Inst%NO_LUT105     => NULL()
      Inst%NO_LUT110     => NULL()
      Inst%NO_LUT115     => NULL()
      Inst%NO_LUT120     => NULL()
      Inst%NO_LUT125     => NULL()

      !------------------------------------------------------------------------
      ! Get species IDs
      !------------------------------------------------------------------------

      ! Get HEMCO species IDs
      CALL HCO_GetExtHcoID( HcoState, ExtNr, HcoIDs, SpcNames, nSpc, RC )
      IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'ERROR 11', RC, THISLOC=LOC )
          RETURN
      ENDIF

      ! Check for NO, NO2, O3, and HNO3
      DO I = 1, nSpc
         SELECT CASE ( TRIM(SpcNames(I)) )
            CASE ( "NO" )
               Inst%IDTNO = HcoIDs(I)
!            CASE ( "NO2" )
!               Inst%IDTNO2 = HcoIDs(I)
!            CASE ( "O3" )
!               Inst%IDTO3 = HcoIDs(I)
!            CASE ( "HNO3" )
!               Inst%IDTHNO3 = HcoIDs(I)
            CASE DEFAULT
               ! leave empty
         END SELECT
      ENDDO

!      ! Get MW of all species. If species not set in the configuration
!      ! file (e.g. they are not being used by PARANOx), determine MW from
!      ! default values.
!
!      ! O3
!      IF ( Inst%IDTO3 <= 0 ) THEN
!         tmpID = HCO_GetHcoID('O3', HcoState )
!         MSG = 'O3 not produced/removed in PARANOX'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!      ELSE
!         tmpID = Inst%IDTO3
!      ENDIF
!      IF ( tmpID > 0 ) THEN
!         Inst%MW_O3 = HcoState%Spc(tmpID)%MW_g
!      ELSE
!         MSG = 'Use default O3 molecular weight of 48g/mol'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!         Inst%MW_O3 = 48.0_dp
!      ENDIF
!
!      ! NO
!      IF ( Inst%IDTNO <= 0 ) THEN
!         tmpID = HCO_GetHcoID('NO', HcoState )
!         MSG = 'NO not produced in PARANOX'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!      ELSE
!         tmpID = Inst%IDTNO
!      ENDIF
!      IF ( tmpID > 0 ) THEN
!         Inst%MW_NO = HcoState%Spc(tmpID)%MW_g
!      ELSE
!         MSG = 'Use default NO molecular weight of 30g/mol'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!         Inst%MW_NO = 30.0_dp
!      ENDIF
!
!      ! NO2
!      IF ( Inst%IDTNO2 <= 0 ) THEN
!         tmpID = HCO_GetHcoID('NO2', HcoState )
!         MSG = 'NO2 not produced in PARANOX'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!      ELSE
!         tmpID = Inst%IDTNO2
!      ENDIF
!      IF ( tmpID > 0 ) THEN
!         Inst%MW_NO2 = HcoState%Spc(tmpID)%MW_g
!      ELSE
!         MSG = 'Use default NO2 molecular weight of 46g/mol'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!         Inst%MW_NO2 = 46.0_dp
!      ENDIF
!
!      ! HNO3
!      IF ( Inst%IDTHNO3 <= 0 ) THEN
!         tmpID = HCO_GetHcoID('HNO3', HcoState )
!         MSG = 'HNO3 not produced/removed in PARANOX'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!      ELSE
!         tmpID = Inst%IDTHNO3
!      ENDIF
!      IF ( tmpID > 0 ) THEN
!         Inst%MW_HNO3 = HcoState%Spc(tmpID)%MW_g
!      ELSE
!         MSG = 'Use default HNO3 molecular weight of 63g/mol'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!         Inst%MW_HNO3 = 63.0_dp
!      ENDIF

      ! Verbose mode

         ! Write the rest of the information only when verbose is set
!         MSG = '    - Use the following species: (MW, emitted as HEMCO ID) '
!         CALL HCO_MSG(HcoState%Config%Err,MSG )
!         WRITE(MSG,"(a,F5.2,I5)") '     NO  : ', Inst%MW_NO, Inst%IDTNO
!         CALL HCO_MSG(HcoState%Config%Err,MSG)
!         WRITE(MSG,"(a,F5.2,I5)") '     NO2 : ', Inst%MW_NO2, Inst%IDTNO2
!         CALL HCO_MSG(HcoState%Config%Err,MSG)
!         WRITE(MSG,"(a,F5.2,I5)") '     O3  : ', Inst%MW_O3, Inst%IDTO3
!         CALL HCO_MSG(HcoState%Config%Err,MSG)
!         WRITE(MSG,"(a,F5.2,I5)") '     HNO3: ', Inst%MW_HNO3, Inst%IDTHNO3
!         CALL HCO_MSG(HcoState%Config%Err,MSG)

      ! Verbose mode
      IF ( HcoState%amIRoot ) THEN

         ! Write the name of the extension regardless of the verbose setting
         msg = 'Using HEMCO extension: MetEmis (met adjusted emissions)'
         CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN, sep1='-' ) ! with separator

      ENDIF

      !--------------------------------
      ! Allocate module arrays
      !--------------------------------

      ALLOCATE( Inst%NO_LUT000(1,1,HcoState%NY,HcoState%NX), STAT=RC ) 
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT000', RC )
         RETURN
      ENDIF
      Inst%NO_LUT000 = 0.0_sp
      
      ALLOCATE( Inst%NO_LUT005(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT005', RC )
         RETURN
      ENDIF
      Inst%NO_LUT005 = 0.0_sp

      ALLOCATE( Inst%NO_LUT010(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT010', RC )
         RETURN
      ENDIF
      Inst%NO_LUT010 = 0.0_sp

      ALLOCATE( Inst%NO_LUT015(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT015', RC )
         RETURN
      ENDIF
      Inst%NO_LUT015 = 0.0_sp

      ALLOCATE( Inst%NO_LUT020(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT020', RC )
         RETURN
      ENDIF
      Inst%NO_LUT020 = 0.0_sp

      ALLOCATE( Inst%NO_LUT025(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT025', RC )
         RETURN
      ENDIF
      Inst%NO_LUT025 = 0.0_sp

      ALLOCATE( Inst%NO_LUT030(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT030', RC )
         RETURN
      ENDIF
      Inst%NO_LUT030 = 0.0_sp

      ALLOCATE( Inst%NO_LUT035(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT035', RC )
         RETURN
      ENDIF
      Inst%NO_LUT035 = 0.0_sp

      ALLOCATE( Inst%NO_LUT040(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT040', RC )
         RETURN
      ENDIF
      Inst%NO_LUT040 = 0.0_sp

      ALLOCATE( Inst%NO_LUT045(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT045', RC )
         RETURN
      ENDIF
      Inst%NO_LUT045 = 0.0_sp

      ALLOCATE( Inst%NO_LUT050(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT050', RC )
         RETURN
      ENDIF
      Inst%NO_LUT050 = 0.0_sp

      ALLOCATE( Inst%NO_LUT055(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT055', RC )
         RETURN
      ENDIF
      Inst%NO_LUT055 = 0.0_sp

      ALLOCATE( Inst%NO_LUT060(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT060', RC )
         RETURN
      ENDIF
      Inst%NO_LUT060 = 0.0_sp

      ALLOCATE( Inst%NO_LUT065(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT065', RC )
         RETURN
      ENDIF
      Inst%NO_LUT065 = 0.0_sp

      ALLOCATE( Inst%NO_LUT070(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT070', RC )
         RETURN
      ENDIF
      Inst%NO_LUT070 = 0.0_sp

      ALLOCATE( Inst%NO_LUT075(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT075', RC )
         RETURN
      ENDIF
      Inst%NO_LUT075 = 0.0_sp

      ALLOCATE( Inst%NO_LUT080(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT080', RC )
         RETURN
      ENDIF
      Inst%NO_LUT080 = 0.0_sp

      ALLOCATE( Inst%NO_LUT085(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT085', RC )
         RETURN
      ENDIF
      Inst%NO_LUT085 = 0.0_sp

      ALLOCATE( Inst%NO_LUT090(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT090', RC )
         RETURN
      ENDIF
      Inst%NO_LUT090 = 0.0_sp

      ALLOCATE( Inst%NO_LUT095(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT095', RC )
         RETURN
      ENDIF
      Inst%NO_LUT095 = 0.0_sp

      ALLOCATE( Inst%NO_LUT100(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT100', RC )
         RETURN
      ENDIF
      Inst%NO_LUT100 = 0.0_sp

      ALLOCATE( Inst%NO_LUT105(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT105', RC )
         RETURN
      ENDIF
      Inst%NO_LUT105 = 0.0_sp

      ALLOCATE( Inst%NO_LUT110(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT110', RC )
         RETURN
      ENDIF
      Inst%NO_LUT110 = 0.0_sp

      ALLOCATE( Inst%NO_LUT115(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT115', RC )
         RETURN
      ENDIF
      Inst%NO_LUT115 = 0.0_sp

      ALLOCATE( Inst%NO_LUT120(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT120', RC )
         RETURN
      ENDIF
      Inst%NO_LUT120 = 0.0_sp

      ALLOCATE( Inst%NO_LUT125(1,1,HcoState%NY,HcoState%NX), STAT=RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR ( 'NO_LUT125', RC )
         RETURN
      ENDIF
      Inst%NO_LUT125 = 0.0_sp

      ! FNOX
!      ALLOCATE( Inst%FRACNOX_LUT02(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'FRACNOX_LUT02', RC )
!         RETURN
!      ENDIF
!      Inst%FRACNOX_LUT02 = 0.0_sp
!
!      ALLOCATE( Inst%FRACNOX_LUT06(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'FRACNOX_LUT06', RC )
!         RETURN
!      ENDIF
!      Inst%FRACNOX_LUT06 = 0.0_sp
!
!      ALLOCATE( Inst%FRACNOX_LUT10(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'FRACNOX_LUT10', RC )
!         RETURN
!      ENDIF
!      Inst%FRACNOX_LUT10 = 0.0_sp
!
!      ALLOCATE( Inst%FRACNOX_LUT14(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'FRACNOX_LUT014', RC )
!         RETURN
!      ENDIF
!      Inst%FRACNOX_LUT14 = 0.0_sp
!
!      ALLOCATE( Inst%FRACNOX_LUT18(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'FRACNOX_LUT18', RC )
!         RETURN
!      ENDIF
!      Inst%FRACNOX_LUT18 = 0.0_sp
!
!      ! OPE
!      ALLOCATE( Inst%OPE_LUT02(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'OPE_LUT02', RC )
!         RETURN
!      ENDIF
!      Inst%OPE_LUT02 = 0.0_sp
!
!      ALLOCATE( Inst%OPE_LUT06(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'OPE_LUT06', RC )
!         RETURN
!      ENDIF
!      Inst%OPE_LUT06 = 0.0_sp
!
!      ALLOCATE( Inst%OPE_LUT10(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= 0 ) THEN
!         CALL HCO_ERROR ( 'OPE_LUT10', RC )
!         RETURN
!      ENDIF
!      Inst%OPE_LUT10 = 0.0_sp
!
!      ALLOCATE( Inst%OPE_LUT14(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= 0 ) THEN
!         CALL HCO_ERROR ( 'OPE_LUT014', RC )
!         RETURN
!      ENDIF
!      Inst%OPE_LUT14 = 0.0_sp
!
!      ALLOCATE( Inst%OPE_LUT18(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'OPE_LUT18', RC )
!         RETURN
!      ENDIF
!      Inst%OPE_LUT18 = 0.0_sp
!
!      ! MOE
!      ALLOCATE( Inst%MOE_LUT02(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'MOE_LUT02', RC )
!         RETURN
!      ENDIF
!      Inst%MOE_LUT02 = 0.0_sp
!
!      ALLOCATE( Inst%MOE_LUT06(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'MOE_LUT06', RC )
!         RETURN
!      ENDIF
!      Inst%MOE_LUT06 = 0.0_sp
!
!      ALLOCATE( Inst%MOE_LUT10(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'MOE_LUT10', RC )
!         RETURN
!      ENDIF
!      Inst%MOE_LUT10 = 0.0_sp
!
!      ALLOCATE( Inst%MOE_LUT14(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'MOE_LUT014', RC )
!         RETURN
!      ENDIF
!      Inst%MOE_LUT14 = 0.0_sp
!
!      ALLOCATE( Inst%MOE_LUT18(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'MOE_LUT18', RC )
!         RETURN
!      ENDIF
!      Inst%MOE_LUT18 = 0.0_sp
!
!      ! DNOx
!      ALLOCATE( Inst%DNOx_LUT02(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'DNOx_LUT02', RC )
!         RETURN
!      ENDIF
!      Inst%DNOx_LUT02 = 0.0_sp
!
!      ALLOCATE( Inst%DNOx_LUT06(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'DNOx_LUT06', RC )
!         RETURN
!      ENDIF
!      Inst%DNOx_LUT06 = 0.0_sp
!
!      ALLOCATE( Inst%DNOx_LUT10(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'DNOx_LUT10', RC )
!         RETURN
!      ENDIF
!      Inst%DNOx_LUT10 = 0.0_sp
!
!      ALLOCATE( Inst%DNOx_LUT14(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'DNOx_LUT014', RC )
!         RETURN
!      ENDIF
!      Inst%DNOx_LUT14 = 0.0_sp
!
!      ALLOCATE( Inst%DNOx_LUT18(nT,nJ,nO3,nSEA,nSEA,nJ,nNOx), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'DNOx_LUT18', RC )
!         RETURN
!      ENDIF
!      Inst%DNOx_LUT18 = 0.0_sp
!
!      ALLOCATE(Inst%DEPO3  (HcoState%NX,HcoState%NY),        &
!               Inst%DEPHNO3(HcoState%NX,HcoState%NY), STAT=RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
!         CALL HCO_ERROR ( 'Deposition arrays', RC )
!         RETURN
!      ENDIF
!      Inst%DEPO3   = 0.0_sp
!      Inst%DEPHNO3 = 0.0_sp

   !   ! O3 loss and HNO3 deposition
   !   ALLOCATE( Inst%SHIPO3LOSS(HcoState%NX,HcoState%NY), STAT=RC )
   !   IF ( RC /= HCO_SUCCESS ) THEN
   !      CALL HCO_ERROR ( 'SHIPO3LOSS', RC )
   !      RETURN
   !   ENDIF
   !   Inst%SHIPO3LOSS = 0d0

   !   ALLOCATE( Inst%SHIPHNO3DEP(HcoState%NX,HcoState%NY), STAT=RC )
   !   IF ( RC /= HCO_SUCCESS ) THEN
   !        CALL HCO_ERROR ( 'SHIPHNO3DEP', RC ); RETURN
   !   ENDIF
   !   Inst%SHIPHNO3DEP = 0d0

   ENDIF

   !========================================================================
   ! Initialize the MetEmis look-up tables
   !========================================================================

   ! LUT data directory
!   CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'MetEmis LUT source dir', &
!                   OptValChar=Inst%LutDir, RC=RC)
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( &
!            'MetEmis: Could not read "MetEmis LUT source dir"!', RC, THISLOC=LOC )
!       RETURN
!   ENDIF


 ! Get location of MetEmis table. This must be provided.
    CALL GetExtOpt( HcoState%Config, ExtNr, 'MetEmis_Table',                 &
                    OptValChar=Inst%FileName, FOUND=FOUND, RC=RC            )

    IF ( RC /= HCO_SUCCESS .OR. .NOT. FOUND ) THEN
       MSG = 'Cannot read MetEmis table file name. Please provide '       // &
             'the MetEmis table as a setting to the MetEmis extension. '  // &
             'The name of this setting must be `MetEmis_Table`.'
       CALL HCO_Error( MSG, RC )
       RETURN
    ENDIF


   ! Call HEMCO parser to replace tokens such as $ROOT, $MET, or $RES.
   ! There shouldn't be any date token in there ($YYYY, etc.), so just
   ! provide some dummy variables here
!   CALL HCO_CharParse( HcoState%Config, Inst%LutDir, -999, -1, -1, -1, -1, RC )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( &
!            'MetEmis: Error encountered in "HCO_CharParse"', RC, THISLOC=LOC )
!       RETURN
!   ENDIF

   ! Data format: ncdf (default) or txt
!   Inst%IsNc = .TRUE.
!   CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'LUT data format', &
!                   OptValChar=Dummy, RC=RC)
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( &
!            'MetEmis: Could not read "LUT data format"', RC, THISLOC=LOC )
!       RETURN
!   ENDIF
 !  IF ( TRIM(Dummy) == 'txt' ) Inst%IsNc = .FALSE.

   ! Read MetEmis look-up tables from disk. This can be netCDF or txt
   ! format, as determined above.
   !
   ! NOTE: For the GEOS-Chem dry-run or HEMCO-standalone dry-run,
   ! these routines will print file paths to the dry-run log file,
   ! but will not actually read any data.
 !  IF ( Inst%IsNc ) THEN
      CALL READ_MetEmis_LUT_NC( HcoState, Inst, RC )
      IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( &
               'METEMIS: Error in "READ_METEMIS_LUT_NC"!', RC, THISLOC=LOC )
          RETURN
      ENDIF
  ! ELSE
!      CALL READ_MetEmis_LUT_TXT( HcoState, Inst, RC )
!      IF ( RC /= HCO_SUCCESS ) THEN
   !       CALL HCO_ERROR( &
   !            'METEMIS: Error in "READ_METEMIS_LUT_NC"!', RC, THISLOC=LOC )
   !       RETURN
   !   ENDIF
   !ENDIF

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

   !------------------------------------------------------------------------
   ! Set other module variables
   !------------------------------------------------------------------------
   ALLOCATE ( Inst%MetEmisNO(HcoState%NX,HcoState%NY,HcoState%NZ), STAT=RC )
   IF ( RC /= HCO_SUCCESS ) THEN
      CALL HCO_ERROR ( 'MetEmisNO', RC )
      RETURN
   ENDIF
   Inst%MetEmisNO = 0.0_hp
!
!   ! Allocate variables for SunCosMid from 5 hours ago.
!   ALLOCATE ( Inst%SC5(HcoState%NX,HcoState%NY), STAT=RC )
!   IF ( RC /= HCO_SUCCESS ) THEN
!      CALL HCO_ERROR ( 'SC5', RC )
!      RETURN
!   ENDIF
!   Inst%SC5 = 0.0_hp
!
!   ! Prompt warning if chemistry time step is more than 60 mins
!   IF ( HcoState%TS_CHEM > 3600.0_hp ) THEN
!      IF ( HcoState%amIRoot ) THEN
!         MSG = ' Cannot properly store SUNCOS values ' // &
!               ' because chemistry time step is more than 60 mins!'
!         CALL HCO_WARNING(HcoState%Config%Err, MSG, RC )
!      ENDIF
!   ENDIF
!
!   ! Molecular weight of AIR
!   Inst%MW_AIR = HcoState%Phys%AIRMW
!
!   !------------------------------------------------------------------------
!   ! Define PARANOX diagnostics for the O3 and HNO3 deposition fluxes (in
!   ! kg/m2/s).
!   !------------------------------------------------------------------------
!   CALL Diagn_Create ( HcoState,                                             &
!                       cName    = 'PARANOX_O3_DEPOSITION_FLUX',              &
!                       Trgt2D   = Inst%DEPO3,                                &
!                       SpaceDim = 2,                                         &
!                       OutUnit  = 'kg/m2/s',                                 &
!                       COL = HcoState%Diagn%HcoDiagnIDManual,                &
!                       RC       = RC                                        )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( 'ERROR 17', RC, THISLOC=LOC )
!       RETURN
!   ENDIF
!
!   CALL Diagn_Create ( HcoState,                                             &
!                       cName    = 'PARANOX_HNO3_DEPOSITION_FLUX',            &
!                       Trgt2D   = Inst%DEPHNO3,                              &
!                       SpaceDim = 2,                                         &
!                       OutUnit  = 'kg/m2/s',                                 &
!                       COL = HcoState%Diagn%HcoDiagnIDManual,                &
!                       RC       = RC                                         )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( 'ERROR 18', RC, THISLOC=LOC )
!       RETURN
!   ENDIF

   !------------------------------------------------------------------------
   ! Set HEMCO extension variables
   !------------------------------------------------------------------------

   ! Met. data required by module
!   ExtState%O3%DoUse          = .TRUE.
!   ExtState%NO2%DoUse         = .TRUE.
!   ExtState%NO%DoUse          = .TRUE.
!   ExtState%AIR%DoUse         = .TRUE.
!   ExtState%AIRVOL%DoUse      = .TRUE.
!   ExtState%SUNCOS%DoUse      = .TRUE.
   ExtState%T2M%DoUse         = .TRUE.
!   ExtState%U10M%DoUse        = .TRUE.
!   ExtState%V10M%DoUse        = .TRUE.
!   ExtState%FRAC_OF_PBL%DoUse = .TRUE.
!   IF ( Inst%IDTHNO3 > 0 ) THEN
!      ExtState%HNO3%DoUse     = .TRUE.
!   ENDIF
!   ExtState%JNO2%DoUse        = .TRUE.
!   ExtState%JOH%DoUse         = .TRUE.

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
! !IROUTINE: read_metemis_lut_nc
!
! !DESCRIPTION: Subroutine READ\_METEMIS\_LUT\_NC reads look-up tables in
!  netCDF format for use in the METEMIS model (B. H. Baek)
!\\
!\\
! !INTERFACE:
!
 SUBROUTINE READ_METEMIS_LUT_NC ( HcoState, Inst, RC )
!
! !USES:
  USE HCO_CLOCK_MOD,      ONLY : HcoClock_Get!
! !INPUT ARGUMENTS:
!
   TYPE(HCO_State), POINTER     :: HcoState    ! HEMCO State object
   TYPE(MyInst),    POINTER     :: Inst
!
! !INPUT/OUTPUT ARGUMENTS:
!
   INTEGER, INTENT(INOUT) :: RC
!
! !REVISION HISTORY:
!  25 Mar 2025 - P. C. Campbell   - Initial version modified from code provided by
!                              B. H. Baek
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
   INTEGER             :: IOS
   INTEGER             :: YYYY, MM, DD, HH
   CHARACTER(LEN=255)  :: FILENAME
   CHARACTER(LEN=255)  :: MSG
   INTEGER             :: fID
   CHARACTER(LEN=255)    :: LOC = 'READ_METEMIS_LUT_NC (hcox_metemis_mod.F90)'
   !=================================================================
   ! READ_METEMIS_LUT_NC begins here
   !=================================================================

   ! NetCDF reading of METEMIS LUT not supported in ESMF environment
!#if defined(ESMF_)
!   MSG = 'In ESMF, cannot read PARANOX look-up-table in netCDF ' // &
!         'format. Please set `LUT data format` to `txt` in the ' // &
!         'HEMCO configuration file.'
!   CALL HCO_ERROR(MSG, RC, &
!           THISLOC = 'READ_PARANOX_LUT_NC (hcox_paranox_mod.F90)' )
!   RETURN
!#else

 ! Get current year, month, day
    CALL HcoClock_Get ( HcoState%Clock, cYYYY=YYYY, cMM=MM, cDD=DD, RC=RC )
    IF ( RC /= HCO_SUCCESS ) RETURN
!#if defined( MODEL_GEOS )
    ! Error trap: skip leap days
!    IF ( MM == 2 .AND. DD > 28 ) DD = 28
!#endif

    ! Compare current day against day on file
!    ThisYMD  = YYYY*10000 + MM*100+ DD

    ! Do only if it's a different day
!    IF ( ThisYMD /= Inst%YmdOnFile ) THEN

       ! Get file name
       FILENAME = Inst%FileName
       CALL HCO_CharParse( HcoState%Config, FILENAME, YYYY, MM, DD, HH, 0, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 9', RC, THISLOC=LOC )
           RETURN
       ENDIF



   ! Clear FILENAME
!   FILENAME = ''

   ! FILENAME format string
! 101  FORMAT( a, '/met_emis_lut_2019.nc'  )


   ! Wind speed levels correspond to the files we will read below
!   Inst%WSlev = (/ 2.0e0, 6.0e0, 10.0e0, 14.0e0, 18.0e0 /)

   ! Temperature levels correspond to the files we will read below
   Inst%Tlev = (/ 0.0e0, 5.0e0, 10.0e0, 15.0e0, 20.0e0, 25.0e0, 30.0e0, & 
                  35.0e0, 40.0e0,  45.0e0,  50.0e0,  55.0e0,  60.0e0,  &
                  65.0e0, 70.0e0,  75.0e0,  80.0e0,  85.0e0,  90.0e0,  &
                  95.0e0, 100.0e0, 105.0e0, 110.0e0, 115.0e0, 120.0e0, & 
                  125.0e0 /)

   ! Read 0 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 1,                       &
        Inst%NO_LUT000, RC=RC)

  ! Read 5 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 2,                       &
        Inst%NO_LUT005, RC=RC)

  ! Read 10 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 3,                       &
        Inst%NO_LUT010, RC=RC)

  ! Read 15 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 4,                       &
        Inst%NO_LUT015, RC=RC)

  ! Read 20 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 5,                       &
        Inst%NO_LUT020, RC=RC)

  ! Read 25 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 6,                       &
        Inst%NO_LUT025, RC=RC)

  ! Read 30 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 7,                       &
        Inst%NO_LUT030, RC=RC)

  ! Read 35 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 8,                       &
        Inst%NO_LUT035, RC=RC)

  ! Read 40 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 9,                       &
        Inst%NO_LUT040, RC=RC)

  ! Read 45 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 10,                       &
        Inst%NO_LUT045, RC=RC)

  ! Read 50 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 11,                       &
        Inst%NO_LUT050, RC=RC)

  ! Read 55 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 12,                       &
        Inst%NO_LUT055, RC=RC)

  ! Read 60 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 13,                       &
        Inst%NO_LUT060, RC=RC)

  ! Read 65 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 14,                       &
        Inst%NO_LUT065, RC=RC)

  ! Read 70 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 15,                       &
        Inst%NO_LUT070, RC=RC)

  ! Read 75 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 16,                       &
        Inst%NO_LUT075, RC=RC)

  ! Read 80 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 17,                       &
        Inst%NO_LUT080, RC=RC)

  ! Read 85 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 18,                       &
        Inst%NO_LUT085, RC=RC)

  ! Read 90 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 19,                       &
        Inst%NO_LUT090, RC=RC)

  ! Read 95 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 20,                       &
        Inst%NO_LUT095, RC=RC)

  ! Read 100 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 21,                       &
        Inst%NO_LUT100, RC=RC)

  ! Read 105 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 22,                       &
        Inst%NO_LUT105, RC=RC)

  ! Read 110 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 23,                       &
        Inst%NO_LUT110, RC=RC)

  ! Read 115 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 24,                       &
        Inst%NO_LUT115, RC=RC)

  ! Read 120 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 25,                       &
        Inst%NO_LUT120, RC=RC)

  ! Read 125 Degrees F LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir)
   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ), 26,                       &
        Inst%NO_LUT125, RC=RC)

!   ! Read 6 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 6
!   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ),                         &
!        Inst%FRACNOX_LUT06, Inst%DNOx_LUT06, Inst%OPE_LUT06,                 &
!        Inst%MOE_LUT06, RC=RC )
!
!   ! Read 10 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 10
!   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ),                         &
!        Inst%FRACNOX_LUT10, Inst%DNOx_LUT10, Inst%OPE_LUT10,                 &
!        Inst%MOE_LUT10, RC=RC )
!
!   ! Read 14 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 14
!   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ),                         &
!        Inst%FRACNOX_LUT14, Inst%DNOx_LUT14, Inst%OPE_LUT14,                 &
!        Inst%MOE_LUT14, RC=RC )
!
!   ! Read 18 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 18
!   CALL READ_LUT_NCFILE( HcoState, TRIM( FILENAME ),                         &
!        Inst%FRACNOX_LUT18, Inst%DNOx_LUT18, Inst%OPE_LUT18,                 &
!        Inst%MOE_LUT18, RC=RC )

!   ! To write into txt-file, uncomment the following lines
!   FILENAME = TRIM(LutDir)//'/ship_plume_lut_02ms.txt'
!   CALL WRITE_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        FRACNOX_LUT02, DNOx_LUT02, OPE_LUT02, MOE_LUT02, RC, &
!        Tlev, JNO2lev, O3lev, SEA0lev, SEA5lev, JRATIOlev, NOXlev )
!   IF ( RC /= HCO_SUCCESS ) RETURN
!
!   FILENAME = TRIM(LutDir)//'/ship_plume_lut_06ms.txt'
!   CALL WRITE_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        FRACNOX_LUT06, DNOx_LUT06, OPE_LUT06, MOE_LUT06, RC, &
!        Tlev, JNO2lev, O3lev, SEA0lev, SEA5lev, JRATIOlev, NOXlev )
!   IF ( RC /= HCO_SUCCESS ) RETURN
!
!   FILENAME = TRIM(LutDir)//'/ship_plume_lut_10ms.txt'
!   CALL WRITE_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        FRACNOX_LUT10, DNOx_LUT10, OPE_LUT10, MOE_LUT10, RC, &
!        Tlev, JNO2lev, O3lev, SEA0lev, SEA5lev, JRATIOlev, NOXlev )
!   IF ( RC /= HCO_SUCCESS ) RETURN
!
!   FILENAME = TRIM(LutDir)//'/ship_plume_lut_14ms.txt'
!   CALL WRITE_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        FRACNOX_LUT14, DNOx_LUT14, OPE_LUT14, MOE_LUT14, RC, &
!        Tlev, JNO2lev, O3lev, SEA0lev, SEA5lev, JRATIOlev, NOXlev )
!   IF ( RC /= HCO_SUCCESS ) RETURN
!
!   FILENAME = TRIM(LutDir)//'/ship_plume_lut_14ms.txt'
!   CALL WRITE_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        FRACNOX_LUT18, DNOx_LUT18, OPE_LUT18, MOE_LUT18, RC, &
!        Tlev, JNO2lev, O3lev, SEA0lev, SEA5lev, JRATIOlev, NOXlev )
!   IF ( RC /= HCO_SUCCESS ) RETURN

   ! Return w/ success
   RC = HCO_SUCCESS
!#endif

 END SUBROUTINE READ_METEMIS_LUT_NC
!EOC
!#if !defined(ESMF_)
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_lut_ncfile
!
! !DESCRIPTION: Subroutine READ\_LUT\_NCFILE reads look up tables for use in
!  the METEMIS  model (B. H. Baek)
!\\
!\\
! !INTERFACE:
!
 SUBROUTINE READ_LUT_NCFILE( HcoState, FILENAME, LUT_IND, NO,                 &
!                             DNOx,      OPE,      MOE,      T,               &
!                             JNO2,      O3,       SEA0,     SEA5,            &
!                             JRATIO,    NOX,      RC                        )
                              RC                        )
! !USES:
!
   ! Modules for netCDF read
   USE HCO_m_netcdf_io_open
   USE HCO_m_netcdf_io_get_dimlen
   USE HCO_m_netcdf_io_read
   USE HCO_m_netcdf_io_readattr
   USE HCO_m_netcdf_io_close

#  include "netcdf.inc"
!
! !INPUT PARAMETERS:
!
   TYPE(HCO_State), POINTER     :: HcoState    ! HEMCO State object
   CHARACTER(LEN=*),INTENT(IN)  :: FILENAME
   INTEGER, INTENT(IN)          :: LUT_IND     ! MetEmis LUT Index
!
! !OUTPUT PARAMETERS:
!
   REAL*4,  INTENT(OUT), DIMENSION(:,:,:,:) :: NO
!   REAL*4,  INTENT(OUT), DIMENSION(:,:,:,:,:,:,:) :: FNOX,OPE,MOE,DNOx
!   REAL*4,  INTENT(OUT), OPTIONAL :: T(:),      JNO2(:), O3(:)
!   REAL*4,  INTENT(OUT), OPTIONAL :: SEA0(:),   SEA5(:)
!   REAL*4,  INTENT(OUT), OPTIONAL :: JRATIO(:), NOX(:)
   INTEGER, INTENT(OUT), OPTIONAL :: RC
!
! !REVISION HISTORY:
!  25 Mar 2025 - P. C. Campbell    - Initial version modified from code provided by
!                              B. H. Baek
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:

   ! Scalars
   LOGICAL             :: FileExists
   INTEGER             :: AS, IOS
   INTEGER             :: fID, HMRC

   ! arrays
!   INTEGER             :: st1d(1), ct1d(1)
   INTEGER             :: st4d(4), ct4d(4)
!   INTEGER             :: st7d(7), ct7d(7)

   CHARACTER(LEN=255)  :: MSG,     FileMsg

   !=================================================================
   ! In dry-run mode, print file path to dryrun log and exit.
   ! Otherwise, print file path to the HEMCO log file and continue.
   !=================================================================

   ! Test if the file exists
   INQUIRE( FILE=TRIM( FileName ), EXIST=FileExists )

   ! Create a display string based on whether or not the file is found
   IF ( FileExists ) THEN
      FileMsg = 'HEMCO (METEMIS): Opening'
   ELSE
      FileMsg = 'HEMCO (METEMIS): REQUIRED FILE NOT FOUND'
   ENDIF

   ! Print file status to stdout and the HEMCO log
   IF ( HcoState%amIRoot ) THEN
      WRITE( 6,   300 ) TRIM( FileMsg ), TRIM( FileName )
      WRITE( MSG, 300 ) TRIM( FileMsg ), TRIM( FileName )
      CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
 300  FORMAT( a, ' ', a )
   ENDIF

   ! For dry-run simulations, return to calling program.
   ! For regular simulations, throw an error if we can't find the file.
   IF ( HcoState%Options%IsDryRun ) THEN
      RETURN
   ELSE
      IF ( .not. FileExists ) THEN
         WRITE( MSG, 300 ) TRIM( FileMsg ), TRIM( FileName )
         CALL HCO_ERROR(MSG, HMRC )
         IF ( PRESENT( RC ) ) RC = HMRC
         RETURN
      ENDIF
   ENDIF

   !=================================================================
   ! READ_LUT_NCFILE begins here!
   !=================================================================

   ! Open file for reading
   CALL Ncop_Rd( fId, TRIM(FILENAME) )

   !-----------------------------------------------------------------
   ! Read reference values used to construct the Look-up table
   ! These are 1d values
   !-----------------------------------------------------------------

   ! Temperature, K
!   IF ( PRESENT(T) ) THEN
!      st1d = (/ 1  /)
!      ct1d = (/ nT /)
!      CALL NcRd( T, fId, 'T', st1d, ct1d )
!   ENDIF
!
!   ! J(NO2), 1/s
!   IF ( PRESENT(JNO2) ) THEN
!      st1d = (/ 1  /)
!      ct1d = (/ nJ /)
!      CALL NcRd( JNO2, fId, 'JNO2', st1d, ct1d )
!   ENDIF
!
!   ! Ambient O3, ppb
!   IF ( PRESENT(O3) ) THEN
!      st1d = (/ 1   /)
!      ct1d = (/ nO3 /)
!      CALL NcRd( O3, fId, 'O3', st1d, ct1d )
!   ENDIF
!
!   ! Solar elevation angle at emission time t=0, deg
!   IF ( PRESENT(SEA0) ) THEN
!      st1d = (/ 1    /)
!      ct1d = (/ nSEA /)
!      CALL NcRd( SEA0, fId, 'SEA0', st1d, ct1d )
!   ENDIF
!
!   ! Solar elevation angle at time t=5h, deg
!   IF ( PRESENT(SEA5) ) THEN
!      st1d = (/ 1    /)
!      ct1d = (/ nSEA /)
!      CALL NcRd( SEA5, fId, 'SEA5', st1d, ct1d )
!   ENDIF
!
!   ! J(OH) / J(NO2) ratio, s/s
!   IF ( PRESENT(JRATIO) ) THEN
!      st1d = (/ 1  /)
!      ct1d = (/ nJ /)
!      CALL NcRd( JRatio, fId, 'Jratio', st1d, ct1d )
!   ENDIF
!
!   ! Ambient NOx, ppt
!   IF ( PRESENT(NOX) ) THEN
!      st1d = (/ 1  /)
!      ct1d = (/ nNOx /)
!      CALL NcRd( NOX, fId, 'NOx', st1d, ct1d )
!   ENDIF

   ! Define 7D variables used below
!   st7d = (/ 1, 1, 1,  1,   1,   1, 1    /)
!   ct7d = (/ nT,nJ,nO3,nSEA,nSEA,nJ,nNOx /)
   ! Define 4D variables used below
   st4d = (/ 1, LUT_IND, 1,  1/)
   ct4d = (/ 1, LUT_IND, HcoState%NY,  HcoState%NX/)
   !-----------------------------------------------------------------
   ! Read look up table for temperature dependent NO from MetEmis
   ! emissions [kg m-2 s-1]
   !-----------------------------------------------------------------

!   CALL NcRd( FNOx, fId, 'FNOx', st7d, ct7d )
   CALL NcRd( NO, fId, 'NO', st4d, ct4d )

  ! testing only
!   PRINT*, "binary_fracnox: ", Fnox(1:4,1,1,2,3,4,4)

   !-----------------------------------------------------------------
   ! Read look up table for 5-h integrated Ozone Production Efficiency
   ! for ship emissions [molec O3 produced / molec NOx lost]
   !-----------------------------------------------------------------

!   CALL NcRd( OPE, fId, 'OPE', st7d, ct7d )

  ! testing only
!   PRINT*, "binary_intope: ", OPE(1:4,1,1,2,3,4,4)

   !-----------------------------------------------------------------
   ! Read look up table for 5-h integrated Methane Oxidation Efficiency
   ! for ship emissions [molec CH4 oxidized / molec NOx emitted]
   !-----------------------------------------------------------------

!   CALL NcRd( MOE, fId, 'MOE', st7d, ct7d )

  ! testing only
!   PRINT*, "binary_intmoe: ", MOE(1:4,1,1,2,3,4,4)

   !-----------------------------------------------------------------
   ! Read look up table for 5-h integrated NOx deposition fraction
   ! for ship emissions [molec NOx deposited / molec NOx emitted]
   !-----------------------------------------------------------------

!   CALL NcRd( DNOx, fId, 'DNOx', st7d, ct7d )

  ! testing only
!  PRINT*, "binary_depnox: ", DNOx(1:4,1,1,2,3,4,4)

   ! Close netCDF file
   CALL NcCl( fId )

 END SUBROUTINE READ_LUT_NCFILE
!EOC
!#endif
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_paranox_lut_txt
!
! !DESCRIPTION: Subroutine READ\_PARANOX\_LUT\_TXT reads look-up tables in
!  txt format for use in the PARANOX ship plume model (G.C.M. Vinken)
!\\
!\\
! !INTERFACE:
!
! SUBROUTINE READ_PARANOX_LUT_TXT ( HcoState, Inst, RC )
!!
!! !USES:
!!
!! !INPUT ARGUMENTS:
!!
!   TYPE(HCO_State), POINTER       :: HcoState    ! HEMCO State object
!   TYPE(MyInst),    POINTER       :: Inst
!!
!! !INPUT/OUTPUT ARGUMENTS:
!!
!   INTEGER, INTENT(INOUT) :: RC
!!
!! !REVISION HISTORY:
!!  05 Feb 2015 - C. Keller   - Initial version modified from code provided by
!!                              G.C.M. Vinken and C. Holmes
!!  See https://github.com/geoschem/hemco for complete history
!!EOP
!!------------------------------------------------------------------------------
!!BOC
!!
!! !LOCAL VARIABLES:
!   INTEGER             :: IOS
!   CHARACTER(LEN=255)  :: FILENAME
!   CHARACTER(LEN=255)  :: MSG, LOC
!   INTEGER             :: fID
!
!   !=================================================================
!   ! READ_PARANOX_LUT_TXT begins here
!   !=================================================================
!   LOC = 'READ_PARANOX_LUT_TXT (HCOX_PARANOX_MOD.F90)'
!
!   ! Clear FILENAME
!   FILENAME = ''
!
!   ! FILENAME format string
! 101  FORMAT( a, '/ship_plume_lut_', I2.2, 'ms.txt'  )
!
!   ! Wind speed levels correspond to the files that we will read below
!   Inst%WSlev = (/ 2.0e0, 6.0e0, 10.0e0, 14.0e0, 18.0e0 /)
!
!   ! Read 2m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 2
!   CALL READ_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        Inst%FRACNOX_LUT02, Inst%DNOx_LUT02, Inst%OPE_LUT02, Inst%MOE_LUT02, RC, &
!        Inst%Tlev, Inst%JNO2lev, Inst%O3lev, Inst%SEA0lev, Inst%SEA5lev, Inst%JRATIOlev, Inst%NOXlev )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( 'ERROR 19', RC, THISLOC=LOC )
!       RETURN
!   ENDIF
!
!   ! Read 6 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 6
!   CALL READ_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        Inst%FRACNOX_LUT06, Inst%DNOx_LUT06, Inst%OPE_LUT06, Inst%MOE_LUT06, RC )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( 'ERROR 20', RC, THISLOC=LOC )
!       RETURN
!   ENDIF
!
!   ! Read 10 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 10
!   CALL READ_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        Inst%FRACNOX_LUT10, Inst%DNOx_LUT10, Inst%OPE_LUT10, Inst%MOE_LUT10, RC )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( 'ERROR 21', RC, THISLOC=LOC )
!       RETURN
!   ENDIF
!
!   ! Read 14 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 14
!   CALL READ_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        Inst%FRACNOX_LUT14, Inst%DNOx_LUT14, Inst%OPE_LUT14, Inst%MOE_LUT14, RC )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( 'ERROR 22', RC, THISLOC=LOC )
!       RETURN
!   ENDIF
!
!   ! Read 18 m/s LUT
!   WRITE( FILENAME, 101 ) TRIM(Inst%LutDir), 18
!   CALL READ_LUT_TXTFILE( HcoState, TRIM( FILENAME ), &
!        Inst%FRACNOX_LUT18, Inst%DNOx_LUT18, Inst%OPE_LUT18, Inst%MOE_LUT18, RC )
!   IF ( RC /= HCO_SUCCESS ) THEN
!       CALL HCO_ERROR( 'ERROR 23', RC, THISLOC=LOC )
!       RETURN
!   ENDIF
!
!   ! Return w/ success
!   RC = HCO_SUCCESS
!
! END SUBROUTINE READ_PARANOX_LUT_TXT
!!EOC
!!------------------------------------------------------------------------------
!!                  GEOS-Chem Global Chemical Transport Model                  !
!!------------------------------------------------------------------------------
!!BOP
!!
!! !IROUTINE: read_lut_txtfile
!!
!! !DESCRIPTION: Subroutine READ\_LUT\_TXTFILE reads look up tables for use in
!!  the PARANOX ship plume model (C. Holmes)
!!\\
!!\\
!! !INTERFACE:
!!
! SUBROUTINE READ_LUT_TXTFILE( HcoState, FILENAME, FNOX, DNOx, OPE, MOE, RC, &
!                              T, JNO2, O3, SEA0, SEA5, JRATIO, NOX )
!!
!! !USES:
!!
!   USE HCO_inquireMod, ONLY : findFreeLUN
!!
!! !INPUT PARAMETERS:
!!
!   TYPE(HCO_State), POINTER     :: HcoState    ! HEMCO State object
!   CHARACTER(LEN=*),INTENT(IN)  :: FILENAME
!!
!! !INPUT/OUTPUT PARAMETERS:
!!
!   INTEGER, INTENT(INOUT)       :: RC
!!
!! !OUTPUT PARAMETERS:
!!
!   REAL*4, INTENT(OUT), TARGET, DIMENSION(:,:,:,:,:,:,:) :: FNOX,OPE,MOE,DNOx
!   REAL*4, INTENT(OUT), OPTIONAL :: T(:),      JNO2(:), O3(:)
!   REAL*4, INTENT(OUT), OPTIONAL :: SEA0(:),   SEA5(:)
!   REAL*4, INTENT(OUT), OPTIONAL :: JRATIO(:), NOX(:)
!!
!! !REVISION HISTORY:
!!  05 Feb 2015 - C. Keller   - Initial version modified from code provided by
!!                              G.C.M. Vinken and C. Holmes
!!  See https://github.com/geoschem/hemco for complete history
!!EOP
!!------------------------------------------------------------------------------
!!BOC
!!
!! !LOCAL VARIABLES:
!!
!   ! Scalars
!   LOGICAL             :: FileExists
!   INTEGER             :: fId, IOS
!
!!   INTEGER             :: I, I1, I2, I3, I4, I5, I6, I7
!!   REAL*4, POINTER     :: TMPARR(:,:,:,:,:,:,:) => NULL()
!
!   CHARACTER(LEN=255)  :: MSG, FileMsg
!!
!! !DEFINED PARAMETERS:
!!
!   CHARACTER(LEN=255), PARAMETER :: FMAT = "(E40.32)"
!   CHARACTER(LEN=255), PARAMETER :: LOC  = &
!                        'READ_LUT_TXTFILE (hcox_paranox_mod.F90)'
!
!   !=================================================================
!   ! In dry-run mode, print file path to dryrun log and exit.
!   ! Otherwise, print file path to the HEMCO log file and continue.
!   !=================================================================
!
!   ! Test if the file exists
!   INQUIRE ( FILE=TRIM( FileName ), EXIST=FileExists )
!
!   ! Create a display string based on whether or not the file is found
!   IF ( FileExists ) THEN
!      FileMsg = 'HEMCO (PARANOX): Opening'
!   ELSE
!      FileMsg = 'HEMCO (PARANOX): REQUIRED FILE NOT FOUND'
!   ENDIF
!
!   ! Print file status to stdout and the HEMCO log file
!   IF ( HcoState%amIRoot ) THEN
!      WRITE( 6,   300 ) TRIM( FileMsg ), TRIM( FileName )
!      WRITE( MSG, 300 ) TRIM( FileMsg ), TRIM( FileName )
!      CALL HCO_MSG(HcoState%Config%Err,MSG)
! 300  FORMAT( a, ' ', a )
!   ENDIF
!
!   ! For dry-run simulations, return to calling program.
!   ! For regular simulations, throw an error if we can't find the file.
!   IF ( HcoState%Options%IsDryRun ) THEN
!      RETURN
!   ELSE
!      IF ( .not. FileExists ) THEN
!         WRITE( MSG, 300 ) TRIM( FileMsg ), TRIM( FileName )
!         CALL HCO_ERROR(MSG, RC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   !=================================================================
!   ! READ_LUT_TXTFILE begins here
!   !=================================================================
!
!   ! Find a free file LUN
!   fId = findFreeLUN()
!
!   ! Open file for reading
!   OPEN ( fID, FILE=TRIM(FILENAME), FORM="FORMATTED", IOSTAT=IOS )
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile:1', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read FNOx
!   READ( fId, FMT=FMAT, IOSTAT=IOS ) FNOx
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile: FNOx', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read OPE
!   READ( fId, FMT=FMAT, IOSTAT=IOS ) OPE
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile: OPE', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read MOE
!   READ( fId, FMT=FMAT, IOSTAT=IOS ) MOE
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile: MOE', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read DNOx
!   READ( fId, FMT=FMAT, IOSTAT=IOS ) DNOx
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile: DNOx', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read optional values
!   IF ( PRESENT(T) ) THEN
!      READ( fId, FMT=FMAT, IOSTAT=IOS ) T
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: T', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(JNO2) ) THEN
!      READ( fId, FMT=FMAT, IOSTAT=IOS ) JNO2
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: JNO2', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(O3) ) THEN
!      READ( fId, FMT=FMAT, IOSTAT=IOS ) O3
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: O3', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(SEA0) ) THEN
!      READ( fId, FMT=FMAT, IOSTAT=IOS ) SEA0
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: SEA0', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(SEA5) ) THEN
!      READ( fId, FMT=FMAT, IOSTAT=IOS ) SEA5
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: SEA5', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(JRATIO) ) THEN
!      READ( fId, FMT=FMAT, IOSTAT=IOS ) JRATIO
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: JRATIO', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(NOX) ) THEN
!      READ( fId, FMT=FMAT, IOSTAT=IOS ) NOX
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: NOX', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!!   ! Read
!!   DO I = 1,4
!!
!!      ! Set pointer to output array
!!      SELECT CASE ( I )
!!         CASE ( 1 )
!!            TMPARR => FNOX
!!         CASE ( 2 )
!!            TMPARR => OPE
!!         CASE ( 3 )
!!            TMPARR => MOE
!!         CASE ( 4 )
!!            TMPARR => DNOx
!!         CASE DEFAULT
!!            CALL HCO_ERROR( 'I > 4', RC, THISLOC=LOC )
!!            RETURN
!!      END SELECT
!!
!!      DO I1 = 1,nT
!!      DO I2 = 1,nJ
!!      DO I3 = 1,nO3
!!      DO I4 = 1,nSEA
!!      DO I5 = 1,nSEA
!!      DO I6 = 1,nJ
!!      DO I7 = 1,nNOx
!!         READ( fId, FMT=FMAT, IOSTAT=IOS ) TMPARR(I1,I2,I3,I4,I5,I6,I7)
!!      ENDDO
!!      ENDDO
!!      ENDDO
!!      ENDDO
!!      ENDDO
!!      ENDDO
!!      ENDDO
!!   ENDDO
!
!   ! Close file
!   CLOSE( fId )
!
!   ! Return w/ success
!   RC = HCO_SUCCESS
!
! END SUBROUTINE READ_LUT_TXTFILE
!!EOC
!!------------------------------------------------------------------------------
!!                  GEOS-Chem Global Chemical Transport Model                  !
!!------------------------------------------------------------------------------
!!BOP
!!
!! !IROUTINE: write_lut_txtfile
!!
!! !DESCRIPTION: write\_lut\_txtfile
!!\\
!!\\
!! !INTERFACE:
!!
! SUBROUTINE WRITE_LUT_TXTFILE( HcoState, FILENAME, FNOX, DNOx, OPE, MOE, RC, &
!                              T, JNO2, O3, SEA0, SEA5, JRATIO, NOX )
!!
!! !USES:
!!
!   USE HCO_inquireMod, ONLY : findFreeLUN
!!
!! !INPUT PARAMETERS:
!!
!   TYPE(HCO_State), INTENT(INOUT)  :: HcoState          ! HEMCO state obj
!   CHARACTER(LEN=*),INTENT(IN)     :: FILENAME
!!
!! !INPUT/OUTPUT PARAMETERS:
!!
!   INTEGER, INTENT(INOUT)       :: RC
!!
!! !OUTPUT PARAMETERS:
!!
!   REAL*4, INTENT(IN), TARGET, DIMENSION(:,:,:,:,:,:,:) :: FNOX,OPE,MOE,DNOx
!   REAL*4, INTENT(IN), OPTIONAL :: T(:),      JNO2(:), O3(:)
!   REAL*4, INTENT(IN), OPTIONAL :: SEA0(:),   SEA5(:)
!   REAL*4, INTENT(IN), OPTIONAL :: JRATIO(:), NOX(:)
!!
!! !REVISION HISTORY:
!!  05 Feb 2015 - C. Keller   - Initial version modified from code provided by
!!                              G.C.M. Vinken and C. Holmes
!!  See https://github.com/geoschem/hemco for complete history
!!EOP
!!------------------------------------------------------------------------------
!!BOC
!!
!! !LOCAL VARIABLES:
!   INTEGER             :: fId, IOS
!
!!   INTEGER             :: I, I1, I2, I3, I4, I5, I6, I7
!!   REAL*4, POINTER     :: TMPARR(:,:,:,:,:,:,:) => NULL()
!
!   CHARACTER(LEN=255)  :: MSG
!   CHARACTER(LEN=255), PARAMETER :: FMAT = "(E40.32)"
!   CHARACTER(LEN=255), PARAMETER :: LOC  = &
!                        'WRITE_LUT_TXTFILE (hcox_paranox_mod.F90)'
!
!   !=================================================================
!   ! WRITE_LUT_TXTFILE begins here
!   !=================================================================
!
!   ! Echo info
!   IF ( Hcostate%amIRoot ) THEN
!      WRITE( MSG, 100 ) TRIM( FILENAME )
!      CALL HCO_MSG(HcoState%Config%Err,MSG)
!100   FORMAT( 'WRITE_LUT_TXTFILE: Writing ', a )
!   ENDIF
!
!   ! Find a free file LUN
!   fId = findFreeLUN()
!
!   ! Open file for reading
!   OPEN ( fID, FILE=TRIM(FILENAME), ACTION="WRITE", FORM="FORMATTED", IOSTAT=IOS )
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'write_lut_txtfile:1', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read FNOx
!   WRITE( fId, FMT=FMAT, IOSTAT=IOS ) FNOx
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'write_lut_txtfile: FNOx', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read OPE
!   WRITE( fId, FMT=FMAT, IOSTAT=IOS ) OPE
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile: OPE', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read MOE
!   WRITE( fId, FMT=FMAT, IOSTAT=IOS ) MOE
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile: MOE', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read DNOx
!   WRITE( fId, FMT=FMAT, IOSTAT=IOS ) DNOx
!   IF ( IOS /= 0 ) THEN
!      CALL HCO_ERROR( 'read_lut_txtfile: DNOx', RC, THISLOC=LOC )
!      RETURN
!   ENDIF
!
!   ! Read optional values
!   IF ( PRESENT(T) ) THEN
!      WRITE( fId, FMT=FMAT, IOSTAT=IOS ) T
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: T', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(JNO2) ) THEN
!      WRITE( fId, FMT=FMAT, IOSTAT=IOS ) JNO2
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: JNO2', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(O3) ) THEN
!      WRITE( fId, FMT=FMAT, IOSTAT=IOS ) O3
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: O3', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(SEA0) ) THEN
!      WRITE( fId, FMT=FMAT, IOSTAT=IOS ) SEA0
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: SEA0', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(SEA5) ) THEN
!      WRITE( fId, FMT=FMAT, IOSTAT=IOS ) SEA5
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: SEA5', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(JRATIO) ) THEN
!      WRITE( fId, FMT=FMAT, IOSTAT=IOS ) JRATIO
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: JRATIO', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   IF ( PRESENT(NOX) ) THEN
!      WRITE( fId, FMT=FMAT, IOSTAT=IOS ) NOX
!      IF ( IOS /= 0 ) THEN
!         CALL HCO_ERROR( 'read_lut_txtfile: NOX', RC, THISLOC=LOC )
!         RETURN
!      ENDIF
!   ENDIF
!
!   ! Close file
!   CLOSE( fId )
!
!   ! Return w/ success
!   RC = HCO_SUCCESS
!
! END SUBROUTINE WRITE_LUT_TXTFILE
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

! In GEOS-Chem v9-01-03 through v9-02, the effects of wind speed on FNOx and OPE
! were not included (wind speed set at 6 m/s). The JRatio also used J(O1D)
! rather than J(OH); this has only a small effect on interpolated values.
! To reproduce the behavior of these earlier versions, modify code below marked
! with ******* and call READ\_PARANOX\_LUT\_v913 in emissions\_mod.F
!\\
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
!   REAL*8, INTENT(OUT)           :: FNOX    ! fraction of NOx remaining, mol/mol
!   REAL*8, INTENT(OUT)           :: DNOX    ! fraction of NOx deposited, mol/mol
!   REAL*8, INTENT(OUT)           :: OPE     ! net OPE, mol(net P(O3))/mol(P(HNO3))
!   REAL*8, INTENT(OUT), OPTIONAL :: MOE_OUT ! net MOE, mol(L(CH4))/mol(E(NOx))

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
!   INTEGER                    :: I1,I2,I3,I4,I5,I6,I7,I8
!   REAL(sp)                   :: FNOX_TMP, DNOX_TMP, OPE_TMP, MOE_TMP
   INTEGER                    :: I1
   REAL(sp)                   :: TEMPNO_TMP
   REAL(sp)                   :: WEIGHT
!   REAL(sp)                   :: JNO2, JOH, TAIR
   REAL(sp)                   :: TAIR !,QH2O (TBD: Add humidity correction; need fuel types)
   REAL(sp)                   :: AIR
!   REAL*8                     :: MOE

   ! Interpolation variables, indices, and weights
   REAL(sp), DIMENSION(1)     :: VARS
   INTEGER,  DIMENSION(1,2)   :: INDX
   REAL(sp), DIMENSION(1,2)   :: WTS

!   REAL(sp), POINTER          :: FRACNOX_LUT(:,:,:,:,:,:,:)
!   REAL(sp), POINTER          :: DNOX_LUT   (:,:,:,:,:,:,:)
!   REAL(sp), POINTER          :: OPE_LUT    (:,:,:,:,:,:,:)
!   REAL(sp), POINTER          :: MOE_LUT    (:,:,:,:,:,:,:)

   REAL(sp), POINTER          :: TEMPNO_LUT(:,:,:,:)

   CHARACTER(LEN=255)         :: MSG
   CHARACTER(LEN=255)         :: LOC = 'METEMIS_LUT'

   !=================================================================
   ! METEMIS_LUT begins here!
   !=================================================================

   ! Initialize for safety's sake
!   FRACNOX_LUT => NULL()
!   DNOX_LUT    => NULL()
!   OPE_LUT     => NULL()
!   MOE_LUT     => NULL()
!   FNOX        = 0.0_sp
!   DNOX        = 0.0_sp
!   OPE         = 0.0_sp
    TEMPNO_LUT  => NULL()
!   IF ( PRESENT( MOE_OUT ) ) THEN
!      MOE_OUT  = 0.0_sp
!   ENDIF

   ! Air mass [kg]
!   AIR = ExtState%AIR%Arr%Val(I,J,1)

   ! Air temperature, K
   TAIR = ExtState%T2M%Arr%Val(I,J)

   ! Specific humidity at midpoint of surface layer [kg H2O/kg air]
!   QH2O       = ExtState%SPHU%Arr%Val(I,J,1)

!   ! for debugging only
!   if(I==3.and.J==35)then
!      write(*,*) 'Call PARANOX_LUT @ ',I,J
!      write(*,*) 'Tair: ', Tair
!      write(*,*) 'SUNCOSmid: ', SC5(I,J)
!   endif

   ! Check if sun is up
!   IF ( ExtState%SUNCOS%Arr%Val(I,J) > 0.0_hp ) THEN

      ! J(NO2), 1/s
!      JNO2 = ExtState%JNO2%Arr%Val(I,J)

!      ! J(O1D), 1/s
!      JO1D = ExtState%JO1D%Arr%Val(I,J)
!
!      ! H2O, molec/cm3. Get from specific humidity, which is in kg/kg.
!      ! NOTE: SPHU is mass H2O / mass total air so use of dry air molecular
!      ! weight is slightly inaccurate. C (ewl, 9/11/15)
!      H2O = ExtState%SPHU%Arr%Val(I,J,1) * DENS &
!          * HcoState%Phys%AIRMW / MWH2O
!
!      ! Calculate J(OH), the effective rate for O3+hv -> OH+OH,
!      ! assuming steady state for O(1D).
!      ! Rate coefficients are cm3/molec/s; concentrations are molec/cm3
!      ! This should match the O3+hv (+H2O) -> OH+OH kinetics in calcrate.F
!      JOH = JO1D *                                            &
!            1.63e-10 * EXP( 60.e0/Tair) * H2O /               &
!          ( 1.63e-10 * EXP( 60.e0/Tair) * H2O +             &
!            1.20e-10                    * DENS * 0.5000e-6  + &
!            2.15e-11 * EXP(110.e0/Tair) * DENS * 0.7808e0   + &
!            3.30e-11 * EXP( 55.e0/Tair) * DENS * 0.2095e0   )

      ! J(OH) - effective rate for O3+hv(+H2O)-> OH+OH, 1/s
!      JOH = ExtState%JOH%Arr%Val(I,J)

!   ELSE

!      ! J-values are zero when sun is down
!      JNO2 = 0e0_sp
!      JOH  = 0e0_sp
!
!   ENDIF

!   ! for debugging only
!   if(I==3.and.J==35)then
!      write(*,*) 'JNO2: ', JNO2
!      write(*,*) 'JOH : ', JOH
!   endif

   !========================================================================
   ! Load all variables into a single array
   !========================================================================

   ! Air Temperature, K --> Fahrenheit
   VARS(1) = (TAIR - 273.15)*1.8 + 32.0
   
   ! Air QH2O, kg/kg
 !  VARS(2) = QH2O

! old
!   ! O3 concentration in ambient air, ppb
!   VARS(3) = ExtState%O3%Arr%Val(I,J,1) / AIR   &
!           * HcoState%Phys%AIRMW        / MW_O3 &
!           * 1.e9_sp
! new
   ! O3 concentration in ambient air, ppb
   ! NOTE: ExtState%O3 units are now kg/kg dry air (ewl, 9/11/15)
!   VARS(3) = ExtState%O3%Arr%Val(I,J,1)         &
!           * HcoState%Phys%AIRMW        / Inst%MW_O3 &
!           * 1.e9_sp
! end new (ewl)

   ! Solar elevation angle, degree
   ! SEA0 = SEA when emitted from ship, 5-h before current model time
   ! SEA5 = SEA at current model time, 5-h after emission from ship
   ! Note: Since SEA = 90 - SZA, then cos(SZA) = sin(SEA) and
   ! thus SEA = arcsin( cos( SZA ) )
   !VARS(4) = ASIND( SC5(I,J) )
   !VARS(5) = ASIND( ExtState%SUNCOS%Arr%Val(I,J) )
!   VARS(4) = ASIN( Inst%SC5(I,J)                ) / HcoState%Phys%PI_180
!   VARS(5) = ASIN( ExtState%SUNCOS%Arr%Val(I,J) ) / HcoState%Phys%PI_180

   ! J(OH)/J(NO2), unitless
   ! Note J(OH) is the loss rate (1/s) of O3 to OH, which accounts for
   ! the temperature, pressure and water vapor dependence of these reactions
!   VARS(6) = 0.0_sp
!   IF ( JNO2 /= 0.0_sp ) THEN
!      IF ( (EXPONENT(JOH)-EXPONENT(JNO2)) < MAXEXPONENT(JOH) ) THEN
!         VARS(6) = JOH / JNO2
!      ENDIF
!   ENDIF

! old
!   ! NOx concetration in ambient air, ppt
!   VARS(7) = ( ( ExtState%NO%Arr%Val(I,J,1)  / AIR        &
!           *     HcoState%Phys%AIRMW         / MW_NO  )   &
!           +   ( ExtState%NO2%Arr%Val(I,J,1) / AIR        &
!           *     HcoState%Phys%AIRMW         / MW_NO2 ) ) &
!           * 1.e12_sp
! new
   ! NOx concetration in ambient air, ppt
   ! NOTE: ExtState vars NO and NO2 units are now kg/kg dry air (ewl, 9/11/15)
!   VARS(7) = ( ( ExtState%NO%Arr%Val(I,J,1)               &
!           *     HcoState%Phys%AIRMW         / Inst%MW_NO  )   &
!           +   ( ExtState%NO2%Arr%Val(I,J,1)              &
!           *     HcoState%Phys%AIRMW         / Inst%MW_NO2 ) ) &
!           * 1.e12_sp
! end new (ewl)

      ! Wind speed, m/s
!   VARS(8) = SQRT( ExtState%U10M%Arr%Val(I,J)**2 &

!+       ExtState%V10M%Arr%Val(I,J)**2 )

!   ! for debugging only
!   if(I==1.and.J==35)then
!      write(*,*) 'VARS(1)      : ', VARS(1)
!      write(*,*) 'VARS(2)      : ', VARS(2)
!      write(*,*) 'VARS(3)      : ', VARS(3)
!      write(*,*) 'VARS(4)      : ', VARS(4)
!      write(*,*) 'VARS(5)      : ', VARS(5)
!      write(*,*) 'VARS(6)      : ', VARS(6)
!      write(*,*) 'VARS(7)      : ', VARS(7)
!      write(*,*) 'VARS(8)      : ', VARS(8)
!      write(*,*) 'AIR          : ', ExtState%AIR%Arr%Val(I,J,1)
!      write(*,*) 'AIRMW        : ', HcoState%Phys%AIRMW
!      write(*,*) 'MWNO, NO2, O3: ', Inst%MW_NO, Inst%MW_NO2, Inst%MW_O3
!      write(*,*) 'O3conc       : ', ExtState%O3%Arr%Val(I,J,1)
!      write(*,*) 'NO,NO2 conc  : ', ExtState%NO%Arr%Val(I,J,1), &
!                                    ExtState%NO2%Arr%Val(I,J,1)
!      write(*,*) 'U, V         : ', ExtState%U10M%Arr%Val(I,J), &
!                                    ExtState%V10M%Arr%Val(I,J)
!   endif

      !*****************************************************
      ! Restoring the following lines reproduces the behavior of
      ! GEOS-Chem v9-01-03 through v9-02
      ! the LUT was indexed with the ratio J(O1D)/J(NO2) (cdh, 3/27/2014)
!      VARS(6)   = SAFE_DIV( JO1D, JNO2, 0D0 )
!      JRATIOlev = (/ 5.e-4, 0.0015, 0.0025, 0.0055 /)
      !*****************************************************

   !========================================================================
   ! Find the indices of nodes and their corresponding weights for the
   ! interpolation
   !========================================================================

   ! Temperature:
   CALL INTERPOL_LINWEIGHTS( Inst%Tlev, VARS(1), INDX(1,:), WTS(1,:) )

   ! J(NO2):
!   CALL INTERPOL_LINWEIGHTS( Inst%JNO2lev, VARS(2), INDX(2,:), WTS(2,:) )
!
!   ! [O3]:
!   CALL INTERPOL_LINWEIGHTS( Inst%O3lev, VARS(3), INDX(3,:), WTS(3,:) )
!
!   ! SEA0:
!   CALL INTERPOL_LINWEIGHTS( Inst%SEA0lev, VARS(4), INDX(4,:), WTS(4,:) )
!
!   ! SEA5:
!   CALL INTERPOL_LINWEIGHTS( Inst%SEA5lev, VARS(5), INDX(5,:), WTS(5,:) )
!
!   ! JRATIO:
!   CALL INTERPOL_LINWEIGHTS( Inst%JRATIOlev, VARS(6), INDX(6,:), WTS(6,:))
!
!   ! [NOx]:
!   CALL INTERPOL_LINWEIGHTS( Inst%NOXlev, VARS(7), INDX(7,:), WTS(7,:) )
!
!   ! Wind speed:
!   CALL INTERPOL_LINWEIGHTS( Inst%WSlev, VARS(8), INDX(8,:), WTS(8,:) )

   !========================================================================
   ! Piecewise linear interpolation
   !========================================================================

   ! Initialize
!   FNOX = 0.0d0
!   DNOx = 0.0d0
!   OPE  = 0.0d0
!   MOE  = 0.0d0

   TEMPNO = 0.0d0

   ! Loop over wind speed
!   DO I8=1,2
!
!      ! Point at the LUT for this wind speed
!      ! Last two digits in fortran variable names indicate wind speed in m/s
!      SELECT CASE ( NINT( Inst%WSlev(INDX(8,I8)) ) )
!         CASE (  2 )
!            FRACNOX_LUT => Inst%FRACNOX_LUT02
!            DNOx_LUT    => Inst%DNOx_LUT02
!            OPE_LUT     => Inst%OPE_LUT02
!            MOE_LUT     => Inst%MOE_LUT02
!         CASE (  6 )
!            FRACNOX_LUT => Inst%FRACNOX_LUT06
!            DNOx_LUT    => Inst%DNOx_LUT06
!            OPE_LUT     => Inst%OPE_LUT06
!            MOE_LUT     => Inst%MOE_LUT06
!         CASE ( 10 )
!            FRACNOX_LUT => Inst%FRACNOX_LUT10
!            DNOx_LUT    => Inst%DNOx_LUT10
!            OPE_LUT     => Inst%OPE_LUT10
!            MOE_LUT     => Inst%MOE_LUT10
!         CASE ( 14 )
!            FRACNOX_LUT => Inst%FRACNOX_LUT14
!            DNOx_LUT    => Inst%DNOx_LUT14
!            OPE_LUT     => Inst%OPE_LUT14
!            MOE_LUT     => Inst%MOE_LUT14
!         CASE ( 18 )
!            FRACNOX_LUT => Inst%FRACNOX_LUT18
!            DNOx_LUT    => Inst%DNOx_LUT18
!            OPE_LUT     => Inst%OPE_LUT18
!            MOE_LUT     => Inst%MOE_LUT18
!         CASE DEFAULT
!             MSG = 'LUT error: Wind speed interpolation error!'
!             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
!             RETURN
!      END SELECT

  ! Loop over temperature bins
   DO I1=1,2

      ! Point at the LUT for this temperature
      ! Last three digits in fortran variable names indicate temperature in F
      SELECT CASE ( NINT( Inst%Tlev(INDX(1,I1)) ) )
         CASE (  0 )
            TEMPNO_LUT  => Inst%NO_LUT000
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE (  5 )
            TEMPNO_LUT  => Inst%NO_LUT005
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 10 )
            TEMPNO_LUT  => Inst%NO_LUT010
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 15 )
            TEMPNO_LUT  => Inst%NO_LUT015
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 20 )
            TEMPNO_LUT  => Inst%NO_LUT020
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 25 )
            TEMPNO_LUT  => Inst%NO_LUT025
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 30 )
            TEMPNO_LUT  => Inst%NO_LUT030
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 35 )
            TEMPNO_LUT  => Inst%NO_LUT035
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 40 )
            TEMPNO_LUT  => Inst%NO_LUT040
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 45 )
            TEMPNO_LUT  => Inst%NO_LUT045
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 50 )
            TEMPNO_LUT  => Inst%NO_LUT050
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 55 )
            TEMPNO_LUT  => Inst%NO_LUT055
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 60 )
            TEMPNO_LUT  => Inst%NO_LUT060
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 65 )
            TEMPNO_LUT  => Inst%NO_LUT065
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 70 )
            TEMPNO_LUT  => Inst%NO_LUT070
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 75 )
            TEMPNO_LUT  => Inst%NO_LUT075
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 80 )
            TEMPNO_LUT  => Inst%NO_LUT080
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 85 )
            TEMPNO_LUT  => Inst%NO_LUT085
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 90 )
            TEMPNO_LUT  => Inst%NO_LUT090
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 95 )
            TEMPNO_LUT  => Inst%NO_LUT095
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 100 )
            TEMPNO_LUT  => Inst%NO_LUT100
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 105 )
            TEMPNO_LUT  => Inst%NO_LUT105
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 110 )
            TEMPNO_LUT  => Inst%NO_LUT110
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 115 )
            TEMPNO_LUT  => Inst%NO_LUT115
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 120 )
            TEMPNO_LUT  => Inst%NO_LUT120
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE ( 125 )
            TEMPNO_LUT  => Inst%NO_LUT125
            TEMPNO_TMP  = TEMPNO_LUT(1,1,J,I)
            WEIGHT      = WTS(1,I1)
         CASE DEFAULT
             MSG = 'LUT error: Temperature interpolation error!'
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
      END SELECT




      !*****************************************************
      ! Restoring the following lines reproduces the behavior of
      ! GEOS-Chem v9-01-03 through v9-02 in which wind speed
      ! effects on FNOx and OPE are neglected (cdh, 3/25/2014)
!      FRACNOX_LUT => FRACNOX_LUT_v913
!      OPE_LUT     => OPE_LUT_v913
      !*****************************************************

      ! loop over all other variables
!      DO I7=1,2
!      DO I6=1,2
!      DO I5=1,2
 !     DO I4=1,2
 !     DO I3=1,2
 !     DO I2=1,2
 !     DO I1=1,2

         !------------------------------------------
         ! Nodes and weights used in the interpolation
         !------------------------------------------

         ! Fraction NOx from the LUT
!         FNOX_TMP = FRACNOX_LUT( INDX(1,I1), INDX(2,I2), INDX(3,I3), &
!                                 INDX(4,I4), INDX(5,I5),             &
!                                 INDX(6,I6), INDX(7,I7) )

!         DNOX_TMP = DNOx_LUT(    INDX(1,I1), INDX(2,I2), INDX(3,I3), &
!                                 INDX(4,I4), INDX(5,I5),             &
!                                 INDX(6,I6), INDX(7,I7) )

         ! OPE from the LUT
!         OPE_TMP  = OPE_LUT(     INDX(1,I1), INDX(2,I2), INDX(3,I3), &
!                                 INDX(4,I4), INDX(5,I5),             &
!                                 INDX(6,I6), INDX(7,I7) )

         ! MOE from the LUT
!         MOE_TMP  = MOE_LUT(     INDX(1,I1), INDX(2,I2), INDX(3,I3), &
!                                 INDX(4,I4), INDX(5,I5),             &
!                                 INDX(6,I6), INDX(7,I7) )

         ! Interpolation weight for this element
!         WEIGHT = WTS(1,I1) * WTS(2,I2) * WTS(3,I3) * WTS(4,I4) * &
!                  WTS(5,I5) * WTS(6,I6) * WTS(7,I7) * WTS(8,I8)
         !-----------------------------------
         ! Error Check
         !-----------------------------------

         !IF ENCOUNTER -999 IN THE LUT PRINT ERROR!!
!         IF ( ( FNOX_TMP < 0. ) .or. ( FNOX_TMP > 1. ) ) THEN
!
!            PRINT*, 'PARANOX_LUT: fracnox = ', FNOX_TMP
!            PRINT*, 'This occured at grid box ', I, J
!            PRINT*, 'Lon/Lat: ', HcoState%Grid%XMID%Val(I,J), HcoState%Grid%YMID%Val(I,J)
!            PRINT*, 'SZA 5 hours ago : ', VARS(4)
!            PRINT*, 'SZA at this time: ', VARS(5)
!            PRINT*, 'The two SZAs should not be more than 75 deg apart!'
!            PRINT*, 'If they are, your restart SZA might be wrong.'
!            PRINT*, 'You can try to coldstart PARANOx by commenting'
!            PRINT*, 'all PARANOX_SUNCOS entries in your HEMCO'
!            PRINT*, 'configuration file.'
!
!            !print*, I1, I2, I3, I4, I5, I6, I7, I8
!            !print*, INDX(1,I1), INDX(2,I2), INDX(3,I3),  INDX(4,I4), &
!            !        INDX(5,I5), INDX(6,I6), INDX(7,I7), INDX(8,I8)
!            !print*, VARS
!
!            MSG = 'LUT error: Fracnox should be between 0 and 1!'
!            CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
!            RETURN
!         ENDIF

         !IF ENCOUNTER -999 IN THE LUT PRINT ERROR!!
!         IF ( ( TEMPNO_TMP < 0. ) .or. ( TEMPNO_TMP > 1. ) ) THEN
!
!            PRINT*, 'METEMIS_LUT: TEMPNO = ', TEMPNO_TMP
!            PRINT*, 'This occured at grid box ', I, J
!            PRINT*, 'Lon/Lat: ', HcoState%Grid%XMID%Val(I,J), HcoState%Grid%YMID%Val(I,J)
!            MSG = 'LUT error: TEMPNO should be between 0 and 125 F!'
!            CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
!            RETURN
!         ENDIF

         
         !-----------------------------------
         ! Final interpolated values
         !-----------------------------------

         ! Weighted sum of FNOx from the LUT
!         FNOx = FNOx + FNOX_TMP * WEIGHT

         ! Weighted sum of DNOx from the LUT
!         DNOx = DNOx + DNOX_TMP * WEIGHT

         ! Weighted sum of OPE from the LUT
!         OPE  = OPE + OPE_TMP * WEIGHT

         ! Weighted sum of MOE from the LUT
!         MOE  = MOE + MOE_TMP * WEIGHT

         ! Weighted sum of TempNO from the LUT
         TEMPNO = TEMPNO + TEMPNO_TMP * WEIGHT
!      END DO
!      END DO
!      END DO
!      END DO
!      END DO
!      END DO
!      END DO

      ! Free pointers
!      FRACNOX_LUT => NULL()
!      DNOx_LUT    => NULL()
!      OPE_LUT     => NULL()
!      MOE_LUT     => NULL()
       TEMPNO_LUT => NULL()
   END DO

   ! Transfer MOE if optional output parameter is present
!   IF ( PRESENT( MOE_OUT ) ) MOE_OUT = MOE

   ! Nullify pointers
!   NULLIFY( FRACNOX_LUT )
!   NULLIFY( DNOx_LUT )
!   NULLIFY( OPE_LUT  )
!   NULLIFY( MOE_LUT  )
   NULLIFY( TEMPNO_LUT )

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

       IF ( ASSOCIATED( Inst%NO_LUT000 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT000 )
       ENDIF
       Inst%NO_LUT000 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT005 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT005 )
       ENDIF
       Inst%NO_LUT005 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT010 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT010 )
       ENDIF
       Inst%NO_LUT010 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT015 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT015 )
       ENDIF
       Inst%NO_LUT015 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT020 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT020 )
       ENDIF
       Inst%NO_LUT020 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT025 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT025 )
       ENDIF
       Inst%NO_LUT025 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT030 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT030 )
       ENDIF
       Inst%NO_LUT030 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT035 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT035 )
       ENDIF
       Inst%NO_LUT035 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT040 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT040 )
       ENDIF
       Inst%NO_LUT040 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT045 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT045 )
       ENDIF
       Inst%NO_LUT045 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT050 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT050 )
       ENDIF
       Inst%NO_LUT050 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT055 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT055 )
       ENDIF
       Inst%NO_LUT055 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT060 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT060 )
       ENDIF
       Inst%NO_LUT060 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT065 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT065 )
       ENDIF
       Inst%NO_LUT065 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT070 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT070 )
       ENDIF
       Inst%NO_LUT070 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT075 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT075 )
       ENDIF
       Inst%NO_LUT075 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT080 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT080 )
       ENDIF
       Inst%NO_LUT080 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT085 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT085 )
       ENDIF
       Inst%NO_LUT085 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT090 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT090 )
       ENDIF
       Inst%NO_LUT090 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT095 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT095 )
       ENDIF
       Inst%NO_LUT095 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT100 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT100 )
       ENDIF
       Inst%NO_LUT100 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT105 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT105 )
       ENDIF
       Inst%NO_LUT105 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT110 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT110 )
       ENDIF
       Inst%NO_LUT110 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT115 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT115 )
       ENDIF
       Inst%NO_LUT115 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT120 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT120 )
       ENDIF
       Inst%NO_LUT120 => NULL()

       IF ( ASSOCIATED( Inst%NO_LUT125 ) ) THEN
          DEALLOCATE ( Inst%NO_LUT125 )
       ENDIF
       Inst%NO_LUT125 => NULL()

       IF ( ASSOCIATED( Inst%MetEmisNO ) ) THEN
          DEALLOCATE ( Inst%MetEmisNO )
       ENDIF
       Inst%MetEmisNO => NULL()
!
!       IF ( ASSOCIATED( Inst%SC5 ) ) THEN
!          DEALLOCATE ( Inst%SC5 )
!       ENDIF
!       Inst%SC5 => NULL()
!
!       IF ( ASSOCIATED( Inst%FRACNOX_LUT02 ) ) THEN
!          DEALLOCATE( Inst%FRACNOX_LUT02 )
!       ENDIF
!       Inst%FRACNOX_LUT02 => NULL()
!
!       IF ( ASSOCIATED( Inst%FRACNOX_LUT06 ) ) THEN
!          DEALLOCATE( Inst%FRACNOX_LUT06 )
!       ENDIF
!       Inst%FRACNOX_LUT06 => NULL()
!
!       IF ( ASSOCIATED( Inst%FRACNOX_LUT10 ) ) THEN
!          DEALLOCATE( Inst%FRACNOX_LUT10 )
!       ENDIF
!       Inst%FRACNOX_LUT10 => NULL()
!
!       IF ( ASSOCIATED( Inst%FRACNOX_LUT14 ) ) THEN
!          DEALLOCATE( Inst%FRACNOX_LUT14 )
!       ENDIF
!       Inst%FRACNOX_LUT14 => NULL()
!
!       IF ( ASSOCIATED( Inst%FRACNOX_LUT18 ) ) THEN
!          DEALLOCATE( Inst%FRACNOX_LUT18 )
!       ENDIF
!       Inst%FRACNOX_LUT18 => NULL()
!
!       IF ( ASSOCIATED( Inst%OPE_LUT02 ) ) THEN
!          DEALLOCATE( Inst%OPE_LUT02 )
!       ENDIF
!       Inst%OPE_LUT02 => NULL()
!
!       IF ( ASSOCIATED( Inst%OPE_LUT06 ) ) THEN
!          DEALLOCATE( Inst%OPE_LUT06 )
!       ENDIF
!       Inst%OPE_LUT06 => NULL()
!
!       IF ( ASSOCIATED( Inst%OPE_LUT10 ) ) THEN
!          DEALLOCATE( Inst%OPE_LUT10 )
!       ENDIF
!       Inst%OPE_LUT10 => NULL()
!
!       IF ( ASSOCIATED( Inst%OPE_LUT14 ) ) THEN
!          DEALLOCATE( Inst%OPE_LUT14 )
!       ENDIF
!       Inst%OPE_LUT14 => NULL()
!
!       IF ( ASSOCIATED( Inst%OPE_LUT18 ) ) THEN
!          DEALLOCATE( Inst%OPE_LUT18 )
!       ENDIF
!       Inst%OPE_LUT18 => NULL()
!
!       IF ( ASSOCIATED( Inst%MOE_LUT02 ) ) THEN
!          DEALLOCATE( Inst%MOE_LUT02 )
!       ENDIF
!       Inst%MOE_LUT02 => NULL()
!
!       IF ( ASSOCIATED( Inst%MOE_LUT06 ) ) THEN
!          DEALLOCATE( Inst%MOE_LUT06 )
!       ENDIF
!       Inst%MOE_LUT06 => NULL()
!
!       IF ( ASSOCIATED( Inst%MOE_LUT10 ) ) THEN
!          DEALLOCATE( Inst%MOE_LUT10 )
!       ENDIF
!       Inst%MOE_LUT10 => NULL()
!
!       IF ( ASSOCIATED( Inst%MOE_LUT14 ) ) THEN
!          DEALLOCATE( Inst%MOE_LUT14 )
!       ENDIF
!       Inst%MOE_LUT14 => NULL()
!
!       IF ( ASSOCIATED( Inst%MOE_LUT18 ) ) THEN
!          DEALLOCATE( Inst%MOE_LUT18 )
!       ENDIF
!       Inst%MOE_LUT18 => NULL()
!
!       IF ( ASSOCIATED( Inst%DNOx_LUT02 ) ) THEN
!          DEALLOCATE( Inst%DNOx_LUT02 )
!       ENDIF
!       Inst%DNOx_LUT02 => NULL()
!
!       IF ( ASSOCIATED( Inst%DNOx_LUT06 ) ) THEN
!          DEALLOCATE( Inst%DNOx_LUT06 )
!       ENDIF
!       Inst%DNOx_LUT06 => NULL()
!
!       IF ( ASSOCIATED( Inst%DNOx_LUT10 ) ) THEN
!          DEALLOCATE( Inst%DNOx_LUT10 )
!       ENDIF
!       Inst%DNOx_LUT10 => NULL()
!
!       IF ( ASSOCIATED( Inst%DNOx_LUT14 ) ) THEN
!          DEALLOCATE( Inst%DNOx_LUT14 )
!       ENDIF
!       Inst%DNOx_LUT14 => NULL()
!
!       IF ( ASSOCIATED( Inst%DNOx_LUT18 ) ) THEN
!          DEALLOCATE( Inst%DNOx_LUT18 )
!       ENDIF
!       Inst%DNOx_LUT18 => NULL()
!
!       IF ( ASSOCIATED( Inst%DEPO3 ) ) THEN
!          DEALLOCATE( Inst%DEPO3  )
!       ENDIF
!       Inst%DEPO3 => NULL()
!
!       IF ( ASSOCIATED( Inst%DEPHNO3 ) ) THEN
!          DEALLOCATE( Inst%DEPHNO3 )
!       ENDIF
!       Inst%DEPHNO3 => NULL()

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
