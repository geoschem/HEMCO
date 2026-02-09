!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_metemis_mod.F90
!
! !DESCRIPTION: Module HCOX\_METEMIS\_MOD contains routines to
! compute mobile source emissions based on inputs from the U.S. EPA MOVES
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
!  11 Mar 2025 - P.C. Campbell   - Initial NOx only version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:

  ! Number of values for each variable in the provided input look-up table
  ! CONUS MetEmis Tables with  10 temperature bins
   INTEGER, PARAMETER ::  nT=10    !10 Temperature bins in degrees F

  ! Now place all module variables in a lderived type object (for a linked
  ! list) so that we can have one instance per node in an MPI environment.
  TYPE :: MyInst

     ! Scalars
     INTEGER               :: Instance
     INTEGER               :: ExtNr
     INTEGER               :: IDTNO
     INTEGER               :: IDTNO2
     INTEGER               :: IDTHONO
     INTEGER               :: IDTCO
     INTEGER               :: IDTSO2
     INTEGER               :: IDTNH3
     INTEGER               :: IDTCH4
     INTEGER               :: IDTACROLEIN
     INTEGER               :: IDTBUTADIENE13
     INTEGER               :: IDTETHY

     INTEGER               :: IDTTERP
     INTEGER               :: IDTFORM
     INTEGER               :: IDTPAR
     INTEGER               :: IDTIOLE
     INTEGER               :: IDTOLE
     INTEGER               :: IDTETH
     INTEGER               :: IDTETHA
     INTEGER               :: IDTETOH
     INTEGER               :: IDTMEOH
     INTEGER               :: IDTBENZ

     INTEGER               :: IDTTOL
     INTEGER               :: IDTXYLMN
     INTEGER               :: IDTNAPH
     INTEGER               :: IDTALD2
     INTEGER               :: IDTALDX
     INTEGER               :: IDTISOP
     INTEGER               :: IDTPRPA
     INTEGER               :: IDTACET
     INTEGER               :: IDTKET
     INTEGER               :: IDTALD2_PRIMARY

     INTEGER               :: IDTFORM_PRIMARY
     INTEGER               :: IDTSOAALK
     INTEGER               :: IDTPEC
     INTEGER               :: IDTPOC
     INTEGER               :: IDTPAL
     INTEGER               :: IDTPCA
     INTEGER               :: IDTPCL
     INTEGER               :: IDTPFE
     INTEGER               :: IDTPH2O
     INTEGER               :: IDTPK

     INTEGER               :: IDTPMG
     INTEGER               :: IDTPMN
     INTEGER               :: IDTPMOTHR
     INTEGER               :: IDTPNA
     INTEGER               :: IDTPNCOM
     INTEGER               :: IDTPNH4
     INTEGER               :: IDTPNO3
     INTEGER               :: IDTPTI
     INTEGER               :: IDTPSI
     INTEGER               :: IDTPMC

     INTEGER               :: IDTPSO4

     LOGICAL               :: RHUMGASDIS  ! Apply humidity correction for split
                                          ! of NOx and HONO gas and diesel fuels
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
!  11 Mar 2025 - P. C. Campbell   - Initial NOx Version
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
        CALL HCO_ERROR( 'ERROR 1', RC, THISLOC=LOC )
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
    REAL(hp), TARGET         :: FLUXNO   (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXNO2  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXHONO (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXCO   (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXSO2  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXNH3  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXCH4  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXACROLEIN  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXBUTADIENE13  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXETHY (HcoState%NX,HcoState%NY)

    REAL(hp), TARGET         :: FLUXTERP (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXFORM (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPAR  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXIOLE (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXOLE  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXETH  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXETHA (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXETOH (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXMEOH (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXBENZ (HcoState%NX,HcoState%NY)

    REAL(hp), TARGET         :: FLUXTOL  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXXYLMN(HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXNAPH (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXALD2 (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXALDX (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXISOP (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPRPA (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXACET (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXKET  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXALD2_PRIMARY (HcoState%NX,HcoState%NY)

    REAL(hp), TARGET         :: FLUXFORM_PRIMARY (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXSOAALK (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPEC  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPOC  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPAL  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPCA  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPCL  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPFE  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPH2O (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPK   (HcoState%NX,HcoState%NY)

    REAL(hp), TARGET         :: FLUXPMG  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPMN  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPMOTHR (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPNA  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPNCOM(HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPNH4 (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPNO3 (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPTI  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPSI  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET         :: FLUXPMC  (HcoState%NX,HcoState%NY)

    REAL(hp), TARGET         :: FLUXPSO4 (HcoState%NX,HcoState%NY)

    ! Pointers
    REAL(hp), POINTER        :: Arr2D(:,:)

    ! For diagnostics
    REAL(hp), TARGET         :: DIAGN  (HcoState%NX,HcoState%NY,51)  ! number of MetEmis Species !IVAI
    LOGICAL, SAVE            :: DO_DIAGN(51) = .FALSE.
    CHARACTER(LEN=31)        :: DiagnName
    CHARACTER(LEN=31), SAVE  :: MEmisNames(51) = (/ &
       'NO',           'NO2',          'HONO',         'CO',           &
       'SO2',          'NH3',          'CH4',          'ACROLEIN',     &
       'BUTADIENE13',  'ETHY',         'TERP',         'FORM',         &
       'PAR',          'IOLE',         'OLE',          'ETH',          &
       'ETHA',         'ETOH',         'MEOH',         'BENZ',         &
       'TOL',          'XYLMN',        'NAPH',         'ALD2',         &
       'ALDX',         'ISOP',         'PRPA',         'ACET',         &
       'KET',          'ALD2_PRIMARY', 'FORM_PRIMARY', 'SOAALK',       &
       'PEC',          'POC',          'PAL',          'PCA',          &
       'PCL',          'PFE',          'PH2O',         'PK',           &
       'PMG',          'PMN',          'PMOTHR',       'PNA',          &
       'PNCOM',        'PNH4',         'PNO3',         'PTI',          &
       'PSI',          'PMC',          'PSO4'          /)
    TYPE(DiagnCont), POINTER :: TmpCnt
    INTEGER                  :: N

    !MetEmis Diag Update
    REAL(dp)                 :: TEMP_NO
    REAL(dp)                 :: TEMP_NO2
    REAL(dp)                 :: TEMP_HONO
    REAL(dp)                 :: TEMP_CO
    REAL(dp)                 :: TEMP_SO2
    REAL(dp)                 :: TEMP_NH3
    REAL(dp)                 :: TEMP_CH4
    REAL(dp)                 :: TEMP_ACROLEIN
    REAL(dp)                 :: TEMP_BUTADIENE13
    REAL(dp)                 :: TEMP_ETHY

    REAL(dp)                 :: TEMP_TERP
    REAL(dp)                 :: TEMP_FORM
    REAL(dp)                 :: TEMP_PAR
    REAL(dp)                 :: TEMP_IOLE
    REAL(dp)                 :: TEMP_OLE
    REAL(dp)                 :: TEMP_ETH
    REAL(dp)                 :: TEMP_ETHA
    REAL(dp)                 :: TEMP_ETOH
    REAL(dp)                 :: TEMP_MEOH
    REAL(dp)                 :: TEMP_BENZ

    REAL(dp)                 :: TEMP_TOL
    REAL(dp)                 :: TEMP_XYLMN
    REAL(dp)                 :: TEMP_NAPH
    REAL(dp)                 :: TEMP_ALD2
    REAL(dp)                 :: TEMP_ALDX
    REAL(dp)                 :: TEMP_ISOP
    REAL(dp)                 :: TEMP_PRPA
    REAL(dp)                 :: TEMP_ACET
    REAL(dp)                 :: TEMP_KET
    REAL(dp)                 :: TEMP_ALD2_PRIMARY

    REAL(dp)                 :: TEMP_FORM_PRIMARY
    REAL(dp)                 :: TEMP_SOAALK
    REAL(dp)                 :: TEMP_PEC
    REAL(dp)                 :: TEMP_POC
    REAL(dp)                 :: TEMP_PAL
    REAL(dp)                 :: TEMP_PCA
    REAL(dp)                 :: TEMP_PCL
    REAL(dp)                 :: TEMP_PFE
    REAL(dp)                 :: TEMP_PH2O
    REAL(dp)                 :: TEMP_PK

    REAL(dp)                 :: TEMP_PMG
    REAL(dp)                 :: TEMP_PMN
    REAL(dp)                 :: TEMP_PMOTHR
    REAL(dp)                 :: TEMP_PNA
    REAL(dp)                 :: TEMP_PNCOM
    REAL(dp)                 :: TEMP_PNH4
    REAL(dp)                 :: TEMP_PNO3
    REAL(dp)                 :: TEMP_PTI
    REAL(dp)                 :: TEMP_PSI
    REAL(dp)                 :: TEMP_PMC

    REAL(dp)                 :: TEMP_PSO4


    !=================================================================
    ! MetEmis begins here!
    !=================================================================
    LOC = 'MetEmis (HCOX_METEMIS_MOD.F90)'

    ! Enter
    CALL HCO_ENTER(HcoState%Config%Err, LOC, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 2', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Leave here if none of the tracers defined
     IF ( Inst%IDTNO <= 0) THEN  !NO
       RC = HCO_SUCCESS
       RETURN
    ENDIF

     IF ( Inst%IDTNO2 <= 0) THEN  !NO2
       RC = HCO_SUCCESS
       RETURN
    ENDIF

     IF ( Inst%IDTHONO <= 0) THEN  !HONO
       RC = HCO_SUCCESS
       RETURN
    ENDIF

    IF ( Inst%IDTCO  <= 0) THEN  !CO
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTSO2 <= 0) THEN  !SO2
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTNH3 <= 0) THEN  !NH3
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTCH4 <= 0) THEN  !CH4
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTACROLEIN <= 0) THEN  !ACROLEIN
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTBUTADIENE13 <= 0) THEN  !BUTADIENE13
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTETHY <= 0) THEN  !ETHY
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTTERP <= 0) THEN  !TERP
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTFORM <= 0) THEN  !FORM
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPAR <= 0) THEN  !PAR
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTIOLE <= 0) THEN  !IOLE
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTOLE <= 0) THEN  !OLE
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTETH <= 0) THEN  !ETH
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTETHA <= 0) THEN  !ETHA
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTETOH <= 0) THEN  !ETOH
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTMEOH <= 0) THEN  !MEOH
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTBENZ <= 0) THEN  !BENZ
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTTOL <= 0) THEN  !TOL
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTXYLMN <= 0) THEN  !XYLMN
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTNAPH <= 0) THEN  !NAPH
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTALD2 <= 0) THEN  !ALD2
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTALDX <= 0) THEN  !ALDX
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTISOP <= 0) THEN  !ISOP
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPRPA <= 0) THEN  !PRPA
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTACET <= 0) THEN  !ACET
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTKET <= 0) THEN  !KET
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTALD2_PRIMARY <= 0) THEN  !ALD2_PRIMARY
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTFORM_PRIMARY<= 0) THEN  !FORM_PRIMARY
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTSOAALK <= 0) THEN  !SOAALK
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPEC <= 0) THEN  !PEC
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPOC <= 0) THEN  !POC
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPAL <= 0) THEN  !PAL
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPCA <= 0) THEN  !PCA
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPCL <= 0) THEN  !PCL
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPFE <= 0) THEN  !PFE
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPH2O <= 0) THEN  !PH2O
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPK  <= 0) THEN  !PK
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPMG <= 0) THEN  !PMG
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPMN <= 0) THEN  !PMN
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPMOTHR <= 0) THEN  !PMOTHR
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPNA <= 0) THEN  !PNA
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPNCOM <= 0) THEN  !PNCOM
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPNH4 <= 0) THEN  !PNH4
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPNO3 <= 0) THEN  !PNO3
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPTI <= 0) THEN  !PTI
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPSI <= 0) THEN  !PSI
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPMC <= 0) THEN  !PMC
      RC = HCO_SUCCESS
      RETURN
    ENDIF

    IF ( Inst%IDTPSO4 <= 0) THEN  !PSO4
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
       DO N=1, 51
          DiagnName = TRIM(MEmisNames(N)) // '_MetEmis_OR'
          CALL DiagnCont_Find ( HcoState%Diagn, -1, -1, -1, -1, -1, &
                                DiagnName, 0, DO_DIAGN(N), TmpCnt )
          TmpCnt => NULL()
       ENDDO
    ENDIF


    ! Clear diagnostics array
    IF ( ANY(DO_DIAGN) ) DIAGN(:,:,:) = 0.0_hp

    ! Error check
    ERR = .FALSE.

    ! Initialize
    FLUXNO        = 0.0_hp
    FLUXNO2       = 0.0_hp
    FLUXHONO      = 0.0_hp
    FLUXCO        = 0.0_hp
    FLUXSO2       = 0.0_hp
    FLUXNH3       = 0.0_hp
    FLUXCH4       = 0.0_hp
    FLUXACROLEIN  = 0.0_hp
    FLUXBUTADIENE13 = 0.0_hp
    FLUXETHY      = 0.0_hp

    FLUXTERP      = 0.0_hp
    FLUXFORM      = 0.0_hp
    FLUXPAR       = 0.0_hp
    FLUXIOLE      = 0.0_hp
    FLUXOLE       = 0.0_hp
    FLUXETH       = 0.0_hp
    FLUXETHA      = 0.0_hp
    FLUXETOH      = 0.0_hp
    FLUXMEOH      = 0.0_hp
    FLUXBENZ      = 0.0_hp

    FLUXTOL       = 0.0_hp
    FLUXXYLMN     = 0.0_hp
    FLUXNAPH      = 0.0_hp
    FLUXALD2      = 0.0_hp
    FLUXALDX      = 0.0_hp
    FLUXISOP      = 0.0_hp
    FLUXPRPA      = 0.0_hp
    FLUXACET      = 0.0_hp
    FLUXKET       = 0.0_hp
    FLUXALD2_PRIMARY = 0.0_hp

    FLUXFORM_PRIMARY = 0.0_hp
    FLUXSOAALK    = 0.0_hp
    FLUXPEC       = 0.0_hp
    FLUXPOC       = 0.0_hp
    FLUXPAL       = 0.0_hp
    FLUXPCA       = 0.0_hp
    FLUXPCL       = 0.0_hp
    FLUXPFE       = 0.0_hp
    FLUXPH2O      = 0.0_hp
    FLUXPK        = 0.0_hp

    FLUXPMG       = 0.0_hp
    FLUXPMN       = 0.0_hp
    FLUXPMOTHR    = 0.0_hp
    FLUXPNA       = 0.0_hp
    FLUXPNCOM     = 0.0_hp
    FLUXPNH4      = 0.0_hp
    FLUXPNO3      = 0.0_hp
    FLUXPTI       = 0.0_hp
    FLUXPSI       = 0.0_hp
    FLUXPMC       = 0.0_hp

    FLUXPSO4      = 0.0_hp

    DO J = 1, HcoState%NY
    DO I = 1, HcoState%NX

       TEMP_NO     = 0.0_hp
       TEMP_NO2    = 0.0_hp
       TEMP_HONO   = 0.0_hp
       TEMP_CO     = 0.0_hp
       TEMP_SO2    = 0.0_hp
       TEMP_NH3    = 0.0_hp
       TEMP_CH4    = 0.0_hp
       TEMP_ACROLEIN  = 0.0_hp
       TEMP_BUTADIENE13 = 0.0_hp
       TEMP_ETHY   = 0.0_hp

       TEMP_TERP   = 0.0_hp
       TEMP_FORM   = 0.0_hp
       TEMP_PAR    = 0.0_hp
       TEMP_IOLE   = 0.0_hp
       TEMP_OLE    = 0.0_hp
       TEMP_ETH    = 0.0_hp
       TEMP_ETHA   = 0.0_hp
       TEMP_ETOH   = 0.0_hp
       TEMP_MEOH   = 0.0_hp
       TEMP_BENZ   = 0.0_hp

       TEMP_TOL    = 0.0_hp
       TEMP_XYLMN  = 0.0_hp
       TEMP_NAPH   = 0.0_hp
       TEMP_ALD2   = 0.0_hp
       TEMP_ALDX   = 0.0_hp
       TEMP_ISOP   = 0.0_hp
       TEMP_PRPA   = 0.0_hp
       TEMP_ACET   = 0.0_hp
       TEMP_KET    = 0.0_hp
       TEMP_ALD2_PRIMARY = 0.0_hp

       TEMP_FORM_PRIMARY = 0.0_hp
       TEMP_SOAALK = 0.0_hp
       TEMP_PEC    = 0.0_hp
       TEMP_POC    = 0.0_hp
       TEMP_PAL    = 0.0_hp
       TEMP_PCA    = 0.0_hp
       TEMP_PCL    = 0.0_hp
       TEMP_PFE    = 0.0_hp
       TEMP_PH2O   = 0.0_hp
       TEMP_PK     = 0.0_hp

       TEMP_PMG    = 0.0_hp
       TEMP_PMN    = 0.0_hp
       TEMP_PMOTHR = 0.0_hp
       TEMP_PNA    = 0.0_hp
       TEMP_PNCOM  = 0.0_hp
       TEMP_PNH4   = 0.0_hp
       TEMP_PNO3   = 0.0_hp
       TEMP_PTI    = 0.0_hp
       TEMP_PSI    = 0.0_hp
       TEMP_PMC    = 0.0_hp

       TEMP_PSO4   = 0.0_hp

       !---------------------------------------------------------------------
       ! MetEmis lookup table for emissions based on temperature
       ! (P.C. Campbell, 03/19/2025)
       !---------------------------------------------------------------------
       CALL METEMIS_LUT( ExtState,  HcoState,  Inst,   I,   J,   RC,                     &
                         TEMP_NO,  TEMP_NO2, TEMP_HONO, TEMP_CO,  TEMP_SO2,              &
                         TEMP_NH3, TEMP_CH4, TEMP_ACROLEIN, TEMP_BUTADIENE13, TEMP_ETHY, &
                         TEMP_TERP, TEMP_FORM, TEMP_PAR, TEMP_IOLE , TEMP_OLE ,          &
                         TEMP_ETH , TEMP_ETHA , TEMP_ETOH, TEMP_MEOH, TEMP_BENZ,         &
                         TEMP_TOL, TEMP_XYLMN, TEMP_NAPH, TEMP_ALD2, TEMP_ALDX,          &
                         TEMP_ISOP, TEMP_PRPA, TEMP_ACET, TEMP_KET, TEMP_ALD2_PRIMARY,   &
                         TEMP_FORM_PRIMARY, TEMP_SOAALK, TEMP_PEC, TEMP_POC, TEMP_PAL,   &
                         TEMP_PCA, TEMP_PCL, TEMP_PFE, TEMP_PH2O, TEMP_PK,               &
                         TEMP_PMG, TEMP_PMN, TEMP_PMOTHR, TEMP_PNA, TEMP_PNCOM,          &
                         TEMP_PNH4, TEMP_PNO3, TEMP_PTI, TEMP_PSI, TEMP_PMC,             &
                         TEMP_PSO4)

       IF ( RC /= HCO_SUCCESS ) THEN
          ERR = .TRUE.; EXIT
       ENDIF

!       !---------------------------------------------------------------------
!       ! Calculate emissions
!       !---------------------------------------------------------------------
       IF ( Inst%IDTNO > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXNO(I,J) = TEMP_NO
       ENDIF

       IF ( Inst%IDTNO2 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXNO2(I,J) = TEMP_NO2
       ENDIF
!
       IF ( Inst%IDTHONO > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXHONO(I,J) = TEMP_HONO
       ENDIF
!
       IF ( Inst%IDTCO  > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXCO (I,J) = TEMP_CO
       ENDIF

       IF ( Inst%IDTSO2 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXSO2(I,J) = TEMP_SO2
       ENDIF

       IF ( Inst%IDTNH3 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXNH3(I,J) = TEMP_NH3
       ENDIF

       IF ( Inst%IDTCH4 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXCH4(I,J) = TEMP_CH4
       ENDIF

       IF ( Inst%IDTACROLEIN > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXACROLEIN(I,J) = TEMP_ACROLEIN
       ENDIF

       IF ( Inst%IDTBUTADIENE13 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXBUTADIENE13 (I,J) = TEMP_BUTADIENE13
       ENDIF

       IF ( Inst%IDTETHY > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXETHY(I,J) = TEMP_ETHY
       ENDIF

       IF ( Inst%IDTTERP > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXTERP(I,J) = TEMP_TERP
       ENDIF

       IF ( Inst%IDTFORM > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXFORM(I,J) = TEMP_FORM
       ENDIF

       IF ( Inst%IDTPAR > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPAR(I,J) = TEMP_PAR
       ENDIF

       IF ( Inst%IDTIOLE > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXIOLE(I,J) = TEMP_IOLE
       ENDIF

       IF ( Inst%IDTOLE > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXOLE(I,J) = TEMP_OLE
       ENDIF

       IF ( Inst%IDTETH > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXETH(I,J) = TEMP_ETH
       ENDIF

       IF ( Inst%IDTETHA > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXETHA(I,J) = TEMP_ETHA
       ENDIF

       IF ( Inst%IDTETOH > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXETOH(I,J) = TEMP_ETOH
       ENDIF

       IF ( Inst%IDTMEOH > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXMEOH(I,J) = TEMP_MEOH
       ENDIF

       IF ( Inst%IDTBENZ > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXBENZ(I,J) = TEMP_BENZ
       ENDIF

       IF ( Inst%IDTTOL > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXTOL(I,J) = TEMP_TOL
       ENDIF

       IF ( Inst%IDTXYLMN > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXXYLMN(I,J) = TEMP_XYLMN
       ENDIF

       IF ( Inst%IDTNAPH > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXNAPH(I,J) = TEMP_NAPH
       ENDIF

       IF ( Inst%IDTALD2 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXALD2(I,J) = TEMP_ALD2
       ENDIF

       IF ( Inst%IDTALDX > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXALDX(I,J) = TEMP_ALDX
       ENDIF

       IF ( Inst%IDTISOP > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXISOP(I,J) = TEMP_ISOP
       ENDIF

       IF ( Inst%IDTPRPA > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPRPA(I,J) = TEMP_PRPA
       ENDIF

       IF ( Inst%IDTACET > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXACET(I,J) = TEMP_ACET
       ENDIF

       IF ( Inst%IDTKET > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXKET(I,J) = TEMP_KET
       ENDIF

       IF ( Inst%IDTALD2_PRIMARY > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXALD2_PRIMARY(I,J) = TEMP_ALD2_PRIMARY
       ENDIF

       IF ( Inst%IDTFORM_PRIMARY > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXFORM_PRIMARY(I,J) = TEMP_FORM_PRIMARY
       ENDIF

       IF ( Inst%IDTSOAALK> 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXSOAALK(I,J) = TEMP_SOAALK
       ENDIF

       IF ( Inst%IDTPEC > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPEC(I,J) = TEMP_PEC
       ENDIF

       IF ( Inst%IDTPOC > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPOC(I,J) = TEMP_POC
       ENDIF

       IF ( Inst%IDTPAL > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPAL(I,J) = TEMP_PAL
       ENDIF

       IF ( Inst%IDTPCA > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPCA(I,J) = TEMP_PCA
       ENDIF

       IF ( Inst%IDTPCL > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPCL(I,J) = TEMP_PCL
       ENDIF

       IF ( Inst%IDTPFE > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPFE(I,J) = TEMP_PFE
       ENDIF

       IF ( Inst%IDTPH2O > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPH2O(I,J) = TEMP_PH2O
       ENDIF

       IF ( Inst%IDTPK > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPK (I,J) = TEMP_PK
       ENDIF

       IF ( Inst%IDTPMG > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPMG(I,J) = TEMP_PMG
       ENDIF

       IF ( Inst%IDTPMN > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPMN(I,J) = TEMP_PMN
       ENDIF

       IF ( Inst%IDTPMOTHR > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPMOTHR(I,J) = TEMP_PMOTHR
       ENDIF

       IF ( Inst%IDTPNA > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPNA(I,J) = TEMP_PNA
       ENDIF

       IF ( Inst%IDTPNCOM> 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPNCOM (I,J) = TEMP_PNCOM
       ENDIF

       IF ( Inst%IDTPNH4 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPNH4 (I,J) = TEMP_PNH4
       ENDIF

       IF ( Inst%IDTPNO3 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPNO3 (I,J) = TEMP_PNO3
       ENDIF

       IF ( Inst%IDTPTI > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPTI(I,J) = TEMP_PTI
       ENDIF

       IF ( Inst%IDTPSI > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPSI(I,J) = TEMP_PSI
       ENDIF

       IF ( Inst%IDTPMC > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPMC(I,J) = TEMP_PMC
       ENDIF

       IF ( Inst%IDTPSO4 > 0 ) THEN
!           ! Unit: kg/m2/s
           FLUXPSO4(I,J) = TEMP_PSO4
       ENDIF



!
       !---------------------------------------------------------------------
       ! Eventually write out into diagnostics array
       !---------------------------------------------------------------------
       IF ( ANY(DO_DIAGN) ) THEN
           DIAGN(I,J,1) =  FLUXNO   (I,J)
           DIAGN(I,J,2) =  FLUXNO2  (I,J)
           DIAGN(I,J,3) =  FLUXHONO (I,J)
           DIAGN(I,J,4) =  FLUXCO   (I,J)
           DIAGN(I,J,5) =  FLUXSO2  (I,J)
           DIAGN(I,J,6) =  FLUXNH3  (I,J)
           DIAGN(I,J,7) =  FLUXCH4  (I,J)
           DIAGN(I,J,8) =  FLUXACROLEIN (I,J)
           DIAGN(I,J,9) =  FLUXBUTADIENE13(I,J)
           DIAGN(I,J,10)=  FLUXETHY (I,J)

           DIAGN(I,J,11)=  FLUXTERP (I,J)
           DIAGN(I,J,12)=  FLUXFORM (I,J)
           DIAGN(I,J,13)=  FLUXPAR  (I,J)
           DIAGN(I,J,14)=  FLUXIOLE (I,J)
           DIAGN(I,J,15)=  FLUXOLE  (I,J)
           DIAGN(I,J,16)=  FLUXETH  (I,J)
           DIAGN(I,J,17)=  FLUXETHA (I,J)
           DIAGN(I,J,18)=  FLUXETOH (I,J)
           DIAGN(I,J,19)=  FLUXMEOH (I,J)
           DIAGN(I,J,20)=  FLUXBENZ (I,J)

           DIAGN(I,J,21)=  FLUXTOL  (I,J)
           DIAGN(I,J,22)=  FLUXXYLMN(I,J)
           DIAGN(I,J,23)=  FLUXNAPH (I,J)
           DIAGN(I,J,24)=  FLUXALD2 (I,J)
           DIAGN(I,J,25)=  FLUXALDX (I,J)
           DIAGN(I,J,26)=  FLUXISOP (I,J)
           DIAGN(I,J,27)=  FLUXPRPA (I,J)
           DIAGN(I,J,28)=  FLUXACET (I,J)
           DIAGN(I,J,29)=  FLUXKET  (I,J)
           DIAGN(I,J,30)=  FLUXALD2_PRIMARY(I,J)

           DIAGN(I,J,31)=  FLUXFORM_PRIMARY(I,J)
           DIAGN(I,J,32)=  FLUXSOAALK(I,J)
           DIAGN(I,J,33)=  FLUXPEC  (I,J)
           DIAGN(I,J,34)=  FLUXPOC  (I,J)
           DIAGN(I,J,35)=  FLUXPAL  (I,J)
           DIAGN(I,J,36)=  FLUXPCA  (I,J)
           DIAGN(I,J,37)=  FLUXPCL  (I,J)
           DIAGN(I,J,38)=  FLUXPFE  (I,J)
           DIAGN(I,J,39)=  FLUXPH2O (I,J)
           DIAGN(I,J,40)=  FLUXPK   (I,J)

           DIAGN(I,J,41)=  FLUXPMG  (I,J)
           DIAGN(I,J,42)=  FLUXPMN  (I,J)
           DIAGN(I,J,43)=  FLUXPMOTHR(I,J)
           DIAGN(I,J,44)=  FLUXPNA  (I,J)
           DIAGN(I,J,45)=  FLUXPNCOM(I,J)
           DIAGN(I,J,46)=  FLUXPNH4 (I,J)
           DIAGN(I,J,47)=  FLUXPNO3 (I,J)
           DIAGN(I,J,48)=  FLUXPTI  (I,J)
           DIAGN(I,J,49)=  FLUXPSI  (I,J)
           DIAGN(I,J,50)=  FLUXPMC  (I,J)

           DIAGN(I,J,51)=  FLUXPSO4 (I,J)


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

    IF ( Inst%IDTNO > 0 ) THEN

       ! Add flux to emission array
       CALL HCO_EmisAdd( HcoState, FLUXNO, Inst%IDTNO, &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXNO', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTNO2 > 0 ) THEN

       ! Add flux to emission array NO2
       CALL HCO_EmisAdd( HcoState, FLUXNO2, Inst%IDTNO2, &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXNO2', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTHONO > 0 ) THEN

       ! Add flux to emission array HONO
       CALL HCO_EmisAdd( HcoState, FLUXHONO, Inst%IDTHONO, &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXHONO', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTCO  > 0 ) THEN

       ! Add flux to emission array CO
       CALL HCO_EmisAdd( HcoState, FLUXCO , Inst%IDTCO , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXCO ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTSO2 > 0 ) THEN

       ! Add flux to emission array SO2
       CALL HCO_EmisAdd( HcoState, FLUXSO2 , Inst%IDTSO2 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXSO2 ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTNH3 > 0 ) THEN

       ! Add flux to emission array NH3
       CALL HCO_EmisAdd( HcoState, FLUXNH3 , Inst%IDTNH3 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXNH3 ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTCH4 > 0 ) THEN

       ! Add flux to emission array CH4
       CALL HCO_EmisAdd( HcoState, FLUXCH4 , Inst%IDTCH4 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXCH4 ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTACROLEIN > 0 ) THEN

       ! Add flux to emission array ACROLEIN
       CALL HCO_EmisAdd( HcoState, FLUXACROLEIN , Inst%IDTACROLEIN , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXACROLEIN ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTBUTADIENE13 > 0 ) THEN

       ! Add flux to emission array BUTADIENE13
       CALL HCO_EmisAdd( HcoState, FLUXBUTADIENE13 , Inst%IDTBUTADIENE13 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXBUTADIENE13 ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTETHY > 0 ) THEN

       ! Add flux to emission array ETHY
       CALL HCO_EmisAdd( HcoState, FLUXETHY , Inst%IDTETHY , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXETHY ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTTERP > 0 ) THEN

       ! Add flux to emission array TERP
       CALL HCO_EmisAdd( HcoState, FLUXTERP , Inst%IDTTERP , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXTERP ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTFORM > 0 ) THEN

       ! Add flux to emission array FORM
       CALL HCO_EmisAdd( HcoState, FLUXFORM , Inst%IDTFORM , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXFORM ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPAR > 0 ) THEN

       ! Add flux to emission array PAR
       CALL HCO_EmisAdd( HcoState, FLUXPAR , Inst%IDTPAR , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPAR ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTIOLE > 0 ) THEN

       ! Add flux to emission array IOLE
       CALL HCO_EmisAdd( HcoState, FLUXIOLE , Inst%IDTIOLE , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXIOLE ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTOLE > 0 ) THEN

       ! Add flux to emission array OLE
       CALL HCO_EmisAdd( HcoState, FLUXOLE , Inst%IDTOLE , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXOLE ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTETH > 0 ) THEN

       ! Add flux to emission array ETH
       CALL HCO_EmisAdd( HcoState, FLUXETH , Inst%IDTETH , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXETH ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTETHA > 0 ) THEN

       ! Add flux to emission array ETHA
       CALL HCO_EmisAdd( HcoState, FLUXETHA , Inst%IDTETHA , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXETHA ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTETOH > 0 ) THEN

       ! Add flux to emission array ETOH
       CALL HCO_EmisAdd( HcoState, FLUXETOH , Inst%IDTETOH , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXETOH ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTMEOH > 0 ) THEN

       ! Add flux to emission array MEOH
       CALL HCO_EmisAdd( HcoState, FLUXMEOH , Inst%IDTMEOH , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXMEOH ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTBENZ > 0 ) THEN

       ! Add flux to emission array BENZ
       CALL HCO_EmisAdd( HcoState, FLUXBENZ , Inst%IDTBENZ , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXBENZ ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTTOL > 0 ) THEN

       ! Add flux to emission array TOL
       CALL HCO_EmisAdd( HcoState, FLUXTOL , Inst%IDTTOL , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXTOL ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTXYLMN > 0 ) THEN

       ! Add flux to emission array XYLMN
       CALL HCO_EmisAdd( HcoState, FLUXXYLMN , Inst%IDTXYLMN , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXXYLMN ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTNAPH > 0 ) THEN

       ! Add flux to emission array NAPH
       CALL HCO_EmisAdd( HcoState, FLUXNAPH , Inst%IDTNAPH , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXNAPH ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTALD2 > 0 ) THEN

       ! Add flux to emission array ALD2
       CALL HCO_EmisAdd( HcoState, FLUXALD2 , Inst%IDTALD2 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXALD2 ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTALDX > 0 ) THEN

       ! Add flux to emission array ALDX
       CALL HCO_EmisAdd( HcoState, FLUXALDX , Inst%IDTALDX , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXALDX ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTISOP > 0 ) THEN

       ! Add flux to emission array ISOP
       CALL HCO_EmisAdd( HcoState, FLUXISOP , Inst%IDTISOP , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXISOP ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPRPA > 0 ) THEN

       ! Add flux to emission array PRPA
       CALL HCO_EmisAdd( HcoState, FLUXPRPA , Inst%IDTPRPA , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPRPA ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTACET > 0 ) THEN

       ! Add flux to emission array ACET
       CALL HCO_EmisAdd( HcoState, FLUXACET , Inst%IDTACET , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXACET ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTKET > 0 ) THEN

       ! Add flux to emission array KET
       CALL HCO_EmisAdd( HcoState, FLUXKET , Inst%IDTKET , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXKET ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTALD2_PRIMARY > 0 ) THEN

       ! Add flux to emission array ALD2_PRIMARY
       CALL HCO_EmisAdd( HcoState, FLUXALD2_PRIMARY , Inst%IDTALD2_PRIMARY , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXALD2_PRIMARY ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTFORM_PRIMARY > 0 ) THEN

       ! Add flux to emission array FORM_PRIMARY
       CALL HCO_EmisAdd( HcoState, FLUXFORM_PRIMARY , Inst%IDTFORM_PRIMARY , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXFORM_PRIMARY ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTSOAALK > 0 ) THEN

       ! Add flux to emission array SOAALK
       CALL HCO_EmisAdd( HcoState, FLUXSOAALK , Inst%IDTSOAALK , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXSOAALK ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPEC > 0 ) THEN

       ! Add flux to emission array PEC
       CALL HCO_EmisAdd( HcoState, FLUXPEC , Inst%IDTPEC , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPEC ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPOC > 0 ) THEN

       ! Add flux to emission array POC
       CALL HCO_EmisAdd( HcoState, FLUXPOC , Inst%IDTPOC , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPOC ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPAL > 0 ) THEN

       ! Add flux to emission array PAL
       CALL HCO_EmisAdd( HcoState, FLUXPAL , Inst%IDTPAL , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPAL ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPCA > 0 ) THEN

       ! Add flux to emission array PCA
       CALL HCO_EmisAdd( HcoState, FLUXPCA , Inst%IDTPCA , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPCA ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPCL > 0 ) THEN

       ! Add flux to emission array PCL
       CALL HCO_EmisAdd( HcoState, FLUXPCL , Inst%IDTPCL , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPCL ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPFE > 0 ) THEN

       ! Add flux to emission array PFE
       CALL HCO_EmisAdd( HcoState, FLUXPFE , Inst%IDTPFE , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPFE ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPH2O > 0 ) THEN

       ! Add flux to emission array PH2O
       CALL HCO_EmisAdd( HcoState, FLUXPH2O , Inst%IDTPH2O , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPH2O ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPK > 0 ) THEN

       ! Add flux to emission array PK
       CALL HCO_EmisAdd( HcoState, FLUXPK , Inst%IDTPK , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPK ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPMG > 0 ) THEN

       ! Add flux to emission array PMG
       CALL HCO_EmisAdd( HcoState, FLUXPMG , Inst%IDTPMG , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPMG ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPMN > 0 ) THEN

       ! Add flux to emission array PMN
       CALL HCO_EmisAdd( HcoState, FLUXPMN , Inst%IDTPMN , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPMN ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPMOTHR > 0 ) THEN

       ! Add flux to emission array PMOTHR
       CALL HCO_EmisAdd( HcoState, FLUXPMOTHR , Inst%IDTPMOTHR , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPMOTHR ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPNA > 0 ) THEN

       ! Add flux to emission array PNA
       CALL HCO_EmisAdd( HcoState, FLUXPNA , Inst%IDTPNA , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPNA ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPNCOM > 0 ) THEN

       ! Add flux to emission array PNCOM
       CALL HCO_EmisAdd( HcoState, FLUXPNCOM , Inst%IDTPNCOM , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPNCOM ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPNH4 > 0 ) THEN

       ! Add flux to emission array PNH4
       CALL HCO_EmisAdd( HcoState, FLUXPNH4 , Inst%IDTPNH4 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPNH4 ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPNO3 > 0 ) THEN

       ! Add flux to emission array PNO3
       CALL HCO_EmisAdd( HcoState, FLUXPNO3 , Inst%IDTPNO3 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPNO3 ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPTI > 0 ) THEN

       ! Add flux to emission array PTI
       CALL HCO_EmisAdd( HcoState, FLUXPTI , Inst%IDTPTI , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPTI ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPSI > 0 ) THEN

       ! Add flux to emission array PSI
       CALL HCO_EmisAdd( HcoState, FLUXPSI , Inst%IDTPSI , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPSI ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPMC > 0 ) THEN

       ! Add flux to emission array PMC
       CALL HCO_EmisAdd( HcoState, FLUXPMC , Inst%IDTPMC , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPMC ', RC )
          RETURN
       ENDIF
    ENDIF

    IF ( Inst%IDTPSO4 > 0 ) THEN

       ! Add flux to emission array PSO4
       CALL HCO_EmisAdd( HcoState, FLUXPSO4 , Inst%IDTPSO4 , &
                         RC,       ExtNr=Inst%ExtNr )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXPSO4 ', RC )
          RETURN
       ENDIF
    ENDIF


    ! Eventually update manual diagnostics
    DO N=1, 51
       IF ( DO_DIAGN(N) ) THEN
          DiagnName = TRIM(MEmisNames(N)) // '_MetEmis_OR'
          Arr2D     => DIAGN(:,:,N)
          CALL Diagn_Update( HcoState, ExtNr=Inst%ExtNr, &
                             cName=TRIM(DiagnName), Array2D=Arr2D, RC=RC)
          IF ( RC /= HCO_SUCCESS ) THEN
              CALL HCO_ERROR( 'Diagn_Update error: ' // TRIM(DiagnName), RC, THISLOC=LOC )
              RETURN
          ENDIF
       ENDIF
    ENDDO
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
       CALL HCO_ERROR( 'ERROR 5', RC, THISLOC=LOC )
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
      Inst%IDTNO          = -1
      Inst%IDTNO2         = -1
      Inst%IDTHONO        = -1
      Inst%IDTCO          = -1
      Inst%IDTSO2         = -1
      Inst%IDTNH3         = -1
      Inst%IDTCH4         = -1
      Inst%IDTACROLEIN    = -1
      Inst%IDTBUTADIENE13 = -1
      Inst%IDTETHY        = -1

      Inst%IDTTERP        = -1
      Inst%IDTFORM        = -1
      Inst%IDTPAR         = -1
      Inst%IDTIOLE        = -1
      Inst%IDTOLE         = -1
      Inst%IDTETH         = -1
      Inst%IDTETHA        = -1
      Inst%IDTETOH        = -1
      Inst%IDTMEOH        = -1
      Inst%IDTBENZ        = -1

      Inst%IDTTOL         = -1
      Inst%IDTXYLMN       = -1
      Inst%IDTNAPH        = -1
      Inst%IDTALD2        = -1
      Inst%IDTALDX        = -1
      Inst%IDTISOP        = -1
      Inst%IDTPRPA        = -1
      Inst%IDTACET        = -1
      Inst%IDTKET         = -1
      Inst%IDTALD2_PRIMARY= -1

      Inst%IDTFORM_PRIMARY= -1
      Inst%IDTSOAALK      = -1
      Inst%IDTPEC         = -1
      Inst%IDTPOC         = -1
      Inst%IDTPAL         = -1
      Inst%IDTPCA         = -1
      Inst%IDTPCL         = -1
      Inst%IDTPFE         = -1
      Inst%IDTPH2O        = -1
      Inst%IDTPK          = -1

      Inst%IDTPMG         = -1
      Inst%IDTPMN         = -1
      Inst%IDTPMOTHR      = -1
      Inst%IDTPNA         = -1
      Inst%IDTPNCOM       = -1
      Inst%IDTPNH4        = -1
      Inst%IDTPNO3        = -1
      Inst%IDTPTI         = -1
      Inst%IDTPSI         = -1
      Inst%IDTPMC         = -1

      Inst%IDTPSO4        = -1

      Inst%Tlev           =  0.0e0

      !------------------------------------------------------------------------
      ! Get species IDs
      !------------------------------------------------------------------------

      ! Get HEMCO species IDs
      CALL HCO_GetExtHcoID( HcoState, ExtNr, HcoIDs, SpcNames, nSpc, RC )
      IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'ERROR 6', RC, THISLOC=LOC )
          RETURN
      ENDIF

      ! Check for MetEmis Species NO/NO2/HONO/CO/SO2...
      DO I = 1, nSpc
         SELECT CASE ( TRIM(SpcNames(I)) )
            CASE ( "NO" )
               Inst%IDTNO = HcoIDs(I)
            CASE ( "NO2" )
               Inst%IDTNO2 = HcoIDs(I)
            CASE ( "HONO" )
               Inst%IDTHONO = HcoIDs(I)
            CASE ( "CO" )
               Inst%IDTCO  = HcoIDs(I)
            CASE ( "SO2" )
               Inst%IDTSO2 = HcoIDs(I)
            CASE ( "NH3" )
               Inst%IDTNH3 = HcoIDs(I)
            CASE ( "CH4" )
               Inst%IDTCH4 = HcoIDs(I)
            CASE ( "ACROLEIN" )
               Inst%IDTACROLEIN = HcoIDs(I)
            CASE ( "BUTADIENE13" )
               Inst%IDTBUTADIENE13 = HcoIDs(I)
            CASE ( "ETHY" )
               Inst%IDTETHY = HcoIDs(I)

            CASE ( "TERP" )
               Inst%IDTTERP = HcoIDs(I)
            CASE ( "FORM" )
               Inst%IDTFORM = HcoIDs(I)
            CASE ( "PAR" )
               Inst%IDTPAR = HcoIDs(I)
            CASE ( "IOLE" )
               Inst%IDTIOLE = HcoIDs(I)
            CASE ( "OLE" )
               Inst%IDTOLE = HcoIDs(I)
            CASE ( "ETH" )
               Inst%IDTETH = HcoIDs(I)
            CASE ( "ETHA" )
               Inst%IDTETHA = HcoIDs(I)
            CASE ( "ETOH" )
               Inst%IDTETOH = HcoIDs(I)
            CASE ( "MEOH" )
               Inst%IDTMEOH = HcoIDs(I)
            CASE ( "BENZ" )
               Inst%IDTBENZ = HcoIDs(I)

            CASE ( "TOL" )
               Inst%IDTTOL = HcoIDs(I)
            CASE ( "XYLMN" )
               Inst%IDTXYLMN = HcoIDs(I)
            CASE ( "NAPH" )
               Inst%IDTNAPH = HcoIDs(I)
            CASE ( "ALD2" )
               Inst%IDTALD2 = HcoIDs(I)
            CASE ( "ALDX" )
               Inst%IDTALDX = HcoIDs(I)
            CASE ( "ISOP" )
               Inst%IDTISOP = HcoIDs(I)
            CASE ( "PRPA" )
               Inst%IDTPRPA = HcoIDs(I)
            CASE ( "ACET" )
               Inst%IDTACET = HcoIDs(I)
            CASE ( "KET" )
               Inst%IDTKET = HcoIDs(I)
            CASE ( "ALD2_PRIMARY" )
               Inst%IDTALD2_PRIMARY = HcoIDs(I)

            CASE ( "FORM_PRIMARY" )
               Inst%IDTFORM_PRIMARY = HcoIDs(I)
            CASE ( "SOAALK" )
               Inst%IDTSOAALK = HcoIDs(I)
            CASE ( "PEC" )
               Inst%IDTPEC = HcoIDs(I)
            CASE ( "POC" )
               Inst%IDTPOC = HcoIDs(I)
            CASE ( "PAL" )
               Inst%IDTPAL = HcoIDs(I)
            CASE ( "PCA" )
               Inst%IDTPCA = HcoIDs(I)
            CASE ( "PCL" )
               Inst%IDTPCL = HcoIDs(I)
            CASE ( "PFE" )
               Inst%IDTPFE = HcoIDs(I)
            CASE ( "PH2O" )
               Inst%IDTPH2O = HcoIDs(I)
            CASE ( "PK" )
               Inst%IDTPK = HcoIDs(I)

            CASE ( "PMG" )
               Inst%IDTPMG = HcoIDs(I)
            CASE ( "PMN" )
               Inst%IDTPMN = HcoIDs(I)
            CASE ( "PMOTHR" )
               Inst%IDTPMOTHR = HcoIDs(I)
            CASE ( "PNA" )
               Inst%IDTPNA = HcoIDs(I)
            CASE ( "PNCOM" )
               Inst%IDTPNCOM = HcoIDs(I)
            CASE ( "PNH4" )
               Inst%IDTPNH4 = HcoIDs(I)
            CASE ( "PNO3" )
               Inst%IDTPNO3 = HcoIDs(I)
            CASE ( "PTI" )
               Inst%IDTPTI = HcoIDs(I)
            CASE ( "PSI" )
               Inst%IDTPSI = HcoIDs(I)
            CASE ( "PMC" )
               Inst%IDTPMC = HcoIDs(I)

            CASE ( "PSO4" )
               Inst%IDTPSO4 = HcoIDs(I)

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

     !-----------------------------------------------------------------
    ! Read settings
    !-----------------------------------------------------------------

    ! Read settings specified in configuration file
    ! Note: the specified strings have to match those in
    !       the config. file!


    CALL GetExtOpt( HcoState%Config, ExtNr, 'RH Gas Diesel', &
                    OptValBool=Inst%RHUMGASDIS, Found=FOUND, RC=RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 7', RC, THISLOC=LOC )
        RETURN
    ENDIF

     ! Verbose mode
    IF ( HcoState%amIRoot ) THEN
       WRITE(MSG,*) ' --> MetEmis Relative Humidity Gas Diesel Split option is ',Inst%RHUMGASDIS
       CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
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
   ExtState%T2M%DoUse                          = .TRUE.
   ExtState%QV2M%DoUse                         = .TRUE.
   ExtState%MEmisNO_GAS_OR_030%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_040%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_050%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_060%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_070%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_080%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_090%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_100%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_110%DoUse           = .TRUE.
   ExtState%MEmisNO_GAS_OR_120%DoUse           = .TRUE.

   ExtState%MEmisNO_DIS_OR_030%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_040%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_050%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_060%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_070%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_080%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_090%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_100%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_110%DoUse           = .TRUE.
   ExtState%MEmisNO_DIS_OR_120%DoUse           = .TRUE.

   ExtState%MEmisNO2_GAS_OR_030%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_040%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_050%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_060%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_070%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_080%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_090%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_100%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_110%DoUse          = .TRUE.
   ExtState%MEmisNO2_GAS_OR_120%DoUse          = .TRUE.

   ExtState%MEmisNO2_DIS_OR_030%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_040%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_050%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_060%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_070%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_080%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_090%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_100%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_110%DoUse          = .TRUE.
   ExtState%MEmisNO2_DIS_OR_120%DoUse          = .TRUE.

   ExtState%MEmisHONO_GAS_OR_030%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_040%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_050%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_060%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_070%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_080%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_090%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_100%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_110%DoUse         = .TRUE.
   ExtState%MEmisHONO_GAS_OR_120%DoUse         = .TRUE.

   ExtState%MEmisHONO_DIS_OR_030%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_040%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_050%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_060%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_070%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_080%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_090%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_100%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_110%DoUse         = .TRUE.
   ExtState%MEmisHONO_DIS_OR_120%DoUse         = .TRUE.

   ExtState%MEmisCO_OR_030%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_040%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_050%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_060%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_070%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_080%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_090%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_100%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_110%DoUse          = .TRUE.
   ExtState%MEmisCO_OR_120%DoUse          = .TRUE.

   ExtState%MEmisSO2_OR_030%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_040%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_050%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_060%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_070%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_080%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_090%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_100%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_110%DoUse          = .TRUE.
   ExtState%MEmisSO2_OR_120%DoUse          = .TRUE.

   ExtState%MEmisNH3_OR_030%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_040%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_050%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_060%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_070%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_080%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_090%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_100%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_110%DoUse          = .TRUE.
   ExtState%MEmisNH3_OR_120%DoUse          = .TRUE.

   ExtState%MEmisCH4_OR_030%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_040%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_050%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_060%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_070%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_080%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_090%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_100%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_110%DoUse          = .TRUE.
   ExtState%MEmisCH4_OR_120%DoUse          = .TRUE.

   ExtState%MEmisACROLEIN_OR_030%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_040%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_050%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_060%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_070%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_080%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_090%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_100%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_110%DoUse          = .TRUE.
   ExtState%MEmisACROLEIN_OR_120%DoUse          = .TRUE.

   ExtState%MEmisBUTADIENE13_OR_030%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_040%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_050%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_060%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_070%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_080%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_090%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_100%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_110%DoUse          = .TRUE.
   ExtState%MEmisBUTADIENE13_OR_120%DoUse          = .TRUE.

   ExtState%MEmisETHY_OR_030%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_040%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_050%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_060%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_070%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_080%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_090%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_100%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_110%DoUse          = .TRUE.
   ExtState%MEmisETHY_OR_120%DoUse          = .TRUE.

   ExtState%MEmisTERP_OR_030%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_040%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_050%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_060%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_070%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_080%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_090%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_100%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_110%DoUse          = .TRUE.
   ExtState%MEmisTERP_OR_120%DoUse          = .TRUE.

   ExtState%MEmisFORM_OR_030%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_040%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_050%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_060%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_070%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_080%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_090%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_100%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_110%DoUse          = .TRUE.
   ExtState%MEmisFORM_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPAR_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPAR_OR_120%DoUse          = .TRUE.

   ExtState%MEmisIOLE_OR_030%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_040%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_050%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_060%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_070%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_080%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_090%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_100%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_110%DoUse          = .TRUE.
   ExtState%MEmisIOLE_OR_120%DoUse          = .TRUE.

   ExtState%MEmisOLE_OR_030%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_040%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_050%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_060%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_070%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_080%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_090%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_100%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_110%DoUse          = .TRUE.
   ExtState%MEmisOLE_OR_120%DoUse          = .TRUE.

   ExtState%MEmisETH_OR_030%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_040%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_050%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_060%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_070%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_080%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_090%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_100%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_110%DoUse          = .TRUE.
   ExtState%MEmisETH_OR_120%DoUse          = .TRUE.

   ExtState%MEmisETHA_OR_030%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_040%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_050%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_060%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_070%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_080%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_090%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_100%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_110%DoUse          = .TRUE.
   ExtState%MEmisETHA_OR_120%DoUse          = .TRUE.

   ExtState%MEmisETOH_OR_030%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_040%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_050%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_060%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_070%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_080%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_090%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_100%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_110%DoUse          = .TRUE.
   ExtState%MEmisETOH_OR_120%DoUse          = .TRUE.

   ExtState%MEmisMEOH_OR_030%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_040%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_050%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_060%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_070%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_080%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_090%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_100%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_110%DoUse          = .TRUE.
   ExtState%MEmisMEOH_OR_120%DoUse          = .TRUE.

   ExtState%MEmisBENZ_OR_030%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_040%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_050%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_060%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_070%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_080%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_090%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_100%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_110%DoUse          = .TRUE.
   ExtState%MEmisBENZ_OR_120%DoUse          = .TRUE.

   ExtState%MEmisTOL_OR_030%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_040%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_050%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_060%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_070%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_080%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_090%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_100%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_110%DoUse          = .TRUE.
   ExtState%MEmisTOL_OR_120%DoUse          = .TRUE.

   ExtState%MEmisXYLMN_OR_030%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_040%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_050%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_060%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_070%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_080%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_090%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_100%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_110%DoUse          = .TRUE.
   ExtState%MEmisXYLMN_OR_120%DoUse          = .TRUE.

   ExtState%MEmisNAPH_OR_030%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_040%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_050%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_060%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_070%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_080%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_090%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_100%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_110%DoUse          = .TRUE.
   ExtState%MEmisNAPH_OR_120%DoUse          = .TRUE.

   ExtState%MEmisALD2_OR_030%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_040%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_050%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_060%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_070%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_080%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_090%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_100%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_110%DoUse          = .TRUE.
   ExtState%MEmisALD2_OR_120%DoUse          = .TRUE.

   ExtState%MEmisALDX_OR_030%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_040%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_050%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_060%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_070%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_080%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_090%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_100%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_110%DoUse          = .TRUE.
   ExtState%MEmisALDX_OR_120%DoUse          = .TRUE.

   ExtState%MEmisISOP_OR_030%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_040%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_050%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_060%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_070%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_080%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_090%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_100%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_110%DoUse          = .TRUE.
   ExtState%MEmisISOP_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPRPA_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPRPA_OR_120%DoUse          = .TRUE.

   ExtState%MEmisACET_OR_030%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_040%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_050%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_060%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_070%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_080%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_090%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_100%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_110%DoUse          = .TRUE.
   ExtState%MEmisACET_OR_120%DoUse          = .TRUE.

   ExtState%MEmisKET_OR_030%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_040%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_050%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_060%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_070%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_080%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_090%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_100%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_110%DoUse          = .TRUE.
   ExtState%MEmisKET_OR_120%DoUse          = .TRUE.

   ExtState%MEmisALD2_PRIMARY_OR_030%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_040%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_050%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_060%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_070%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_080%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_090%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_100%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_110%DoUse          = .TRUE.
   ExtState%MEmisALD2_PRIMARY_OR_120%DoUse          = .TRUE.

   ExtState%MEmisFORM_PRIMARY_OR_030%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_040%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_050%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_060%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_070%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_080%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_090%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_100%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_110%DoUse          = .TRUE.
   ExtState%MEmisFORM_PRIMARY_OR_120%DoUse          = .TRUE.

   ExtState%MEmisSOAALK_OR_030%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_040%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_050%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_060%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_070%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_080%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_090%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_100%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_110%DoUse          = .TRUE.
   ExtState%MEmisSOAALK_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPEC_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPEC_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPOC_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPOC_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPAL_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPAL_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPCA_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPCA_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPCL_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPCL_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPFE_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPFE_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPH2O_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPH2O_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPK_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPK_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPMG_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPMG_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPMN_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPMN_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPMOTHR_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPMOTHR_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPNA_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPNA_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPNCOM_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPNCOM_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPNH4_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPNH4_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPNO3_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPNO3_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPTI_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPTI_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPSI_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPSI_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPMC_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPMC_OR_120%DoUse          = .TRUE.

   ExtState%MEmisPSO4_OR_030%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_040%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_050%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_060%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_070%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_080%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_090%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_100%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_110%DoUse          = .TRUE.
   ExtState%MEmisPSO4_OR_120%DoUse          = .TRUE.

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
   DO I=1, SIZE(NODES)-1
      INDICES(1) = I
      IF ( VALUE <= NODES(I+1) ) EXIT
   END DO

   ! The next node
   INDICES(2) = INDICES(1) + 1

   ! Weights for the corresponding node indices
   WEIGHTS(1) = ( NODES(INDICES(2)) - VALUE ) / &
                ( NODES(INDICES(2)) - NODES(INDICES(1)) )
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
 SUBROUTINE METEMIS_LUT( ExtState,  HcoState, Inst, I, J, RC,                          &
                         TEMPNO,   TEMPNO2,   TEMPHONO,  TEMPCO,   TEMPSO2,            &
                         TEMPNH3,  TEMPCH4,   TEMPACROLEIN, TEMPBUTADIENE13, TEMPETHY, &
                         TEMPTERP, TEMPFORM,  TEMPPAR,   TEMPIOLE, TEMPOLE ,           &
                         TEMPETH , TEMPETHA , TEMPETOH,  TEMPMEOH, TEMPBENZ,           &
                         TEMPTOL,  TEMPXYLMN, TEMPNAPH,  TEMPALD2, TEMPALDX,           &
                         TEMPISOP, TEMPPRPA,  TEMPACET,  TEMPKET,  TEMPALD2_PRIMARY,   &
                 TEMPFORM_PRIMARY, TEMPSOAALK, TEMPPEC,  TEMPPOC,  TEMPPAL,            &
                         TEMPPCA,  TEMPPCL,   TEMPPFE,   TEMPPH2O, TEMPPK,             &
                         TEMPPMG,  TEMPPMN,   TEMPPMOTHR, TEMPPNA, TEMPPNCOM,          &
                         TEMPPNH4, TEMPPNO3,  TEMPPTI,   TEMPPSI,  TEMPPMC,            &
                         TEMPPSO4)
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
! Temp dependent MetEmis emission species 51 in total , kg/m2/s
!
   REAL*8, INTENT(OUT)           :: TEMPNO   ! Temp dependent NO emissions, kg/m2/s
   REAL*8, INTENT(OUT)           :: TEMPNO2  ! Temp dependent NO2 emissions, kg/m2/s
   REAL*8, INTENT(OUT)           :: TEMPHONO ! Temp dependent HONO emissions, kg/m2/s
   REAL*8, INTENT(OUT)           :: TEMPCO   ! Temp dependent CO  emissions, kg/m2/s
   REAL*8, INTENT(OUT)           :: TEMPSO2  ! Temp dependent SO2 emissions, kg/m2/s
   REAL*8, INTENT(OUT)           :: TEMPNH3  ! Temp dependent NH3 emissions, kg/m2/s

   REAL*8, INTENT(OUT)           :: TEMPCH4
   REAL*8, INTENT(OUT)           :: TEMPACROLEIN
   REAL*8, INTENT(OUT)           :: TEMPBUTADIENE13
   REAL*8, INTENT(OUT)           :: TEMPETHY

   REAL*8, INTENT(OUT)           :: TEMPTERP
   REAL*8, INTENT(OUT)           :: TEMPFORM
   REAL*8, INTENT(OUT)           :: TEMPPAR
   REAL*8, INTENT(OUT)           :: TEMPIOLE
   REAL*8, INTENT(OUT)           :: TEMPOLE
   REAL*8, INTENT(OUT)           :: TEMPETH
   REAL*8, INTENT(OUT)           :: TEMPETHA
   REAL*8, INTENT(OUT)           :: TEMPETOH
   REAL*8, INTENT(OUT)           :: TEMPMEOH
   REAL*8, INTENT(OUT)           :: TEMPBENZ

   REAL*8, INTENT(OUT)           :: TEMPTOL
   REAL*8, INTENT(OUT)           :: TEMPXYLMN
   REAL*8, INTENT(OUT)           :: TEMPNAPH
   REAL*8, INTENT(OUT)           :: TEMPALD2
   REAL*8, INTENT(OUT)           :: TEMPALDX
   REAL*8, INTENT(OUT)           :: TEMPISOP
   REAL*8, INTENT(OUT)           :: TEMPPRPA
   REAL*8, INTENT(OUT)           :: TEMPACET
   REAL*8, INTENT(OUT)           :: TEMPKET
   REAL*8, INTENT(OUT)           :: TEMPALD2_PRIMARY

   REAL*8, INTENT(OUT)           :: TEMPFORM_PRIMARY
   REAL*8, INTENT(OUT)           :: TEMPSOAALK
   REAL*8, INTENT(OUT)           :: TEMPPEC
   REAL*8, INTENT(OUT)           :: TEMPPOC
   REAL*8, INTENT(OUT)           :: TEMPPAL
   REAL*8, INTENT(OUT)           :: TEMPPCA
   REAL*8, INTENT(OUT)           :: TEMPPCL
   REAL*8, INTENT(OUT)           :: TEMPPFE
   REAL*8, INTENT(OUT)           :: TEMPPH2O
   REAL*8, INTENT(OUT)           :: TEMPPK

   REAL*8, INTENT(OUT)           :: TEMPPMG
   REAL*8, INTENT(OUT)           :: TEMPPMN
   REAL*8, INTENT(OUT)           :: TEMPPMOTHR
   REAL*8, INTENT(OUT)           :: TEMPPNA
   REAL*8, INTENT(OUT)           :: TEMPPNCOM
   REAL*8, INTENT(OUT)           :: TEMPPNH4
   REAL*8, INTENT(OUT)           :: TEMPPNO3
   REAL*8, INTENT(OUT)           :: TEMPPTI
   REAL*8, INTENT(OUT)           :: TEMPPSI
   REAL*8, INTENT(OUT)           :: TEMPPMC

   REAL*8, INTENT(OUT)           :: TEMPPSO4

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
   REAL(sp)                   :: RHUMGAS,RHUMDIS
   REAL(sp)                   :: TEMPNO_GAS,   TEMPNO_GAS_TMP
   REAL(sp)                   :: TEMPNO_DIS,   TEMPNO_DIS_TMP
   REAL(sp)                   :: TEMPNO2_GAS,  TEMPNO2_GAS_TMP
   REAL(sp)                   :: TEMPNO2_DIS,  TEMPNO2_DIS_TMP
   REAL(sp)                   :: TEMPHONO_GAS, TEMPHONO_GAS_TMP
   REAL(sp)                   :: TEMPHONO_DIS, TEMPHONO_DIS_TMP
   REAL(sp)                   :: TEMPCO_TMP
   REAL(sp)                   :: TEMPSO2_TMP
   REAL(sp)                   :: TEMPNH3_TMP
   REAL(dp)                   :: TEMPCH4_TMP
   REAL(dp)                   :: TEMPACROLEIN_TMP
   REAL(dp)                   :: TEMPBUTADIENE13_TMP
   REAL(dp)                   :: TEMPETHY_TMP

   REAL(dp)                   :: TEMPTERP_TMP
   REAL(dp)                   :: TEMPFORM_TMP
   REAL(dp)                   :: TEMPPAR_TMP
   REAL(dp)                   :: TEMPIOLE_TMP
   REAL(dp)                   :: TEMPOLE_TMP
   REAL(dp)                   :: TEMPETH_TMP
   REAL(dp)                   :: TEMPETHA_TMP
   REAL(dp)                   :: TEMPETOH_TMP
   REAL(dp)                   :: TEMPMEOH_TMP
   REAL(dp)                   :: TEMPBENZ_TMP

   REAL(dp)                   :: TEMPTOL_TMP
   REAL(dp)                   :: TEMPXYLMN_TMP
   REAL(dp)                   :: TEMPNAPH_TMP
   REAL(dp)                   :: TEMPALD2_TMP
   REAL(dp)                   :: TEMPALDX_TMP
   REAL(dp)                   :: TEMPISOP_TMP
   REAL(dp)                   :: TEMPPRPA_TMP
   REAL(dp)                   :: TEMPACET_TMP
   REAL(dp)                   :: TEMPKET_TMP
   REAL(dp)                   :: TEMPALD2_PRIMARY_TMP

   REAL(dp)                   :: TEMPFORM_PRIMARY_TMP
   REAL(dp)                   :: TEMPSOAALK_TMP
   REAL(dp)                   :: TEMPPEC_TMP
   REAL(dp)                   :: TEMPPOC_TMP
   REAL(dp)                   :: TEMPPAL_TMP
   REAL(dp)                   :: TEMPPCA_TMP
   REAL(dp)                   :: TEMPPCL_TMP
   REAL(dp)                   :: TEMPPFE_TMP
   REAL(dp)                   :: TEMPPH2O_TMP
   REAL(dp)                   :: TEMPPK_TMP

   REAL(dp)                   :: TEMPPMG_TMP
   REAL(dp)                   :: TEMPPMN_TMP
   REAL(dp)                   :: TEMPPMOTHR_TMP
   REAL(dp)                   :: TEMPPNA_TMP
   REAL(dp)                   :: TEMPPNCOM_TMP
   REAL(dp)                   :: TEMPPNH4_TMP
   REAL(dp)                   :: TEMPPNO3_TMP
   REAL(dp)                   :: TEMPPTI_TMP
   REAL(dp)                   :: TEMPPSI_TMP
   REAL(dp)                   :: TEMPPMC_TMP

   REAL(dp)                   :: TEMPPSO4_TMP

   REAL(sp)                   :: WEIGHT
   REAL(sp)                   :: TAIR
   REAL(sp)                   :: QAIR,QMOL,A,B

   ! Interpolation variables, indices, and weights
   REAL(sp), DIMENSION(1)     :: VARS
   INTEGER,  DIMENSION(1,2)   :: INDX
   REAL(sp), DIMENSION(1,2)   :: WTS

   CHARACTER(LEN=255)         :: MSG
   CHARACTER(LEN=255)         :: LOC = 'METEMIS_LUT'

   !=================================================================
   ! METEMIS_LUT begins here!
   !=================================================================

   !MetEmis Temperature bins (Degrees Fahrenheit) = 10 from explicit nT
   !These are set to lower bin edge defined in MetEmis files. 
   !e.g., 20 - 30 (20), ... 110 - 120 (110)
   Inst%Tlev = (/ 20.0e0, 30.0e0, 40.0e0, 50.0e0,  60.0e0,  &
                  70.0e0, 80.0e0, 90.0e0, 100.0e0, 110.0e0 /)

   !Get 2-m air temperature, K
   TAIR = ExtState%T2M%Arr%Val(I,J)
   !Get 2-m air specific humidity, kg/kg
   QAIR = ExtState%QV2M%Arr%Val(I,J)

   !========================================================================
   ! Load all variables into a single array
   !========================================================================

   ! Air Temperature, K --> Fahrenheit for MetEmis consistency
   VARS(1) = (TAIR - 273.15)*1.8 + 32.0

   ! Check if outside bounds of MetEmis Temperature Bins and set , i.e., <= 20F or >=120 F
   IF ( VARS(1) <= 20.0  ) THEN
      VARS(1) = 20.0
   ENDIF

   IF ( VARS(1) >= 120.0  ) THEN
      VARS(1) = 120.0
   ENDIF

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
   TEMPNO      = 0.0d0
   TEMPNO_GAS  = 0.0d0
   TEMPNO_DIS  = 0.0d0
   TEMPNO2     = 0.0d0
   TEMPNO2_GAS = 0.0d0
   TEMPNO2_DIS = 0.0d0
   TEMPHONO    = 0.0d0
   TEMPHONO_GAS= 0.0d0
   TEMPHONO_DIS= 0.0d0
   TEMPCO      = 0.0d0
   TEMPSO2     = 0.0d0
   TEMPNH3     = 0.0d0
   TEMPCH4     = 0.0d0
   TEMPACROLEIN = 0.0d0
   TEMPBUTADIENE13 = 0.0d0
   TEMPETHY    = 0.0d0

   TEMPTERP    = 0.0d0
   TEMPFORM    = 0.0d0
   TEMPPAR     = 0.0d0
   TEMPIOLE    = 0.0d0
   TEMPOLE     = 0.0d0
   TEMPETH     = 0.0d0
   TEMPETHA    = 0.0d0
   TEMPETOH    = 0.0d0
   TEMPMEOH    = 0.0d0
   TEMPBENZ    = 0.0d0

   TEMPTOL     = 0.0d0
   TEMPXYLMN   = 0.0d0
   TEMPNAPH    = 0.0d0
   TEMPALD2    = 0.0d0
   TEMPALDX    = 0.0d0
   TEMPISOP    = 0.0d0
   TEMPPRPA    = 0.0d0
   TEMPACET    = 0.0d0
   TEMPKET     = 0.0d0
   TEMPALD2_PRIMARY = 0.0d0

   TEMPFORM_PRIMARY = 0.0d0
   TEMPSOAALK  = 0.0d0
   TEMPPEC     = 0.0d0
   TEMPPOC     = 0.0d0
   TEMPPAL     = 0.0d0
   TEMPPCA     = 0.0d0
   TEMPPCL     = 0.0d0
   TEMPPFE     = 0.0d0
   TEMPPH2O    = 0.0d0
   TEMPPK      = 0.0d0

   TEMPPMG     = 0.0d0
   TEMPPMN     = 0.0d0
   TEMPPMOTHR  = 0.0d0
   TEMPPNA     = 0.0d0
   TEMPPNCOM   = 0.0d0
   TEMPPNH4    = 0.0d0
   TEMPPNO3    = 0.0d0
   TEMPPTI     = 0.0d0
   TEMPPSI     = 0.0d0
   TEMPPMC     = 0.0d0

   TEMPPSO4    = 0.0d0

  ! Loop over temperature bins
   DO I1=1,2
      SELECT CASE ( NINT( Inst%Tlev(INDX(1,I1)) ) )
         CASE ( 20 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_030%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_030%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_030%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_030%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_030%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_030%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_030%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_030%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_030%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_030%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_030%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_030%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_030%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_030%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_030%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_030%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_030%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_030%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_030%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_030%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_030%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_030%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_030%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_030%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_030%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_030%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_030%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_030%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_030%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_030%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_030%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_030%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_030%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_030%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_030%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_030%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_030%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_030%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_030%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_030%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_030%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_030%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_030%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_030%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_030%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_030%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_030%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_030%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_030%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_030%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_030%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_030%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_030%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_030%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 30 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_040%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_040%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_040%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_040%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_040%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_040%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_040%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_040%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_040%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_040%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_040%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_040%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_040%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_040%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_040%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_040%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_040%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_040%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_040%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_040%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_040%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_040%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_040%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_040%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_040%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_040%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_040%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_040%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_040%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_040%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_040%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_040%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_040%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_040%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_040%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_040%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_040%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_040%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_040%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_040%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_040%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_040%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_040%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_040%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_040%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_040%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_040%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_040%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_040%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_040%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_040%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_040%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_040%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_040%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 40 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_050%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_050%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_050%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_050%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_050%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_050%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_050%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_050%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_050%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_050%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_050%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_050%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_050%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_050%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_050%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_050%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_050%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_050%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_050%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_050%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_050%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_050%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_050%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_050%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_050%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_050%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_050%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_050%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_050%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_050%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_050%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_050%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_050%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_050%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_050%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_050%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_050%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_050%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_050%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_050%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_050%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_050%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_050%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_050%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_050%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_050%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_050%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_050%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_050%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_050%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_050%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_050%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_050%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_050%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 50 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_060%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_060%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_060%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_060%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_060%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_060%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_060%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_060%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_060%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_060%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_060%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_060%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_060%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_060%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_060%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_060%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_060%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_060%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_060%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_060%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_060%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_060%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_060%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_060%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_060%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_060%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_060%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_060%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_060%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_060%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_060%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_060%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_060%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_060%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_060%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_060%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_060%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_060%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_060%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_060%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_060%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_060%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_060%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_060%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_060%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_060%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_060%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_060%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_060%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_060%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_060%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_060%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_060%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_060%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 60 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_070%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_070%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_070%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_070%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_070%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_070%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_070%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_070%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_070%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_070%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_070%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_070%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_070%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_070%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_070%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_070%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_070%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_070%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_070%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_070%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_070%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_070%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_070%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_070%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_070%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_070%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_070%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_070%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_070%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_070%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_070%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_070%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_070%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_070%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_070%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_070%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_070%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_070%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_070%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_070%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_070%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_070%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_070%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_070%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_070%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_070%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_070%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_070%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_070%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_070%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_070%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_070%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_070%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_070%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 70 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_080%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_080%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_080%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_080%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_080%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_080%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_080%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_080%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_080%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_080%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_080%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_080%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_080%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_080%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_080%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_080%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_080%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_080%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_080%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_080%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_080%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_080%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_080%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_080%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_080%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_080%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_080%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_080%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_080%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_080%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_080%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_080%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_080%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_080%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_080%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_080%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_080%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_080%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_080%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_080%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_080%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_080%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_080%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_080%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_080%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_080%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_080%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_080%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_080%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_080%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_080%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_080%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_080%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_080%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 80 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_090%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_090%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_090%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_090%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_090%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_090%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_090%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_090%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_090%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_090%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_090%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_090%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_090%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_090%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_090%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_090%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_090%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_090%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_090%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_090%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_090%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_090%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_090%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_090%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_090%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_090%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_090%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_090%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_090%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_090%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_090%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_090%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_090%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_090%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_090%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_090%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_090%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_090%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_090%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_090%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_090%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_090%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_090%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_090%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_090%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_090%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_090%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_090%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_090%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_090%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_090%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_090%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_090%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_090%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 90 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_100%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_100%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_100%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_100%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_100%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_100%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_100%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_100%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_100%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_100%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_100%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_100%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_100%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_100%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_100%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_100%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_100%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_100%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_100%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_100%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_100%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_100%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_100%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_100%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_100%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_100%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_100%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_100%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_100%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_100%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_100%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_100%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_100%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_100%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_100%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_100%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_100%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_100%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_100%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_100%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_100%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_100%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_100%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_100%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_100%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_100%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_100%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_100%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_100%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_100%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_100%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_100%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_100%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_100%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 100 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_110%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_110%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_110%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_110%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_110%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_110%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_110%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_110%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_110%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_110%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_110%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_110%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_110%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_110%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_110%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_110%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_110%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_110%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_110%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_110%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_110%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_110%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_110%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_110%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_110%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_110%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_110%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_110%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_110%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_110%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_110%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_110%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_110%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_110%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_110%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_110%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_110%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_110%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_110%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_110%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_110%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_110%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_110%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_110%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_110%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_110%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_110%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_110%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_110%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_110%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_110%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_110%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_110%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_110%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE ( 110 )
            TEMPNO_GAS_TMP   =  ExtState%MEmisNO_GAS_OR_120%Arr%Val(I,J)
            TEMPNO_DIS_TMP   =  ExtState%MEmisNO_DIS_OR_120%Arr%Val(I,J)
            TEMPNO2_GAS_TMP  =  ExtState%MEmisNO2_GAS_OR_120%Arr%Val(I,J)
            TEMPNO2_DIS_TMP  =  ExtState%MEmisNO2_DIS_OR_120%Arr%Val(I,J)
            TEMPHONO_GAS_TMP =  ExtState%MEmisHONO_GAS_OR_120%Arr%Val(I,J)
            TEMPHONO_DIS_TMP =  ExtState%MEmisHONO_DIS_OR_120%Arr%Val(I,J)
            TEMPCO_TMP       =  ExtState%MEmisCO_OR_120%Arr%Val(I,J)
            TEMPSO2_TMP      =  ExtState%MEmisSO2_OR_120%Arr%Val(I,J)
            TEMPNH3_TMP      =  ExtState%MEmisNH3_OR_120%Arr%Val(I,J)
            TEMPCH4_TMP      =  ExtState%MEmisCH4_OR_120%Arr%Val(I,J)
            TEMPACROLEIN_TMP =  ExtState%MEmisACROLEIN_OR_120%Arr%Val(I,J)
            TEMPBUTADIENE13_TMP =  ExtState%MEmisBUTADIENE13_OR_120%Arr%Val(I,J)
            TEMPETHY_TMP     =  ExtState%MEmisETHY_OR_120%Arr%Val(I,J)

            TEMPTERP_TMP     =  ExtState%MEmisTERP_OR_120%Arr%Val(I,J)
            TEMPFORM_TMP     =  ExtState%MEmisFORM_OR_120%Arr%Val(I,J)
            TEMPPAR_TMP      =  ExtState%MEmisPAR_OR_120%Arr%Val(I,J)
            TEMPIOLE_TMP     =  ExtState%MEmisIOLE_OR_120%Arr%Val(I,J)
            TEMPOLE_TMP      =  ExtState%MEmisOLE_OR_120%Arr%Val(I,J)
            TEMPETH_TMP      =  ExtState%MEmisETH_OR_120%Arr%Val(I,J)
            TEMPETHA_TMP     =  ExtState%MEmisETHA_OR_120%Arr%Val(I,J)
            TEMPETOH_TMP     =  ExtState%MEmisETOH_OR_120%Arr%Val(I,J)
            TEMPMEOH_TMP     =  ExtState%MEmisMEOH_OR_120%Arr%Val(I,J)
            TEMPBENZ_TMP     =  ExtState%MEmisBENZ_OR_120%Arr%Val(I,J)

            TEMPTOL_TMP      =  ExtState%MEmisTOL_OR_120%Arr%Val(I,J)
            TEMPXYLMN_TMP    =  ExtState%MEmisXYLMN_OR_120%Arr%Val(I,J)
            TEMPNAPH_TMP     =  ExtState%MEmisNAPH_OR_120%Arr%Val(I,J)
            TEMPALD2_TMP     =  ExtState%MEmisALD2_OR_120%Arr%Val(I,J)
            TEMPALDX_TMP     =  ExtState%MEmisALDX_OR_120%Arr%Val(I,J)
            TEMPISOP_TMP     =  ExtState%MEmisISOP_OR_120%Arr%Val(I,J)
            TEMPPRPA_TMP     =  ExtState%MEmisPRPA_OR_120%Arr%Val(I,J)
            TEMPACET_TMP     =  ExtState%MEmisACET_OR_120%Arr%Val(I,J)
            TEMPKET_TMP      =  ExtState%MEmisKET_OR_120%Arr%Val(I,J)
            TEMPALD2_PRIMARY_TMP =  ExtState%MEmisALD2_PRIMARY_OR_120%Arr%Val(I,J)

            TEMPFORM_PRIMARY_TMP =  ExtState%MEmisFORM_PRIMARY_OR_120%Arr%Val(I,J)
            TEMPSOAALK_TMP   =  ExtState%MEmisSOAALK_OR_120%Arr%Val(I,J)
            TEMPPEC_TMP      =  ExtState%MEmisPEC_OR_120%Arr%Val(I,J)
            TEMPPOC_TMP      =  ExtState%MEmisPOC_OR_120%Arr%Val(I,J)
            TEMPPAL_TMP      =  ExtState%MEmisPAL_OR_120%Arr%Val(I,J)
            TEMPPCA_TMP      =  ExtState%MEmisPCA_OR_120%Arr%Val(I,J)
            TEMPPCL_TMP      =  ExtState%MEmisPCL_OR_120%Arr%Val(I,J)
            TEMPPFE_TMP      =  ExtState%MEmisPFE_OR_120%Arr%Val(I,J)
            TEMPPH2O_TMP     =  ExtState%MEmisPH2O_OR_120%Arr%Val(I,J)
            TEMPPK_TMP       =  ExtState%MEmisPK_OR_120%Arr%Val(I,J)

            TEMPPMG_TMP      =  ExtState%MEmisPMG_OR_120%Arr%Val(I,J)
            TEMPPMN_TMP      =  ExtState%MEmisPMN_OR_120%Arr%Val(I,J)
            TEMPPMOTHR_TMP   =  ExtState%MEmisPMOTHR_OR_120%Arr%Val(I,J)
            TEMPPNA_TMP      =  ExtState%MEmisPNA_OR_120%Arr%Val(I,J)
            TEMPPNCOM_TMP    =  ExtState%MEmisPNCOM_OR_120%Arr%Val(I,J)
            TEMPPNH4_TMP     =  ExtState%MEmisPNH4_OR_120%Arr%Val(I,J)
            TEMPPNO3_TMP     =  ExtState%MEmisPNO3_OR_120%Arr%Val(I,J)
            TEMPPTI_TMP      =  ExtState%MEmisPTI_OR_120%Arr%Val(I,J)
            TEMPPSI_TMP      =  ExtState%MEmisPSI_OR_120%Arr%Val(I,J)
            TEMPPMC_TMP      =  ExtState%MEmisPMC_OR_120%Arr%Val(I,J)

            TEMPPSO4_TMP     =  ExtState%MEmisPSO4_OR_120%Arr%Val(I,J)

            WEIGHT       = WTS(1,I1)
         CASE DEFAULT
             MSG = 'LUT error: Temperature interpolation error!'
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
      END SELECT

         !-----------------------------------
         ! Final interpolated values
         !-----------------------------------
         ! Weighted sum of TempNO from the LUT
         TEMPNO_GAS  = TEMPNO_GAS  + TEMPNO_GAS_TMP  * WEIGHT
         TEMPNO_DIS  = TEMPNO_DIS  + TEMPNO_DIS_TMP  * WEIGHT
         TEMPNO2_GAS = TEMPNO2_GAS + TEMPNO2_GAS_TMP * WEIGHT
         TEMPNO2_DIS = TEMPNO2_DIS + TEMPNO2_DIS_TMP * WEIGHT
         TEMPHONO_GAS= TEMPHONO_GAS+ TEMPHONO_GAS_TMP* WEIGHT
         TEMPHONO_DIS= TEMPHONO_DIS+ TEMPHONO_DIS_TMP* WEIGHT
         TEMPCO      = TEMPCO      + TEMPCO_TMP      * WEIGHT
         TEMPSO2     = TEMPSO2     + TEMPSO2_TMP     * WEIGHT
         TEMPNH3     = TEMPNH3     + TEMPNH3_TMP     * WEIGHT
         TEMPCH4     = TEMPCH4     + TEMPCH4_TMP     * WEIGHT
         TEMPACROLEIN = TEMPACROLEIN + TEMPACROLEIN_TMP * WEIGHT
         TEMPBUTADIENE13 = TEMPBUTADIENE13 + TEMPBUTADIENE13_TMP * WEIGHT
         TEMPETHY     = TEMPETHY   + TEMPETHY_TMP    * WEIGHT

         TEMPTERP    = TEMPTERP    + TEMPTERP_TMP    * WEIGHT
         TEMPFORM    = TEMPFORM    + TEMPFORM_TMP    * WEIGHT
         TEMPPAR     = TEMPPAR     + TEMPPAR_TMP     * WEIGHT
         TEMPIOLE    = TEMPIOLE    + TEMPIOLE_TMP    * WEIGHT
         TEMPOLE     = TEMPOLE     + TEMPOLE_TMP     * WEIGHT
         TEMPETH     = TEMPETH     + TEMPETH_TMP     * WEIGHT
         TEMPETHA    = TEMPETHA    + TEMPETHA_TMP    * WEIGHT
         TEMPETOH    = TEMPETOH    + TEMPETOH_TMP    * WEIGHT
         TEMPMEOH    = TEMPMEOH    + TEMPMEOH_TMP    * WEIGHT
         TEMPBENZ    = TEMPBENZ    + TEMPBENZ_TMP    * WEIGHT

         TEMPTOL     = TEMPTOL     + TEMPTOL_TMP     * WEIGHT
         TEMPXYLMN   = TEMPXYLMN   + TEMPXYLMN_TMP   * WEIGHT
         TEMPNAPH    = TEMPNAPH    + TEMPNAPH_TMP    * WEIGHT
         TEMPALD2    = TEMPALD2    + TEMPALD2_TMP    * WEIGHT
         TEMPALDX    = TEMPALDX    + TEMPALDX_TMP    * WEIGHT
         TEMPISOP    = TEMPISOP    + TEMPISOP_TMP    * WEIGHT
         TEMPPRPA    = TEMPPRPA    + TEMPPRPA_TMP    * WEIGHT
         TEMPACET    = TEMPACET    + TEMPACET_TMP    * WEIGHT
         TEMPKET     = TEMPKET     + TEMPKET_TMP     * WEIGHT
         TEMPALD2_PRIMARY = TEMPALD2_PRIMARY + TEMPALD2_PRIMARY_TMP * WEIGHT

         TEMPFORM_PRIMARY = TEMPFORM_PRIMARY + TEMPFORM_PRIMARY_TMP * WEIGHT
         TEMPSOAALK  = TEMPSOAALK  + TEMPSOAALK_TMP  * WEIGHT
         TEMPPEC     = TEMPPEC     + TEMPPEC_TMP     * WEIGHT
         TEMPPOC     = TEMPPOC     + TEMPPOC_TMP     * WEIGHT
         TEMPPAL     = TEMPPAL     + TEMPPAL_TMP     * WEIGHT
         TEMPPCA     = TEMPPCA     + TEMPPCA_TMP     * WEIGHT
         TEMPPCL     = TEMPPCL     + TEMPPCL_TMP     * WEIGHT
         TEMPPFE     = TEMPPFE     + TEMPPFE_TMP     * WEIGHT
         TEMPPH2O    = TEMPPH2O    + TEMPPH2O_TMP    * WEIGHT
         TEMPPK      = TEMPPK      + TEMPPK_TMP      * WEIGHT

         TEMPPMG     = TEMPPMG     + TEMPPMG_TMP     * WEIGHT
         TEMPPMN     = TEMPPMN     + TEMPPMN_TMP     * WEIGHT
         TEMPPMOTHR  = TEMPPMOTHR  + TEMPPMOTHR_TMP  * WEIGHT
         TEMPPNA     = TEMPPNA     + TEMPPNA_TMP     * WEIGHT
         TEMPPNCOM   = TEMPPNCOM   + TEMPPNCOM_TMP   * WEIGHT
         TEMPPNH4    = TEMPPNH4    + TEMPPNH4_TMP    * WEIGHT
         TEMPPNO3    = TEMPPNO3    + TEMPPNO3_TMP    * WEIGHT
         TEMPPTI     = TEMPPTI     + TEMPPTI_TMP     * WEIGHT
         TEMPPSI     = TEMPPSI     + TEMPPSI_TMP     * WEIGHT
         TEMPPMC     = TEMPPMC     + TEMPPMC_TMP     * WEIGHT

         TEMPPSO4    = TEMPPSO4    + TEMPPSO4_TMP    * WEIGHT

   END DO

   IF ( Inst%RHUMGASDIS ) THEN
      !Calculate and apply humidity correction for NOx and HONO across split gas vs. diesel fuels
      QAIR = QAIR*1000.0   !convert from kg water/kg dry air to g/kg
      A = MIN( QAIR, 17.71 )
      B = MAX( 3.0, A )
      RHUMGAS = 1.0 - 0.0329 * ( B - 10.71 )   ! RH humidity correction for Gasoline fuel
      QMOL = QAIR * 0.001607524  ! convert from g of water/kg of dry air to moles of water/moles of dry air
      A = MIN( QMOL, 0.035 )
      B = MAX( 0.002, A )
      RHUMDIS = 1.0 / ( 9.953 * B  + 0.832 )  ! RH humidity correction for Diesel fuel

      !::: These RHUMGAS and RHUMDIS correction factors can be multiplied with the
      !estimated emissions between temperature bins to reflect the impact of humidity up to 20% (+/-)

      !Apply humidity correction to temperature-depended NOx GAS and DIESEL
      TEMPNO_GAS   = RHUMGAS * TEMPNO_GAS
      TEMPNO_DIS   = RHUMDIS * TEMPNO_DIS
      TEMPNO2_GAS  = RHUMGAS * TEMPNO2_GAS
      TEMPNO2_DIS  = RHUMDIS * TEMPNO2_DIS

      !Apply humidity correction to temperature-depended HONO GAS and DIESEL
      TEMPHONO_GAS = RHUMGAS * TEMPHONO_GAS
      TEMPHONO_DIS = RHUMDIS * TEMPHONO_DIS
   ENDIF
      !Sum GAS and DIESEL for Output NOx and HONO
      TEMPNO   = TEMPNO_GAS   + TEMPNO_DIS
      TEMPNO2  = TEMPNO2_GAS  + TEMPNO2_DIS
      TEMPHONO = TEMPHONO_GAS + TEMPHONO_DIS

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
