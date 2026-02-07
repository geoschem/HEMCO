!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_state_mod.F90
!
! !DESCRIPTION: Module HCOX\_State\_Mod contains routines and variables
! to organize the extensions state type ExtState. ExtState contains the
! logical switches for each extension (denoting whether or not it is
! enabled) as well as pointers to all met fields used by the extensions.
! ExtState is passed to all extension modules, and the met fields
! defined in here are thus available to all extensions. Additional met
! fields (and extension switches) can be added as required.
!\\
! This module contains the routines to initialize and finalize the
! ExtState object, but doesn't link the met field pointers to the
! corresponding fields. This is done in the HEMCO-model interface
! routines (e.g. hcoi\_standalone\_mod.F90, hcoi\_gc\_main\_mod.F90).
! Newly added met fields will only work if the corresponding pointer
! assignments are added to these interface routines!
!\\
!\\
! !INTERFACE:
!
MODULE HCOX_STATE_MOD
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_ARR_MOD

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: ExtStateInit
  PUBLIC :: ExtStateFinal
  PUBLIC :: ExtDat_Set
!
! !DERIVED TYPES:
!
  !=========================================================================
  ! ExtDat_*: Derived types containing pointers to the met field arrays
  ! (Arr) and a logical flag whether or not the field is used by any of
  ! the extensions (DoUse).  Arrays can be 3D reals or 2D reals or integer
  ! All real values are of default precision! (df), as specified in
  ! HCO\_ERROR\_MOD.  You can add more types if necessary.
  !=========================================================================

  ! 2D real, default precision
  TYPE, PUBLIC :: ExtDat_2R
     TYPE(Arr2D_HP), POINTER :: Arr
     LOGICAL                 :: DoUse
     LOGICAL                 :: FromList
  END TYPE ExtDat_2R

  ! 2D real, single precision
  TYPE, PUBLIC :: ExtDat_2S
     TYPE(Arr2D_SP), POINTER :: Arr
     LOGICAL                 :: DoUse
     LOGICAL                 :: FromList
  END TYPE ExtDat_2S

  ! 2D integer
  TYPE, PUBLIC :: ExtDat_2I
     TYPE(Arr2D_I),  POINTER :: Arr
     LOGICAL                 :: DoUse
     LOGICAL                 :: FromList
  END TYPE ExtDat_2I

  ! 3D real, default precision
  TYPE, PUBLIC :: ExtDat_3R
     TYPE(Arr3D_HP), POINTER :: Arr
     LOGICAL                 :: DoUse
     LOGICAL                 :: FromList
  END TYPE ExtDat_3R

  ! 3D real, single precision
  TYPE, PUBLIC :: ExtDat_3S
     TYPE(Arr3D_SP), POINTER :: Arr
     LOGICAL                 :: DoUse
     LOGICAL                 :: FromList
  END TYPE ExtDat_3S

  !=========================================================================
  ! Ext_State: Derived type declaration for the State object containing
  ! pointers to all met fields and related quantities used by the HEMCO
  ! extensions. An 'Ext_State' type called ExtState is defined at the
  ! beginning of a HEMCO run and populated according to the specifications
  ! set in the configuration file.  You can add more fields if necessary.
  !=========================================================================
  TYPE, PUBLIC :: Ext_State

     !----------------------------------------------------------------------
     ! Extension switches (enabled?)
     ! NOTE: When adding a new extension, don't forget to initialize this
     ! switch in subroutine ExtStateInit below!
     !----------------------------------------------------------------------
     INTEGER                   :: Custom         ! Customizable ext.
     INTEGER                   :: DustL23M       ! DustL23M dust model
     INTEGER                   :: DustDead       ! DEAD dust model
     INTEGER                   :: DustGinoux     ! Ginoux dust emissions
     INTEGER                   :: DustAlk        ! Dust alkalinity
     INTEGER                   :: LightNOx       ! Lightning NOx
     INTEGER                   :: ParaNOx        ! PARANOX ship emissions
     INTEGER                   :: SoilNOx        ! Soil NOx emissions
     INTEGER                   :: Megan          ! MEGAN biogenic emissions
     INTEGER                   :: SeaFlux        ! air-sea exchange
     INTEGER                   :: SeaSalt        ! Seasalt emissions
     INTEGER                   :: GFED           ! GFED biomass burning
     INTEGER                   :: FINN           ! FINN biomass burning
     INTEGER                   :: GC_RnPbBe      ! GEOS-Chem Rn-Pb-Be simulation
     INTEGER                   :: GC_POPs        ! GEOS-Chem POPs simulation
     INTEGER                   :: Wetland_CH4    ! Methane emiss from wetlands
     INTEGER                   :: TOMAS_Jeagle   ! TOMAS Jeagle sea salt
     INTEGER                   :: TOMAS_DustDead ! TOMAS sectional Dead Dust
     INTEGER                   :: Volcano        ! Volcano emissions
     INTEGER                   :: Inorg_Iodine   ! Oceanic inorganic iodine emissions
     INTEGER                   :: MetEmis        ! MetEmis Emissions

     !----------------------------------------------------------------------
     ! Data directory
     !----------------------------------------------------------------------
     CHARACTER(LEN=255)        :: DATA_DIR    ! Directory for data

     !----------------------------------------------------------------------
     ! Met fields
     !----------------------------------------------------------------------
     TYPE(ExtDat_2R),  POINTER :: U10M        ! E/W 10m wind speed [m/s]
     TYPE(ExtDat_2R),  POINTER :: V10M        ! N/S 10m wind speed [m/s]
     TYPE(ExtDat_2R),  POINTER :: ALBD        ! Surface albedo [-]
     TYPE(ExtDat_2R),  POINTER :: T2M         ! T at 2m above sfc [K]; Used as
                                              !  a proxy for GMAO surface temp.
     TYPE(ExtDat_2R),  POINTER :: TS          ! Surface temperature [K]; Keep
                                              !  in case non-GEOS met needs it.
     TYPE(ExtDat_2R),  POINTER :: TSKIN       ! Surface skin temperature [K]
     TYPE(ExtDat_2R),  POINTER :: TSOIL1      ! Soil temperature, layer 1 [K]
     TYPE(ExtDat_2R),  POINTER :: GWETROOT    ! Root soil wetness [1]
     TYPE(ExtDat_2R),  POINTER :: GWETTOP     ! Top soil moisture [1]
     TYPE(ExtDat_2R),  POINTER :: SNOMAS     ! Snow mass [mm H2O = kg H2O/m2]
     TYPE(ExtDat_2R),  POINTER :: SNODP       ! Snow depth [m ]
     TYPE(ExtDat_2R),  POINTER :: SNICE       ! Fraction of snow/ice [1]
     TYPE(ExtDat_2R),  POINTER :: USTAR       ! Friction velocity [m/s]
     TYPE(ExtDat_2R),  POINTER :: Z0          ! Sfc roughness height [m]
     TYPE(ExtDat_2R),  POINTER :: TROPP       ! Tropopause pressure [Pa]
     TYPE(ExtDat_2R),  POINTER :: SUNCOS      ! COS (SZA)
     TYPE(ExtDat_2R),  POINTER :: SZAFACT     ! current SZA/total daily SZA
     TYPE(ExtDat_2R),  POINTER :: PARDR       ! direct photsyn radiation [W/m2]
     TYPE(ExtDat_2R),  POINTER :: PARDF       ! diffuse photsyn radiation [W/m2]
     TYPE(ExtDat_2R),  POINTER :: HFLUX       ! Sensible height flux due to turbulence [W m-2]
     TYPE(ExtDat_2R),  POINTER :: PSC2_WET    ! Interpolated sfc pressure [hPa]
     TYPE(ExtDat_2R),  POINTER :: RADSWG      ! surface radiation [W/m2]
     TYPE(ExtDat_2R),  POINTER :: FRCLND      ! Olson land fraction [-]
     TYPE(ExtDat_2R),  POINTER :: FRLAND      ! land fraction [-]
     TYPE(ExtDat_2R),  POINTER :: FROCEAN     ! ocean fraction [-]
     TYPE(ExtDat_2R),  POINTER :: FRSEAICE    ! sea ice fraction [-]
     TYPE(ExtDat_2R),  POINTER :: QV2M        ! 2m specific humidity [-]
     TYPE(ExtDat_2R),  POINTER :: FRLAKE      ! lake fraction [-]
     TYPE(ExtDat_2R),  POINTER :: FRLANDIC    ! land ice fraction [-]
     TYPE(ExtDat_2R),  POINTER :: CLDFRC      ! cloud fraction [-]
     TYPE(ExtDat_2R),  POINTER :: JNO2        ! J-Value for NO2 [1/s]
     TYPE(ExtDat_2R),  POINTER :: JOH         ! J-Value for O3->OH  [1/s]
     TYPE(ExtDat_2R),  POINTER :: LAI         ! daily leaf area index [cm2/cm2]
     TYPE(ExtDat_2R),  POINTER :: CHLR        ! daily chlorophyll-a [mg/m3]
     TYPE(ExtDat_2I),  POINTER :: TropLev     ! Tropopause level [1]
     TYPE(ExtDat_2R),  POINTER :: FLASH_DENS  ! Lightning flash density [#/km2/s]
     TYPE(ExtDat_2R),  POINTER :: CONV_DEPTH  ! Convective cloud depth [m]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_030  ! MetEmis NO GAS Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_040  ! MetEmis NO GAS Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_050  ! MetEmis NO GAS Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_060  ! MetEmis NO GAS Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_070  ! MetEmis NO GAS Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_080  ! MetEmis NO GAS Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_090  ! MetEmis NO GAS Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_100  ! MetEmis NO GAS Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_110  ! MetEmis NO GAS Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_GAS_OR_120  ! MetEmis NO GAS Onroad Table 120F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_030  ! MetEmis NO DIS Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_040  ! MetEmis NO DIS Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_050  ! MetEmis NO DIS Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_060  ! MetEmis NO DIS Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_070  ! MetEmis NO DIS Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_080  ! MetEmis NO DIS Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_090  ! MetEmis NO DIS Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_100  ! MetEmis NO DIS Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_110  ! MetEmis NO DIS Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO_DIS_OR_120  ! MetEmis NO DIS Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_030  ! MetEmis NO2 GAS Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_040  ! MetEmis NO2 GAS Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_050  ! MetEmis NO2 GAS Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_060  ! MetEmis NO2 GAS Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_070  ! MetEmis NO2 GAS Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_080  ! MetEmis NO2 GAS Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_090  ! MetEmis NO2 GAS Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_100  ! MetEmis NO2 GAS Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_110  ! MetEmis NO2 GAS Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_GAS_OR_120  ! MetEmis NO2 GAS Onroad Table 120F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_030  ! MetEmis NO2 DIS Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_040  ! MetEmis NO2 DIS Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_050  ! MetEmis NO2 DIS Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_060  ! MetEmis NO2 DIS Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_070  ! MetEmis NO2 DIS Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_080  ! MetEmis NO2 DIS Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_090  ! MetEmis NO2 DIS Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_100  ! MetEmis NO2 DIS Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_110  ! MetEmis NO2 DIS Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNO2_DIS_OR_120  ! MetEmis NO2 DIS Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_030  ! MetEmis HONO GAS Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_040  ! MetEmis HONO GAS Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_050  ! MetEmis HONO GAS Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_060  ! MetEmis HONO GAS Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_070  ! MetEmis HONO GAS Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_080  ! MetEmis HONO GAS Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_090  ! MetEmis HONO GAS Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_100  ! MetEmis HONO GAS Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_110  ! MetEmis HONO GAS Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_GAS_OR_120  ! MetEmis HONO GAS Onroad Table 120F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_030  ! MetEmis HONO DIS Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_040  ! MetEmis HONO DIS Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_050  ! MetEmis HONO DIS Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_060  ! MetEmis HONO DIS Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_070  ! MetEmis HONO DIS Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_080  ! MetEmis HONO DIS Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_090  ! MetEmis HONO DIS Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_100  ! MetEmis HONO DIS Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_110  ! MetEmis HONO DIS Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisHONO_DIS_OR_120  ! MetEmis HONO DIS Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_030  ! MetEmis CO  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_040  ! MetEmis CO  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_050  ! MetEmis CO  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_060  ! MetEmis CO  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_070  ! MetEmis CO  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_080  ! MetEmis CO  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_090  ! MetEmis CO  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_100  ! MetEmis CO  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_110  ! MetEmis CO  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCO_OR_120  ! MetEmis CO  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_030  ! MetEmis SO2  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_040  ! MetEmis SO2  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_050  ! MetEmis SO2  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_060  ! MetEmis SO2  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_070  ! MetEmis SO2  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_080  ! MetEmis SO2  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_090  ! MetEmis SO2  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_100  ! MetEmis SO2  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_110  ! MetEmis SO2  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSO2_OR_120  ! MetEmis SO2  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_030  ! MetEmis NH3  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_040  ! MetEmis NH3  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_050  ! MetEmis NH3  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_060  ! MetEmis NH3  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_070  ! MetEmis NH3  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_080  ! MetEmis NH3  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_090  ! MetEmis NH3  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_100  ! MetEmis NH3  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_110  ! MetEmis NH3  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNH3_OR_120  ! MetEmis NH3  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_030  ! MetEmis CH4  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_040  ! MetEmis CH4  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_050  ! MetEmis CH4  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_060  ! MetEmis CH4  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_070  ! MetEmis CH4  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_080  ! MetEmis CH4  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_090  ! MetEmis CH4  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_100  ! MetEmis CH4  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_110  ! MetEmis CH4  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisCH4_OR_120  ! MetEmis CH4  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_030  ! MetEmis ACROLEIN  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_040  ! MetEmis ACROLEIN  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_050  ! MetEmis ACROLEIN  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_060  ! MetEmis ACROLEIN  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_070  ! MetEmis ACROLEIN  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_080  ! MetEmis ACROLEIN  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_090  ! MetEmis ACROLEIN  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_100  ! MetEmis ACROLEIN  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_110  ! MetEmis ACROLEIN  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACROLEIN_OR_120  ! MetEmis ACROLEIN  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_030  ! MetEmis BUTADIENE13  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_040  ! MetEmis BUTADIENE13  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_050  ! MetEmis BUTADIENE13  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_060  ! MetEmis BUTADIENE13  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_070  ! MetEmis BUTADIENE13  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_080  ! MetEmis BUTADIENE13  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_090  ! MetEmis BUTADIENE13  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_100  ! MetEmis BUTADIENE13  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_110  ! MetEmis BUTADIENE13  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBUTADIENE13_OR_120  ! MetEmis BUTADIENE13  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_030  ! MetEmis ETHY  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_040  ! MetEmis ETHY  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_050  ! MetEmis ETHY  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_060  ! MetEmis ETHY  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_070  ! MetEmis ETHY  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_080  ! MetEmis ETHY  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_090  ! MetEmis ETHY  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_100  ! MetEmis ETHY  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_110  ! MetEmis ETHY  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHY_OR_120  ! MetEmis ETHY  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_030  ! MetEmis TERP  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_040  ! MetEmis TERP  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_050  ! MetEmis TERP  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_060  ! MetEmis TERP  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_070  ! MetEmis TERP  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_080  ! MetEmis TERP  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_090  ! MetEmis TERP  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_100  ! MetEmis TERP  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_110  ! MetEmis TERP  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTERP_OR_120  ! MetEmis TERP  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_030  ! MetEmis FORM  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_040  ! MetEmis FORM  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_050  ! MetEmis FORM  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_060  ! MetEmis FORM  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_070  ! MetEmis FORM  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_080  ! MetEmis FORM  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_090  ! MetEmis FORM  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_100  ! MetEmis FORM  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_110  ! MetEmis FORM  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_OR_120  ! MetEmis FORM  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_030  ! MetEmis PAR  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_040  ! MetEmis PAR  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_050  ! MetEmis PAR  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_060  ! MetEmis PAR  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_070  ! MetEmis PAR  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_080  ! MetEmis PAR  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_090  ! MetEmis PAR  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_100  ! MetEmis PAR  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_110  ! MetEmis PAR  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAR_OR_120  ! MetEmis PAR  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_030  ! MetEmis IOLE  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_040  ! MetEmis IOLE  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_050  ! MetEmis IOLE  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_060  ! MetEmis IOLE  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_070  ! MetEmis IOLE  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_080  ! MetEmis IOLE  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_090  ! MetEmis IOLE  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_100  ! MetEmis IOLE  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_110  ! MetEmis IOLE  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisIOLE_OR_120  ! MetEmis IOLE  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_030  ! MetEmis OLE  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_040  ! MetEmis OLE  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_050  ! MetEmis OLE  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_060  ! MetEmis OLE  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_070  ! MetEmis OLE  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_080  ! MetEmis OLE  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_090  ! MetEmis OLE  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_100  ! MetEmis OLE  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_110  ! MetEmis OLE  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisOLE_OR_120  ! MetEmis OLE  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_030  ! MetEmis ETH  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_040  ! MetEmis ETH  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_050  ! MetEmis ETH  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_060  ! MetEmis ETH  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_070  ! MetEmis ETH  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_080  ! MetEmis ETH  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_090  ! MetEmis ETH  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_100  ! MetEmis ETH  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_110  ! MetEmis ETH  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETH_OR_120  ! MetEmis ETH  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_030  ! MetEmis ETHA  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_040  ! MetEmis ETHA  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_050  ! MetEmis ETHA  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_060  ! MetEmis ETHA  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_070  ! MetEmis ETHA  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_080  ! MetEmis ETHA  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_090  ! MetEmis ETHA  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_100  ! MetEmis ETHA  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_110  ! MetEmis ETHA  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETHA_OR_120  ! MetEmis ETHA  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_030  ! MetEmis ETOH  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_040  ! MetEmis ETOH  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_050  ! MetEmis ETOH  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_060  ! MetEmis ETOH  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_070  ! MetEmis ETOH  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_080  ! MetEmis ETOH  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_090  ! MetEmis ETOH  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_100  ! MetEmis ETOH  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_110  ! MetEmis ETOH  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisETOH_OR_120  ! MetEmis ETOH  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_030  ! MetEmis MEOH  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_040  ! MetEmis MEOH  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_050  ! MetEmis MEOH  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_060  ! MetEmis MEOH  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_070  ! MetEmis MEOH  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_080  ! MetEmis MEOH  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_090  ! MetEmis MEOH  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_100  ! MetEmis MEOH  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_110  ! MetEmis MEOH  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisMEOH_OR_120  ! MetEmis MEOH  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_030  ! MetEmis BENZ  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_040  ! MetEmis BENZ  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_050  ! MetEmis BENZ  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_060  ! MetEmis BENZ  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_070  ! MetEmis BENZ  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_080  ! MetEmis BENZ  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_090  ! MetEmis BENZ  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_100  ! MetEmis BENZ  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_110  ! MetEmis BENZ  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisBENZ_OR_120  ! MetEmis BENZ  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_030  ! MetEmis TOL  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_040  ! MetEmis TOL  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_050  ! MetEmis TOL  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_060  ! MetEmis TOL  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_070  ! MetEmis TOL  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_080  ! MetEmis TOL  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_090  ! MetEmis TOL  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_100  ! MetEmis TOL  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_110  ! MetEmis TOL  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisTOL_OR_120  ! MetEmis TOL  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_030  ! MetEmis XYLMN  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_040  ! MetEmis XYLMN  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_050  ! MetEmis XYLMN  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_060  ! MetEmis XYLMN  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_070  ! MetEmis XYLMN  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_080  ! MetEmis XYLMN  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_090  ! MetEmis XYLMN  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_100  ! MetEmis XYLMN  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_110  ! MetEmis XYLMN  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisXYLMN_OR_120  ! MetEmis XYLMN  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_030  ! MetEmis NAPH  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_040  ! MetEmis NAPH  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_050  ! MetEmis NAPH  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_060  ! MetEmis NAPH  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_070  ! MetEmis NAPH  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_080  ! MetEmis NAPH  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_090  ! MetEmis NAPH  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_100  ! MetEmis NAPH  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_110  ! MetEmis NAPH  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisNAPH_OR_120  ! MetEmis NAPH  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_030  ! MetEmis ALD2  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_040  ! MetEmis ALD2  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_050  ! MetEmis ALD2  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_060  ! MetEmis ALD2  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_070  ! MetEmis ALD2  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_080  ! MetEmis ALD2  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_090  ! MetEmis ALD2  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_100  ! MetEmis ALD2  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_110  ! MetEmis ALD2  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_OR_120  ! MetEmis ALD2  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_030  ! MetEmis ALDX  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_040  ! MetEmis ALDX  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_050  ! MetEmis ALDX  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_060  ! MetEmis ALDX  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_070  ! MetEmis ALDX  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_080  ! MetEmis ALDX  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_090  ! MetEmis ALDX  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_100  ! MetEmis ALDX  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_110  ! MetEmis ALDX  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALDX_OR_120  ! MetEmis ALDX  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_030  ! MetEmis ISOP  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_040  ! MetEmis ISOP  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_050  ! MetEmis ISOP  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_060  ! MetEmis ISOP  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_070  ! MetEmis ISOP  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_080  ! MetEmis ISOP  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_090  ! MetEmis ISOP  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_100  ! MetEmis ISOP  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_110  ! MetEmis ISOP  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisISOP_OR_120  ! MetEmis ISOP  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_030  ! MetEmis PRPA  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_040  ! MetEmis PRPA  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_050  ! MetEmis PRPA  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_060  ! MetEmis PRPA  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_070  ! MetEmis PRPA  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_080  ! MetEmis PRPA  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_090  ! MetEmis PRPA  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_100  ! MetEmis PRPA  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_110  ! MetEmis PRPA  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPRPA_OR_120  ! MetEmis PRPA  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_030  ! MetEmis ACET  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_040  ! MetEmis ACET  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_050  ! MetEmis ACET  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_060  ! MetEmis ACET  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_070  ! MetEmis ACET  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_080  ! MetEmis ACET  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_090  ! MetEmis ACET  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_100  ! MetEmis ACET  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_110  ! MetEmis ACET  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisACET_OR_120  ! MetEmis ACET  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_030  ! MetEmis KET  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_040  ! MetEmis KET  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_050  ! MetEmis KET  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_060  ! MetEmis KET  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_070  ! MetEmis KET  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_080  ! MetEmis KET  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_090  ! MetEmis KET  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_100  ! MetEmis KET  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_110  ! MetEmis KET  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisKET_OR_120  ! MetEmis KET  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_030  ! MetEmis ALD2_PRIMARY  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_040  ! MetEmis ALD2_PRIMARY  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_050  ! MetEmis ALD2_PRIMARY  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_060  ! MetEmis ALD2_PRIMARY  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_070  ! MetEmis ALD2_PRIMARY  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_080  ! MetEmis ALD2_PRIMARY  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_090  ! MetEmis ALD2_PRIMARY  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_100  ! MetEmis ALD2_PRIMARY  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_110  ! MetEmis ALD2_PRIMARY  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisALD2_PRIMARY_OR_120  ! MetEmis ALD2_PRIMARY  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_030  ! MetEmis FORM_PRIMARY  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_040  ! MetEmis FORM_PRIMARY  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_050  ! MetEmis FORM_PRIMARY  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_060  ! MetEmis FORM_PRIMARY  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_070  ! MetEmis FORM_PRIMARY  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_080  ! MetEmis FORM_PRIMARY  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_090  ! MetEmis FORM_PRIMARY  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_100  ! MetEmis FORM_PRIMARY  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_110  ! MetEmis FORM_PRIMARY  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisFORM_PRIMARY_OR_120  ! MetEmis FORM_PRIMARY  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_030  ! MetEmis SOAALK  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_040  ! MetEmis SOAALK  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_050  ! MetEmis SOAALK  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_060  ! MetEmis SOAALK  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_070  ! MetEmis SOAALK  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_080  ! MetEmis SOAALK  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_090  ! MetEmis SOAALK  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_100  ! MetEmis SOAALK  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_110  ! MetEmis SOAALK  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisSOAALK_OR_120  ! MetEmis SOAALK  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_030  ! MetEmis PEC  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_040  ! MetEmis PEC  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_050  ! MetEmis PEC  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_060  ! MetEmis PEC  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_070  ! MetEmis PEC  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_080  ! MetEmis PEC  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_090  ! MetEmis PEC  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_100  ! MetEmis PEC  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_110  ! MetEmis PEC  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPEC_OR_120  ! MetEmis PEC  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_030  ! MetEmis POC  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_040  ! MetEmis POC  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_050  ! MetEmis POC  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_060  ! MetEmis POC  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_070  ! MetEmis POC  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_080  ! MetEmis POC  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_090  ! MetEmis POC  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_100  ! MetEmis POC  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_110  ! MetEmis POC  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPOC_OR_120  ! MetEmis POC  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_030  ! MetEmis PAL  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_040  ! MetEmis PAL  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_050  ! MetEmis PAL  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_060  ! MetEmis PAL  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_070  ! MetEmis PAL  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_080  ! MetEmis PAL  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_090  ! MetEmis PAL  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_100  ! MetEmis PAL  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_110  ! MetEmis PAL  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPAL_OR_120  ! MetEmis PAL  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_030  ! MetEmis PCA  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_040  ! MetEmis PCA  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_050  ! MetEmis PCA  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_060  ! MetEmis PCA  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_070  ! MetEmis PCA  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_080  ! MetEmis PCA  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_090  ! MetEmis PCA  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_100  ! MetEmis PCA  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_110  ! MetEmis PCA  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCA_OR_120  ! MetEmis PCA  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_030  ! MetEmis PCL  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_040  ! MetEmis PCL  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_050  ! MetEmis PCL  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_060  ! MetEmis PCL  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_070  ! MetEmis PCL  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_080  ! MetEmis PCL  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_090  ! MetEmis PCL  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_100  ! MetEmis PCL  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_110  ! MetEmis PCL  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPCL_OR_120  ! MetEmis PCL  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_030  ! MetEmis PFE  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_040  ! MetEmis PFE  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_050  ! MetEmis PFE  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_060  ! MetEmis PFE  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_070  ! MetEmis PFE  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_080  ! MetEmis PFE  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_090  ! MetEmis PFE  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_100  ! MetEmis PFE  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_110  ! MetEmis PFE  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPFE_OR_120  ! MetEmis PFE  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_030  ! MetEmis PH2O  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_040  ! MetEmis PH2O  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_050  ! MetEmis PH2O  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_060  ! MetEmis PH2O  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_070  ! MetEmis PH2O  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_080  ! MetEmis PH2O  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_090  ! MetEmis PH2O  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_100  ! MetEmis PH2O  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_110  ! MetEmis PH2O  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPH2O_OR_120  ! MetEmis PH2O  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_030  ! MetEmis PK  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_040  ! MetEmis PK  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_050  ! MetEmis PK  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_060  ! MetEmis PK  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_070  ! MetEmis PK  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_080  ! MetEmis PK  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_090  ! MetEmis PK  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_100  ! MetEmis PK  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_110  ! MetEmis PK  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPK_OR_120  ! MetEmis PK  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_030  ! MetEmis PMG  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_040  ! MetEmis PMG  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_050  ! MetEmis PMG  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_060  ! MetEmis PMG  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_070  ! MetEmis PMG  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_080  ! MetEmis PMG  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_090  ! MetEmis PMG  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_100  ! MetEmis PMG  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_110  ! MetEmis PMG  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMG_OR_120  ! MetEmis PMG  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_030  ! MetEmis PMN  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_040  ! MetEmis PMN  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_050  ! MetEmis PMN  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_060  ! MetEmis PMN  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_070  ! MetEmis PMN  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_080  ! MetEmis PMN  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_090  ! MetEmis PMN  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_100  ! MetEmis PMN  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_110  ! MetEmis PMN  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMN_OR_120  ! MetEmis PMN  Onroad Table 120F [kg/m2/s]


     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_030  ! MetEmis PMOTHR  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_040  ! MetEmis PMOTHR  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_050  ! MetEmis PMOTHR  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_060  ! MetEmis PMOTHR  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_070  ! MetEmis PMOTHR  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_080  ! MetEmis PMOTHR  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_090  ! MetEmis PMOTHR  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_100  ! MetEmis PMOTHR  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_110  ! MetEmis PMOTHR  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMOTHR_OR_120  ! MetEmis PMOTHR  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_030  ! MetEmis PNA  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_040  ! MetEmis PNA  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_050  ! MetEmis PNA  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_060  ! MetEmis PNA  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_070  ! MetEmis PNA  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_080  ! MetEmis PNA  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_090  ! MetEmis PNA  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_100  ! MetEmis PNA  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_110  ! MetEmis PNA  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNA_OR_120  ! MetEmis PNA  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_030  ! MetEmis PNCOM  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_040  ! MetEmis PNCOM  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_050  ! MetEmis PNCOM  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_060  ! MetEmis PNCOM  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_070  ! MetEmis PNCOM  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_080  ! MetEmis PNCOM  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_090  ! MetEmis PNCOM  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_100  ! MetEmis PNCOM  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_110  ! MetEmis PNCOM  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNCOM_OR_120  ! MetEmis PNCOM  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_030  ! MetEmis PNH4  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_040  ! MetEmis PNH4  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_050  ! MetEmis PNH4  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_060  ! MetEmis PNH4  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_070  ! MetEmis PNH4  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_080  ! MetEmis PNH4  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_090  ! MetEmis PNH4  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_100  ! MetEmis PNH4  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_110  ! MetEmis PNH4  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNH4_OR_120  ! MetEmis PNH4  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_030  ! MetEmis PNO3  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_040  ! MetEmis PNO3  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_050  ! MetEmis PNO3  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_060  ! MetEmis PNO3  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_070  ! MetEmis PNO3  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_080  ! MetEmis PNO3  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_090  ! MetEmis PNO3  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_100  ! MetEmis PNO3  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_110  ! MetEmis PNO3  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPNO3_OR_120  ! MetEmis PNO3  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_030  ! MetEmis PTI  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_040  ! MetEmis PTI  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_050  ! MetEmis PTI  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_060  ! MetEmis PTI  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_070  ! MetEmis PTI  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_080  ! MetEmis PTI  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_090  ! MetEmis PTI  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_100  ! MetEmis PTI  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_110  ! MetEmis PTI  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPTI_OR_120  ! MetEmis PTI  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_030  ! MetEmis PSI  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_040  ! MetEmis PSI  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_050  ! MetEmis PSI  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_060  ! MetEmis PSI  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_070  ! MetEmis PSI  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_080  ! MetEmis PSI  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_090  ! MetEmis PSI  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_100  ! MetEmis PSI  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_110  ! MetEmis PSI  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSI_OR_120  ! MetEmis PSI  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_030  ! MetEmis PMC  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_040  ! MetEmis PMC  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_050  ! MetEmis PMC  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_060  ! MetEmis PMC  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_070  ! MetEmis PMC  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_080  ! MetEmis PMC  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_090  ! MetEmis PMC  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_100  ! MetEmis PMC  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_110  ! MetEmis PMC  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPMC_OR_120  ! MetEmis PMC  Onroad Table 120F [kg/m2/s]

     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_030  ! MetEmis PSO4  Onroad Table 30F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_040  ! MetEmis PSO4  Onroad Table 40F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_050  ! MetEmis PSO4  Onroad Table 50F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_060  ! MetEmis PSO4  Onroad Table 60F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_070  ! MetEmis PSO4  Onroad Table 70F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_080  ! MetEmis PSO4  Onroad Table 80F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_090  ! MetEmis PSO4  Onroad Table 90F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_100  ! MetEmis PSO4  Onroad Table 100F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_110  ! MetEmis PSO4  Onroad Table 110F [kg/m2/s]
     TYPE(ExtDat_2R),  POINTER :: MEmisPSO4_OR_120  ! MetEmis PSO4  Onroad Table 120F [kg/m2/s]

     INTEGER,          POINTER :: PBL_MAX     ! Max height of PBL [level]
     TYPE(ExtDat_3R),  POINTER :: CNV_MFC     ! Convective cloud mass flux [kg/m2/s]
     TYPE(ExtDat_3R),  POINTER :: FRAC_OF_PBL ! Fraction of grid box in PBL
     TYPE(ExtDat_3R),  POINTER :: SPHU        ! Spec. humidity [kg H2O/kg total air]
     TYPE(ExtDat_3R),  POINTER :: TK          ! Air temperature [K]
     TYPE(ExtDat_3R),  POINTER :: AIR         ! Dry air mass [kg]
     TYPE(ExtDat_3R),  POINTER :: AIRVOL      ! Air volume [m3]
     TYPE(ExtDat_3R),  POINTER :: AIRDEN      ! Dry air density [kg/m3]
     TYPE(ExtDat_3R),  POINTER :: O3          ! O3  mass [kg/kg dry air]
     TYPE(ExtDat_3R),  POINTER :: NO          ! NO  mass [kg/kg dry air]
     TYPE(ExtDat_3R),  POINTER :: NO2         ! NO2 mass [kg/kg dry air]
     TYPE(ExtDat_3R),  POINTER :: HNO3        ! HNO3 mass [kg/kg dry air]
     TYPE(ExtDat_3R),  POINTER :: POPG        ! POPG mass [kg/kg dry air]

     !----------------------------------------------------------------------
     ! Deposition parameter
     ! DRY_TOTN and WET_TOTN are the total (dry/wet) deposited N since the
     ! last emission timestep. Even though these numbers are per second,
     ! they may represent accumulated deposition velocities if chemistry
     ! and/or dynamic timestep are not equal to the emission timestep.
     ! These values are used by the soil NOx module. Note that it is assumed
     ! that DRY_TOTN and WET_TOTN are summed over chemistry and transport
     ! timesteps, respectively!
     !----------------------------------------------------------------------
     TYPE(ExtDat_2R),  POINTER :: DRY_TOTN    ! Dry deposited N   [molec/cm2/s]
     TYPE(ExtDat_2R),  POINTER :: WET_TOTN    ! Wet deposited N   [kg N/s]
     REAL(hp),         POINTER :: DRYCOEFF(:) ! Baldocci drydep coeff.

     !----------------------------------------------------------------------
     ! Constants for POPs emissions module
     !----------------------------------------------------------------------
     REAL(dp)                  :: POP_DEL_H   ! Delta H [J/mol]
     REAL(dp)                  :: POP_DEL_Hw  ! Delta Hw [J/mol]
     REAL(dp)                  :: POP_HSTAR   ! Henry's law constant [atm/M/L]
     REAL(dp)                  :: POP_KOA     ! POP octanol-water partition coef
     REAL(dp)                  :: POP_KBC     ! POP BC-air partition coeff.
     REAL(dp)                  :: POP_XMW     ! POP molecular weight [kg/mol]

     !----------------------------------------------------------------------
     ! Fields used in ESMF environment only. These arrays won't be used
     ! in a classic environment. They become filled in HCO_SetExtState_ESMF
     ! in hcoi_esmf_mod.F90 (called from within hcoi_gc_main_mod.F90).
     !----------------------------------------------------------------------
     TYPE(ExtDat_3S),  POINTER :: BYNCY       ! Buoyancy
     TYPE(ExtDat_2S),  POINTER :: LFR         ! Lightning flash rate
     TYPE(ExtDat_2R),  POINTER :: CNV_FRC     ! convective fraction (filled
                                              ! from State_Met)
  END TYPE Ext_State
!
! !PRIVATE MEMBER FUNCTIONS:
!
! !REVISION HISTORY:
!  02 Oct 2013 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!-----------------------------------------------------------------------------
!BOC
!
! !MODULE INTERFACES:
!
  INTERFACE ExtDat_Init
     MODULE PROCEDURE ExtDat_Init_2R
     MODULE PROCEDURE ExtDat_Init_2S
     MODULE PROCEDURE ExtDat_Init_2I
     MODULE PROCEDURE ExtDat_Init_3R
     MODULE PROCEDURE ExtDat_Init_3S
  END INTERFACE ExtDat_Init

  INTERFACE ExtDat_Set
     MODULE PROCEDURE ExtDat_Set_2R
     MODULE PROCEDURE ExtDat_Set_2S
     MODULE PROCEDURE ExtDat_Set_2I
     MODULE PROCEDURE ExtDat_Set_3R
     MODULE PROCEDURE ExtDat_Set_3S
  END INTERFACE ExtDat_Set

  INTERFACE ExtDat_Cleanup
     MODULE PROCEDURE ExtDat_Cleanup_2R
     MODULE PROCEDURE ExtDat_Cleanup_2S
     MODULE PROCEDURE ExtDat_Cleanup_2I
     MODULE PROCEDURE ExtDat_Cleanup_3R
     MODULE PROCEDURE ExtDat_Cleanup_3S
  END INTERFACE ExtDat_Cleanup

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: ExtStateInit
!
! !DESCRIPTION: Initializes all fields of the ExtState object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtStateInit( ExtState, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER        :: ExtState   ! ExtState object
    INTEGER,         INTENT(INOUT)  :: RC         ! Success or failure?
!
! !REMARKS:
!  You can add more initialization statements as is necessary.
!
! !REVISION HISTORY:
!  15 Dec 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC, MSG

    !======================================================================
    ! ExtStateInit begins here
    !======================================================================
    LOC = 'ExtStateInit (HCOX_STATE_MOD.F90)'

    ! Allocate object
    IF ( .NOT. ASSOCIATED ( ExtState ) ) ALLOCATE ( ExtState )

    !-----------------------------------------------------------------------
    ! Set all switches to -1
    !-----------------------------------------------------------------------
    ExtState%Custom         = -1
    ExtState%DustL23M       = -1
    ExtState%DustDead       = -1
    ExtState%DustGinoux     = -1
    ExtState%DustAlk        = -1
    ExtState%LightNOx       = -1
    ExtState%ParaNOx        = -1
    ExtState%SoilNOx        = -1
    ExtState%Megan          = -1
    ExtState%SeaFlux        = -1
    ExtState%SeaSalt        = -1
    ExtState%GFED           = -1
    ExtState%FINN           = -1
    ExtState%GC_RnPbBe      = -1
    ExtState%GC_POPs        = -1
    ExtState%Wetland_CH4    = -1
    ExtState%TOMAS_Jeagle   = -1
    ExtState%TOMAS_DustDead = -1
    ExtState%Volcano        = -1
    ExtState%Inorg_Iodine   = -1
    ExtState%MetEmis        = -1

    !-----------------------------------------------------------------------
    ! Initialize constants for POPs emissions module
    !-----------------------------------------------------------------------
    ExtState%POP_DEL_H      = 0d0
    ExtState%POP_DEL_Hw     = 0d0
    ExtState%POP_HSTAR      = 0d0
    ExtState%POP_KOA        = 0d0
    ExtState%POP_KBC        = 0d0
    ExtState%POP_XMW        = 0d0

    !-----------------------------------------------------------------------
    ! Initialize all met arrays.
    ! This defines a nullified pointer for every met field and sets the
    ! corresponding DoUse flag to FALSE. The pointers to the met fields
    ! need to be defined in the HEMCO-model interface routine.
    !-----------------------------------------------------------------------
    CALL ExtDat_Init( ExtState%U10M, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%U10M'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%V10M, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%V10M'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%ALBD, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%ALBD'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%T2M, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%T2M'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%TS, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%TS'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%TSKIN, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%TSKIN'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%TSOIL1, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%TSOIL1'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%GWETROOT, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%GWETROOT'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%GWETTOP, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%GWETTOP'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%SNOMAS, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%SNOMAS'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%SNODP, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%SNODP'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%SNICE, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%SNICE'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%USTAR, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%USTAR'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%Z0, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%Z0'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%TROPP, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%TROPP'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%SUNCOS, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%SUNCOS'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%SZAFACT, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%SZAFACT'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%PARDR, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%PARDR'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%HFLUX, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%HFLUX'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%PARDF, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%PARDF'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%PSC2_WET, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%PSC2_WET'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%RADSWG, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%RADSWG'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%FRCLND, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FRCLND'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%FRLAND, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FRLAND'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%FROCEAN, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FROCEAN'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%FRSEAICE, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FRSEAICE'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%QV2M, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%QV2M'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%FRLAKE, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FRLAKE'
       CALL HCO_ERROR( 'ERROR 25', RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%FRLANDIC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FRLANDIC'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%CLDFRC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%CLDFRC'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%LAI, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%LAI'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%CHLR, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%CHLR'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%FLASH_DENS, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FLASH_DENS'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%CONV_DEPTH, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%CONV_DEPTH'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%JNO2, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%JNO2'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%JOH, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%JOH'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%CNV_MFC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%CNV_MFC'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    ExtState%PBL_MAX    => NULL()

    CALL ExtDat_Init( ExtState%FRAC_OF_PBL, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%FRAC_OF_PBL'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%SPHU, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%SPHU'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%TK, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%TK'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%AIR, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%AIR'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%AIRVOL, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%AIRVOL'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%AIRDEN, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%AIRDEN'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%O3, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%O3'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%NO, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%NO'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%NO2, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%NO2'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%HNO3, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%HNO3'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%POPG, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%POPG'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%DRY_TOTN, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%DRY_TOTN'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%WET_TOTN, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%WET_TOTN'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%BYNCY, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%BYNCY'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%LFR, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%LFR'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%CNV_FRC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%CNV_FRC'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init( ExtState%TropLev, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Could not allocate ExtState%TropLev'
       CALL HCO_ERROR( MSG, RC, THISLOC=LOC )
       RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_100', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_GAS_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_GAS_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO_DIS_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO_DIS_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

     CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF


    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_GAS_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_GAS_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNO2_DIS_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNO2_DIS_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

     CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_090', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_GAS_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_GAS_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisHONO_DIS_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisHONO_DIS_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_080', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCO_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCO_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSO2_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSO2_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNH3_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNH3_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisCH4_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisCH4_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACROLEIN_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACROLEIN_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBUTADIENE13_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBUTADIENE13_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHY_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHY_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTERP_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTERP_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAR_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAR_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisIOLE_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisIOLE_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisOLE_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisOLE_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETH_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETH_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETHA_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETHA_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisETOH_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisETOH_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisMEOH_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisMEOH_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisBENZ_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisBENZ_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisTOL_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisTOL_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisXYLMN_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisXYLMN_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisNAPH_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisNAPH_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALDX_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALDX_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisISOP_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisISOP_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPRPA_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPRPA_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisACET_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisACET_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisKET_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisKET_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisALD2_PRIMARY_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisALD2_PRIMARY_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisFORM_PRIMARY_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisFORM_PRIMARY_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisSOAALK_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisSOAALK_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPEC_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPEC_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPOC_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPOC_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPAL_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPAL_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCA_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCA_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPCL_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPCL_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPFE_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPFE_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPH2O_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPH2O_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPK_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPK_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMG_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMG_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMN_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMN_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMOTHR_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMOTHR_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNA_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNA_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNCOM_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNCOM_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNH4_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNH4_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPNO3_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPNO3_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPTI_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPTI_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSI_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSI_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPMC_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPMC_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_030, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_030 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_040, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_040 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_050, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_050 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_060, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_060 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_070, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_070 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_080, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_080 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_090, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_090 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_100, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_100 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_110, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_110 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL ExtDat_Init ( ExtState%MEmisPSO4_OR_120, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'Init error: MEmisPSO4_OR_120 ', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ExtStateInit
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtStateFinal
!
! !DESCRIPTION: Finalizes the ExtState object. This removes all defined
!  pointer links (i.e. nullifies ExtDat\%Arr), but does not deallocate
!  the target array!
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtStateFinal( ExtState )
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER  :: ExtState
!
! !REVISION HISTORY:
!  03 Oct 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    !======================================================================
    ! ExtStateFinal begins here
    !======================================================================

    IF ( ASSOCIATED(ExtState) ) THEN

       ! Cleanup arrays. Don't do deepclean, i.e. only nullify pointers!
       CALL ExtDat_Cleanup( ExtState%U10M       )
       CALL ExtDat_Cleanup( ExtState%V10M       )
       CALL ExtDat_Cleanup( ExtState%ALBD       )
       CALL ExtDat_Cleanup( ExtState%T2M        )
       CALL ExtDat_Cleanup( ExtState%TS         )
       CALL ExtDat_Cleanup( ExtState%TSKIN      )
       CALL ExtDat_Cleanup( ExtState%TSOIL1     )
       CALL ExtDat_Cleanup( ExtState%GWETROOT   )
       CALL ExtDat_Cleanup( ExtState%GWETTOP    )
       CALL ExtDat_Cleanup( ExtState%SNOMAS     )
       CALL ExtDat_Cleanup( ExtState%SNODP      )
       CALL ExtDat_Cleanup( ExtState%SNICE      )
       CALL ExtDat_Cleanup( ExtState%USTAR      )
       CALL ExtDat_Cleanup( ExtState%Z0         )
       CALL ExtDat_Cleanup( ExtState%TROPP      )
       CALL ExtDat_Cleanup( ExtState%SUNCOS     )
       CALL ExtDat_Cleanup( ExtState%SZAFACT    )
       CALL ExtDat_Cleanup( ExtState%PARDR      )
       CALL ExtDat_Cleanup( ExtState%PARDF      )
       CALL ExtDat_Cleanup( ExtState%HFLUX      )
       CALL ExtDat_Cleanup( ExtState%PSC2_WET   )
       CALL ExtDat_Cleanup( ExtState%RADSWG     )
       CALL ExtDat_Cleanup( ExtState%FRCLND     )
       CALL ExtDat_Cleanup( ExtState%FRLAND     )
       CALL ExtDat_Cleanup( ExtState%FROCEAN    )
       CALL ExtDat_Cleanup( ExtState%FRSEAICE   )
       CALL ExtDat_Cleanup( ExtState%QV2M       )
       CALL ExtDat_Cleanup( ExtState%FRLAKE     )
       CALL ExtDat_Cleanup( ExtState%FRLANDIC   )
       CALL ExtDat_Cleanup( ExtState%CLDFRC     )
       CALL ExtDat_Cleanup( ExtState%LAI        )
       CALL ExtDat_Cleanup( ExtState%CHLR       )
       CALL ExtDat_Cleanup( ExtState%FLASH_DENS )
       CALL ExtDat_Cleanup( ExtState%CONV_DEPTH )
       CALL ExtDat_Cleanup( ExtState%JNO2       )
       CALL ExtDat_Cleanup( ExtState%JOH        )
       CALL ExtDat_Cleanup( ExtState%CNV_MFC    )
       CALL ExtDat_Cleanup( ExtState%FRAC_OF_PBL)
       CALL ExtDat_Cleanup( ExtState%SPHU       )
       CALL ExtDat_Cleanup( ExtState%TK         )
       CALL ExtDat_Cleanup( ExtState%AIR        )
       CALL ExtDat_Cleanup( ExtState%AIRVOL     )
       CALL ExtDat_Cleanup( ExtState%AIRDEN     )
       CALL ExtDat_Cleanup( ExtState%O3         )
       CALL ExtDat_Cleanup( ExtState%NO         )
       CALL ExtDat_Cleanup( ExtState%NO2        )
       CALL ExtDat_Cleanup( ExtState%HNO3       )
       CALL ExtDat_Cleanup( ExtState%POPG       )
       CALL ExtDat_Cleanup( ExtState%DRY_TOTN   )
       CALL ExtDat_Cleanup( ExtState%WET_TOTN   )
       CALL ExtDat_Cleanup( ExtState%CNV_FRC    )
       CALL ExtDat_Cleanup( ExtState%BYNCY      )
       CALL ExtDat_Cleanup( ExtState%LFR        )
       CALL ExtDat_Cleanup( ExtState%TropLev    )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_GAS_OR_120 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO_DIS_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_GAS_OR_120 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisNO2_DIS_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_GAS_OR_120 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisHONO_DIS_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisCO_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisSO2_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisNH3_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisCH4_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisACROLEIN_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisBUTADIENE13_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHY_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisTERP_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAR_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisIOLE_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisOLE_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisETH_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisETHA_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisETOH_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisMEOH_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisBENZ_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisTOL_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisXYLMN_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisNAPH_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisALDX_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisISOP_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPRPA_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisACET_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisKET_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisALD2_PRIMARY_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisFORM_PRIMARY_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisSOAALK_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPEC_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPOC_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPAL_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCA_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPCL_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPFE_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPH2O_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPK_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMG_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMN_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMOTHR_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNA_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNCOM_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNH4_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPNO3_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPTI_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSI_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPMC_OR_120 )

       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_030 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_040 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_050 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_060 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_070 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_080 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_090 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_100 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_110 )
       CALL ExtDat_Cleanup( ExtState%MEmisPSO4_OR_120 )


       ExtState%DRYCOEFF   => NULL()
       ExtState%PBL_MAX    => NULL()

    ENDIF

  END SUBROUTINE ExtStateFinal
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Init_2R
!
! !DESCRIPTION: Subroutine ExtDat\_Init\_2R initializes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Init_2R ( ExtDat, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_2R), POINTER       :: ExtDat
    INTEGER,         INTENT(INOUT) :: RC        ! Return code
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC

    ! ================================================================
    ! ExtDat_Init_2R begins here
    ! ================================================================
    LOC = 'ExtDat_Init_2R (HCOX_STATE_MOD.F90)'

    ExtDat     => NULL()
    ALLOCATE(ExtDat)
    ExtDat%Arr => NULL()

    ! Establish pointer to ExtDat%Arr%Val
    CALL HCO_ArrInit( ExtDat%Arr, 0, 0, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 52', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ExtDat%DoUse = .FALSE.
    ExtDat%FromList = .FALSE.

    ! Leave
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Init_2R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Init_2S
!
! !DESCRIPTION: Subroutine ExtDat\_Init\_2S initializes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Init_2S ( ExtDat, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_2S), POINTER       :: ExtDat
    INTEGER,         INTENT(INOUT) :: RC        ! Return code
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC

    ! ================================================================
    ! ExtDat_Init_2S begins here
    ! ================================================================
    LOC = 'ExtDat_Init_2S (HCOX_STATE_MOD.F90)'

    ExtDat     => NULL()
    ALLOCATE(ExtDat)
    ExtDat%Arr => NULL()

    ! Establish pointer to ExtDat%Arr%Val
    CALL HCO_ArrInit( ExtDat%Arr, 0, 0, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 53', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ExtDat%DoUse = .FALSE.
    ExtDat%FromList = .FALSE.

    ! Leave
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Init_2S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Init_2I
!
! !DESCRIPTION: Subroutine ExtDat\_Init\_2I initializes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Init_2I ( ExtDat, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_2I), POINTER       :: ExtDat
    INTEGER,         INTENT(INOUT) :: RC        ! Return code
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC

    ! ================================================================
    ! ExtDat_Init_2I begins here
    ! ================================================================
    LOC = 'ExtDat_Init_2I (HCOX_STATE_MOD.F90)'

    ExtDat => NULL()
    ALLOCATE(ExtDat)
    ExtDat%Arr => NULL()

    ! Establish pointer to ExtDat%Arr%Val
    CALL HCO_ArrInit( ExtDat%Arr, 0, 0, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 54', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ExtDat%DoUse = .FALSE.
    ExtDat%FromList = .FALSE.

    ! Leave
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Init_2I
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Init_3R
!
! !DESCRIPTION: Subroutine ExtDat\_Init\_3R initializes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Init_3R ( ExtDat, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_3R), POINTER       :: ExtDat
    INTEGER,         INTENT(INOUT) :: RC        ! Return code
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC
    ! ================================================================
    ! ExtDat_Init_3R begins here
    ! ================================================================
    LOC = 'ExtDat_Init_3R (HCOX_STATE_MOD.F90)'

    ExtDat => NULL()
    ALLOCATE(ExtDat)
    ExtDat%Arr => NULL()

    ! Establish pointer to ExtDat%Arr%Val
    CALL HCO_ArrInit( ExtDat%Arr, 0, 0, 0, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 55', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ExtDat%DoUse = .FALSE.
    ExtDat%FromList = .FALSE.

    ! Leave
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Init_3R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Init_3S
!
! !DESCRIPTION: Subroutine ExtDat\_Init\_3S initializes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Init_3S ( ExtDat, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_3S), POINTER       :: ExtDat
    INTEGER,         INTENT(INOUT) :: RC        ! Return code
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC
    ! ================================================================
    ! ExtDat_Init_3S begins here
    ! ================================================================
    LOC = 'ExtDat_Init_3S (HCOX_STATE_MOD.F90)'

    ExtDat => NULL()
    ALLOCATE(ExtDat)
    ExtDat%Arr => NULL()

    ! Establish pointer to ExtDat%Arr%Val
    CALL HCO_ArrInit( ExtDat%Arr, 0, 0, 0, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 56', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ExtDat%DoUse = .FALSE.
    ExtDat%FromList = .FALSE.

    ! Leave
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Init_3S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Cleanup_2R
!
! !DESCRIPTION: Subroutine ExtDat\_Cleanup\_2R removes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Cleanup_2R ( ExtDat )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_2R), POINTER       :: ExtDat
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    ! ================================================================
    ! ExtDat_Cleanup_2R begins here
    ! ================================================================

    IF ( ASSOCIATED( ExtDat) ) THEN
       CALL HCO_ArrCleanup( ExtDat%Arr, DeepClean=.TRUE. )
       DEALLOCATE ( ExtDat )
    ENDIF

  END SUBROUTINE ExtDat_Cleanup_2R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Cleanup_2S
!
! !DESCRIPTION: Subroutine ExtDat\_Cleanup\_2S removes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Cleanup_2S ( ExtDat )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_2S), POINTER       :: ExtDat
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    ! ================================================================
    ! ExtDat_Cleanup_2S begins here
    ! ================================================================

    IF ( ASSOCIATED( ExtDat) ) THEN
       CALL HCO_ArrCleanup( ExtDat%Arr, DeepClean=.TRUE. )
       DEALLOCATE ( ExtDat )
    ENDIF

  END SUBROUTINE ExtDat_Cleanup_2S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Cleanup_2I
!
! !DESCRIPTION: Subroutine ExtDat\_Cleanup\_2I removes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Cleanup_2I ( ExtDat )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_2I), POINTER       :: ExtDat
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    ! ================================================================
    ! ExtDat_Cleanup_2I begins here
    ! ================================================================

    IF ( ASSOCIATED( ExtDat) ) THEN
       CALL HCO_ArrCleanup( ExtDat%Arr, DeepClean=.TRUE. )
       DEALLOCATE ( ExtDat )
    ENDIF

  END SUBROUTINE ExtDat_Cleanup_2I
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Cleanup_3R
!
! !DESCRIPTION: Subroutine ExtDat\_Cleanup\_3R removes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Cleanup_3R( ExtDat )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_3R), POINTER       :: ExtDat
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    ! ================================================================
    ! ExtDat_Cleanup_3R begins here
    ! ================================================================

    IF ( ASSOCIATED( ExtDat) ) THEN
       CALL HCO_ArrCleanup( ExtDat%Arr, DeepClean=.TRUE. )
       DEALLOCATE ( ExtDat )
    ENDIF

  END SUBROUTINE ExtDat_Cleanup_3R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Cleanup_3S
!
! !DESCRIPTION: Subroutine ExtDat\_Cleanup\_3S removes the given ExtDat type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Cleanup_3S( ExtDat )
!
! !INPUT PARAMETERS:
!
    TYPE(ExtDat_3S), POINTER       :: ExtDat
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    ! ================================================================
    ! ExtDat_Cleanup_3S begins here
    ! ================================================================

    IF ( ASSOCIATED( ExtDat) ) THEN
       CALL HCO_ArrCleanup( ExtDat%Arr, DeepClean=.TRUE. )
       DEALLOCATE ( ExtDat )
    ENDIF

  END SUBROUTINE ExtDat_Cleanup_3S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Set_2R
!
! !DESCRIPTION: Subroutine ExtDat\_Set\_2R sets/updates the data array of an
! ExtDat object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Set_2R ( HcoState, ExtDat,           &
                             FldName,  RC,     First,    &
                             Trgt,     Filled, NotFillOk )
!
! !USES:
!
    USE HCO_ARR_MOD,        ONLY : HCO_ArrAssert
    USE HCO_STATE_MOD,      ONLY : HCO_State
    USE HCO_CALC_MOD,       ONLY : HCO_EvalFld
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER                         :: HcoState
    TYPE(ExtDat_2R),  POINTER                         :: ExtDat
    CHARACTER(LEN=*), INTENT(IN   )                   :: FldName
    INTEGER,          INTENT(INOUT)                   :: RC
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: First
    REAL(hp),         POINTER      , OPTIONAL         :: Trgt(:,:)
    LOGICAL,          INTENT(  OUT), OPTIONAL         :: Filled
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: NotFillOk
!
! !REVISION HISTORY:
!  03 Apr 2015 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                :: AS, NX, NY
    REAL(hp), ALLOCATABLE  :: Arr2D(:,:)
    CHARACTER(LEN=255)     :: MSG
    CHARACTER(LEN=255)     :: LOC = 'ExtDat_Set_2R (hcox_state_mod.F90)'
    LOGICAL                :: FRST
    LOGICAL                :: FOUND
    LOGICAL                :: FailIfNotFilled

    ! ================================================================
    ! ExtDat_Set_2R begins here
    ! ================================================================

    ! Initialize
    RC = HCO_SUCCESS
    IF ( PRESENT(Filled) ) Filled = .FALSE.

    ! Nothing to do if this ExtDat field is not in use
    IF ( .NOT. ExtDat%DoUse ) RETURN

    ! Check for fill requirement
    IF ( PRESENT(NotFillOk) ) THEN
       FailIfNotFilled = .NOT. NotFillOk
    ELSE
       FailIfNotFilled = .TRUE.
    ENDIF

    ! First time
    IF ( PRESENT(FIRST) ) THEN
       FRST = FIRST
    ELSE
       FRST = .FALSE.
    ENDIF

    ! On first call or if data is flagged as being read from list, get data
    ! from emissions list
    IF ( FRST .OR. ExtDat%FromList ) THEN

       ! Allocate temporary array
       ALLOCATE(Arr2D(HcoState%NX,HcoState%NY),STAT=AS)
       IF ( AS /= 0 ) THEN
          CALL HCO_ERROR ( "Arr2D allocation error", RC, THISLOC=LOC )
          RETURN
       ENDIF

       ! Try to get data from list
       CALL HCO_EvalFld( HcoState, TRIM(FldName), Arr2D, RC, FOUND=FOUND )
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 57', RC, THISLOC=LOC )
           RETURN
       ENDIF

       ! On first call, need to make additional checks
       IF ( FRST ) THEN

          ! If read from list
          IF ( FOUND ) THEN
             ExtDat%FromList = .TRUE.

             ! Make sure array is allocated
             CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, RC )
             IF ( RC /= HCO_SUCCESS ) THEN
                 CALL HCO_ERROR( 'ERROR 58', RC, THISLOC=LOC )
                 RETURN
             ENDIF

             ! Verbose
             IF ( HcoState%Config%doVerbose ) THEN
                MSG = 'Will fill extension field from HEMCO data list field ' // TRIM(FldName)
                CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
             ENDIF

          ! Target to data
          ELSEIF ( PRESENT(Trgt) ) THEN

             ! If target is not associated:
             IF ( .NOT. ASSOCIATED(Trgt) ) THEN
                IF ( FailIfNotFilled ) THEN
                   MSG = 'Cannot fill extension field ' // TRIM(FldName) // &
                         ' because target field is not associated.'
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

             ! If target is associated:
             ELSE

                ! Make sure dimensions agree
                NX = SIZE(Trgt,1)
                NY = SIZE(Trgt,2)

                ! Must cover the horizontal grid
                IF ( (NX/=HcoState%NX) .OR. (NY/=HcoState%NY) ) THEN
                   WRITE(MSG,*) 'Horizontal dimensions of target data do not ', &
                      'correspond to simulation grid: ', &
                      'Expected dimensions: ', HcoState%NX, HcoState%NY, &
                      '; encountered dimensions: ', NX, NY, '. Error occured ', &
                      'for field ', TRIM(FldName)
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

                ! Link data to target
                ExtDat%Arr%Val => Trgt

                ! Make sure it's not from list
                ExtDat%FromList = .FALSE.

                ! This array is now filled
                IF ( PRESENT(Filled) ) Filled = .TRUE.

                ! Verbose
                IF ( HcoState%Config%doVerbose ) THEN
                   MSG = 'Set extension field pointer to external data: ' // TRIM(FldName)
                   CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
                ENDIF
             ENDIF

          ! Field not found and no target defined
          ELSEIF ( FailIfNotFilled ) THEN
             MSG = 'Cannot fill extension field ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
          ENDIF
       ENDIF ! FIRST

       ! Eventually copy field from HEMCO list to ExtState. We need to
       ! make a copy and cannot just set a pointer because ExtState fields
       ! are in HEMCO precision but the EmisList fields are in single
       ! precisions.
       IF ( ExtDat%FromList ) THEN
          IF ( FOUND ) THEN
             ! Copy values and mark array as filled
             ExtDat%Arr%Val(:,:) = Arr2D(:,:)
             IF ( PRESENT(Filled) ) Filled = .TRUE.
          ELSEIF ( FailIfNotFilled ) Then
             MSG = 'Cannot find extension field in HEMCO data list: ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
          ENDIF
       ENDIF ! FromList
    ENDIF

    ! Make sure array exists
    IF ( FailIfNotFilled .AND. .NOT. ASSOCIATED(ExtDat%Arr%Val) ) THEN
       MSG = 'ExtState array not filled: ' // TRIM(FldName)
       CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
    ENDIF

    ! Cleanup
    IF ( ALLOCATED(Arr2D) ) DEALLOCATE(Arr2D)

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Set_2R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Set_2S
!
! !DESCRIPTION: Subroutine ExtDat\_Set\_2S sets/updates the data array of an
! ExtDat object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Set_2S ( HcoState, ExtDat,           &
                             FldName,  RC,     First,    &
                             Trgt,     Filled, NotFillOk )
!
! !USES:
!
    USE HCO_ARR_MOD,        ONLY : HCO_ArrAssert
    USE HCO_STATE_MOD,      ONLY : HCO_State
    USE HCO_CALC_MOD,       ONLY : HCO_EvalFld
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER                         :: HcoState
    TYPE(ExtDat_2S),  POINTER                         :: ExtDat
    CHARACTER(LEN=*), INTENT(IN   )                   :: FldName
    INTEGER,          INTENT(INOUT)                   :: RC
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: First
    REAL(sp),         POINTER      , OPTIONAL         :: Trgt(:,:)
    LOGICAL,          INTENT(  OUT), OPTIONAL         :: Filled
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: NotFillOk
!
! !REVISION HISTORY:
!  03 Apr 2015 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                :: AS, NX, NY
    REAL(hp), ALLOCATABLE  :: Arr2D(:,:)
    CHARACTER(LEN=255)     :: MSG
    CHARACTER(LEN=255)     :: LOC = 'ExtDat_Set_2S (hcox_state_mod.F90)'
    LOGICAL                :: FRST
    LOGICAL                :: FOUND
    LOGICAL                :: FailIfNotFilled

    ! ================================================================
    ! ExtDat_Set_2S begins here
    ! ================================================================

    ! Init
    RC = HCO_SUCCESS
    IF ( PRESENT(Filled) ) Filled = .FALSE.

    ! Nothing to do if this ExtDat field is not in use
    IF ( .NOT. ExtDat%DoUse ) RETURN

    ! Check for fill requirement
    IF ( PRESENT(NotFillOk) ) THEN
       FailIfNotFilled = .NOT. NotFillOk
    ELSE
       FailIfNotFilled = .TRUE.
    ENDIF

    ! First time
    IF ( PRESENT(FIRST) ) THEN
       FRST = FIRST
    ELSE
       FRST = .FALSE.
    ENDIF

    ! On first call or if data is flagged as being read from list, get data
    ! from emissions list
    IF ( FRST .OR. ExtDat%FromList ) THEN

       ! Allocate temporary array
       ALLOCATE(Arr2D(HcoState%NX,HcoState%NY),STAT=AS)
       IF ( AS /= 0 ) THEN
          CALL HCO_ERROR ( "Arr2D allocation error", RC, THISLOC=LOC )
          RETURN
       ENDIF

       ! Try to get data from list
       CALL HCO_EvalFld( HcoState, TRIM(FldName), Arr2D, RC, FOUND=FOUND )
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 59', RC, THISLOC=LOC )
           RETURN
       ENDIF

       ! On first call, need to make additional checks
       IF ( FRST ) THEN

          ! If read from list
          IF ( FOUND ) THEN
             ExtDat%FromList = .TRUE.

             ! Make sure array is allocated
             CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, RC )
             IF ( RC /= HCO_SUCCESS ) THEN
                 CALL HCO_ERROR( 'ERROR 60', RC, THISLOC=LOC )
                 RETURN
             ENDIF

             ! Verbose
             IF ( HcoState%Config%doVerbose ) THEN
                MSG = 'Will fill extension field from HEMCO data list field ' // TRIM(FldName)
                CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
             ENDIF

          ! Target to data
          ELSEIF ( PRESENT(Trgt) ) THEN

             ! If target is not associated:
             IF ( .NOT. ASSOCIATED(Trgt) ) THEN
                IF ( FailIfNotFilled ) THEN
                   MSG = 'Cannot fill extension field ' // TRIM(FldName) // &
                         ' because target field is not associated.'
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

             ! If target is associated:
             ELSE

                ! Make sure dimensions agree
                NX = SIZE(Trgt,1)
                NY = SIZE(Trgt,2)

                ! Must cover the horizontal grid
                IF ( (NX/=HcoState%NX) .OR. (NY/=HcoState%NY) ) THEN
                   WRITE(MSG,*) 'Horizontal dimensions of target data do not ', &
                      'correspond to simulation grid: ', &
                      'Expected dimensions: ', HcoState%NX, HcoState%NY, &
                      '; encountered dimensions: ', NX, NY, '. Error occured ', &
                      'for field ', TRIM(FldName)
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

                ! Link data to target
                ExtDat%Arr%Val => Trgt

                ! Make sure it's not from list
                ExtDat%FromList = .FALSE.

                ! Mark as filled
                IF ( PRESENT(Filled) ) Filled = .TRUE.

                ! Verbose
                IF ( HcoState%Config%doVerbose ) THEN
                   MSG = 'Set extension field pointer to external data: ' // TRIM(FldName)
                   CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
                ENDIF
             ENDIF

          ! Field not found and no target defined
          ELSEIF ( FailIfNotFilled ) THEN
             MSG = 'Cannot fill extension field ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
          ENDIF
       ENDIF ! FIRST

       ! Eventually copy field from HEMCO list to ExtState. We need to
       ! make a copy and cannot just set a pointer because ExtState fields
       ! are in HEMCO precision but the EmisList fields are in single
       ! precisions.
       IF ( ExtDat%FromList ) THEN
          IF ( FOUND ) THEN
             ! Copy values and mark as filled
             ExtDat%Arr%Val(:,:) = Arr2D(:,:)
             IF ( PRESENT(Filled) ) Filled = .TRUE.
          ELSEIF ( FailIfNotFilled ) THEN
             MSG = 'Cannot find extension field in HEMCO data list: ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
          ENDIF
       ENDIF ! FromList
    ENDIF

    ! Make sure array exists
    IF ( FailIfNotFilled .AND. .NOT. ASSOCIATED(ExtDat%Arr%Val) ) THEN
       MSG = 'ExtState array not filled: ' // TRIM(FldName)
       CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
    ENDIF

    ! Cleanup
    IF ( ALLOCATED(Arr2D) ) DEALLOCATE(Arr2D)

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Set_2S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Set_2I
!
! !DESCRIPTION: Subroutine ExtDat\_Set\_2I sets/updates the data array of an
! ExtDat object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Set_2I ( HcoState, ExtDat,           &
                             FldName,  RC,     First,    &
                             Trgt,     Filled, NotFillOk )
!
! !USES:
!
    USE HCO_ARR_MOD,        ONLY : HCO_ArrAssert
    USE HCO_STATE_MOD,      ONLY : HCO_State
    USE HCO_CALC_MOD,       ONLY : HCO_EvalFld
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER                         :: HcoState
    TYPE(ExtDat_2I),  POINTER                         :: ExtDat
    CHARACTER(LEN=*), INTENT(IN   )                   :: FldName
    INTEGER,          INTENT(INOUT)                   :: RC
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: First
    INTEGER,          POINTER,       OPTIONAL         :: Trgt(:,:)
    LOGICAL,          INTENT(  OUT), OPTIONAL         :: Filled
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: NotFillOk
!
! !REVISION HISTORY:
!  03 Apr 2015 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                :: AS, NX, NY
    REAL(hp), ALLOCATABLE  :: Arr2D(:,:)
    CHARACTER(LEN=255)     :: MSG
    CHARACTER(LEN=255)     :: LOC = 'ExtDat_Set_2I (hcox_state_mod.F90)'
    LOGICAL                :: FRST
    LOGICAL                :: FOUND
    LOGICAL                :: FailIfNotFilled

    ! ================================================================
    ! ExtDat_Set_2I begins here
    ! ================================================================

    ! Init
    RC = HCO_SUCCESS
    IF ( PRESENT(Filled) ) Filled = .FALSE.

    ! Nothing to do if this ExtDat field is not in use
    IF ( .NOT. ExtDat%DoUse ) RETURN

    ! First time
    IF ( PRESENT(FIRST) ) THEN
       FRST = FIRST
    ELSE
       FRST = .FALSE.
    ENDIF

    ! Check for fill requirement
    IF ( PRESENT(NotFillOk) ) THEN
       FailIfNotFilled = .NOT. NotFillOk
    ELSE
       FailIfNotFilled = .TRUE.
    ENDIF

    ! On first call or if data is flagged as being read from list, get data
    ! from emissions list
    IF ( FRST .OR. ExtDat%FromList ) THEN

       ! Allocate temporary array
       ALLOCATE(Arr2D(HcoState%NX,HcoState%NY),STAT=AS)
       IF ( AS /= 0 ) THEN
          CALL HCO_ERROR ( "Arr2D allocation error", RC, THISLOC=LOC )
          RETURN
       ENDIF

       ! Try to get data from list
       CALL HCO_EvalFld( HcoState, TRIM(FldName), Arr2D, RC, FOUND=FOUND )
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 61', RC, THISLOC=LOC )
           RETURN
       ENDIF

       ! On first call, need to make additional checks
       IF ( FRST ) THEN

          ! If read from list
          IF ( FOUND ) THEN
             ExtDat%FromList = .TRUE.

             ! Make sure array is allocated
             CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, RC )
             IF ( RC /= HCO_SUCCESS ) THEN
                 CALL HCO_ERROR( 'ERROR 62', RC, THISLOC=LOC )
                 RETURN
             ENDIF

             ! Verbose
             IF ( HcoState%Config%doVerbose ) THEN
                MSG = 'Will fill extension field from HEMCO data list field ' // TRIM(FldName)
                CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
             ENDIF

          ! Target to data
          ELSEIF ( PRESENT(Trgt) ) THEN

             ! If target is not associated:
             IF ( .NOT. ASSOCIATED(Trgt) ) THEN
                IF ( FailIfNotFilled ) THEN
                   MSG = 'Cannot fill extension field ' // TRIM(FldName) // &
                         ' because target field is not associated.'
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

             ! If target is associated:
             ELSE

                ! Make sure dimensions agree
                NX = SIZE(Trgt,1)
                NY = SIZE(Trgt,2)

                ! Must cover the horizontal grid
                IF ( (NX /= HcoState%NX) .OR. (NY /= HcoState%NY) ) THEN
                   WRITE(MSG,*) 'Horizontal dimensions of target data do not ', &
                      'correspond to simulation grid: ', &
                      'Expected dimensions: ', HcoState%NX, HcoState%NY, &
                      '; encountered dimensions: ', NX, NY, '. Error occured ', &
                      'for field ', TRIM(FldName)
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

                ! Link data to target
                ExtDat%Arr%Val => Trgt

                ! Make sure it's not from list
                ExtDat%FromList = .FALSE.

                ! Mark as filled
                IF ( PRESENT(Filled) ) Filled = .TRUE.

                ! Verbose
                IF ( HcoState%Config%doVerbose ) THEN
                   MSG = 'Set extension field pointer to external data: ' // TRIM(FldName)
                   CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
                ENDIF
             ENDIF

          ! Not found in list and no target defined
          ELSEIF ( FailIfNotFilled ) THEN
             MSG = 'Cannot fill extension field ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
          ENDIF

       ENDIF ! FIRST

       ! Eventually copy field from HEMCO list to ExtState. We need to
       ! make a copy and cannot just set a pointer because ExtState fields
       ! are in HEMCO precision but the EmisList fields are in single
       ! precisions.
       IF ( ExtDat%FromList ) THEN
          IF ( FOUND ) THEN

             ! Copy values and mark as filled
             ExtDat%Arr%Val(:,:) = Arr2D(:,:)
             IF ( PRESENT(Filled) ) Filled = .TRUE.

          ELSEIF ( FailIfNotFilled ) THEN
             MSG = 'Cannot find extension field in HEMCO data list: ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
          ENDIF

       ENDIF !FromList
    ENDIF

    ! Make sure array exists
    IF ( FailIfNotFilled .AND. .NOT. ASSOCIATED(ExtDat%Arr%Val) ) THEN
       MSG = 'ExtState array not filled: ' // TRIM(FldName)
       CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
    ENDIF

    ! Cleanup
    IF ( ALLOCATED(Arr2D) ) DEALLOCATE(Arr2D)

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Set_2I
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Set_3R
!
! !DESCRIPTION: Subroutine ExtDat\_Set\_3R sets/updates the data array of an
! ExtDat object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Set_3R ( HcoState, ExtDat,   FldName,            &
                             RC,       First,    Trgt,   OnLevEdge,  &
                             Filled,   NotFillOk                     )
!
! !USES:
!
    USE HCO_ARR_MOD,        ONLY : HCO_ArrAssert
    USE HCO_STATE_MOD,      ONLY : HCO_State
    USE HCO_CALC_MOD,       ONLY : HCO_EvalFld
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER                         :: HcoState
    TYPE(ExtDat_3R),  POINTER                         :: ExtDat
    CHARACTER(LEN=*), INTENT(IN   )                   :: FldName
    INTEGER,          INTENT(INOUT)                   :: RC
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: First
    REAL(hp),         POINTER      , OPTIONAL         :: Trgt(:,:,:)
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: OnLevEdge
    LOGICAL,          INTENT(  OUT), OPTIONAL         :: Filled
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: NotFillOk
!
! !REVISION HISTORY:
!  03 Apr 2015 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                :: AS, NX, NY, NZ, NZ_EXPECTED
    INTEGER                :: L
    LOGICAL                :: FRST
    LOGICAL                :: FOUND
    LOGICAL                :: FailIfNotFilled
    REAL(hp), ALLOCATABLE  :: Arr3D(:,:,:)
    CHARACTER(LEN=255)     :: MSG
    CHARACTER(LEN=255)     :: LOC = 'ExtDat_Set_3R (hcox_state_mod.F90)'

    ! ================================================================
    ! ExtDat_Set_3R begins here
    ! ================================================================

    ! Init
    RC = HCO_SUCCESS
    IF ( PRESENT(Filled) ) Filled = .FALSE.

    ! Nothing to do if this ExtDat field is not in use
    IF ( .NOT. ExtDat%DoUse ) RETURN

    ! First time
    IF ( PRESENT(FIRST) ) THEN
       FRST = FIRST
    ELSE
       FRST = .FALSE.
    ENDIF

    ! Check for fill requirement
    IF ( PRESENT(NotFillOk) ) THEN
       FailIfNotFilled = .NOT. NotFillOk
    ELSE
       FailIfNotFilled = .TRUE.
    ENDIF

    ! Expected number of vertical levels: NZ if not on edge, NZ+1 if on edge
    NZ_EXPECTED = HcoState%NZ
    IF ( PRESENT(OnLevEdge) ) THEN
       IF ( OnLevEdge ) THEN
          NZ_EXPECTED = HcoState%NZ + 1
       ENDIF
    ENDIF

    ! On first call or if data is flagged as being read from list, get data
    ! from emissions list
    IF ( FRST .OR. ExtDat%FromList ) THEN

       ! Allocate temporary array
       ALLOCATE(Arr3D(HcoState%NX,HcoState%NY,NZ_EXPECTED),STAT=AS)
       IF ( AS /= 0 ) THEN
          CALL HCO_ERROR ( "Arr3D allocation error", RC, THISLOC=LOC )
          RETURN
       ENDIF

       ! Try to get data from list
       CALL HCO_EvalFld( HcoState, TRIM(FldName), Arr3D, RC, FOUND=FOUND )
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 63', RC, THISLOC=LOC )
           RETURN
       ENDIF

       ! On first call, need to make additional checks
       IF ( FRST ) THEN

          ! If read from list
          IF ( FOUND ) THEN
             ExtDat%FromList = .TRUE.

             ! Make sure array is allocated
             CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, NZ_EXPECTED, RC )
             IF ( RC /= HCO_SUCCESS ) THEN
                 CALL HCO_ERROR( 'ERROR 64', RC, THISLOC=LOC )
                 RETURN
             ENDIF

             ! Verbose
             IF ( HcoState%Config%doVerbose ) THEN
                MSG = 'Will fill extension field from HEMCO data list field ' // TRIM(FldName)
                CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
             ENDIF

          ! Target to data
          ELSEIF ( PRESENT(Trgt) ) THEN

             ! If target is not associated:
             IF ( .NOT. ASSOCIATED(Trgt) ) THEN
                IF ( FailIfNotFilled ) THEN
                   MSG = 'Cannot fill extension field ' // TRIM(FldName) // &
                         ' because target field is not associated.'
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

             ! If target is associated:
             ELSE

                ! Make sure dimensions agree
                NX = SIZE(Trgt,1)
                NY = SIZE(Trgt,2)
                NZ = SIZE(Trgt,3)

                ! Must cover the horizontal grid
                IF ( (NX/=HcoState%NX) .OR. (NY/=HcoState%NY) .OR. (NZ/=NZ_EXPECTED) ) THEN
                   WRITE(MSG,*) 'Dimensions of target data do not ', &
                      'correspond to simulation grid: ', &
                      'Expected dimensions: ', HcoState%NX, HcoState%NY, NZ_EXPECTED, &
                      '; encountered dimensions: ', NX, NY, NZ, '. Error occured ', &
                      'for field ', TRIM(FldName)
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

                ! Link data to target
                ExtDat%Arr%Val => Trgt

                ! Make sure it's not from list
                ExtDat%FromList = .FALSE.

                ! Mark as filled
                IF ( PRESENT(Filled) ) Filled = .TRUE.

                ! Verbose
                IF ( HcoState%Config%doVerbose ) THEN
                   MSG = 'Set extension field pointer to external data: ' // TRIM(FldName)
                   CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
                ENDIF
             ENDIF

          ! Not found in list and no target defined
          ELSEIF ( FailIfNotFilled ) THEN
             ! Target array must be present
             IF ( .NOT. PRESENT(Trgt) ) THEN
                MSG = 'Cannot fill extension field ' // TRIM(FldName)
                CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                RETURN
             ENDIF
          ENDIF

       ENDIF ! FIRST

       ! Eventually copy field from HEMCO list to ExtState. We need to
       ! make a copy and cannot just set a pointer because ExtState fields
       ! are in HEMCO precision but the EmisList fields are in single
       ! precisions.
       IF ( ExtDat%FromList ) THEN
          IF ( FOUND ) THEN

             ! Copy data and mark as filled
             ExtDat%Arr%Val(:,:,:) = Arr3D(:,:,:)
             IF ( PRESENT(Filled) ) Filled = .TRUE.

          ELSEIF ( FailIfNotFilled ) THEN
             MSG = 'Cannot find extension field in HEMCO data list: ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN

          ENDIF
       ENDIF !FromList
    ENDIF

    ! Make sure array exists
    IF ( FailIfNotFilled .AND. .NOT. ASSOCIATED(ExtDat%Arr%Val) ) THEN
       MSG = 'ExtState array not filled: ' // TRIM(FldName)
       CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
    ENDIF

    ! Cleanup
    IF ( ALLOCATED(Arr3D) ) DEALLOCATE(Arr3D)

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Set_3R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtDat_Set_3S
!
! !DESCRIPTION: Subroutine ExtDat\_Set\_3S sets/updates the data array of an
! ExtDat object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ExtDat_Set_3S ( HcoState, ExtDat,   FldName,            &
                             RC,       First,    Trgt,    OnLevEdge, &
                             Filled,   NotFillOk                     )
!
! !USES:
!
    USE HCO_ARR_MOD,        ONLY : HCO_ArrAssert
    USE HCO_STATE_MOD,      ONLY : HCO_State
    USE HCO_CALC_MOD,       ONLY : HCO_EvalFld
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER                         :: HcoState
    TYPE(ExtDat_3S),  POINTER                         :: ExtDat
    CHARACTER(LEN=*), INTENT(IN   )                   :: FldName
    INTEGER,          INTENT(INOUT)                   :: RC
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: First
    REAL(sp),         POINTER      , OPTIONAL         :: Trgt(:,:,:)
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: OnLevEdge
    LOGICAL,          INTENT(  OUT), OPTIONAL         :: Filled
    LOGICAL,          INTENT(IN   ), OPTIONAL         :: NotFillOk
!
! !REVISION HISTORY:
!  03 Apr 2015 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                :: AS, NX, NY, NZ, NZ_EXPECTED
    INTEGER                :: L
    LOGICAL                :: FRST
    LOGICAL                :: FOUND
    LOGICAL                :: FailIfNotFilled
    REAL(hp), ALLOCATABLE  :: Arr3D(:,:,:)
    CHARACTER(LEN=255)     :: MSG
    CHARACTER(LEN=255)     :: LOC = 'ExtDat_Set_3S (hcox_state_mod.F90)'

    ! ================================================================
    ! ExtDat_Set_3S begins here
    ! ================================================================

    ! Init
    RC = HCO_SUCCESS
    IF ( PRESENT(Filled) ) Filled = .FALSE.

    ! Nothing to do if this ExtDat field is not in use
    IF ( .NOT. ExtDat%DoUse ) RETURN

    ! First time
    IF ( PRESENT(FIRST) ) THEN
       FRST = FIRST
    ELSE
       FRST = .FALSE.
    ENDIF

    ! Check for fill requirement
    IF ( PRESENT(NotFillOk) ) THEN
       FailIfNotFilled = .NOT. NotFillOk
    ELSE
       FailIfNotFilled = .TRUE.
    ENDIF

    ! Expected number of vertical levels: NZ if not on edge, NZ+1 if on edge
    NZ_EXPECTED = HcoState%NZ
    IF ( PRESENT(OnLevEdge) ) THEN
       IF ( OnLevEdge ) THEN
          NZ_EXPECTED = HcoState%NZ + 1
       ENDIF
    ENDIF

    ! On first call or if data is flagged as being read from list, get data
    ! from emissions list
    IF ( FRST .OR. ExtDat%FromList ) THEN

       ! Allocate temporary array
       ALLOCATE(Arr3D(HcoState%NX,HcoState%NY,NZ_EXPECTED),STAT=AS)
       IF ( AS /= 0 ) THEN
          CALL HCO_ERROR ( "Arr3D allocation error", RC, THISLOC=LOC )
          RETURN
       ENDIF

       ! Try to get data from list
       CALL HCO_EvalFld( HcoState, TRIM(FldName), Arr3D, RC, FOUND=FOUND )
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 65', RC, THISLOC=LOC )
           RETURN
       ENDIF

       ! On first call, need to make additional checks
       IF ( FRST ) THEN

          ! If read from list
          IF ( FOUND ) THEN
             ExtDat%FromList = .TRUE.

             ! Make sure array is allocated
             CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, NZ_EXPECTED, RC )
             IF ( RC /= HCO_SUCCESS ) THEN
                 CALL HCO_ERROR( 'ERROR 66', RC, THISLOC=LOC )
                 RETURN
             ENDIF

             ! Verbose
             IF ( HcoState%Config%doVerbose ) THEN
                MSG = 'Will fill extension field from HEMCO data list field ' // TRIM(FldName)
                CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
             ENDIF

          ! Target to data
          ELSEIF ( PRESENT(Trgt) ) THEN

             ! If target is not associated:
             IF ( .NOT. ASSOCIATED(Trgt) ) THEN
                IF ( FailIfNotFilled ) THEN
                   MSG = 'Cannot fill extension field ' // TRIM(FldName) // &
                         ' because target field is not associated.'
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

             ! If target is associated:
             ELSE

                ! Make sure dimensions agree
                NX = SIZE(Trgt,1)
                NY = SIZE(Trgt,2)
                NZ = SIZE(Trgt,3)

                ! Must cover the horizontal grid
                IF ( (NX/=HcoState%NX) .OR. (NY/=HcoState%NY) .OR. (NZ/=NZ_EXPECTED) ) THEN
                   WRITE(MSG,*) 'Dimensions of target data do not ', &
                      'correspond to simulation grid: ', &
                      'Expected dimensions: ', HcoState%NX, HcoState%NY, NZ_EXPECTED, &
                      '; encountered dimensions: ', NX, NY, NZ, '. Error occured ', &
                      'for field ', TRIM(FldName)
                   CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                   RETURN
                ENDIF

                ! Link data to target
                ExtDat%Arr%Val => Trgt

                ! Make sure it's not from list
                ExtDat%FromList = .FALSE.

                ! Mark as filled
                IF ( PRESENT(Filled) ) Filled = .TRUE.

                ! Verbose
                IF ( HcoState%Config%doVerbose ) THEN
                   MSG = 'Set extension field pointer to external data: ' // TRIM(FldName)
                   CALL HCO_MSG( msg, LUN=HcoState%Config%hcoLogLUN )
                ENDIF
             ENDIF

          ! Not found in list and no target defined
          ELSEIF ( FailIfNotFilled ) THEN
             ! Target array must be present
             IF ( .NOT. PRESENT(Trgt) ) THEN
                MSG = 'Cannot fill extension field ' // TRIM(FldName)
                CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
                RETURN
             ENDIF
          ENDIF

       ENDIF ! FIRST

       ! Eventually copy field from HEMCO list to ExtState. We need to
       ! make a copy and cannot just set a pointer because ExtState fields
       ! are in HEMCO precision but the EmisList fields are in single
       ! precisions.
       IF ( ExtDat%FromList ) THEN
          IF ( FOUND ) THEN
             ! Copy data and mark as filled
             ExtDat%Arr%Val(:,:,:) = Arr3D(:,:,:)
             IF ( PRESENT(Filled) ) Filled = .TRUE.
          ELSEIF ( FailIfNotFilled ) THEN
             MSG = 'Cannot find extension field in HEMCO data list: ' // TRIM(FldName)
             CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
             RETURN
          ENDIF
       ENDIF !FromList
    ENDIF

    ! Make sure array exists
    IF ( FailIfNotFilled .AND. .NOT. ASSOCIATED(ExtDat%Arr%Val) ) THEN
       MSG = 'ExtState array not filled: ' // TRIM(FldName)
       CALL HCO_ERROR(MSG, RC, THISLOC=LOC )
    ENDIF

    ! Cleanup
    IF ( ALLOCATED(Arr3D) ) DEALLOCATE(Arr3D)

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ExtDat_Set_3S
!EOC
END MODULE HCOX_STATE_MOD
