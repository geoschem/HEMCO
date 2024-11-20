!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_tomas_jeagle_mod.F90
!
! !DESCRIPTION: Module HCOX\_TOMAS\_JEAGLE\_Mod contains routines to
!  calculate sea salt aerosol emissions for the TOMAS aerosol microphysics
!  package. JKODROS - This is an update of hcox\_tomas\_seasalt\_mod.F90 to
!  use Jeagle emissions. Should bring TOMAS emissions in line with bulk sea
!  salt.
!\\
!\\
!  This is a HEMCO extension module that uses many of the HEMCO core
!  utilities.
!\\
!\\
!  References:
!  \begin{itemize}
!  \item Clarke, A.D., Owens, S., Zhou, J. \emph{An ultrafine sea-salt flux
!        from breaking waves: Implications for CCN in the remote marine
!        atmosphere}, \underline{J. Geophys. Res.}, 2006.
!  \end{itemize}
!
! !INTERFACE:
!
MODULE HCOX_TOMAS_Jeagle_Mod
#if defined( TOMAS )
!
! !USES:
!
  USE HCO_Error_Mod
  USE HCO_Diagn_Mod
  USE HCO_State_Mod,  ONLY : HCO_State
  USE HCOX_State_Mod, ONLY : Ext_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCOX_TOMAS_Jeagle_Init
  PUBLIC :: HCOX_TOMAS_Jeagle_Run
  PUBLIC :: HCOX_TOMAS_Jeagle_Final
!
! !REVISION HISTORY:
!  01 Oct 2014 - R. Yantosca - Initial version, based on TOMAS code
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!
! !PRIVATE TYPES:
!
  TYPE :: MyInst

   ! Scalars
   INTEGER               :: Instance
   INTEGER               :: ExtNr                  ! HEMCO extension #
   REAL(dp)              :: TOMAS_COEF             ! Seasalt emiss coeff.

  !Tracer IDs
   LOGICAL             :: EmitSnowSS        ! Calculate sea salt emission blowing snow


   ! Arrays
   INTEGER,  ALLOCATABLE :: HcoIDs    (:      )    ! HEMCO species ID's
   REAL(dp), POINTER     :: TOMAS_DBIN(:      )    ! TOMAS bin width
   REAL(dp), POINTER     :: DRFAC     (:      )    ! TOMAS area?
   REAL(dp), POINTER     :: TC1       (:,:,:,:)    ! Aerosol number
   REAL(dp), POINTER     :: TC2       (:,:,:,:)    ! Aerosol mass
   LOGICAL               :: ColdSST                ! Flag to correct SSA emissions over cold waters

   ! Scale factors
   REAL*8              :: NSLNT_FYI         ! North Hemisphere snow salinity on first year ice (FYI) (psu)
   REAL*8              :: NSLNT_MYI         ! North Hemisphere snow salinity on multiyear ice (MYI) (psu)
   REAL*8              :: SSLNT_FYI         ! South Hemisphere snow salinity on FYI (psu)
   REAL*8              :: SSLNT_MYI         ! South Hemisphere snow salinity on MYI (psu)
   REAL*8              :: NAGE              ! North Hemisphere snow age (days)
   REAL*8              :: SAGE              ! South Hemisphere snow age (days)
   REAL*8              :: NP                ! number of particle per snowflake

   !Module variables
   REAL*8,  POINTER     :: SS_DEN(:)         ! densities
   REAL*8,  POINTER     :: F_DI_N_FYI(:,:)   ! add for blowing snow for NH
   REAL*8,  POINTER     :: F_DI_N_MYI(:,:)   ! add for blowing snow for NH
   REAL*8,  POINTER     :: F_DI_S_FYI(:,:)   ! add for blowing snow for SH
   REAL*8,  POINTER     :: F_DI_S_MYI(:,:)   ! add for blowing snow for SH
   REAL*8,  POINTER     :: F_DN_N_FYI(:,:)   ! add for blowing snow for NH
   REAL*8,  POINTER     :: F_DN_N_MYI(:,:)   ! add for blowing snow for NH
   REAL*8,  POINTER     :: F_DN_S_FYI(:,:)   ! add for blowing snow for SH
   REAL*8,  POINTER     :: F_DN_S_MYI(:,:)   ! add for blowing snow for SH

   !Number densities
   REAL(sp), POINTER   :: MULTIICE(:,:)   => NULL() ! add for blowing snow

   TYPE(MyInst), POINTER :: NextInst => NULL()
  END TYPE MyInst

  ! Pointer to instances
  TYPE(MyInst), POINTER  :: AllInst => NULL()

   !INTEGER, PARAMETER  :: NR_MAX = 200  ! max. # of bins per mode
   INTEGER, PARAMETER  :: NR_MAX = 4000  ! max. # of bins per mode

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_TOMAS_Jeagle_Run
!
! !DESCRIPTION: Subroutine HCOX\_TOMAS\_Jeagle\_Run emits sea-salt into the
!  TOMAS sectional sea-salt mass and aerosol number arrays.  Sea-salt emission
!  parameterization of Jeagle et al. (2011).
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_TOMAS_Jeagle_Run( ExtState, HcoState, RC )
!
! !USES:
!
    USE HCO_GeoTools_Mod, ONLY : HCO_LandType
    USE HCO_FluxArr_mod,  ONLY : HCO_EmisAdd
    USE HCO_State_Mod,    ONLY : HCO_GetHcoID
    USE HCO_Calc_Mod,     ONLY : HCO_EvalFld
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State),  POINTER       :: ExtState    ! Extension Options object
    TYPE(HCO_State),  POINTER       :: HcoState    ! HEMCO state object
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT) :: RC          ! Success or failure?
!
! !REMARKS:
! References
!  ============================================================================
!  (1 ) Chin, M., P. Ginoux, S. Kinne, B. Holben, B. Duncan, R. Martin,
!        J. Logan, A. Higurashi, and T. Nakajima, "Tropospheric aerosol
!        optical thickness from the GOCART model and comparisons with
!        satellite and sunphotometers measurements", J. Atmos Sci., 2001.
!  (2 ) Gong, S., L. Barrie, and J.-P. Blanchet, "Modeling sea-salt
!        aerosols in the atmosphere. 1. Model development", J. Geophys. Res.,
!        v. 102, 3805-3818, 1997.
!  (3 ) Gong, S. L., "A parameterization of sea-salt aerosol source function
!        for sub- and super-micron particles", Global Biogeochem.  Cy., 17(4),
!        1097, doi:10.1029/2003GB002079, 2003.
!  (4 ) Jaegle, L., P.K. Quinn, T.S. Bates, B. Alexander, J.-T. Lin, "Global
!        distribution of sea salt aerosols: New constraints from in situ and
!        remote sensing observations", Atmos. Chem. Phys., 11, 3137-3157,
!        doi:10.5194/acp-11-3137-2011.
!  (5 ) Huang, J., Jaeglé, L., "Wintertime enhancements of sea salt aerosol in
!        polar regions consistent with a sea ice source from blowing snow."
!        Atmos. Chem. Phys. 17, 3699–3712. https://doi.org/10.5194/acp-17-3699-2017, 2017.
!  (6 ) Huang, J., Jaeglé, L., Chen, Q., Alexander, B., Sherwen, T.,
!        Evans, M. J., Theys, N., and Choi, S. "Evaluating the impact of
!        blowing snow sea salt aerosol on springtime BrO and O3 in the Arctic,
!        Atmos. Chem. Phys. Discuss., https://doi.org/10.5194/acp-2019-1094, 2020.
!  (7 ) Tschudi, M., W. N. Meier, J. S. Stewart, C. Fowler, and J. Maslanik.
!        "EASE-Grid Sea Ice Age, Version 4." NASA National Snow and Ice Data Center
!        Distributed Active Archive Center. doi: https://doi.org/10.5067/UTAV7490FEPB., 2019.
!
! !REVISION HISTORY:
!  01 Oct 2014 - R. Yantosca - Initial version, based on TOMAS SRCSALT30 code
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER           :: I,      J,    L,      K, HcoID
    REAL(sp)          :: FOCEAN, W10M, DTEMIS
    REAL(dp)          :: F100,   W, A_M2, FEMIS, NUMBER, MASS, NUMBER_TOT
    REAL(dp)          :: NUMBER2
    REAL(dp)          :: rwet, dfo, B, A, SST, SCALE
    CHARACTER(LEN=255):: MSG, LOC

    ! New variables for blowing snow (huang, 04/09/20)
    REAL*8                 :: SNOWSALT
    REAL*8                 :: FROPEN, FRFIRST
    REAL*8                 :: FRICTVEL, WVMR, TEMP
    REAL*8                 :: PRESS, P_ICE, RH_ICE
    REAL*8                 :: D, FK, FD
    REAL*8                 :: PSI, QSPRIME, UT, APRIM
    REAL*8                 :: QS, QSNOWICE_FYI, QSNOWICE_MYI,QBSALT, QB0
    REAL*8                 :: SLNT, SLNT_FYI, SLNT_MYI
    REAL*8                 :: AGE, ISFROST

    ! New parameters for blowing snow (huang, 04/09/20)
    REAL*8, PARAMETER      :: LS = 2839d3    ! Latent heat of sublimation @ T=-30C (J/kg).
                                             ! Varies very little with Temperature
    REAL*8, PARAMETER      :: RV = 461.5d0   !J kg-1 K-1
    REAL*8, PARAMETER      :: RHONACL = 2160.0d0    !kg/m3
    REAL*8, PARAMETER      :: RHOICE  = 900.0d0     !kg/m3
    REAL*8, PARAMETER      :: K0  = 2.16d-2          !J m-1 s-1 K-1
    REAL*8, PARAMETER      :: A0 = 3.78407d-1
    REAL*8, PARAMETER      :: A1 = -8.64089d-2
    REAL*8, PARAMETER      :: A2 = -1.60570d-2
    REAL*8, PARAMETER      :: A3 = 7.25516d-4
    REAL*8, PARAMETER      :: A4 = -1.25650d-1
    REAL*8, PARAMETER      :: A5 = 2.48430d-2
    REAL*8, PARAMETER      :: A6 = -9.56871d-4
    REAL*8, PARAMETER      :: A7 = 1.24600d-2
    REAL*8, PARAMETER      :: A8 = 1.56862d-3
    REAL*8, PARAMETER      :: A9 = -2.93002d-4
    REAL*8, PARAMETER      :: A_SALT = 2.0d0  !from Mann et al. 2000
    REAL*8, PARAMETER      :: B_SALT = 37.5d0 !in um
    !REAL*8, PARAMETER      :: DDSNOW = 2.0d0  !in um for snow particle interval
    REAL*8, PARAMETER      :: DDSNOW = 0.1d0  !in um for snow particle interval
    LOGICAL, SAVE          :: FIRST = .TRUE.
    LOGICAL, SAVE          :: FIRSTSAL = .TRUE.
    CHARACTER(LEN=31)      :: FLDNME
    INTEGER                :: NDAYS!, cYYYY, cMM, cDD, K
    REAL(hp), TARGET       :: MULTI(HcoState%NX,HcoState%NY)
    REAL(hp), TARGET       :: SNOWSALA  (HcoState%NX,HcoState%NY)
    REAL(hp), TARGET       :: SNOWSALC  (HcoState%NX,HcoState%NY)

    REAL*8, PARAMETER :: BETHA=2.22d0   !wet diameter (80% Rel Hum) to dry diam

    ! Strings
    CHARACTER(LEN=31) :: SpcName

    ! Pointers
    TYPE(MyInst), POINTER :: Inst
    REAL(dp), POINTER :: ptr3D(:,:,:)

    ! For debugging
    INTEGER            :: ii=50, jj=10

    !=================================================================
    ! SRCSALT30 begins here!
    !=================================================================
    LOC = 'SRCSALT30 (HCOX_TOMAS_JEAGLE_MOD.F90)'

    ! Return if extension disabled
    IF ( ExtState%TOMAS_Jeagle <= 0 ) RETURN

    ! Enter
    CALL HCO_ENTER ( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Get instance
    Inst   => NULL()
    CALL InstGet ( ExtState%TOMAS_Jeagle, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       WRITE(MSG,*) 'Cannot find TOMAS_Jeagle instance Nr. ', ExtState%TOMAS_Jeagle
       CALL HCO_ERROR(MSG, RC )
       RETURN
    ENDIF

    !INIT VALUES
    Inst%TC1 = 0.0_hp
    Inst%TC2 = 0.0_hp

    IF ( Inst%EmitSnowSS ) THEN
      ! Read in distribution of multi-year sea ice from
      ! remotely sensed observations of sea ice motion and sea
      ! ice extent for the Arctic (Tschudi et al., 2019). For the
      ! Antarctic, the multi year sea ice extent is based on the minimum
      ! MERRA-2 sea ice extent of the previous summer.

      CALL HCO_EvalFld ( HcoState, 'MULTISEAICE', MULTI, RC )
      IF ( RC /= HCO_SUCCESS ) THEN
          WRITE(MSG,*) 'Cannot find MULTISEAICE data for blowing snow'
          CALL HCO_ERROR(MSG, RC )
          RETURN
      ENDIF
    ENDIF

    ! Depending on the grid resolution. 4x5 (default) doesn't need
    ! adjusting coeff

    !### Debug
    !print*, 'IN HCOX_TOMAS_Jeagle_Mod.F90'

    ! Init
    ptr3D => NULL()

    ! Emission timestep [s]
    DTEMIS = HcoState%TS_EMIS

    ! Loop over grid cells
    DO J = 1, HcoState%NY
    DO I = 1, HcoState%NX

       Inst%TC1(I,J,:,:) = 0d0
       Inst%TC2(I,J,:,:) = 0d0

       ! Grid box surface area [m2]
       A_M2  = HcoState%Grid%AREA_M2%Val(I,J)

       ! Advance to next grid box if it's not over water or sea ice
       IF ( ExtState%FROCEAN%Arr%Val(I,J)<=0d0 .and. &
            ExtState%FRSEAICE%Arr%Val(I,J)<=0d0 ) CYCLE

       ! Wind speed at 10 m altitude [m/s]
       W10M = SQRT( ExtState%U10M%Arr%Val(I,J)**2  &
            +       ExtState%V10M%Arr%Val(I,J)**2 )

       ! Sea surface temperature in Celcius
       SST = ExtState%TSKIN%Arr%Val(I,J) - 273.15d0

       ! Limit SST to 0-30C range
       SST = MAX( SST , 0d0 )  ! limit to  0C
       SST = MIN( SST , 30d0 ) ! limit to 30C

       ! Empirical SST scaling factor (jaegle 5/11/11)
       SCALE = 0.329d0 + 0.0904d0*SST -  &
               0.00717d0*SST**2d0 + 0.000207d0*SST**3d0

       ! Limit the SST scaling factor to 0.25 over cold SST (below 5C)
       IF ( Inst%ColdSST .and. SST<= 5.0d0 ) SCALE = 0.25d0

       ! Apply to only the open ocean fraction of the gridbox (Huang 06/12/20)
       FROPEN = ExtState%FROCEAN%Arr%Val(I,J)-ExtState%FRSEAICE%Arr%Val(I,J)
       IF ( FROPEN < 0d0 ) FROPEN = 0d0

       ! Eventually apply wind scaling factor.
       !SCALE = SCALE * Inst%WindScale * FROPEN
       SCALE = SCALE * FROPEN

       !----------------------------------------------------------------
       ! huang, 04/09/20: Add blowing snow emissions over sea ice
       !----------------------------------------------------------------

       IF ( Inst%EmitSnowSS ) THEN
         IF ( ExtState%FRSEAICE%Arr%Val(I,J) > 0d0 )THEN
          ! Friction velocity [m/s]
          FRICTVEL = ExtState%USTAR%Arr%Val(I,J)
          ! Convert specific humidity [g H2O/kg air] to water vapor mixing ratio [v/v]
          ! QV2m is in kg H2O/kg air
          WVMR = ExtState%QV2M%Arr%Val(I,J) * 28.973d0 / 18.0d0
          ! Temperature at 2M in grid box (I,J) [K]
          TEMP = ExtState%T2M%Arr%Val(I,J)
          ! Surface pressure at grid box (I,J). Convert from [Pa] to [hPa]
          PRESS = HcoState%Grid%PSFC%Val( I, J ) /100d0
          ! Calculate saturation vapor pressure over ice [in Pa] at temperature
          ! TS [K]
          P_ICE = 10d0**(-2663.5d0/TEMP+12.537d0)
          ! Calculate relative humidity with respect to ice [%]
          RH_ICE = PRESS * WVMR / (P_ICE*0.01d0) *100.0d0
          ! Limit RH to 100%
          IF (RH_ICE > 100d0) RH_ICE =100.0d0
          ! Coefficient of Diffusion of water vapor in air [m2/s]
          ! Parameterization of Massman, W.J. "A review of teh molecular diffusivities of
          ! H2O, CO2, CH4... in air, O2 and N2 near STP" Atmos. Env., 32, 6, 1111-1127, 1998.
          D = 2.178d-5*(1000d0/PRESS)*(TEMP/273.15d0)**1.81
          ! Heat conductivity and vapor diffusion terms [m s/kg]
          ! Rogers and Yau "A short course in cloud physics", 1989, Eqn 9.4, with
          !   RV =   461.5     [J/kg/K] Individual gas constant for water vapor
          !   LS =  2839.0*1d3 [J/kg  ] Latent heat of sublimation @ T=-30C
          !   K  =  2.16d-2    [J/(m s K)] Coeff of thermal conductivity of Air [Table 7.1 Rogers and Yau]
          FK = ( LS / (RV * TEMP ) -1d0 ) * LS / (K0 * TEMP)
          FD = ( RV * TEMP ) / (D * P_ICE)
          ! Variable PSI [m2/s] Equation 11 from Dery and Yau (2001)
          !  RHOICE = 900 kg/m3 Density of ice
          PSI = (RH_ICE/100.d0 - 1d0)/(2d0 * RHOICE * (FK + FD))
          ! Convert PSI from m2/s to units of -1x10d-12 m2/s
          PSI = PSI * (-1.0d12)
          ! Qs prime [mm/day snow water equivalent] Equation 11 Dery and Yau (2001)
          QSPRIME = A0 + A1*PSI + A2*PSI**2d0 + A3*PSI**3d0 &
                     + A4* W10M    + A5*PSI*W10M &
                     + A6*W10M*PSI**2d0 + A7*W10M**2d0 &
                     +  A8*PSI*W10M**2d0 + A9*W10M**3d0
          IF ( QSPRIME < 0.0d0 ) QSPRIME = 0.0d0
          !APRIM
          IF ( HcoState%Grid%YEDGE%Val(I,J) .lt. 0 ) AGE = Inst%SAGE*24.0d0
          IF ( HcoState%Grid%YEDGE%Val(I,J) .ge. 0 ) AGE = Inst%NAGE*24.0d0
          APRIM = (1.038d0+0.03758d0*AGE-0.00014349d0*AGE**2d0 &
                 + (1.911315d-7*AGE**3d0) )**(-1d0)
          ! Threshold wind speed [m/s]
          UT = 6.975d0 +  0.0033d0 * (TEMP - 273.15d0 + 27.27d0 )**2.0d0
               !IF (W10M > UT) THEN
               ! add RH<100 too

          IF (W10M > UT .and. RH_ICE<100d0) THEN
            QBSALT = 0.385d0*(1.0d0-Ut/W10M)**2.59d0/FRICTVEL
            QB0 = 0.385d0*(1d0-6.975d0/W10M)**2.59d0/FRICTVEL
            ! Snow sublimation rate [kg/m2/s] Equation 1 in Yang et al. (2008)
            ! The constant 1.1574d-5 converts mm/day column integrated sublimation rate to kg m-2 s-1

            QS = 1.1574d-5*APRIM*QSPRIME*QBSALT/QB0
          ELSE
            QS = 0d0
          ENDIF
          !set up the snow salinity
          IF ( HcoState%Grid%YEDGE%Val(I,J) .lt. 0 ) SLNT_FYI = Inst%SSLNT_FYI
          IF ( HcoState%Grid%YEDGE%Val(I,J) .lt. 0 ) SLNT_MYI = Inst%SSLNT_MYI
          IF ( HcoState%Grid%YEDGE%Val(I,J) .ge. 0 ) SLNT_FYI = Inst%NSLNT_FYI
          IF ( HcoState%Grid%YEDGE%Val(I,J) .ge. 0 ) SLNT_MYI = Inst%NSLNT_MYI
          ! Sea ice fraction that is first year
          FRFIRST = ExtState%FRSEAICE%Arr%Val(I,J) - MULTI(I,J)
          IF ( FRFIRST < 0d0 ) FRFIRST = 0d0
          ! Apply FYI salinity to FYI seaice fraction and MYI salinity to MYI fraction
          !SLNT =  SLNT_FYI * FRFIRST + SLNT_MYI * MULTI(I,J)
          ! Assume MYI salinity is 50% of FYI
          !SLNT =  SLNT * FRFIRST  + SLNT * 0.5 * MULTI(I,J)
          ! Convert snow sublimation rate to sea salt production rate [kg/m2/s]
          ! Calculate it separately for FYI and MYI, scaled by their respective sea ice fraction
          QSNOWICE_FYI = QS * SLNT_FYI * FRFIRST / 1000d0
          QSNOWICE_MYI = QS * SLNT_MYI * MULTI(I,J) / 1000d0
          !print *, 'Bettyhere is QSNOW ',QSNOWICE_FYI,QSNOWICE_MYI, I,J
         ELSE
          QSNOWICE_FYI = 0.0d0
         QSNOWICE_MYI = 0.0d0
         ENDIF
       ENDIF
       ! End of added blowing snow section

       !-----------------------------------------------------------------

          !---------------------------------------------------------------
          ! Partition TOMAS_Jeagle emissions w/in the boundary layer
          !---------------------------------------------------------------
          DO K = 1, HcoState%MicroPhys%nBins
             rwet=Inst%TOMAS_DBIN(k)*1.0E6*BETHA/2. ! convert from dry diameter [m] to wet (80% RH) radius [um]
         ! jkodros - testing out BETHA 7/29/15
             if (rwet > 0.d0) then
                  A=4.7*(1.+30.*rwet)**(-0.017*rwet**(-1.44))
                  B=(0.433-log10(rwet))/0.433

                  dfo=1.373*W10M**3.41*rwet**(-1.*A)  & !m-2 um-1 s-1
                    *(1.+0.057*rwet**3.45)*10.**(1.607*exp(-1.*B**2))

             else

                  dfo=0.d0
             endif

             dfo=dfo*Inst%DRFAC(k)*BETHA  !hemco units???? jkodros (?m-2 s-1)

             !dfo=dfo*focean*SCALE  ! scale now includes ocean fraction - remove this line
             dfo=dfo*SCALE  ! note: scale now includes ocean fraction

             ! Loop thru the boundary layer
             DO L = 1, HcoState%Nz

                ! Fraction of the PBL spanned by box (I,J,L) [unitless]
                FEMIS = ExtState%FRAC_OF_PBL%Arr%Val(I,J,L)

                ! Only handle grid boxes w/in the boundary layer
                IF ( FEMIS > 0d0 ) THEN

                   ! Number
                 !betty  NUMBER = dfo * FEMIS  ! need to move after add blowing snow

         ! update seasalt from blowing snow - huang 1/4/18
          IF (( Inst%EmitSnowSS )) THEN
             IF ( HcoState%Grid%YEDGE%Val(I,J) .lt. 0 ) THEN
                ! Southern Hemisphere

                   !if (K > 3 ) THEN
                   NUMBER = FEMIS* (dfo  + &
                      (( QSNOWICE_FYI * SUM( Inst%F_DN_S_FYI(:,K) ) + &
                         QSNOWICE_MYI * SUM( Inst%F_DN_S_MYI(:,K) ) ) * DDSNOW))

                   !NUMBER = FEMIS* (dfo)  + FEMIS* ( &
                   !   (( QSNOWICE_FYI * SUM( Inst%F_DI_S_FYI(:,K) ) + &
                   !      QSNOWICE_MYI * SUM( Inst%F_DI_S_MYI(:,K) ) ) * DDSNOW)) &
                   !     / (Inst%SS_DEN( K ) * ((Inst%TOMAS_DBIN(k))**3.d0) &
                   !                 * HcoState%Phys%PI/6.0d0)
                   !else
                   !   NUMBER = FEMIS * dfo
                   !endif
                !IF (NUMBER2 .GT. 5.e5) THEN
                !   print *, 'First', NUMBER2,NUMBER, I,J,K,L,FEMIS
                !ENDIF

                !IF (NUMBER .GT. 5.e5) THEN
                !!print*,'Betty here are emissions',NUMBER,dfo*FEMIS,K,I,J,L, &
                !     Inst%F_DN_S_FYI(:,K), DDSNOW, SUM( Inst%F_DN_S_FYI(:,K) ), &
                !      QSNOWICE_FYI, QSNOWICE_MYI, FEMIS
                !ENDIF
             ELSE
               ! Northern Hemisphere
                   NUMBER = FEMIS* (dfo  + &
                     (( QSNOWICE_FYI * SUM( Inst%F_DN_N_FYI(:,K) ) + &
                        QSNOWICE_MYI * SUM( Inst%F_DN_N_MYI(:,K) ) ) * DDSNOW))
             ENDIF
          ELSE  ! only open ocean sea salt
                  NUMBER  = dfo * FEMIS
          ENDIF

                   ! Mass
                   MASS   = NUMBER                                      &
                          * SQRT( HcoState%MicroPhys%BinBound(K  ) *    &
                                  HcoState%MicroPhys%BinBound(K+1)   )

                   ! Store number & mass
                   Inst%TC1(I,J,L,K) = NUMBER
                   Inst%TC2(I,J,L,K) = MASS

                ENDIF
             ENDDO
          ENDDO
    ENDDO
    ENDDO

    !### Debug
    !print*, 'Aerosol mass AT 50, 10, 7: ', Inst%TC2(ii,jj,1,7)
    !print*, 'BINS: ', HcoState%MicroPhys%nBins

    ! Loop over # of microphysics bins
    DO K = 1, HcoState%MicroPhys%nBins

       ! Add mass to the HEMCO data structure (jkodros)
       CALL HCO_EmisAdd( HcoState, Inst%TC2(:,:,:,K), Inst%HcoIDs(K), RC)
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXSALT', RC )
          RETURN
       ENDIF

       ! Get the proper species name
       IF ( K<10  ) THEN
         WRITE(SpcName,'(A3,I1)') 'NK0', K
       ELSE
         WRITE(SpcName,'(A2,I2)') 'NK', K
       ENDIF

       ! HEMCO species ID
       HcoID = HCO_GetHcoID( TRIM(SpcName), HcoState )

       !### Debug - comment out print over random oceen box - update indices if needed bc, 18/12/23
       !!print*, 'Aerosol number AT 1, 1,: ', Inst%TC1(ii,jj,1,k)
       !!print*, 'HCO ID: ', K, SpcName, HcoID

       ! Add number to the HEMCO data structure
       CALL HCO_EmisAdd( HcoState, Inst%TC1(:,:,:,K), HcoID, RC)
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'HCO_EmisAdd error: FLUXSALT', RC )
          RETURN
       ENDIF
    ENDDO

    !=======================================================================
    ! Cleanup & quit
    !=======================================================================

    ! Nullify pointers
    Inst    => NULL()

    ! Leave w/ success
    CALL HCO_LEAVE ( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_TOMAS_Jeagle_Run
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_TOMAS_Jeagle_Init
!
! !DESCRIPTION: Subroutine HcoX\_TOMAS\_Jeagle\_Init initializes all
!  extension variables.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_TOMAS_Jeagle_Init( HcoState, ExtName, ExtState, RC )
!
! !USES:
!
    USE HCO_State_Mod,   ONLY : HCO_GetHcoID
    USE HCO_STATE_MOD,   ONLY : HCO_GetExtHcoID
    USE HCO_ExtList_Mod, ONLY : GetExtNr
    USE HCO_ExtList_Mod, ONLY : GetExtOpt
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState    ! HEMCO state object
    CHARACTER(LEN=*), INTENT(IN   )  :: ExtName     ! Extension name
    TYPE(Ext_State),  POINTER        :: ExtState    ! Extension options object
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC          ! Success or failure?
!
! !REVISION HISTORY:
!  15 Dec 2013 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER                        :: N, R, AS, ExtNr
    REAL*8                         :: A, B, R0, R1
    REAL*8                         :: CONST_N
    INTEGER                        :: nSpc, minLen
    CHARACTER(LEN=255)             :: MSG, LOC

    ! Arrays
!    INTEGER,           ALLOCATABLE :: HcoIDs(:)
    CHARACTER(LEN=31), ALLOCATABLE :: SpcNames(:)

    ! Pointers
    TYPE(MyInst), POINTER          :: Inst

    ! Local variables for blowing snow
    INTEGER                :: K, ND, IH !IH for different hemisphere
    REAL*8                 :: D_SNOW, D_DRY
    REAL*8, PARAMETER      :: A_SALT = 2.0d0  !from Mann et al. 2000
    REAL*8, PARAMETER      :: B_SALT = 37.5d0 !in um
    !REAL*8, PARAMETER      :: DDSNOW = 2.0d0  !in um for snow particle interval
    REAL*8, PARAMETER      :: DDSNOW = 0.1d0  !in um for snow particle interval
    REAL*8, PARAMETER      :: RHONACL = 2160.0d0    !kg/m3
    REAL*8, PARAMETER      :: RHOICE  = 900.0d0     !kg/m3

    !=================================================================
    ! HCOX_TOMAS_Jeagle_Init begins here!
    !=================================================================
    LOC = 'HCOX_TOMAS_Jeagle_Init (HCOX_TOMAS_JEAGLE_MOD.F90)'

    ! Extension Nr.
    ExtNr = GetExtNr( HcoState%Config%ExtList, TRIM(ExtName) )
    IF ( ExtNr <= 0 ) RETURN

    ! Enter
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 1', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Only print on the root core
    IF ( HcoState%amIRoot ) THEN

       ! Write the name of the extension regardless of the verbose setting
       msg = &
        'Using HEMCO extension: TOMAS_Jeagle (sea salt emissions for TOMAS)'
       IF ( HCO_IsVerb( HcoState%Config%Err ) ) THEN
          CALL HCO_Msg( HcoState%Config%Err, msg, sep1='-' ) ! with separator
       ELSE
          CALL HCO_Msg( msg, LUN=HcoState%Config%outLUN ) ! w/o separator
       ENDIF
    ENDIF

    ! Create Instance
    Inst => NULL()
    CALL InstCreate ( ExtNr, ExtState%TOMAS_Jeagle, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR ( 'Cannot create TOMAS_Jeagle instance', RC )
       RETURN
    ENDIF
    ! Also fill Inst%ExtNr
    Inst%ExtNr = ExtNr

    ! ----------------------------------------------------------------------
    ! Get species IDs and settings
    ! ----------------------------------------------------------------------

    ! betty
    ! print *,'Betty start of options'

    ! fix scaling factor over cold water SST (<5 degC)
    CALL GetExtOpt ( HcoState%Config, Inst%ExtNr, 'Reduce SS cold water', &
                     OptValBool=Inst%ColdSST, RC=RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Add a SSA source from blowing snow (by J. Huang)
    CALL GetExtOpt ( HcoState%Config, Inst%ExtNr, 'Blowing Snow SS', &
                     OptValBool=Inst%EmitSnowSS, RC=RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

   ! Whether or not differentiate snow salinity on FYI and MYI (by J. Huang)
    !CALL GetExtOpt ( HcoState%Config, Inst%ExtNr, 'Diff salinity on ice', &
    !                 OptValBool=Inst%FYIsnow, RC=RC )
    !IF ( RC /= HCO_SUCCESS ) RETURN
    ! Add snow salinity (NH and SH), snow age and number of particles
    ! per snowflake as external factor from configuration file
    IF ( Inst%EmitSnowSS ) THEN
       CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'NH FYI snow salinity', &
                    OptValDp=Inst%NSLNT_FYI, RC=RC )
       IF ( RC /= HCO_SUCCESS ) RETURN
       CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'NH MYI snow salinity', &
                    OptValDp=Inst%NSLNT_MYI, RC=RC )
       IF ( RC /= HCO_SUCCESS ) RETURN
       CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'SH FYI snow salinity', &
                    OptValDp=Inst%SSLNT_FYI, RC=RC )
       IF ( RC /= HCO_SUCCESS ) RETURN
       CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'SH MYI snow salinity', &
                    OptValDp=Inst%SSLNT_MYI, RC=RC )
       IF ( RC /= HCO_SUCCESS ) RETURN
       CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'NH snow age', &
                    OptValDp=Inst%NAGE, RC=RC )
       IF ( RC /= HCO_SUCCESS ) RETURN
       CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'SH snow age', &
                    OptValDp=Inst%SAGE, RC=RC )
       IF ( RC /= HCO_SUCCESS ) RETURN
       CALL GetExtOpt( HcoState%Config, Inst%ExtNr, 'N per snowflake', &
                    OptValDp=Inst%NP, RC=RC )
       IF ( RC /= HCO_SUCCESS ) RETURN
    ELSE
       Inst%NSLNT_FYI = 0.1d0   ! default value 0.1 psu for NH FYI snow
       Inst%NSLNT_MYI = 0.05d0  ! default value 0.05 psu for NH MYI snow
       Inst%SSLNT_FYI = 0.03d0  ! default value 0.03 psu for SH FYI snow
       Inst%SSLNT_FYI = 0.015d0 ! default value 0.015 psu for SH MYI snow
       Inst%NAGE = 3.0d0   ! default value 3 days snow age in NH
       Inst%SAGE = 1.5d0   ! default value 1.5 days snow age in SH
       Inst%NP = 5.0d0     ! default value of 5 particles per snowflake
    ENDIF

   ! Verbose mode
    IF ( HcoState%amIRoot ) THEN
       MSG = 'Use sea salt aerosol emissions (extension module)'
       CALL HCO_MSG(HcoState%Config%Err,MSG, SEP1='-' )
       IF ( Inst%EmitSnowSS ) THEN
          WRITE(MSG,*) ' - Arctic Snow Salinity on FYI (psu): ', Inst%NSLNT_FYI
          CALL HCO_MSG(HcoState%Config%Err,MSG)
          WRITE(MSG,*) ' - Arctic Snow Salinity on MYI (psu): ', Inst%NSLNT_MYI
          CALL HCO_MSG(HcoState%Config%Err,MSG)
          WRITE(MSG,*) ' - Antarctic Snow Salinity on FYI (psu): ', Inst%SSLNT_FYI
          CALL HCO_MSG(HcoState%Config%Err,MSG)
          WRITE(MSG,*) ' - Antarctic Snow Salinity on FYI (psu): ', Inst%SSLNT_MYI
          CALL HCO_MSG(HcoState%Config%Err,MSG)
          WRITE(MSG,*) ' - Arctic Snow age (days): ', Inst%NAGE
          CALL HCO_MSG(HcoState%Config%Err,MSG)
          WRITE(MSG,*) ' - Antarctic Snow age(days): ', Inst%SAGE
          CALL HCO_MSG(HcoState%Config%Err,MSG)
          WRITE(MSG,*) ' - Number of particle per snowflake: ', Inst%NP
          CALL HCO_MSG(HcoState%Config%Err,MSG)
       ENDIF
    ENDIF

    ! Get HEMCO species IDs
    CALL HCO_GetExtHcoID( HcoState, Inst%ExtNr, Inst%HcoIDs, SpcNames, &
                          nSpc, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 2', RC, THISLOC=LOC )
        RETURN
    ENDIF
    IF ( nSpc < HcoState%MicroPhys%nBins ) THEN
       MSG = 'Not enough sea salt emission species set'
       CALL HCO_ERROR(MSG, RC )
       RETURN
    ENDIF
    IF ( HcoState%amIRoot ) THEN
       MSG = 'Use the following species (Name: HcoID):'
       CALL HCO_MSG(HcoState%Config%Err,MSG)
       DO N = 1, nSpc
          WRITE(MSG,*) TRIM(SpcNames(N)), ':', Inst%HcoIDs(N)
          CALL HCO_MSG(HcoState%Config%Err,MSG)
       ENDDO
    ENDIF

    ALLOCATE ( Inst%SS_DEN  ( HcoState%MicroPhys%nBins ), STAT=AS )
    IF ( AS/=0 ) THEN
       MSG = 'Cannot allocate SS_DEN'
       CALL HCO_ERROR(MSG, RC )
       RETURN
    ENDIF
    Inst%SS_DEN = 2200.d0

    ! Allocate for blowing snow simulation
    IF ( Inst%EmitSnowSS ) THEN
        ALLOCATE ( Inst%F_DI_N_FYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
        IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DI_N_FYI'
           CALL HCO_ERROR(MSG, RC )
           RETURN
        ENDIF
    	Inst%F_DI_N_FYI = 0.0_sp

        ALLOCATE ( Inst%F_DI_N_MYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
        IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DI_N_MYI'
           CALL HCO_ERROR(MSG, RC )
           RETURN
       ENDIF
        Inst%F_DI_N_MYI = 0.0_sp

        ALLOCATE ( Inst%F_DN_N_FYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
        IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DN_N_FYI'
           CALL HCO_ERROR(MSG, RC )
           RETURN
        ENDIF
        Inst%F_DN_N_FYI = 0.0_sp

        ALLOCATE ( Inst%F_DN_N_MYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
        IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DN_N_MYI'
           CALL HCO_ERROR(MSG, RC )
           RETURN
        ENDIF
        Inst%F_DN_N_MYI = 0.0_sp

    	ALLOCATE ( Inst%F_DI_S_FYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
	IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DI_S_FYI'
           CALL HCO_ERROR(MSG, RC )
           RETURN
        ENDIF
        Inst%F_DI_S_FYI = 0.0_sp

        ALLOCATE ( Inst%F_DI_S_MYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
	IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DI_S_MYI'
           CALL HCO_ERROR(MSG, RC )
           RETURN
        ENDIF
        Inst%F_DI_S_MYI = 0.0_sp

        ALLOCATE ( Inst%F_DN_S_FYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
	IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DN_S_FYI'
           CALL HCO_ERROR(MSG, RC )
           RETURN
        ENDIF
        Inst%F_DN_S_FYI = 0.0_sp

        ALLOCATE ( Inst%F_DN_S_MYI( NR_MAX,   HcoState%MicroPhys%nBins ), STAT=AS )
	IF ( AS/=0 ) THEN
           MSG = 'Cannot allocate F_DN_S_MYI'
           CALL HCO_ERROR(MSG, RC )
	      RETURN
        ENDIF
        Inst%F_DN_S_MYI = 0.0_sp
    ENDIF

    ! Allocate TOMAS_DBIN
    ALLOCATE ( Inst%TOMAS_DBIN( HcoState%MicroPhys%nBins ), STAT=RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Cannot allocate TOMAS_DBIN array (hcox_tomas_jeagle_mod.F90)'
       CALL HCO_ERROR(MSG, RC )
       RETURN
    ENDIF

    ! Allocate TOMAS_A
    ALLOCATE ( Inst%DRFAC( HcoState%MicroPhys%nBins ), STAT=RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Cannot allocate DRFAC array (hcox_tomas_jeagle_mod.F90)'
       CALL HCO_ERROR(MSG, RC )
       RETURN
    ENDIF

    ! Allocate TC1
    ALLOCATE ( Inst%TC1( HcoState%NX, HcoState%NY,&
               HcoState%NZ, HcoState%MicroPhys%nBins ), STAT=RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Cannot allocate TC1 array (hcox_tomas_jeagle_mod.F90)'
       CALL HCO_ERROR(MSG, RC )
       RETURN
    ELSE
    Inst%TC1 = 0d0
    ENDIF

    ! Allocate TC2
    ALLOCATE ( Inst%TC2( HcoState%NX, HcoState%NY,&
               HcoState%NZ, HcoState%MicroPhys%nBins ), STAT=RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       MSG = 'Cannot allocate TC2 array (hcox_tomas_jeagle_mod.F90)'
       CALL HCO_ERROR(MSG, RC )
       RETURN
    ELSE
    Inst%TC2 = 0d0
    ENDIF

    !size bins for blowing snow - Huang 6/12/20
       IF ( Inst%EmitSnowSS ) THEN

    DO K = 1, HcoState%MicroPhys%nBins
         ! TOMAS dry bin limits (radius)

         R0=0.5d6*((HcoState%MicroPhys%BinBound(K)*6.0d0)/  &
               (Inst%SS_DEN( K )*HcoState%Phys%PI))**( 1d0 / 3d0 ) ! in um
         R1=0.5d6*((HcoState%MicroPhys%BinBound(K+1)*6.0d0)/ &
               (Inst%SS_DEN( K )*HcoState%Phys%PI))**( 1d0 / 3d0 ) ! in um

         !-------------- Define size distribution ---------------------
         ! for northern hemisphere FYI
         D_SNOW = 1.0d0
         DO ND = 1, NR_MAX
            D_DRY =  ( Inst%NSLNT_FYI * RHOICE / (1000.d0 &
                  * Inst%NP * RHONACL ) )**( 1d0 / 3d0 ) * D_SNOW
            !print*,'Here is Ddry ',D_DRY, R0*2d0 , R1*2d0
            IF (D_DRY .ge. R0*2d0 .and. D_DRY .le. R1*2d0 ) THEN
           !----------------------------------------------------------
           ! NOTES:
           ! For size distribution
           ! define the two-parameter gamma probability density funtion here
           ! Yang et al 2008 eq (6)
           !----------------------------------------------------------
           ! Midpoint of IRth bin
               Inst%F_DI_N_FYI(ND, K) = EXP( - D_SNOW / B_SALT ) &
                * D_SNOW**( A_SALT - 1.d0 ) &
                / ( B_SALT**A_SALT * GAMMA( A_SALT ) )
           !print*,'Here is F_DI ', ND, K, Inst%F_DI_N_FYI(ND, K)
            ELSE
               Inst%F_DI_N_FYI(ND, K) = 0d0
            ENDIF
            Inst%F_DN_N_FYI(ND, K) = Inst%F_DI_N_FYI(ND,K) / (4d0/3d0 * HcoState%Phys%PI &
                      * 1.d-18 * Inst%SS_DEN( K ) * (D_DRY/2d0)**3)
            D_SNOW = D_SNOW + DDSNOW

           !print*,'Here is F_DN ', ND, K, Inst%F_DN_N_FYI(ND, K)

         ENDDO

         ! for northern hemisphere MYI
         D_SNOW = 1.0d0
         DO ND = 1, NR_MAX
            D_DRY =  ( Inst%NSLNT_MYI * RHOICE / (1000.d0 &
                  * Inst%NP * RHONACL ) )**( 1d0 / 3d0 ) * D_SNOW

            IF (D_DRY .ge. R0*2d0 .and. D_DRY .le. R1*2d0 ) THEN
           ! Midpoint of IRth bin
               Inst%F_DI_N_MYI(ND, K) = EXP( - D_SNOW / B_SALT ) &
                * D_SNOW**( A_SALT - 1.d0 ) &
                / ( B_SALT**A_SALT * GAMMA( A_SALT ) )
            ELSE
               Inst%F_DI_N_MYI(ND, K) = 0d0
            ENDIF
            Inst%F_DN_N_MYI(ND, K) = Inst%F_DI_N_MYI(ND,K) / (4d0/3d0 * HcoState%Phys%PI &
                      * 1.d-18 * Inst%SS_DEN( K ) * (D_DRY/2d0)**3)

            D_SNOW = D_SNOW + DDSNOW
         ENDDO


         ! for southern hemisphere FYI
         D_SNOW = 1.0d0
         DO ND = 1, NR_MAX
            D_DRY =  ( Inst%SSLNT_FYI * RHOICE / (1000.d0 &
                  * Inst%NP * RHONACL ) )**( 1d0 / 3d0 ) * D_SNOW

            IF (D_DRY .ge. R0*2d0 .and. D_DRY .le. R1*2d0 ) THEN
          ! Midpoint of IRth bin
	         Inst%F_DI_S_FYI(ND, K) = EXP( - D_SNOW / B_SALT ) &
                * D_SNOW**( A_SALT - 1.d0 ) &
                / ( B_SALT**A_SALT * GAMMA( A_SALT ) )
            ELSE
               Inst%F_DI_S_FYI(ND, K) = 0d0
            ENDIF
            Inst%F_DN_S_FYI(ND, K) = Inst%F_DI_S_FYI(ND,K)/ (4d0/3d0 * HcoState%Phys%PI &
                      * 1.d-18 * Inst%SS_DEN( K ) * (D_DRY/2d0)**3)
            D_SNOW = D_SNOW + DDSNOW
         ENDDO

         ! for southern hemisphere MYI
         D_SNOW = 1.0d0
         DO ND = 1, NR_MAX
           D_DRY =  ( Inst%SSLNT_MYI * RHOICE / (1000.d0 &
                  * Inst%NP * RHONACL ) )**( 1d0 / 3d0 ) * D_SNOW
            IF (D_DRY .ge. R0*2d0 .and. D_DRY .le. R1*2d0 ) THEN
          ! Midpoint of IRth bin
	         Inst%F_DI_S_MYI(ND, K) = EXP( - D_SNOW / B_SALT ) &
                * D_SNOW**( A_SALT - 1.d0 ) &
                / ( B_SALT**A_SALT * GAMMA( A_SALT ) )
            ELSE
               Inst%F_DI_S_MYI(ND, K) = 0d0
            ENDIF
            Inst%F_DN_S_MYI(ND, K) = Inst%F_DI_S_MYI(ND,K)/ (4d0/3d0 * HcoState%Phys%PI &
                      * 1.d-18 * Inst%SS_DEN( K ) * (D_DRY/2d0)**3)
            D_SNOW = D_SNOW + DDSNOW
         ENDDO
       ENDDO !K
          ENDIF

! ----- IMPORTANT BINS ONLY CORRECTLY SET UP FOR TOMAS 15 PLEASE ADJUST OTHERS -jkodros (7/21/15)
! ----  6/24/16 - JKodros - I have updated the DRFAC. They should (hopefully) be
! ----  correct now. DRFAC is the bin width (radius not diameter) for DRY SS
#if defined( TOMAS12 )

    !-----------------------------------------------------------------------
    ! TOMAS simulation w/ 12 size-resolved bins
    !-----------------------------------------------------------------------

    Inst%TOMAS_DBIN = (/                                                &
          9.68859d-09,   1.53797d-08,    2.44137d-08,    3.87544d-08,   &
          6.15187d-08,   9.76549d-08,    1.55017d-07,    2.46075d-07,   &
          3.90620d-07,   6.20070d-07,    9.84300d-07,    3.12500d-06  /)

    Inst%DRFAC     = (/                                                 &
          2.84132d-03,   4.51031d-03,    7.15968d-03,    1.13653d-02,   &
          1.80413d-02,   2.86387d-02,    4.54612d-02,    7.21651d-02,   &
          1.14555d-01,   1.81845d-01,    1.06874d+00,    3.39304d+00 /)
#elif defined( TOMAS15 )

    !-----------------------------------------------------------------------
    ! TOMAS simulation w/ 15 size-resolved bins
    !-----------------------------------------------------------------------

    Inst%TOMAS_DBIN = (/         0d0,            0d0,             0d0,   &
          1.22069d-08,   1.93772d-08,    3.07594d-08,     4.88274d-08,   &
          7.75087d-08,   1.23037d-07,    1.95310d-07,     3.10035d-07,   &
          4.92150d-07,   7.81239d-07,    1.74054d-06,     5.52588d-06 /)

    Inst%DRFAC      = (/         0d0,            0d0,             0d0,   &
          2.84132d-03,   4.51031d-03,    7.15968d-03,     1.13653d-02,   &
          1.80413d-02,   2.86387d-02,    4.54612d-02,     7.21651d-02,   &
          1.14555d-01,   1.81845d-01,    1.06874d+00,     3.39304d+00 /)

#elif defined( TOMAS40 )

    !-----------------------------------------------------------------------
    ! TOMAS simulation w/ 40 size-resolved bins
    !-----------------------------------------------------------------------

    Inst%TOMAS_DBIN = (/                                                 &
       0.0d0      , 0.0d0      , 0.0d0     ,  0.0d0      , 0.0d0      ,  &
       0.0d0      , 0.0d0      , 0.0d0     ,  0.0d0      , 0.0d0      ,  &
       9.68859d-09, 1.22069d-08, 1.53797d-08, 1.93772d-08, 2.44137d-08,  &
       3.07594d-08, 3.87544d-08, 4.88274d-08, 6.15187d-08, 7.75087d-08,  &
       9.76549d-08, 1.23037d-07, 1.55017d-07, 1.95310d-07, 2.46075d-07,  &
       3.10035d-07, 3.90620d-07, 4.92150d-07, 6.20070d-07, 7.81239d-07,  &
       9.84300d-07, 1.24014d-06, 1.56248d-06, 1.96860d-06, 2.48028d-06,  &
       3.12496d-06, 3.93720d-06, 4.96056d-06, 6.24991d-06, 7.87440d-06 /)

    Inst%DRFAC     = (/                                                  &
       0.0d0      , 0.0d0      , 0.0d0      , 0.0d0      , 0.0d0      ,  &
       0.0d0      , 0.0d0      , 0.0d0      , 0.0d0      , 0.0d0      ,  &
       1.24737d-03, 1.57158d-03, 1.98007d-03, 2.49473d-03, 3.14317d-03,  &
       3.96014d-03, 4.98947d-03, 6.28633d-03, 7.92028d-03, 9.97893d-03,  &
       1.25727d-02, 1.58406d-02, 1.99579d-02, 2.51453d-02, 3.16811d-02,  &
       3.99157d-02, 5.02906d-02, 6.33623d-02, 7.98314d-02, 1.00581d-01,  &
       1.26725d-01, 1.59663d-01, 2.01163d-01, 2.53449d-01, 3.19326d-01,  &
       4.02325d-01, 5.06898d-01, 6.38652d-01, 8.04651d-01, 1.01380d+00 /)
#else

    !-----------------------------------------------------------------------
    ! TOMAS simulation w/ 30 size-resolved bins (default)
    !-----------------------------------------------------------------------

    Inst%TOMAS_DBIN = (/                                                 &
       9.68859d-09, 1.22069d-08, 1.53797d-08, 1.93772d-08, 2.44137d-08,  &
       3.07594d-08, 3.87544d-08, 4.88274d-08, 6.15187d-08, 7.75087d-08,  &
       9.76549d-08, 1.23037d-07, 1.55017d-07, 1.95310d-07, 2.46075d-07,  &
       3.10035d-07, 3.90620d-07, 4.92150d-07, 6.20070d-07, 7.81239d-07,  &
       9.84300d-07, 1.24014d-06, 1.56248d-06, 1.96860d-06, 2.48028d-06,  &
       3.12496d-06, 3.93720d-06, 4.96056d-06, 6.24991d-06, 7.87440d-06 /)

    Inst%DRFAC     = (/                                                  &
       1.24737d-03, 1.57158d-03, 1.98007d-03, 2.49473d-03, 3.14317d-03,  &
       3.96014d-03, 4.98947d-03, 6.28633d-03, 7.92028d-03, 9.97893d-03,  &
       1.25727d-02, 1.58406d-02, 1.99579d-02, 2.51453d-02, 3.16811d-02,  &
       3.99157d-02, 5.02906d-02, 6.33623d-02, 7.98314d-02, 1.00581d-01,  &
       1.26725d-01, 1.59663d-01, 2.01163d-01, 2.53449d-01, 3.19326d-01,  &
       4.02325d-01, 5.06898d-01, 6.38652d-01, 8.04651d-01, 1.01380d+00 /)
#endif


    !bc, 10/01/24 - comment this out as coef is not being used
    !=======================================================================
    ! Allocate quantities depending on horizontal resolution
    !=======================================================================
!    !IF ( TRIM( HcoState%Config%GridRes) == '4.0x5.0' ) THEN !comment out for
!    resolution independence in code and consider implementing offline SS in
!    TOMAS
!
!       !-----------------------------------------------------------------------
!       ! TOMAS simulations at 4 x 5 global resolution
!       !-----------------------------------------------------------------------
!       Inst%TOMAS_COEF = 1.d0
!
!    ELSE IF ( TRIM( HcoState%Config%GridRes) == '2.0x2.5' ) THEN
!
!       !-----------------------------------------------------------------------
!       ! TOMAS simulations at 2 x 2.5 global resolution
!       !-----------------------------------------------------------------------
!       Inst%TOMAS_COEF = 1.d0
!
!    ELSE
!
!       MSG = 'Adjust TOMAS_Jeagle emiss coeff (TOMAS_COEF) for your model res: SRCSALT30: hcox_TOMAS_jeagle_mod.F90'
!       CALL HCO_ERROR(MSG, RC )
!
!    ENDIF

    !=======================================================================
    ! Activate this module and the fields of ExtState that it uses
    !=======================================================================

    ! Activate met fields
    ExtState%FRLAND%DoUse      = .TRUE.
    ExtState%FROCEAN%DoUse     = .TRUE.
    ExtState%FRSEAICE%DoUse    = .TRUE.
    ExtState%TSKIN%DoUse       = .TRUE.
    ExtState%U10M%DoUse        = .TRUE.
    ExtState%V10M%DoUse        = .TRUE.
    ExtState%FRAC_OF_PBL%DoUse = .TRUE.
    ExtState%FRCLND%DoUse      = .TRUE.

   ! for blowing snow
    IF ( Inst%EmitSnowSS ) THEN
       ExtState%USTAR%DoUse    = .TRUE.
       ExtState%T2M%DoUse      = .TRUE.
       ExtState%QV2M%DoUse     = .TRUE.
    ENDIF

    !=======================================================================
    ! Leave w/ success
    !=======================================================================
    IF ( ALLOCATED( SpcNames ) ) DEALLOCATE( SpcNames )

    ! Nullify pointers
    Inst    => NULL()

    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_TOMAS_Jeagle_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_TOMAS_Jeagle_Final
!
! !DESCRIPTION: Subroutine HcoX\_TOMAS\_Jeagle\_Final deallocates
!  all module arrays.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_TOMAS_Jeagle_Final( ExtState )
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
!
! !REVISION HISTORY:
!  15 Dec 2013 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
    CALL InstRemove ( ExtState%TOMAS_Jeagle )

  END SUBROUTINE HCOX_TOMAS_Jeagle_Final
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

    !betty
    !print*, 'Betty In instance'

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

    !Init values
    Inst%ColdSST       = .FALSE.
    Inst%EmitSnowSS    = .FALSE.
    Inst%NSLNT_FYI     = 0.0
    Inst%NSLNT_MYI     = 0.0
    Inst%SSLNT_FYI     = 0.0
    Inst%SSLNT_MYI     = 0.0
    Inst%NAGE          = 0.0
    Inst%SAGE          = 0.0
    Inst%NP            = 1.0

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
       !---------------------------------------------------------------------
       IF ( ASSOCIATED( Inst%TOMAS_DBIN ) ) THEN
          DEALLOCATE( Inst%TOMAS_DBIN )
       ENDIF
       Inst%TOMAS_DBIN => NULL()

       IF ( ASSOCIATED( Inst%DRFAC ) ) THEN
          DEALLOCATE( Inst%DRFAC )
       ENDIF
       Inst%DRFAC => NULL()

       IF ( ASSOCIATED( Inst%TC1 ) ) THEN
          DEALLOCATE( Inst%TC1 )
       ENDIF
       Inst%TC1 => NULL()

       IF ( ASSOCIATED( Inst%TC2 ) ) THEN
          DEALLOCATE( Inst%TC2 )
       ENDIF
       Inst%TC2 => NULL()

       IF ( ALLOCATED ( Inst%HcoIDs ) ) THEN
          DEALLOCATE( Inst%HcoIDs )
       ENDIF

       IF ( ASSOCIATED( Inst%SS_DEN ) ) THEN
          DEALLOCATE( Inst%SS_DEN )
       ENDIF
       Inst%SS_DEN => NULL()

       IF ( ASSOCIATED( Inst%F_DI_N_FYI ) ) THEN
          DEALLOCATE( Inst%F_DI_N_FYI )
       ENDIF
       Inst%F_DI_N_FYI => NULL()

       IF ( ASSOCIATED( Inst%F_DI_N_MYI ) ) THEN
          DEALLOCATE( Inst%F_DI_N_MYI )
       ENDIF
       Inst%F_DI_N_MYI => NULL()

       IF ( ASSOCIATED( Inst%F_DI_S_FYI ) ) THEN
          DEALLOCATE( Inst%F_DI_S_FYI )
       ENDIF
       Inst%F_DI_S_FYI => NULL()

       IF ( ASSOCIATED( Inst%F_DI_S_MYI ) ) THEN
          DEALLOCATE( Inst%F_DI_S_MYI )
       ENDIF
       Inst%F_DI_S_MYI => NULL()

       IF ( ASSOCIATED( Inst%F_DN_N_FYI ) ) THEN
          DEALLOCATE( Inst%F_DN_N_FYI )
       ENDIF
       Inst%F_DN_N_FYI => NULL()

       IF ( ASSOCIATED( Inst%F_DN_N_MYI ) ) THEN
          DEALLOCATE( Inst%F_DN_N_MYI )
       ENDIF
       Inst%F_DN_N_MYI => NULL()

       IF ( ASSOCIATED( Inst%F_DN_S_FYI ) ) THEN
          DEALLOCATE( Inst%F_DN_S_FYI )
       ENDIF
       Inst%F_DN_S_FYI => NULL()

       IF ( ASSOCIATED( Inst%F_DN_S_MYI ) ) THEN
          DEALLOCATE( Inst%F_DN_S_MYI )
       ENDIF
       Inst%F_DN_S_MYI => NULL()

       IF ( ASSOCIATED( Inst%MULTIICE ) ) THEN
          DEALLOCATE( Inst%MULTIICE )
       ENDIF
       Inst%MULTIICE => NULL()

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
#endif
END MODULE HCOX_TOMAS_Jeagle_Mod
