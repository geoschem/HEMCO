!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_dustl23m_mod.F90
!
! !DESCRIPTION: Module hcox\_dustl23m\_mod.F90 contains routines and
!  variables from modified Danny M. Leung's dust emission scheme.
!  Ref: 
!  [1] Leung, D. M., Kok, J. F., Li, L., Okin, G. S., Prigent, C., Klose, M., Pérez García-Pando, C., Menut, L., Mahowald, N. M., Lawrence, D. M., and Chamecki, M.: 
!      A new process-based and scale-aware desert dust emission scheme for global climate models – Part I: Description and evaluation against inverse modeling emissions, 
!      Atmos. Chem. Phys., 23, 6487–6523, 
!      https://doi.org/10.5194/acp-23-6487-2023, 2023.
!  [2] Zhang, D., Martin, R. V., Liu, X., van Donkelaar, A., Oxford, C. R., Li, Y., Meng, J., Leung, D. M., Kok, J. F., Li, L., Zhu, H., Turner, J. R., Yan, Y., Brauer, M., Rudich, Y., and Windwer, E.: 
!      Improving Fine Mineral Dust Representation from the Surface to the Column in GEOS-Chem 14.4.1, 
!      EGUsphere [preprint], 
!      https://doi.org/10.5194/egusphere-2025-438, 2025.
!\\
!\\
! !INTERFACE:
!
MODULE HCOX_DustL23M_mod
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
  PUBLIC :: HCOX_DustL23M_Run
  PUBLIC :: HCOX_DustL23M_Init
  PUBLIC :: HCOX_DustL23M_Final
!
! !REVISION HISTORY:
!  2 May 2024 - Dandan Zhang - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! MyInst is the extension-specific derived type. It should hold all module
  ! variables and arrays that are required to compute the emissions.
  ! For instance, if the extension relies on an input field read through the
  ! HEMCO configuration file (e.g. MY_INPUT_FIELD), the data array pointer
  ! to that field should be listed within the instance and NOT outside of it.
  ! This ensures that the same extension can be invoked in various instances,
  ! all of them potentially pointing to different data fields.
  TYPE :: MyInst
   ! Fields required by module
   INTEGER                         :: Instance
   INTEGER                         :: ExtNr            ! Extension num for DustL23
   INTEGER                         :: ExtNrAlk         ! Extension num for DustAlk
   INTEGER, ALLOCATABLE            :: HcoIDs(:)        ! tracer IDs for DustDead
   INTEGER, ALLOCATABLE            :: HcoIDsAlk(:)     ! tracer IDs for DustAlk
   INTEGER                         :: nSpc             ! # of species
   REAL(sp), ALLOCATABLE           :: SpcScl(:)        ! Species scale factors
   CHARACTER(LEN=31), ALLOCATABLE  :: SpcNames(:)
   INTEGER                         :: nSpcAlk          ! # of species
   CHARACTER(LEN=31), ALLOCATABLE  :: SpcNamesAlk(:)
   CHARACTER(LEN=61), ALLOCATABLE  :: SpcScalFldNme(:) ! Names of scale factor fields

   ! Other fields
   REAL(hp),  ALLOCATABLE            :: DMT_MIN(:)       ! Bin size min diameter [m]
   REAL(hp),  ALLOCATABLE            :: DMT_MAX(:)       ! Bin size max diameter [m]
   
   !---------------------------------------
   ! 2-D pointers pointing to netCDF arrays
   !---------------------------------------

   ! Land cover map
   REAL(hp), POINTER          :: A_bare    (:,:) => NULL() ! The fraction of barren and sparsely vegetated land cover [unitless]
   REAL(hp), POINTER          :: A_veg     (:,:) => NULL() ! The fraction of short vegetation land cover [unitless]
   
   ! Scaling factor of 0.6 over the Sahara [unitless]
   REAL(hp), POINTER          :: C_sah     (:,:) => NULL()

   ! Read leaf area index from MODIS MCD12C1 processed by Yuan et al. (XLAI in HEMCO)
   REAL(hp), POINTER          :: XLAI_t    (:,:) => NULL() ! The total XLAI [cm2 cm-2]

   ! Soil textture map
   REAL(hp), POINTER          :: f_clay    (:,:) => NULL() ! The fraction of clay content in topmost soil [unitless]
   REAL(hp), POINTER          :: bulk_den  (:,:) => NULL() ! The bulk density of the topmost soil [kg m-3]

   ! Soil porosity taken from the constant field from MERRA2 M2C0NXLND collection [unitless]
   REAL(hp), POINTER          :: poros     (:,:) => NULL()

   ! Surface roughness length due to rocks [m]
   REAL(hp), POINTER          :: roughness_r    (:,:) => NULL()
   
   TYPE(MyInst), POINTER           :: NextInst => NULL()

  END TYPE MyInst

  ! Pointer to all instances
  TYPE(MyInst), POINTER            :: AllInst => NULL()


    !---------------------------------------
    ! MODULE PARAMETER
    !---------------------------------------
    INTEGER, PARAMETER   :: NBINS  = 7        ! # of dust bins
    INTEGER, PARAMETER   :: TNSPEC = 8        ! # of dust bins + 1
    ! Fundamental physical constants
    REAL(hp),  PARAMETER   :: CST_VON_KRM      = 0.386_hp
    REAL(hp),  PARAMETER   :: SPC_HEAT_DRY_AIR = 1005.0_hp
    
    ! Other dust parameters
    REAL(hp),  PARAMETER   :: D_p         = 127.0e-6_hp ! Median diameter of soil particle [m]
    REAL(hp),  PARAMETER   :: rho_p       = 2650.0_hp   ! Soil particle density [kg m-3]
    REAL(hp),  PARAMETER   :: rho_w       = 1000.0_hp   ! Water density [kg m-3]
    REAL(hp),  PARAMETER   :: rho_a0      = 1.225_hp    ! Standard air density at sea level and 15 degrees C [kg m-3]
    REAL(hp),  PARAMETER   :: LAI_thr     = 0.5_hp      ! Threshold LAI [unitless]
    REAL(hp),  PARAMETER   :: snowdep_thr = 0.05_hp     ! Threshold snow depth [m]
    REAL(hp),  PARAMETER   :: T0          = 273.15_hp   ! Frozen temperature of soil to prevent dust emissions [K]
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_dustl23m_Run
!
! !DESCRIPTION: Subroutine HcoX\_dustl23m\_Run is the driver routine
! for the HEMCO DustL23M extension.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_DustL23M_Run( ExtState, HcoState, RC )
!
! !USES:
!   
    USE HCO_CALC_MOD,      ONLY : HCO_EvalFld, HCO_CalcEmis
    USE HCO_FLUXARR_MOD,   ONLY : HCO_EmisAdd
    USE HCO_CLOCK_MOD,     ONLY : HcoClock_Get
    USE HCO_CLOCK_MOD,     ONLY : HcoClock_First
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State), POINTER       :: ExtState    ! Module options
    TYPE(HCO_State), POINTER       :: HcoState    ! Hemco state
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(INOUT) :: RC          ! Success or failure
!
! !REMARKS:
!
!
! !REVISION HISTORY:
!  2 May 2024 - Dandan Zhang - Revised from template for DustL23
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
    ! Local variables
    LOGICAL                :: ERR
    INTEGER                :: N
    ! Total dust emission flux [kg/m2/s]
    REAL(hp), TARGET       :: TFLUX(HcoState%NX, HcoState%NY)
    ! Flux array [kg/m2/s]
    REAL(hp), TARGET       :: FLUX(HcoState%NX, HcoState%NY, TNSPEC)
    ! Flux array for dust alkalinity [kg/m2/s]
    REAL(hp), TARGET       :: FLUX_ALK(HcoState%NX,HcoState%NY,TNSPEC)

    TYPE(MyInst), POINTER :: Inst => NULL()
    CHARACTER(LEN=255)    :: MSG, LOC

    !=================================================================
    ! HCOX_DustL23M_RUN begins here!
    !=================================================================
    LOC = 'HCOX_DustL23M_RUN (HCOX_DustL23M_MOD.F90)'
    ! Return if extension disabled
    IF ( ExtState%DustL23M <= 0 ) RETURN

    ! Enter
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Get pointer to this instance. Varible Inst contains all module
    ! variables for the current instance. The instance number is
    ! ExtState%DustL23
    ! Get instance
    Inst => NULL()
    CALL InstGet ( ExtState%DustL23M, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       WRITE(MSG,*) 'Cannot find DustL23M instance Nr. ', ExtState%DustL23M
       CALL HCO_ERROR(MSG,RC)
       RETURN
    ENDIF

    !=================================================================
    ! Module code comes below
    !=================================================================
    CALL HCO_EvalFld( HcoState, 'L23M_A_bare', Inst%A_bare, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL HCO_EvalFld( HcoState, 'L23M_A_veg', Inst%A_veg, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL HCO_EvalFld( HcoState, 'L23M_Csah', Inst%C_sah, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL HCO_EvalFld( HcoState, 'L23M_LAI', Inst%XLAI_t, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL HCO_EvalFld( HcoState, 'L23M_fclay', Inst%f_clay, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL HCO_EvalFld( HcoState, 'L23M_BD', Inst%bulk_den, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL HCO_EvalFld( HcoState, 'L23M_poros', Inst%poros, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    CALL HCO_EvalFld( HcoState, 'L23M_roughness_r', Inst%roughness_r, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR', RC, THISLOC=LOC )
        RETURN
    ENDIF

    !=================================================================
    ! DustL23M Emission Scheme
    !=================================================================
    ! Error check
    ERR = .FALSE.

    CALL CAL_DUSTL23M_EmisFlux(HcoState, ExtState, Inst, TFLUX, RC)
    ! Error check
    IF ( RC /= HCO_SUCCESS ) THEN
      ERR = .TRUE.
      RETURN
    ENDIF
    FLUX(:,:,1) = TFLUX
    FLUX(:,:,2) = TFLUX * 3.344e-4_hp
    FLUX(:,:,3) = TFLUX * 1.593e-3_hp
    FLUX(:,:,4) = TFLUX * 1.194e-2_hp
    FLUX(:,:,5) = TFLUX * 3.430e-2_hp
    FLUX(:,:,6) = TFLUX * 1.248e-1_hp
    FLUX(:,:,7) = TFLUX * 2.573e-1_hp
    FLUX(:,:,7) = TFLUX * 5.698e-1_hp

    ! Include DUST Alkalinity SOURCE, assuming an alkalinity
    ! of 4% by weight [kg].                  !tdf 05/10/08
    !tdf with 3% Ca, there's also 1% equ. Mg, makes 4%
    IF ( Inst%ExtNrAlk > 0 ) THEN
      FLUX_ALK = 0.04_hp * FLUX
    ENDIF

    ! Error check
    IF ( ERR ) THEN
      RC = HCO_FAIL
      RETURN
    ENDIF

   !=================================================================
   ! PASS TO HEMCO STATE AND UPDATE DIAGNOSTICS
   !=================================================================
    DO N = 1, TNSPEC
      IF ( Inst%HcoIDs(N) > 0 ) THEN
        ! Add to emissions array
        CALL HCO_EmisAdd( HcoState, FLUX(:,:,N), Inst%HcoIDs(N), RC, ExtNr=Inst%ExtNr )
        IF ( RC /= HCO_SUCCESS ) THEN
          WRITE(MSG,*) 'HCO_EmisAdd error: dust bin ', N
          CALL HCO_ERROR(MSG, RC )
          RETURN
        ENDIF
      ENDIF

      IF ( Inst%ExtNrAlk > 0 ) THEN
        IF ( Inst%HcoIDsAlk(N) > 0 ) THEN
          ! Add to dust alkalinity emissions array
          CALL HCO_EmisAdd( HcoState, FLUX_Alk(:,:,N), Inst%HcoIDsAlk(N), RC, ExtNr=Inst%ExtNrAlk )
           IF ( RC /= HCO_SUCCESS ) THEN
              WRITE(MSG,*) 'HCO_EmisAdd error: dust alk bin ', N
              CALL HCO_ERROR(MSG, RC )
              RETURN
           ENDIF

        ENDIF
     ENDIF
    ENDDO

    ! Cleanup
    Inst => NULL()

    ! Return w/ success
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_DustL23M_Run
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_DustL23M_Init
!
! !DESCRIPTION: Subroutine HcoX\_DustL23M\_Init initializes the HEMCO
! DUSTL23M extension.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_DustL23M_Init( HcoState, ExtName, ExtState, RC )
!
! !USES:
!
    USE HCO_ExtList_Mod,    ONLY : GetExtNr
    USE HCO_ExtList_Mod,    ONLY : GetExtOpt
    USE HCO_STATE_MOD,      ONLY : HCO_GetExtHcoID
    USE HCO_ExtList_Mod,    ONLY : GetExtSpcVal
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
!  06 May 2024 - Dandan Zhang - Initial version for DustL23
!  25 Nov 2013 - C. Keller   - Now a HEMCO extension
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(MyInst), POINTER          :: Inst => NULL()
    INTEGER                        :: ExtNr, N, AS
    CHARACTER(LEN=255)             :: MSG, LOC

    !=================================================================
    ! HCOX_DustL23M_INIT begins here!
    !=================================================================
    LOC = 'HCOX_DustL23M_INIT (HCOX_DUSTL23M_MOD.F90)'

    ! Extension Nr.
    ExtNr = GetExtNr( HcoState%Config%ExtList, TRIM(ExtName) )
    IF ( ExtNr <= 0 ) RETURN

    ! Enter
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 1', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Create instance for this simulation. Link instance number to the ExtState object
    ! for future reference to the instance. See InstCreate for more details.
    CALL InstCreate ( ExtNr, ExtState%DustL23M, Inst, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR ( 'Cannot create DustL23M instance', RC )
       RETURN
    ENDIF

    ! Check for dust alkalinity option
    Inst%ExtNrAlk = GetExtNr( HcoState%Config%ExtList, 'DustAlk')

    ! Get species IDs.
    CALL HCO_GetExtHcoID( HcoState, Inst%ExtNr, Inst%HcoIDs, Inst%SpcNames, Inst%nSpc, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 2', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Get the dust alkalinity species defined for DustAlk option
    IF ( Inst%ExtNrAlk > 0 ) THEN
      CALL HCO_GetExtHcoID( HcoState, Inst%ExtNrAlk, Inst%HcoIDsAlk, Inst%SpcNamesAlk, Inst%nSpcAlk, RC)
      IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 3', RC, THISLOC=LOC )
        RETURN
      ENDIF
    ENDIF

    ! Sanity check
    IF ( Inst%nSpc /= TNSPEC ) THEN
      MSG = 'DustL23M model does not have 7(+1 total) species!'
      CALL HCO_ERROR(MSG, RC )
      RETURN
    ENDIF

    ! There must be at least one species
    IF ( Inst%nSpc == 0 ) THEN
       CALL HCO_ERROR ( 'No DustL23M species specified', RC )
       RETURN
    ENDIF

    ! Determine scale factor to be applied to each species. This is 1.00
    ! by default, but can be set in the HEMCO configuration file via setting
    ! Scaling_<SpcName>.
    CALL GetExtSpcVal( HcoState%Config, Inst%ExtNr, Inst%nSpc, &
                       Inst%SpcNames, 'Scaling', 1.0_sp, Inst%SpcScl, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 3', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Get species mask fields
    CALL GetExtSpcVal( HcoState%Config, Inst%ExtNr, Inst%nSpc, &
                       Inst%SpcNames, 'ScaleField', HCOX_NOSCALE, Inst%SpcScalFldNme, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 4', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Verbose mode
    IF ( HcoState%amIRoot ) THEN

       ! Write the name of the extension regardless of the verbose setting
       msg = 'Using HEMCO extension: DustL23M (dust emission scheme)'
       IF ( HcoState%Config%doVerbose ) THEN
          CALL HCO_Msg( msg, sep1='-', LUN=HcoState%Config%hcoLogLUN ) ! with separator
       ELSE
          CALL HCO_Msg( msg, LUN=HcoState%Config%hcoLogLUN ) ! w/o separator
       ENDIF 

       ! Write all other messages as debug printout only
       MSG = ' - use the following species (Name, HcoID, Scaling):'
       CALL HCO_MSG( MSG, LUN=HcoState%Config%hcoLogLUN )
       DO N = 1, Inst%nSpc
          WRITE(MSG,*) TRIM(Inst%SpcNames(N)), ', ', Inst%HcoIDs(N), ', ', Inst%SpcScl(N)
          CALL HCO_MSG( MSG, LUN=HcoState%Config%hcoLogLUN )
          WRITE(MSG,*) 'Apply scale field: ', TRIM(Inst%SpcScalFldNme(N))
          CALL HCO_MSG( MSG, LUN=HcoState%Config%hcoLogLUN )
       ENDDO
    ENDIF

    !-----------------------------------------------------------------
    ! Init module arrays
    !-----------------------------------------------------------------
    ALLOCATE( Inst%A_bare( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%A_bare!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%A_bare = -9999.0_hp

    ALLOCATE( Inst%A_veg( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%A_veg!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%A_veg = -9999.0_hp

    ALLOCATE( Inst%C_sah( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%C_sah!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%C_sah = -9999.0_hp

    ALLOCATE( Inst%XLAI_t( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%XLAI_t!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%XLAI_t = -9999.0_hp

    ALLOCATE( Inst%f_clay( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%f_clay!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%f_clay = -9999.0_hp

    ALLOCATE( Inst%bulk_den( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%bulk_den!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%bulk_den = -9999.0_hp

    ALLOCATE( Inst%poros( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%poros!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%poros = -9999.0_hp

    ALLOCATE( Inst%roughness_r( HcoState%NX, HcoState%NY), STAT=AS )
    IF ( AS /= 0 ) THEN
        msg = 'Could not allocate Inst%roughness_r!'
        CALL HCO_ERROR( msg, RC, thisLoc=loc )
        RETURN
    ENDIF
    Inst%roughness_r = -9999.0_hp
    
    ! Bin size min diameter [m]
    ALLOCATE( Inst%DMT_MIN( NBINS ), STAT=AS )
    IF ( AS /= 0 ) THEN
      CALL HCO_ERROR ( 'DMT_MIN', RC )
      RETURN
    ENDIF
    Inst%DMT_MIN(1) = 0.2e-6_hp
    Inst%DMT_MIN(2) = 0.36e-6_hp
    Inst%DMT_MIN(3) = 0.6e-6_hp
    Inst%DMT_MIN(4) = 1.2e-6_hp
    Inst%DMT_MIN(5) = 2.0e-6_hp
    Inst%DMT_MIN(6) = 3.6e-6_hp
    Inst%DMT_MIN(7) = 6.0e-6_hp

    ! Bin size max diameter [m]
    ALLOCATE( Inst%DMT_MAX( NBINS ), STAT=AS )
    IF ( AS /= 0 ) THEN
      CALL HCO_ERROR ( 'DMT_MAX', RC )
      RETURN
    ENDIF
    Inst%DMT_MAX(1) = 0.36e-6_hp
    Inst%DMT_MAX(2) = 0.6e-6_hp
    Inst%DMT_MAX(3) = 1.2e-6_hp
    Inst%DMT_MAX(4) = 2.0e-6_hp
    Inst%DMT_MAX(5) = 3.6e-6_hp
    Inst%DMT_MAX(6) = 6.0e-6_hp
    Inst%DMT_MAX(7) = 1.2e-5_hp

    ! Activate met fields used by this extension
    ExtState%T2M%DoUse      = .TRUE.
    ExtState%TS%DoUse       = .TRUE.
    ExtState%PS%DoUse       = .TRUE.
    ExtState%GWETTOP%DoUse  = .TRUE.
    ExtState%SNOWHGT%DoUse  = .TRUE.
    ExtState%USTAR%DoUse    = .TRUE.
    ExtState%PBLH%DoUse     = .TRUE.
    ExtState%HFLUX%DoUse    = .TRUE.

    ! Cleanup
    Inst => NULL()
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOX_DustL23M_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_DustL23M_Final
!
! !DESCRIPTION: Subroutine HcoX\_DustL23M\_Final finalizes the HEMCO
! DustL23M extension.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_DustL23M_Final ( ExtState )
!
! !INPUT PARAMETERS:
!
    TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
!
! !REVISION HISTORY:
!  06 May 2024 - Dandan Zhang - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    !=================================================================
    ! HCOX_DustL23M_FINAL begins here!
    !=================================================================
    CALL InstRemove ( ExtState%DustL23M )

  END SUBROUTINE HCOX_DustL23M_Final
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InstGet
!
! !DESCRIPTION: Subroutine InstGet returns a pointer to the desired instance.
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
! !DESCRIPTION: Subroutine InstCreate adds a new instance to the list of
!  instances, assigns a unique instance number to this new instance, and
!  archives this instance number to output argument Instance.
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
    TYPE(MyInst), POINTER          :: TmpInst  => NULL()
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
    Inst%A_bare          => NULL()
    Inst%A_veg           => NULL()
    Inst%C_sah           => NULL()
    Inst%XLAI_t          => NULL()
    Inst%f_clay          => NULL()
    Inst%bulk_den        => NULL()
    Inst%poros           => NULL()
    Inst%roughness_r     => NULL()

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
! !DESCRIPTION: Subroutine InstRemove removes an instance from the list of
! instances.
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

    ! Initialize
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
       IF ( ALLOCATED( Inst%HcoIDs ) ) THEN
          DEALLOCATE ( Inst%HcoIDs )
       ENDIF

       IF ( ALLOCATED( Inst%SpcScl ) ) THEN
          DEALLOCATE ( Inst%SpcScl )
       ENDIF

       IF ( ALLOCATED( Inst%SpcNames ) ) THEN
          DEALLOCATE ( Inst%SpcNames )
       ENDIF

       IF ( ALLOCATED( Inst%SpcScalFldNme ) ) THEN
          DEALLOCATE( Inst%SpcScalFldNme  )
       ENDIF
       
       ! ----------------------------------------------------------------
       ! Type specific initialization statements follow below
       ! ----------------------------------------------------------------
       IF ( ASSOCIATED( Inst%A_bare ) ) THEN
        DEALLOCATE(Inst%A_bare )
       ENDIF
       Inst%A_bare => NULL()

       IF ( ASSOCIATED( Inst%A_veg ) ) THEN
        DEALLOCATE(Inst%A_veg )
       ENDIF
       Inst%A_veg => NULL()

       IF ( ASSOCIATED( Inst%C_sah ) ) THEN
        DEALLOCATE(Inst%C_sah )
       ENDIF
       Inst%C_sah => NULL()

       IF ( ASSOCIATED( Inst%XLAI_t ) ) THEN
        DEALLOCATE(Inst%XLAI_t )
       ENDIF
       Inst%XLAI_t => NULL()

       IF ( ASSOCIATED( Inst%f_clay ) ) THEN
        DEALLOCATE(Inst%f_clay )
       ENDIF
       Inst%f_clay => NULL()

       IF ( ASSOCIATED( Inst%bulk_den ) ) THEN
        DEALLOCATE(Inst%bulk_den )
       ENDIF
       Inst%bulk_den => NULL()

       IF ( ASSOCIATED( Inst%poros ) ) THEN
        DEALLOCATE(Inst%poros )
       ENDIF
       Inst%poros => NULL()

       IF ( ASSOCIATED( Inst%roughness_r ) ) THEN
        DEALLOCATE(Inst%roughness_r )
       ENDIF
       Inst%roughness_r => NULL()

       ! ----------------------------------------------------------------
       ! Pop off instance from list
       ! ----------------------------------------------------------------
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
  
  SUBROUTINE CAL_THR_FRIC_VEL(HcoState, rho_a, f_clay, bulk_den, poros, theta, &
                             u_star_ft0, u_star_ft, u_star_it, u_star_st, RC)
    ! Description: calculate threshold friction velocities

    !----------------
    ! Arguments
    !----------------
    TYPE(HCO_State), POINTER :: HcoState
    REAL(hp),  INTENT(IN)  :: rho_a(HcoState%NX, HcoState%NY) ! surface air density [kg m-3]
    REAL(hp),  INTENT(IN)  :: f_clay(HcoState%NX, HcoState%NY) ! Soil clay fraction [unitless]
    REAL(hp),  INTENT(IN)  :: bulk_den(HcoState%NX, HcoState%NY) ! Bulk density of the topmost soil [kg m-3]
    REAL(hp),  INTENT(IN)  :: poros(HcoState%NX, HcoState%NY)  ! Soil porosity [unitless]
    REAL(hp),  INTENT(IN)  :: theta(HcoState%NX, HcoState%NY) ! Volumetric soil water content [unitless]
    REAL(hp),  INTENT(OUT) :: u_star_ft0(HcoState%NX, HcoState%NY) ! Dry fluid thershold friction velocity [m s-1]
    REAL(hp),  INTENT(OUT) :: u_star_ft(HcoState%NX, HcoState%NY) ! Wet fluid thershold friction velocity [m s-1]
    REAL(hp),  INTENT(OUT) :: u_star_it(HcoState%NX, HcoState%NY) ! Dynamic fluid thershold friction velocity [m s-1]
    REAL(hp),  INTENT(OUT) :: u_star_st(HcoState%NX, HcoState%NY) ! Standardized wet fluid thershold friction velocity [m s-1]
    INTEGER, INTENT(INOUT) :: RC

    !-----------------
    ! Local variables
    !-----------------
    INTEGER                :: I, J
    REAL(hp),  PARAMETER   :: A         = 0.0123_hp
    REAL(hp),  PARAMETER   :: gamma     = 1.65e-4_hp ! [kg s-2]
    REAL(hp),  PARAMETER   :: B_it      = 0.82_hp

    ! Gravimetric soil moisture [unitless]
    REAL(hp)               :: w(HcoState%NX, HcoState%NY)

    ! Threshols gravimetric soil moisture [unitlss]
    REAL(hp)               :: w_t(HcoState%NX, HcoState%NY)

    ! Factor by which threhold velocity increases due to soil wetness
    REAL(hp)               :: f_m(HcoState%NX, HcoState%NY)

    !=================================================================
    ! CAL_THR_FRIC_VEL begins here!
    !=================================================================

    DO J = 1, HcoState%NY
      DO I = 1, HcoState%NX
        ! Dry fluid threshold velocity [m s-1]: 
        ! calculate u_star_ft0 = sqrt(A * (rho_p * g * D_p + gamma / D_p) / rho_a)
        u_star_ft0(I,J) =  SQRT(A * (rho_p * HcoState%Phys%g0 * D_p + gamma / D_p) / rho_a(I,J)) ! [m s-1]

        ! Factor by which soil wetness enhancing threhold friction velocity
        ! calculate f_m = sqrt (1 + 1.21 * ((100 * (w - w_t)) ** 0.68)) for w > w_t; and f_m = 1 for w <= w_t
        !! calculate w = rho_w / rho_b * theta with additional 0.5 scaling 
        ! To prevent divided by 0 and here make the restriction stronger (> snow density)
        IF ((bulk_den(I,J) > 100.0_hp) .and. (theta > 1.0e-15_hp)) THEN
          w(I,J) = rho_w / (bulk_den(I,J)) * theta(I,J) * 0.5_hp
        ELSE 
          w(I,J) = 0.0_hp
        ENDIF

        !! calculate w_t = 0.01 * a * (17 * f_clay + 14 * f_clay ** 2) where a is a tuning factor and was set to be 1.0
        IF (f_clay(I,J) > 0.0_hp) THEN
          w_t(I,J) = 0.01_hp * (17.0_hp * f_clay(I,J) + 14.0_hp * (f_clay(I,J) ** 2.0_hp))
        ELSE 
          w_t(I,J) = 0.0_hp
        ENDIF

        ! calculate f_m [unitless]
        IF ( (w(I,J) > w_t(I,J)) .and. (w(I,J) > 0.0_hp) .and. (w_t(I,J) > 0.0_hp) ) THEN
          f_m(I,J) = SQRT(1.0_hp + 1.21_hp * ((100.0_hp * (w(I,J) - w_t(I,J)) ** 0.68_hp)))
        ELSE
          f_m(I,J) = 1.0_hp
        ENDIF
        
      ENDDO
    ENDDO

    ! Wet threshold friction velocity [m s-1]
    u_star_ft = u_star_ft0 * f_m

    ! Dynamic threshold friction velocity [m s-1]
    ! calculate u_star_it = B_it * u_star_ft0
    u_star_it = B_it * u_star_ft0

    ! Standardized wet fluid thershold friction velocity [m s-1]
    u_star_st = u_star_ft * SQRT(rho_a / rho_a0)

    ! Return w/ success
    RC = HCO_SUCCESS

    END SUBROUTINE CAL_THR_FRIC_VEL
  
  SUBROUTINE CAL_DRAG_PART(HcoState, z_0a, LAI, A_r, A_v, &
                          f_eff_r, f_eff_v, F_eff, RC)
    ! Description: calculate drag partioning effects due to rocks and vegetation

    !----------------
    ! Arguments
    !----------------
    TYPE(HCO_State), POINTER :: HcoState
    REAL(hp),  INTENT(IN)  :: z_0a(HcoState%NX, HcoState%NY) ! surface roughness length due to rocks [m]
    REAL(hp),  INTENT(IN)  :: LAI(HcoState%NX, HcoState%NY) ! Leaf area index [unitless]
    REAL(hp),  INTENT(IN)  :: A_r(HcoState%NX, HcoState%NY) ! The fraction of barren and sparsely vegetated land cover [unitless]
    REAL(hp),  INTENT(IN)  :: A_v(HcoState%NX, HcoState%NY) ! The fraction of short vegetation land cover [unitless]
    REAL(hp),  INTENT(OUT) :: f_eff_r(HcoState%NX, HcoState%NY) ! The drag partitioning effects due to rocks [unitless]
    REAL(hp),  INTENT(OUT) :: f_eff_v(HcoState%NX, HcoState%NY) ! The drag partitioning effects due to short vegetation [unitless]
    REAL(hp),  INTENT(OUT) :: F_eff(HcoState%NX, HcoState%NY) ! The total drag partitioning effects due to rocks and short vegetation [unitless]
    INTEGER, INTENT(INOUT) :: RC

    !-----------------
    ! Local variables
    !-----------------
    INTEGER                :: I, J
    ! parameters
    REAL(hp),  PARAMETER   :: b1        = 0.7_hp
    REAL(hp),  PARAMETER   :: b2        = 0.8_hp
    REAL(hp),  PARAMETER   :: X         = 10.0_hp ! [m]
    REAL(hp),  PARAMETER   :: f0        = 0.32_hp
    REAL(hp),  PARAMETER   :: c         = 4.8_hp

    ! Derived quantities
    ! smooth roughness length which quantifies the roughness of a bed of fine soil particles in the absence of roughness elements [m]
    REAL(hp),  PARAMETER   :: z_0s      = 2.0_hp * D_p / 30.0_hp

    ! variables
    REAL(hp)               :: K(HcoState%NX, HcoState%NY)

    DO J = 1, HcoState%NY
      DO I = 1, HcoState%NX

        ! calculate K = pi/2 * (1 / f_v - 1) = pi/2 * (LAI_thr / LAI - 1)
        K(I,J) = HcoState%Phys%PI / 2.0_hp * (LAI_thr / LAI(I,J) - 1.0_hp)

        IF (K(I,J) < 0.0_hp) THEN
          K(I,J) = 0.0_hp
        ENDIF

        ! Calculate drag partioning effects due to rocks:
        ! f_eff_r = 1 - ln(z_0a / z_0s) / ln(b1 * (X / z_0s) ** b2)
        IF (z_0a(I,J) > 0.0_hp) THEN
          f_eff_r(I,J) = 1.0d0 - LOG(z_0a(I,J) / z_0s) / LOG(b1 * (X / z_0s) ** b2)
        ELSE 
          f_eff_r(I,J) = 1.0_hp
        ENDIF

        IF ((f_eff_r(I,J) < 0.0_hp)) THEN
          f_eff_r(I,J) = 0.0_hp
        ELSEIF ((f_eff_r(I,J) > 1.0_hp) .or. (LAI(I,J) > LAI_thr)) THEN
          f_eff_r(I,J) = 1.0_hp
        ENDIF
        
        ! calculate drag partioning effects due to vegetation:
        ! f_eff_v = (K + f0 * c) / (K + c)
        f_eff_v(I,J) = (K(I,J) + f0 * c) / (K(I,J) + c)
        IF ((f_eff_v(I,J) < 0.0_hp)) THEN
          f_eff_v(I,J) = 0.0_hp
        ELSEIF ((f_eff_v(I,J) > 1.0_hp) .or. (LAI(I,J) < 1.0e-15_hp)) THEN
          f_eff_v(I,J) = 1.0_hp
        ENDIF

        ! calculate the weighted-mean drag partioning effects due to rocks and vegetation
        F_eff(I,J) = (A_r(I,J) * (f_eff_r(I,J) ** 3.0_hp) + A_v(I,J) * (f_eff_v(I,J) ** 3.0_hp)) ** (1.0_hp/3.0_hp)
        IF ((F_eff(I,J) < 0.0_hp)) THEN
          F_eff(I,J) = 0.0_hp
        ELSEIF (F_eff(I,J) > 1.0_hp) THEN
          F_eff(I,J) = 1.0_hp
        ENDIF
      ENDDO
    ENDDO

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE CAL_DRAG_PART

  SUBROUTINE CAL_INTERM_FACTOR(HcoState, u_star_ft, u_star_it, u_star_s, &
                               PBLH, rho_a, T2M, u_star, HFLUX, eta, RC)
    ! Description: calculate intermittency factor due to turbulence

    !----------------
    ! Arguments
    !----------------
    TYPE(HCO_State), POINTER :: HcoState
    REAL(hp),  INTENT(IN) :: u_star_ft(HcoState%NX, HcoState%NY) ! Wet fluid thershold friction velocity [m s-1]
    REAL(hp),  INTENT(IN) :: u_star_it(HcoState%NX, HcoState%NY) ! Dynamic fluid thershold friction velocity [m s-1]
    REAL(hp),  INTENT(IN) :: u_star_s(HcoState%NX, HcoState%NY)  ! Soil friction velocity [m s-1]
    REAL(hp),  INTENT(IN) :: PBLH(HcoState%NX, HcoState%NY) ! Planetary boundary layer height [m]
    REAL(hp),  INTENT(IN) :: rho_a(HcoState%NX, HcoState%NY) ! Surface air density [kg m-3]
    REAL(hp),  INTENT(IN) :: T2M(HcoState%NX, HcoState%NY) ! Temperature at 10 meter [K]
    REAL(hp),  INTENT(IN) :: u_star(HcoState%NX, HcoState%NY) ! Friction velocity [m s-1]
    REAL(hp),  INTENT(IN) :: HFLUX(HcoState%NX, HcoState%NY) ! Sensible heat flux [W m-2]
    REAL(hp),  INTENT(OUT) :: eta(HcoState%NX, HcoState%NY) ! Intermittency factor with values in [0,1] [unitless]
    INTEGER, INTENT(INOUT) :: RC

    !-----------------
    ! Local variables
    !-----------------
    INTEGER                :: I, J
    ! parameters
    REAL(hp),  PARAMETER   :: z_sal        = 0.1_hp  ! Saltation height [m]
    REAL(hp),  PARAMETER   :: z_0a_c       = 1.0e-4_hp ! take as constant for simplicity [m]

    ! variables
    ! derived friction velocities at saltation height [m s-1]
    REAL(hp)               :: u_ft(HcoState%NX, HcoState%NY)
    REAL(hp)               :: u_it(HcoState%NX, HcoState%NY)
    REAL(hp)               :: u_s(HcoState%NX, HcoState%NY)

    REAL(hp)               :: L(HcoState%NX, HcoState%NY) ! Monin-bukhov length [m]
    REAL(hp)               :: sigma(HcoState%NX, HcoState%NY) ! standard deviation of u_s due to turbulence [m s-1]
    REAL(hp)               :: alpha(HcoState%NX, HcoState%NY)
    REAL(hp)               :: P_ft(HcoState%NX, HcoState%NY)
    REAL(hp)               :: P_it(HcoState%NX, HcoState%NY)
    REAL(hp)               :: eta_temp(HcoState%NX, HcoState%NY) ! Intermittency factor with values in [0,1] [unitless]
    
    DO J = 1, HcoState%NY
      DO I = 1, HcoState%NX

        u_ft(I,J) = u_star_ft(I,J) / CST_VON_KRM * LOG(z_sal / z_0a_c)
        u_it(I,J) = u_star_it(I,J) / CST_VON_KRM * LOG(z_sal / z_0a_c)
        u_s(I,J) = u_star_s(I,J) / CST_VON_KRM * LOG(z_sal / z_0a_c)
        
        ! calculate sigma for instantaneous soil friction velocity:
        ! sigma = u_star_s * (12 - 0.5 * PBLH / L) ** (1/3) for (12 - 0.5 * PBLH / L)>=0
        !! calculate the Monin-bukhov length L = - rho_a * cp * T * u_star ** 3 / (k * g * H)
        L(I,J) = - rho_a(I,J) * SPC_HEAT_DRY_AIR * T2M(I,J) * u_star(I,J) ** 3.0_hp / (CST_VON_KRM * HcoState%Phys%g0 * HFLUX(I,J))
        sigma(I,J) = u_star_s(I,J) * ((12.0_hp - 0.5_hp * PBLH(I,J) / L(I,J)) ** (1.0_hp/3.0_hp))
        
        ! calculate the fluid thresholf crossing fraction: 
        ! alpha = (exp ((u_ft ** 2 - u_it ** 2 - 2 * u_s * (u_ft - u_it)) / (2 * sigma ** 2)) + 1) ** (-1)
        alpha(I,J) = (EXP (((u_ft(I,J) ** 2.0_hp) - (u_it(I,J) ** 2.0_hp) - 2.0_hp * u_s(I,J) * (u_ft(I,J) - u_it(I,J))) / (2.0_hp * (sigma(I,J) ** 2.0_hp))) + 1.0_hp) ** (-1.0_hp)

        ! calculate the cumulative probability that instananeous soil friction velocity does not exceed 
        ! the wet fluid threshold u_ft or the impact threshold u_it of P_ft or P_it
        ! P_ft = 0.5 * (1 + erf((u_ft - u_s) / (sqrt(2) * sigma))); P_it = 0.5 * (1 + erf((u_it - u_s) / (sqrt(2) * sigma)))
        P_ft(I,J) = 0.5_hp * (1.0_hp + ERF((u_ft(I,J) - u_s(I,J)) / (SQRT(2.0_hp) * sigma(I,J))))
        P_it(I,J) = 0.5_hp * (1.0_hp + ERF((u_it(I,J) - u_s(I,J)) / (SQRT(2.0_hp) * sigma(I,J))))

        ! calculate intermittency factor: eta = 1 - P_ft + alpha * (P_ft - P_it)
        eta_temp(I,J) = 1.0_hp - P_ft(I,J) + alpha(I,J) * (P_ft(I,J) - P_it(I,J))
        ! if eta is out of range of [0,1], then skip eta multipling by making the value as 1
        IF ((eta_temp(I,J) > 0.0_hp) .and. (eta_temp(I,J) < 1.0_hp)) THEN
          eta(I,J) = eta_temp(I,J)
        ELSE 
          eta(I,J) = 1.0_hp
        ENDIF
      ENDDO
    ENDDO

    ! Return w/ success
    RC = HCO_SUCCESS
  END SUBROUTINE CAL_INTERM_FACTOR

  SUBROUTINE CAL_DUSTL23M_EmisFlux(HcoState, ExtState, Inst, DUST_EMIS_FLUX, RC)
    ! Description: calculate DustL23M total emission flux [kg m-2 s-1]
    !----------------
    ! Arguments
    !----------------
    TYPE(HCO_State), POINTER      :: HcoState    ! Hemco state
    TYPE(Ext_State), POINTER      :: ExtState    ! Module options
    TYPE(MyInst),    POINTER      :: Inst        ! Specific instances of DustL23
    REAL(hp),  INTENT(OUT)        :: DUST_EMIS_FLUX(HcoState%NX, HcoState%NY) ! Total dust emission flux [kg m-2 s-1]
    INTEGER, INTENT(INOUT)        :: RC

    ! Local variables
    INTEGER                 :: I, J
    REAL(hp)                :: snowdep(HcoState%NX, HcoState%NY)    ! Snow depth [m]
    REAL(hp)                :: A_snow(HcoState%NX, HcoState%NY)     ! Fraction of snow cover [unitless]
    REAL(hp)                :: u_star_ft0(HcoState%NX, HcoState%NY) ! Dry fluid thershold friction velocity [m s-1]
    REAL(hp)                :: u_star_ft(HcoState%NX, HcoState%NY)  ! Wet fluid thershold friction velocity [m s-1]
    REAL(hp)                :: u_star_it(HcoState%NX, HcoState%NY)  ! Dynamic fluid thershold friction velocity [m s-1]
    REAL(hp)                :: u_star_st(HcoState%NX, HcoState%NY)  ! Standardized wet fluid thershold friction velocity [m s-1]

    REAL(hp)                :: f_eff_r(HcoState%NX, HcoState%NY) ! The drag partitioning effects due to rocks [unitless]
    REAL(hp)                :: f_eff_v(HcoState%NX, HcoState%NY) ! The drag partitioning effects due to short vegetation [unitless]
    REAL(hp)                :: F_eff(HcoState%NX, HcoState%NY) ! The total drag partitioning effects due to rocks and short vegetation [unitless]
    
    REAL(hp)                :: eta(HcoState%NX, HcoState%NY) ! Intermittency factor with values in [0,1] [unitless]
    REAL(hp)                :: DUST_EMIS_FLUX_Tmp(HcoState%NX, HcoState%NY) ! Total dust emission flux [kg m-2 s-1]

    CHARACTER(LEN=255)    :: SUBLOC
    ! empirical constants
    REAL(hp),  PARAMETER    :: C_tune         = 2.832e-3_hp    ! [unitless]
    REAL(hp),  PARAMETER    :: C_d0           = 4.4e-5_hp  ! [unitless]
    REAL(hp),  PARAMETER    :: C_e            = 2.0_hp    ! [unitless]
    REAL(hp),  PARAMETER    :: C_kappa        = 2.7_hp   ! [unitless]

    ! other constants
    REAL(hp),  PARAMETER    :: u_star_st0     = 0.16_hp  ! [m s-1]
    
    REAL(hp)        :: rho_a(HcoState%NX, HcoState%NY) ! surface air density [kg m-3]
    REAL(hp)        :: T2M(HcoState%NX, HcoState%NY)   ! 2-m temperature [K]
    REAL(hp)        :: TS(HcoState%NX, HcoState%NY)    ! Surface temperature [K]
    REAL(hp)        :: PS(HcoState%NX, HcoState%NY)    ! Surface pressure [Pa]
    
    REAL(hp)        :: theta(HcoState%NX, HcoState%NY)          ! Volumetric soil moisture [unitless]
    REAL(hp)        :: C_d(HcoState%NX, HcoState%NY)            ! Soil erodibility coefficient [unitless]
    REAL(hp)        :: f_bare(HcoState%NX, HcoState%NY)         ! [unitless]
    REAL(hp)        :: u_star_s(HcoState%NX, HcoState%NY)       ! Soil surface friction velocity [m s-1]
    REAL(hp)        :: kappa(HcoState%NX, HcoState%NY)          ! Fragmentaion exponent [unitless]
    REAL(hp)        :: u_star_t(HcoState%NX, HcoState%NY)       ! Thershold friction velocity used [m s-1]
    
    DO J = 1, HcoState%NY
      DO I = 1, HcoState%NX
        TS(I,J) = ExtState%TS%Arr%Val(I,J)
        T2M(I,J) = ExtState%T2M%Arr%Val(I,J)
        PS(I,J) = ExtState%PS%Arr%Val(I,J) * 100.0_hp ! convert hPa to Pa
        rho_a(I,J) = PS(I,J) * (HcoState%Phys%AIRMW * 1.0e-3_hp) / (HcoState%Phys%RSTARG * T2M(I,J))

        snowdep(I,J) = ExtState%SNOWHGT%Arr%Val(I,J) / 1000 * (1000 / 100) ! convert kg H2O / m2 to m
        A_snow(I,J) = snowdep(I,J) / snowdep_thr
        IF ((A_snow(I,J) > 1.0_hp) .or. (TS(I,J) < T0)) THEN
          A_snow(I,J) = 1.0_hp
        ENDIF
      ENDDO
    ENDDO

    theta = ExtState%GWETTOP%Arr%Val * Inst%poros
    SUBLOC = 'CAL_THR_FRIC_VEL'
    CALL CAL_THR_FRIC_VEL(HcoState, rho_a, Inst%f_clay, Inst%bulk_den, Inst%poros, theta, &
                          u_star_ft0, u_star_ft, u_star_it, u_star_st, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
      CALL HCO_ERROR( 'ERROR', RC, THISLOC=SUBLOC )
      RETURN
    ENDIF

    SUBLOC = 'CAL_DRAG_PART'
    CALL CAL_DRAG_PART(HcoState, Inst%roughness_r, Inst%XLAI_t, Inst%A_bare, Inst%A_veg, &
                      f_eff_r, f_eff_v, F_eff, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
      CALL HCO_ERROR( 'ERROR', RC, THISLOC=SUBLOC )
      RETURN
    ENDIF
        
    u_star_s = ExtState%USTAR%Arr%Val * F_eff

    SUBLOC = 'CAL_INTERM_FACTOR'
    CALL CAL_INTERM_FACTOR(HcoState, u_star_ft, u_star_it, u_star_s, &
                          ExtState%PBLH%Arr%Val, rho_a, T2M, ExtState%USTAR%Arr%Val, ExtState%HFLUX%Arr%Val, eta, RC)
    IF ( RC /= HCO_SUCCESS ) THEN
      CALL HCO_ERROR( 'ERROR', RC, THISLOC=SUBLOC )
      RETURN
    ENDIF

    DO J = 1, HcoState%NY
      DO I = 1, HcoState%NX
        ! calculate C_d = C_d0 * exp (- C_e * (u_star_st - u_star_st0) / u_star_st0)
        C_d(I,J) = C_d0 * EXP (- C_e * (u_star_st(I,J) - u_star_st0) / u_star_st0)

        ! calculate f_bare = A_bare * (1 - A_snow) * (1 - LAI / LAI_thr) for LAI <= LAI_thr, and f_bare = 0 for LAI > LAI_thr
        f_bare(I,J) = Inst%A_bare(I,J) * (1.0_hp - A_snow(I,J)) * (1.0_hp - Inst%XLAI_t(I,J) / LAI_thr)
        IF (Inst%XLAI_t(I,J) > LAI_thr) THEN
          f_bare(I,J) = 0.0_hp
        ENDIF

        kappa(I,J) = C_kappa * (u_star_st(I,J) - u_star_st0) / u_star_st0
        IF (kappa(I,J)>3.0_hp) THEN
          kappa(I,J) = 3.0_hp
        ENDIF
        
        u_star_t(I,J) = u_star_it(I,J)
        DUST_EMIS_FLUX_Tmp(I,J) = eta(I,J) * C_tune * Inst%C_sah(I,J) * C_d(I,J) * f_bare(I,J) * \
            rho_a(I,J) * ((u_star_s(I,J) ** 2.0_hp) - (u_star_t(I,J) ** 2.0_hp)) / u_star_st(I,J) * \
            ((u_star_s(I,J) / u_star_t(I,J)) ** kappa(I,J))
        IF ((DUST_EMIS_FLUX_Tmp(I,J) < 0.0_hp) .or. (u_star_s(I,J) .LE. u_star_t(I,J))) THEN
          DUST_EMIS_FLUX_Tmp(I,J) = 0.0_hp
        ENDIF

        IF (DUST_EMIS_FLUX_Tmp(I,J) > 0.0_hp) THEN
          DUST_EMIS_FLUX(I,J) = DUST_EMIS_FLUX_Tmp (I,J)
        ENDIF
      ENDDO
    ENDDO

    PRINT*, '### eta Min, Max: ', MINVAL( eta, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( eta, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### Feff Min, Max: ', MINVAL( F_eff, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( F_eff, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### u_star_s Min, Max: ', MINVAL( u_star_s, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( u_star_s, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### Cd Min, Max: ', MINVAL( C_d, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( C_d, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### u_star_ft0 Min, Max: ', MINVAL( u_star_ft0, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( u_star_ft0, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### bulk_den Min, Max: ', MINVAL( Inst%bulk_den, mask=((Inst%C_sah<1.0_hp) .and. (Inst%bulk_den>10.0_hp)) ), MAXVAL( Inst%bulk_den, mask=((Inst%C_sah<1.0_hp) .and. (Inst%bulk_den>10.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### theta Min, Max: ', MINVAL( theta, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( theta, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### poros Min, Max: ', MINVAL( Inst%poros, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( Inst%poros, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### u_star_ft Min, Max: ', MINVAL( u_star_ft, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( u_star_ft, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    PRINT*, '### u_star_st Min, Max: ', MINVAL( u_star_st, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) ), MAXVAL( u_star_st, mask=((Inst%C_sah<1.0_hp) .and. (DUST_EMIS_FLUX > 0.0_hp)) )
    CALL FLUSH( 6 )
    ! Return w/ success
    RC = HCO_SUCCESS
  END SUBROUTINE CAL_DUSTL23M_EmisFlux
!EOC
END MODULE HCOX_DustL23M_Mod