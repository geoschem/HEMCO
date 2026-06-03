#if defined(MODEL_CESM)
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_pio_mod.F90
!
! !DESCRIPTION: Module HCO\_PIO\_MOD contains routines to read data from
! netCDF files using PIO (ParallelIO) for CESM.
! This is a PIO replacement for the read-related functions in hco\_ncdf\_mod.F90.
! The write functions have not been implemented.
!
! This module is only valid for HEMCO_CESM, but is included in src/Core
! in order to be usable for hcoio_read_pio_mod.F90 as a dependency.
!\\
!\\
! !INTERFACE:
!
MODULE HCO_PIO_MOD
!
! !USES:
!
  USE pio
  USE cam_pio_utils, ONLY : cam_pio_openfile
  USE HCO_CHARPAK_MOD,   ONLY : TRANLC
  USE HCO_JULDAY_MOD,    ONLY : JULDAY, CALDATE

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: NC_OPEN
  PUBLIC  :: NC_CLOSE
  PUBLIC  :: NC_READ_TIME
  PUBLIC  :: NC_READ_TIME_YYYYMMDDhhmm
  PUBLIC  :: NC_READ_VAR
  PUBLIC  :: NC_READ_ARR
  PUBLIC  :: NC_GET_REFDATETIME
  PUBLIC  :: NC_GET_GRID_EDGES
  PUBLIC  :: NC_GET_SIGMA_LEVELS
  PUBLIC  :: GET_TAU0
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: NC_READ_VAR_SP
  PRIVATE :: NC_READ_VAR_DP
  PRIVATE :: NC_READ_VAR_CORE
  PRIVATE :: NC_GET_GRID_EDGES_SP
  PRIVATE :: NC_GET_GRID_EDGES_DP
  PRIVATE :: NC_GET_GRID_EDGES_C
  PRIVATE :: NC_GET_SIGMA_LEVELS_SP
  PRIVATE :: NC_GET_SIGMA_LEVELS_DP
  PRIVATE :: NC_GET_SIGMA_LEVELS_C
  PRIVATE :: NC_GET_SIG_FROM_HYBRID
  PRIVATE :: GetVarFromFormula
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE INTERFACES:
!
  INTERFACE NC_READ_VAR
     MODULE PROCEDURE NC_READ_VAR_SP
     MODULE PROCEDURE NC_READ_VAR_DP
  END INTERFACE NC_READ_VAR

  INTERFACE NC_GET_GRID_EDGES
     MODULE PROCEDURE NC_GET_GRID_EDGES_SP
     MODULE PROCEDURE NC_GET_GRID_EDGES_DP
  END INTERFACE NC_GET_GRID_EDGES

  INTERFACE NC_GET_SIGMA_LEVELS
     MODULE PROCEDURE NC_GET_SIGMA_LEVELS_SP
     MODULE PROCEDURE NC_GET_SIGMA_LEVELS_DP
  END INTERFACE NC_GET_SIGMA_LEVELS

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Open
!
! !DESCRIPTION: Simple wrapper routine to open the given netCDF file
!  using PIO.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_OPEN( FileName, fID )
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN)     :: FileName
!
! !OUTPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    !=================================================================
    ! NC_OPEN begins here
    !=================================================================

    ! Open netCDF file via PIO
    CALL cam_pio_openfile( fId, TRIM( FileName ), PIO_NOWRITE )

    ! HEMCO's netCDF wrapper routines inspect PIO return codes directly
    ! (netCDF-style) to detect optional dimensions, variables and attributes.
    ! PIO's default error handler (PIO_INTERNAL_ERROR) aborts the run on any
    ! netCDF error (e.g. a missing optional attribute), so switch the I/O
    ! system to PIO_BCAST_ERROR, which returns the error code instead. The
    ! default handler is restored in NC_CLOSE.
    CALL pio_seterrorhandling( fID, PIO_BCAST_ERROR )

  END SUBROUTINE NC_OPEN
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Close
!
! !DESCRIPTION: Simple wrapper routine to close the given file descriptor.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_CLOSE( fID )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    !=================================================================
    ! NC_CLOSE begins here
    !=================================================================

    ! Restore PIO's default (abort-on-error) handler that was switched to
    ! PIO_BCAST_ERROR in NC_OPEN, so the shared I/O system is left in its
    ! expected state for the rest of the model. (PIO update)
    CALL pio_seterrorhandling( fID, PIO_INTERNAL_ERROR )

    CALL pio_closefile( fID )

  END SUBROUTINE NC_CLOSE
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Read_Time
!
! !DESCRIPTION: Subroutine NC\_READ\_TIME reads the time variable of the
! given fID and returns the time slices and unit.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_READ_TIME( fID,     nTime,        timeUnit, &
                           timeVec, timeCalendar, RC       )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT)           :: fID
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT)            :: nTime
    CHARACTER(LEN=*), INTENT(  OUT)            :: timeUnit
    REAL*8,           POINTER,       OPTIONAL  :: timeVec(:)
    CHARACTER(LEN=*), INTENT(  OUT), OPTIONAL  :: timeCalendar
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    LOGICAL                :: hasTime
    CHARACTER(LEN=255)     :: v_name             ! netCDF variable name
    CHARACTER(LEN=255)     :: a_name             ! netCDF attribute name
    INTEGER                :: st1d(1), ct1d(1)   ! For 1D arrays

    ! Arrays
    REAL*8 , ALLOCATABLE   :: tmpTime(:)

    ! PIO variables
    INTEGER                :: pio_varid
    INTEGER                :: pio_ierr
    INTEGER                :: tmp_dimid, tmp_ierr
    INTEGER                :: tmp_varid, tmp_ierr_inner
    INTEGER                :: tmp_len

    !=================================================================
    ! NC_READ_TIME begins here
    !=================================================================

    ! Init
    RC      = 0
    nTime   = 0
    hasTime = .FALSE.

    ! Variable name
    v_name = "time"

    ! Check if dimension "time" exist
    tmp_ierr = pio_inq_dimid(fID, TRIM(v_name), tmp_dimid)
    hasTime = (tmp_ierr == PIO_NOERR)

    ! If time dim not found, also check for dimension "date"
    IF ( .NOT. hasTime ) THEN
       v_name   = "date"
       tmp_ierr = pio_inq_dimid(fID, TRIM(v_name), tmp_dimid)
       hasTime = (tmp_ierr == PIO_NOERR)
    ENDIF

    ! Return here if no time variable defined
    IF ( .NOT. hasTime ) RETURN

    ! Get dimension length
    tmp_ierr = pio_inq_dimid(fID, TRIM(v_name), tmp_dimid)
    tmp_ierr = pio_inq_dimlen(fID, tmp_dimid, nTime)

    ! Read time/date units attribute
    a_name = "units"
    pio_ierr = pio_inq_varid(fID, TRIM(v_name), pio_varid)
    pio_ierr = pio_get_att(fID, pio_varid, TRIM(a_name), timeUnit)

    ! Read time vector from file.
    IF ( PRESENT(timeVec) ) THEN
       IF ( ASSOCIATED(timeVec) ) DEALLOCATE ( timeVec)
       ALLOCATE ( tmpTime(nTime) )
       ALLOCATE ( timeVec(nTime) )
       st1d = (/ 1     /)
       ct1d = (/ nTime /)
       pio_ierr = pio_inq_varid(fID, TRIM(v_name), pio_varid)
       pio_ierr = pio_get_var(fID, pio_varid, st1d, ct1d, tmpTime)
       timevec(:) = tmpTime
       DEALLOCATE(tmpTime)
    ENDIF

    ! Read calendar attribute
    IF ( PRESENT( timeCalendar ) ) THEN

       ! We now get the status variable RC.  This will allow program
       ! flow to continue if the "time:calendar" attribute is not found.
       tmp_ierr_inner = pio_inq_varid(fID, TRIM(v_name), tmp_varid)
       IF (tmp_ierr_inner /= PIO_NOERR) THEN
          RC = -1
       ELSE
          tmp_ierr_inner = pio_get_att(fID, tmp_varid, 'calendar', timeCalendar)
          IF (tmp_ierr_inner /= PIO_NOERR) THEN
             RC = -1
          ELSE
             RC = 0
          ENDIF
       ENDIF

       ! If "time:calendar" is found, then throw an error for
       ! climatological calendars without leap years.
       IF ( RC == 0 ) THEN
        SELECT CASE( TRIM( timeCalendar ) )
          CASE( '360_day', '365_day', '366_day', 'all_leap',                 &
                'allleap', 'no_leap', 'noleap'                              )
             WRITE( 6, '(/,a)' ) REPEAT( '=', 79 )
             WRITE( 6, '(a  )' ) 'HEMCO does not support calendar type '  // &
                                 TRIM( timeCalendar )
             WRITE( 6, '(/,a)' )  'HEMCO supports the following calendars:'
             WRITE( 6, '(a)'   )  ' - standard (i.e. mixed gregorian/julian)'
             WRITE( 6, '(a)'   )  ' - gregorian'
             WRITE( 6, '(a,/)' ) REPEAT( '=', 79 )
             RC = -1
          CASE DEFAULT
             ! Do nothing
        END SELECT
       ENDIF

       ! Reset RC so that we won't halt execution elsewhere
       RC = 0
    ENDIF

  END SUBROUTINE NC_READ_TIME
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Read_Var_Sp
!
! !DESCRIPTION: Subroutine NC\_READ\_VAR\_SP reads the given variable from the
! given fID and returns the corresponding variable values and units.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_READ_VAR_SP( fID, Var, nVar, varUnit, varVec, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT)           :: fID
    CHARACTER(LEN=*), INTENT(IN   )            :: var
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT)            :: nVar
    CHARACTER(LEN=*), INTENT(  OUT)            :: varUnit
    REAL*4,           POINTER                  :: varVec(:)
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    CALL NC_READ_VAR_CORE( fID, Var, nVar, varUnit, varVecSp=varVec, RC=RC )

  END SUBROUTINE NC_READ_VAR_SP
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Read_Var_Dp
!
! !DESCRIPTION: Subroutine NC\_READ\_VAR\_DP reads the given variable from the
! given fID and returns the corresponding variable values and units.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_READ_VAR_DP( fID, Var, nVar, varUnit, varVec, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT)           :: fID
    CHARACTER(LEN=*), INTENT(IN   )            :: var
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT)            :: nVar
    CHARACTER(LEN=*), INTENT(  OUT)            :: varUnit
    REAL*8,           POINTER                  :: varVec(:)
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    CALL NC_READ_VAR_CORE( fID, Var, nVar, varUnit, varVecDp=varVec, RC=RC )

  END SUBROUTINE NC_READ_VAR_DP
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Read_Var_Core
!
! !DESCRIPTION: Subroutine NC\_READ\_VAR\_CORE reads the given variable from the
! given fID and returns the corresponding variable values and units.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_READ_VAR_CORE( fID, Var, nVar, varUnit, varVecDp, varVecSp, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT)           :: fID
    CHARACTER(LEN=*), INTENT(IN   )            :: var
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT)            :: nVar
    CHARACTER(LEN=*), INTENT(  OUT)            :: varUnit
    REAL*4,           POINTER,       OPTIONAL  :: varVecSp(:)
    REAL*8,           POINTER,       OPTIONAL  :: varVecDp(:)
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL                :: hasVar
    CHARACTER(LEN=255)     :: v_name             ! netCDF variable name
    CHARACTER(LEN=255)     :: a_name             ! netCDF attribute name
    CHARACTER(LEN=255)     :: a_val              ! netCDF attribute value
    INTEGER                :: a_type             ! netCDF attribute type
    INTEGER                :: st1d(1), ct1d(1)   ! For 1D arrays
    INTEGER                :: I

    ! PIO variables
    INTEGER                :: pio_varid
    INTEGER                :: pio_ierr
    INTEGER                :: tmp_dimid, tmp_ierr
    INTEGER                :: tmp_varid
    INTEGER(PIO_OFFSET_KIND) :: tmp_len

    !=================================================================
    ! NC_READ_VAR_CORE begins here
    !=================================================================

    ! Init
    RC      = 0
    nVar    = 0
    hasVar  = .FALSE.

    ! Variable name
    v_name = var

    ! Check if variable exists (check dimension)
    tmp_ierr = pio_inq_dimid(fID, TRIM(v_name), tmp_dimid)
    hasVar = (tmp_ierr == PIO_NOERR)

    ! Return here if variable not defined
    IF ( .NOT. hasVar ) RETURN

    ! Get dimension length
    tmp_ierr = pio_inq_dimid(fID, TRIM(v_name), tmp_dimid)
    tmp_ierr = pio_inq_dimlen(fID, tmp_dimid, nVar)

    ! Read vector from file.
    IF ( PRESENT(VarVecSp) ) THEN
       IF ( ASSOCIATED( VarVecSp ) ) DEALLOCATE(VarVecSp)
       ALLOCATE ( VarVecSp(nVar) )
       st1d = (/ 1    /)
       ct1d = (/ nVar /)
       pio_ierr = pio_inq_varid(fID, TRIM(v_name), pio_varid)
       pio_ierr = pio_get_var(fID, pio_varid, st1d, ct1d, VarVecSp)
    ENDIF
    IF ( PRESENT(VarVecDp) ) THEN
       IF ( ASSOCIATED( VarVecDp ) ) DEALLOCATE(VarVecDp)
       ALLOCATE ( VarVecDp(nVar) )
       st1d = (/ 1    /)
       ct1d = (/ nVar /)
       pio_ierr = pio_inq_varid(fID, TRIM(v_name), pio_varid)
       pio_ierr = pio_get_var(fID, pio_varid, st1d, ct1d, VarVecDp)
    ENDIF

    ! Read units attribute. If unit attribute does not exist, return
    ! empty string (dimensionless vertical coordinates do not require
    ! a units attribute).
    a_name  = "units"
    tmp_ierr = pio_inq_varid(fID, TRIM(v_name), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       hasVar = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       hasVar = (tmp_ierr == PIO_NOERR)
    ENDIF
    IF ( .NOT. hasVar ) THEN
       varUnit = ''
    ELSE
       pio_ierr = pio_inq_varid(fID, TRIM(v_name), pio_varid)
       pio_ierr = pio_get_att(fID, pio_varid, TRIM(a_name), varUnit)

       ! Check if the last character of VarUnit is the ASCII null character
       ! ("\0", ASCII value = 0), which is used to denote the end of a string.
       ! The ASCII null character may be introduced if the netCDF file was
       ! written using a language other than Fortran.  The compiler might
       ! interpret the null character as part of the string instead of as
       ! an empty space.  If the null space is there, then replace it with
       ! a Fortran empty string value (''). (bmy, 7/17/18)
       I = LEN_TRIM( VarUnit )
       IF ( ICHAR( VarUnit(I:I) ) == 0 ) THEN
          VarUnit(I:I) = ''
       ENDIF
    ENDIF

  END SUBROUTINE NC_READ_VAR_CORE
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Read_Arr
!
! !DESCRIPTION: Routine NC\_READ\_ARR reads variable ncVar into a 4-D array
! (lon,lat,lev,time). Domain boundaries can be provided by input arguments
! lon1,lon2, lat1,lat2, lev1,lev2, and time1,time2. The level and time bounds
! are optional and can be set to zero (lev1=0 and/or time1=0) for data with
! undefined level/time coordinates.
!\\
!\\
! The default behavior for time slices is to read all slices (time1:time2),
! and pass all of them to the output array. It is also possible to assign
! specific weights (wgt1 and wgt2) to the two time slices time1 and time2,
! respectively. In this case, only those two slices will be read and merged
! using the given weights. The output array will then contain only one time
! dimension. Negative weights are currently not supported and will be ignored,
! e.g. providing negative weights has the same effect as providing no weights
! at all.
!\\
!\\
! If the passed variable contains attribute names `offset` and/or
! `scale\_factor`, those operations will be applied to the data array
! before returning it.
!\\
!\\
! Missing values in the netCDF file are replaced with value 'MissVal'
! (default = 0). Currently, the routine identifies attributes 'missing\_value'
! and '\_FillValue' as missing values.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_READ_ARR( fID,    ncVar,   lon1,    lon2,  lat1,  &
                          lat2,   lev1,    lev2,    time1, time2, &
                          ncArr,  VarUnit, MissVal, wgt1,  wgt2,  &
                          ArbIdx, RC                               )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT)        :: fID
    CHARACTER(LEN=*), INTENT(IN)            :: ncVar        ! variable to read
    INTEGER,          INTENT(IN)            :: lon1,  lon2
    INTEGER,          INTENT(IN)            :: lat1,  lat2
    INTEGER,          INTENT(IN)            :: lev1,  lev2
    INTEGER,          INTENT(IN)            :: time1, time2
    REAL*4,           INTENT(IN ), OPTIONAL :: MissVal
    REAL*4,           INTENT(IN ), OPTIONAL :: wgt1
    REAL*4,           INTENT(IN ), OPTIONAL :: wgt2
    INTEGER,          INTENT(IN ), OPTIONAL :: ArbIdx      ! Index of arbitrary additional dimension (-1 if none)
!
! !OUTPUT PARAMETERS:
!
    ! Array to write data
    REAL*4,           POINTER               :: ncArr(:,:,:,:)

    ! Optional output
    CHARACTER(LEN=*), INTENT(OUT), OPTIONAL :: VarUnit
!
! !INPUT/OUTPUT PARAMETERS:
!
    ! Error handling
    INTEGER,          INTENT(INOUT)         :: RC
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    !=================================================================
    ! Variable declarations
    !=================================================================

    ! Data arrays
    CHARACTER(LEN=255)     :: v_name    ! netCDF variable name
    CHARACTER(LEN=255)     :: a_name    ! netCDF attribute name
    CHARACTER(LEN=255)     :: a_val     ! netCDF attribute value
    INTEGER                :: a_type    ! netCDF attribute type
    REAL*8                 :: corr      ! netCDF attribute value

    ! Arrays for netCDF start and count values
    INTEGER                :: I, nRead, l1, l2
    INTEGER                :: ndims
    INTEGER                :: nlon,  nlat, nlev, ntime, arbdim
    INTEGER                :: nclev, nctime
    INTEGER                :: s1, s2, s3, s4, s5
    INTEGER                :: n1, n2, n3, n4, n5
    INTEGER                :: nt, st, tdim, sti, nti
    INTEGER                :: st2d(2), ct2d(2)   ! For 2D arrays
    INTEGER                :: st3d(3), ct3d(3)   ! For 3D arrays
    INTEGER                :: st4d(4), ct4d(4)   ! For 4D arrays
    INTEGER                :: st5d(5), ct5d(5)   ! For 5D arrays

    ! Temporary arrays
    REAL*4, ALLOCATABLE    :: TMPARR_5D(:,:,:,:,:)
    REAL*4, ALLOCATABLE    :: WGTARR_5D(:,:,:,:,:)
    REAL*4, ALLOCATABLE    :: TMPARR_4D(:,:,:,:)
    REAL*4, ALLOCATABLE    :: WGTARR_4D(:,:,:,:)
    REAL*4, ALLOCATABLE    :: TMPARR_3D(:,:,:)
    REAL*4, ALLOCATABLE    :: WGTARR_3D(:,:,:)
    REAL*4, ALLOCATABLE    :: TMPARR_2D(:,:)

    ! Logicals
    LOGICAL                :: FlipZ
    LOGICAL                :: ReadAtt

    ! Missing value
    REAL*8                 :: miss8
    REAL*4                 :: miss4
    REAL*4                 :: MissValue

    ! Weights
    LOGICAL                :: ApplyWeights
    REAL*4                 :: weight1, weight2

    ! For error handling
    CHARACTER(LEN=255)     :: LOC, MSG

    ! PIO variables
    INTEGER                :: pio_varid
    INTEGER                :: pio_ierr
    INTEGER                :: tmp_dimid, tmp_ierr
    INTEGER                :: tmp_varid
    INTEGER(PIO_OFFSET_KIND) :: tmp_len

    !=================================================================
    ! NC_READ_ARR begins here
    !=================================================================

    !-----------------------------------------------------------------
    ! Initialize
    !-----------------------------------------------------------------

    ! For error handling
    LOC = 'NC_READ_ARR ("hco_pio_mod.F90")'

    ! Eventually deallocate output array
    IF ( ASSOCIATED ( ncArr ) ) DEALLOCATE ( ncArr )

    ! weights to be applied to time1 and time2 (if any):
    weight1 = -999.0
    weight2 = -999.0
    IF(PRESENT(wgt1)) weight1 = wgt1
    IF(PRESENT(wgt2)) weight2 = wgt2

    ! apply weights?
    IF ( time1 > 0 .AND. weight1 >= 0.0 ) THEN
       ApplyWeights = .TRUE.
    ELSE
       ApplyWeights = .FALSE.
    ENDIF

    ! # of horizontal dimensions to read
    nLon = lon2 - lon1 + 1
    nLat = lat2 - lat1 + 1

    ! # of vertical levels
    FlipZ = .FALSE. ! Flip z-axis?
    l1    = lev1    ! Lower level to be read
    l2    = lev2    ! Upper level to be read
    IF ( lev1 > 0 ) THEN

       ! Check if we need to flip the vertical axis
       IF ( lev1 > lev2 ) THEN
          FlipZ = .TRUE.
          l1    = lev2
          l2    = lev1
       ENDIF

       ! Number of levels to be read
       nLev = l2 - l1 + 1

    ! no vertical levels:
    ELSE
       nLev = 0
    ENDIF

    ! # of time slices
    ! read all time slices time1:time2:
    IF ( time1 > 0 .AND. weight1 < 0.0 ) THEN
       ntime = time2 - time1 + 1
    ! Interpolate amongs 2 time slices:
    ELSEIF ( ApplyWeights ) THEN
       ntime = 1
    ! no time dimension:
    ELSE
       ntime = 0
    ENDIF

    ! # of arbitrary other dimensions
    arbdim = -1
    IF ( PRESENT(ArbIdx) ) THEN
       IF ( ArbIdx > 0 ) THEN
          arbdim = ArbIdx
       ENDIF
    ENDIF

    ! Set dimensions of output array
    ! --> must have at least dimension 1
    nclev  = max(nlev ,1)
    nctime = max(ntime,1)

    ! set total number of dimensions to be read. This is at least 2 and
    ! at most 5.
    ndims = 2
    if ( nlev   > 0 ) ndims = ndims + 1
    if ( ntime  > 0 ) ndims = ndims + 1
    if ( arbdim > 0 ) ndims = ndims + 1

    !----------------------------------------
    ! Read array
    !----------------------------------------

    ! Variable name
    v_name = TRIM(ncVar)

    ! Get PIO variable ID for reading
    pio_ierr = pio_inq_varid(fID, TRIM(v_name), pio_varid)

    ! Allocate the output array
    ALLOCATE ( ncArr( nLon, nLat, ncLev, ncTime ) )
    ncArr = 0.0

    ! Define number of required reads and time dimension on temporary array
    nRead = 1
    IF ( ntime > 0 ) THEN
       IF ( ApplyWeights ) THEN
          nRead = 2
          nt    = 2
       ELSE
          nRead = 1
          nt    = ntime
       ENDIF
    ENDIF

    !----------------------------------------
    ! Read 5D array:
    IF ( ndims == 5 ) THEN

       ! Allocate array. If time weights are applied, the two
       ! time slices are read into TMPARR_5D and then temporarily
       ! stored in WGTARR_5D. Same applies to 4D and 3D below.
       ! (ckeller, 01/09/17)
       IF ( ApplyWeights ) THEN
          ALLOCATE ( TMPARR_5D( nlon, nlat, nlev, 1, 1 ) )
          TMPARR_5D = 0.0
          ALLOCATE ( WGTARR_5D( nlon, nlat, nlev, nt, 1 ) )
          WGTARR_5D = 0.0
       ELSE
          ALLOCATE ( TMPARR_5D( nlon, nlat, nlev, nt, 1 ) )
          TMPARR_5D = 0.0
       ENDIF

       ! Set default start/end indeces
       s1 = lon1
       n1 = nlon
       s2 = lat1
       n2 = nlat
       s3 = l1
       n3 = nlev
       s5 = arbdim
       n5 = 1

       ! Read arrays from file
       DO I = 1, nRead

          ! time index
          IF ( .NOT. ApplyWeights ) THEN
             s4 = time1
             n4 = ntime
          ELSE
             IF ( I == 1 ) THEN
                s4 = time1
             ELSE
                s4 = time2
             ENDIF
             n4 = 1
          ENDIF

          st5d = (/ s1, s2, s3, s4, s5 /)
          ct5d = (/ n1, n2, n3, n4, n5 /)
          pio_ierr = pio_get_var(fId, pio_varid, st5d, ct5d, TMPARR_5D)

          ! Eventually pass time weighted arrays to temporary array
          IF ( ApplyWeights ) THEN
             WGTARR_5D(:,:,:,I,:) = TMPARR_5D(:,:,:,1,:)
          ENDIF

       ENDDO

       ! Pass to output array. Eventually apply time weights.
       IF ( ApplyWeights ) THEN
          ncArr(:,:,:,1) = WGTARR_5D(:,:,:,1,1) * weight1 &
                         + WGTARR_5D(:,:,:,2,1) * weight2
       ELSE
          ncArr(:,:,:,:) = TMPARR_5D(:,:,:,:,1)
       ENDIF

       ! Cleanup
       DEALLOCATE(TMPARR_5D)
       IF(ALLOCATED(WGTARR_5D)) DEALLOCATE(WGTARR_5D)
    ENDIF

    !----------------------------------------
    ! Read 4D array:
    ! This can be:
    ! - lon,lat,lev,time
    ! - lon,lat,lev,arb
    ! - lon,lat,time,arb
    IF ( ndims == 4 ) THEN

       ! Allocate temporary array
       s1    = lon1
       n1    = nlon
       s2    = lat1
       n2    = nlat
       tdim  = -1

       ! 3rd and 4th dim

       ! lev is defined
       IF ( nlev > 0 ) THEN
          s3   = l1
          n3   = nlev
          ! plus time...
          IF ( ntime > 0 ) THEN
             n4   = nt
             tdim = 4
          ! ... or plus arbitrary dim
          ELSE
             s4 = arbdim
             n4 = 1
          ENDIF

       ! lev not defined: time + arbitrary dim
       ELSE
          n3 = nt
          tdim = 3
          s4 = arbdim
          n4 = 1
       ENDIF

       IF ( ApplyWeights ) THEN
          ALLOCATE ( WGTARR_4D(n1,n2,n3,n4) )
          WGTARR_4D = 0.0
          IF ( tdim == 3 ) THEN
             ALLOCATE ( TMPARR_4D(n1,n2,1,n4) )
             TMPARR_4D = 0.0
          ELSEIF ( tdim == 4 ) THEN
             ALLOCATE ( TMPARR_4D(n1,n2,n3,1) )
             TMPARR_4D = 0.0
          ENDIF

       ELSE
          ALLOCATE ( TMPARR_4D(n1,n2,n3,n4) )
          TMPARR_4D = 0.0
       ENDIF

       ! Read arrays from file
       DO I = 1, nRead

          ! time index
          IF ( .NOT. ApplyWeights ) THEN
             sti = time1
             nti = ntime
          ELSE
             IF ( I == 1 ) THEN
                sti = time1
             ELSE
                sti = time2
             ENDIF
             nti = 1
          ENDIF

          ! need to adjust time index: this is either 3rd or 4th dimension:
          IF ( tdim == 3 ) THEN
             s3 = sti
             n3 = nti
          ELSEIF ( tdim == 4 ) THEN
             s4 = sti
             n4 = nti
          ENDIF

          st4d = (/ s1, s2, s3, s4 /)
          ct4d = (/ n1, n2, n3, n4 /)

          ! Read data from disk
          pio_ierr = pio_get_var(fId, pio_varid, st4d, ct4d, TMPARR_4D)

          ! Eventually pass time weighted arrays to temporary array
          IF ( ApplyWeights ) THEN
             IF ( tdim == 3 ) THEN
                WGTARR_4D(:,:,I,:) = TMPARR_4D(:,:,1,:)
             ELSEIF ( tdim == 4 ) THEN
                WGTARR_4D(:,:,:,I) = TMPARR_4D(:,:,:,1)
             ENDIF
          ENDIF
       ENDDO

       ! Pass to output array. Eventually apply time weights.
       IF ( ApplyWeights ) THEN
          IF ( tdim == 3 ) THEN
             ncArr(:,:,:,1) = WGTARR_4D(:,:,1,:) * weight1 &
                            + WGTARR_4D(:,:,2,:) * weight2
          ELSEIF ( tdim == 4 ) THEN
             ncArr(:,:,:,1) = WGTARR_4D(:,:,:,1) * weight1 &
                            + WGTARR_4D(:,:,:,2) * weight2
          ENDIF
       ELSE
          ncArr(:,:,:,:) = TMPARR_4D(:,:,:,:)
       ENDIF

       ! Cleanup
       DEALLOCATE(TMPARR_4D)
       IF(ALLOCATED(WGTARR_4D)) DEALLOCATE(WGTARR_4D)
    ENDIF

    !----------------------------------------
    ! Read 3D array:
    ! This can be:
    ! - lon,lat,lev
    ! - lon,lat,time
    ! - lon,lat,arb
    IF ( ndims == 3 ) THEN

       ! Allocate temporary array
       s1    = lon1
       n1    = nlon
       s2    = lat1
       n2    = nlat
       tdim  = -1

       ! 3rd dim:
       ! - lev is defined:
       IF ( nlev > 0 ) THEN
          s3   = l1
          n3   = nlev
       ! - time is defined:
       ELSEIF ( ntime > 0 ) THEN
          n3   = nt
          tdim = 3
       ! - arbitrary dimension is defined:
       ELSEIF ( arbdim > 0 ) THEN
          s3   = arbdim
          n3   = 1
       ENDIF

       IF ( ApplyWeights ) THEN
          ALLOCATE ( TMPARR_3D(n1,n2,1) )
          TMPARR_3D = 0.0
          ALLOCATE ( WGTARR_3D(n1,n2,n3) )
          WGTARR_3D = 0.0
       ELSE
          ALLOCATE ( TMPARR_3D(n1,n2,n3) )
          TMPARR_3D = 0.0
       ENDIF

       ! Read arrays from file
       DO I = 1, nRead

          ! time index
          IF ( tdim  == 3 ) THEN
             IF ( .NOT. ApplyWeights ) THEN
                s3 = time1
                n3 = ntime
             ELSE
                IF ( I == 1 ) THEN
                   s3 = time1
                ELSE
                   s3 = time2
                ENDIF
                n3 = 1
             ENDIF
          ENDIF

          st3d = (/ s1, s2, s3 /)
          ct3d = (/ n1, n2, n3 /)
          pio_ierr = pio_get_var(fId, pio_varid, st3d, ct3d, TMPARR_3D)

          ! Eventually pass time weighted arrays to temporary array
          IF ( ApplyWeights ) THEN
           WGTARR_3D(:,:,I) = TMPARR_3D(:,:,1)
          ENDIF

       ENDDO

       ! Pass to output array. Eventually apply time weights.
       IF ( ApplyWeights ) THEN
          ncArr(:,:,1,1) = WGTARR_3D(:,:,1) * weight1 &
                         + WGTARR_3D(:,:,2) * weight2
       ELSE
          IF ( tdim == 3 ) THEN
             ncArr(:,:,1,:) = TMPARR_3D(:,:,:)
          ELSE
             ncArr(:,:,:,1) = TMPARR_3D(:,:,:)
          ENDIF
       ENDIF

       ! Cleanup
       IF(ALLOCATED(TMPARR_3D)) DEALLOCATE(TMPARR_3D)
       IF(ALLOCATED(WGTARR_3D)) DEALLOCATE(WGTARR_3D)
    ENDIF

    !----------------------------------------
    ! Read a 2D array (lon and lat only):
    IF ( ndims == 2 ) THEN
       ALLOCATE ( TMPARR_2D( nLon, nLat ) )
       TMPARR_2D = 0.0
       st2d      = (/ lon1, lat1 /)
       ct2d      = (/ nlon, nlat /)
       pio_ierr = pio_get_var(fId, pio_varid, st2d, ct2d, TMPARR_2D)
       ncArr(:,:,1,1) = TMPARR_2D(:,:)
       DEALLOCATE(TMPARR_2D)
    ENDIF

    ! ------------------------------------------
    ! Eventually apply scale / offset factors
    ! ------------------------------------------

    ! Check for scale factor
    a_name  = "scale_factor"
    tmp_ierr = pio_inq_varid(fID, TRIM(v_name), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       ReadAtt = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       ReadAtt = (tmp_ierr == PIO_NOERR)
    ENDIF

    IF ( ReadAtt ) THEN
       pio_ierr = pio_inq_varid(fId, TRIM(v_name), pio_varid)
       pio_ierr = pio_get_att(fId, pio_varid, TRIM(a_name), corr)
       ncArr(:,:,:,:) = ncArr(:,:,:,:) * corr
    ENDIF

    ! Check for offset factor
    a_name  = "add_offset"
    tmp_ierr = pio_inq_varid(fID, TRIM(v_name), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       ReadAtt = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       ReadAtt = (tmp_ierr == PIO_NOERR)
    ENDIF

    IF ( ReadAtt ) THEN
       pio_ierr = pio_inq_varid(fId, TRIM(v_name), pio_varid)
       pio_ierr = pio_get_att(fId, pio_varid, TRIM(a_name), corr)
       ncArr(:,:,:,:) = ncArr(:,:,:,:) + corr
    ENDIF

    ! ------------------------------------------
    ! Check for filling values
    ! NOTE: Test for REAL*4 and REAL*8
    ! ------------------------------------------

    ! Define missing value
    IF ( PRESENT(MissVal) ) THEN
       MissValue = MissVal
    ELSE
       MissValue = 0.0
    ENDIF

    ! 1: 'missing_value'
    a_name  = "missing_value"
    tmp_ierr = pio_inq_varid(fID, TRIM(v_name), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       ReadAtt = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       ReadAtt = (tmp_ierr == PIO_NOERR)
    ENDIF
    IF ( ReadAtt ) THEN
       IF ( a_type == PIO_REAL ) THEN
          pio_ierr = pio_inq_varid(fId, TRIM(v_name), pio_varid)
          pio_ierr = pio_get_att(fId, pio_varid, TRIM(a_name), miss4)
          WHERE ( ncArr == miss4 )
             ncArr = MissValue
          END WHERE
       ELSE IF ( a_type == PIO_DOUBLE ) THEN
          pio_ierr = pio_inq_varid(fId, TRIM(v_name), pio_varid)
          pio_ierr = pio_get_att(fId, pio_varid, TRIM(a_name), miss8)
          miss4 = REAL( miss8 )
          WHERE ( ncArr == miss4 )
             ncArr = MissValue
          END WHERE
       ENDIF
    ENDIF

    ! 2: '_FillValue'
    a_name  = "_FillValue"
    tmp_ierr = pio_inq_varid(fID, TRIM(v_name), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       ReadAtt = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       ReadAtt = (tmp_ierr == PIO_NOERR)
    ENDIF
    IF ( ReadAtt ) THEN
       IF ( a_type == PIO_REAL ) THEN
          pio_ierr = pio_inq_varid(fId, TRIM(v_name), pio_varid)
          pio_ierr = pio_get_att(fId, pio_varid, TRIM(a_name), miss4)
          WHERE ( ncArr == miss4 )
             ncArr = MissValue
          END WHERE
       ELSE IF ( a_type == PIO_DOUBLE ) THEN
          pio_ierr = pio_inq_varid(fId, TRIM(v_name), pio_varid)
          pio_ierr = pio_get_att(fId, pio_varid, TRIM(a_name), miss8)
          miss4 = REAL( miss8 )
          WHERE ( ncArr == miss4 )
             ncArr = MissValue
          END WHERE
       ENDIF
    ENDIF

    ! ------------------------------------------
    ! Flip z-axis if needed
    ! ------------------------------------------
    IF ( FlipZ ) THEN
       ncArr(:,:,:,:) = ncArr(:,:,ncLev:1:-1,:)
    ENDIF

    ! ----------------------------
    ! Read optional arguments
    ! ----------------------------

    ! Read units
    IF ( PRESENT(VarUnit) )THEN
       a_name = "units"
       pio_ierr = pio_inq_varid(fId, TRIM(v_name), pio_varid)
       pio_ierr = pio_get_att(fId, pio_varid, TRIM(a_name), a_val)
       VarUnit = TRIM(a_val)

       ! Check if the last character of VarUnit is the ASCII null character
       ! ("\0", ASCII value = 0), which is used to denote the end of a string.
       ! The ASCII null character may be introduced if the netCDF file was
       ! written using a language other than Fortran.  The compiler might
       ! interpret the null character as part of the string instead of as
       ! an empty space.  If the null space is there, then replace it with
       ! a Fortran empty string value (''). (bmy, 7/17/18)
       I = LEN_TRIM( VarUnit )
       IF ( ICHAR( VarUnit(I:I) ) == 0 ) THEN
          VarUnit(I:I) = ''
       ENDIF
    ENDIF

    !=================================================================
    ! Cleanup and quit
    !=================================================================

    ! Return w/ success
    RC = 0

  END SUBROUTINE NC_READ_ARR
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Read_Time_yyyymmddhhmm
!
! !DESCRIPTION: Returns a vector containing the datetimes (YYYYMMDDhhmm) of
! all time slices in the netCDF file.
!\\
! !INTERFACE:
!
  SUBROUTINE NC_READ_TIME_YYYYMMDDhhmm( fID,              nTime,    &
                                        all_YYYYMMDDhhmm, timeUnit, &
                                        refYear,          RC )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT)          :: fID
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*8,           POINTER                 :: all_YYYYMMDDhhmm(:)
    CHARACTER(LEN=*), INTENT(  OUT), OPTIONAL :: timeUnit
    INTEGER,          INTENT(  OUT), OPTIONAL :: refYear
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)           :: nTime
    INTEGER,          INTENT(INOUT)           :: RC
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    CHARACTER(LEN=255)  :: ncUnit, cal
    INTEGER             :: refYr, refMt, refDy, refHr, refMn, refSc
    INTEGER             :: T, YYYYMMDD, hhmmss
    REAL*8              :: realrefDy, refJulday, tJulday

    ! Pointers
    REAL*8,   POINTER   :: tVec(:)

    !=================================================================
    ! NC_READ_TIME_YYYYMMDDhhmm begins here
    !=================================================================

    ! Init values
    RC = 0
    tVec => NULL()
    IF ( PRESENT(TimeUnit) ) TimeUnit = ''
    IF ( PRESENT(refYear ) ) refYear  = 0

    ! Read time vector
    CALL NC_READ_TIME ( fID,          nTime,            ncUnit,              &
                        timeVec=tVec, timeCalendar=cal, RC=RC               )
    IF ( RC/=0 ) THEN
       WRITE( 6, '(/,a)' ) REPEAT( '=', 79 )
       WRITE( 6, '(a)'   ) 'Error encountered in NC_READ_TIME (hco_pio_mod.F90)'
       WRITE( 6, '(a,/)' ) REPEAT( '=', 79 )
       RETURN
    ENDIF

    ! If nTime is zero, return here!
    IF ( nTime == 0 ) RETURN

    ! Get reference date in julian days
    CALL NC_GET_REFDATETIME ( ncUnit, refYr, refMt, &
                              refDy,  refHr, refMn, refSc, RC )
    IF ( RC /= 0 ) RETURN
    realrefDy =         refDy              &
              + ( MAX(0,refHr) / 24d0    ) &
              + ( MAX(0,refMn) / 1440d0  ) &
              + ( MAX(0,refSc) / 86400d0 )
    refJulday = JULDAY ( refYr, refMt, realrefDy )

    ! NOTE: It seems that there is an issue with reference dates
    ! between 1800 and 1901: the respective time stamps all seem to
    ! be off by one day (this problem doesn't appear for netCDF files
    ! with reference date zero, i.e. hours since 1-1-1)!
    ! I'm not sure what causes this problem, but adding one day to
    ! reference dates that lie between 1600 and 1900 seems to fix the
    ! problem.
    ! TODO: requires more testing!
    IF ( refYr <= 1900 .AND. refYr >= 1600 ) THEN
       refJulday = refJulday + 1.0
       !PRINT *, 'Reference julian day increased by one day!!!'
    ENDIF

    ! Get calendar dates
    IF ( ASSOCIATED ( all_YYYYMMDDhhmm ) ) DEALLOCATE( all_YYYYMMDDhhmm )
    ALLOCATE( all_YYYYMMDDhhmm(nTime) )
    all_YYYYMMDDhhmm = 0.0d0

    ! Construct julian date for every available time slice. Make sure it is
    ! in the proper 'units', e.g. in days, hours or minutes, depending on
    ! the reference unit.
    DO T = 1, nTime
       tJulDay = tVec(T)
       IF ( refHr >= 0 ) tJulday = tJulday / 24.d0
       IF ( refMn >= 0 ) tJulday = tJulday / 60.d0
       IF ( refSc >= 0 ) tJulday = tJulday / 60.d0
       tJulday = tJulday + refJulday
       CALL CALDATE ( tJulday, YYYYMMDD, hhmmss )
       all_YYYYMMDDhhmm(T) = ( DBLE( YYYYMMDD ) * 1d4   ) + &
                             ( DBLE( hhmmss     / 100 ) )
    ENDDO

    ! Cleanup
    IF ( ASSOCIATED( tVec ) ) DEALLOCATE( tVec )

    ! Return
    IF ( PRESENT(timeUnit) ) timeUnit = ncUnit
    IF ( PRESENT(refYear ) ) refYear  = refYr
    RC = 0

  END SUBROUTINE NC_READ_TIME_YYYYMMDDhhmm
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_RefDateTime
!
! !DESCRIPTION: Returns the reference datetime (tYr / tMt / tDy / tHr /
! tMn ) of the provided time unit. For now, supported formats are
! "days since YYYY-MM-DD", "hours since YYYY-MM-DD HH:MM:SS", and
! "minutes since YYYY-MM-DD HH:NN:SS". For times in days since refdate,
! the returned reference hour rHr is set to -1. The same applies for the
! reference minute for units in days / hours since XXX.
!\\
! !INTERFACE:
!
  SUBROUTINE NC_GET_REFDATETIME( tUnit, tYr, tMt, tDy, tHr, tMn, tSc, RC )
!
! !INPUT PARAMETERS:
!
    ! Required
    CHARACTER(LEN=*), INTENT( IN)    :: tUnit
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT)    :: tYr
    INTEGER,          INTENT(OUT)    :: tMt
    INTEGER,          INTENT(OUT)    :: tDy
    INTEGER,          INTENT(OUT)    :: tHr
    INTEGER,          INTENT(OUT)    :: tMn
    INTEGER,          INTENT(OUT)    :: tSc
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC
!
! !REMARKS:
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)    :: LOC, MSG
    CHARACTER(LEN=255)    :: MIRRUNIT
    INTEGER               :: TTYPE, STAT, L1, L2
    INTEGER               :: MINLEN, STRLEN, I

    !=================================================================
    ! NC_GET_REFDATETIME starts here
    !=================================================================

    ! Init
    LOC = 'NC_GET_REFDATETIME (hco_pio_mod.F90)'

    ! ----------------------------------------------------------------------
    ! Determine time unit type
    ! ----------------------------------------------------------------------

    ! Mirror time unit and convert to lower case
    MIRRUNIT = tUnit
    CALL TRANLC( MIRRUNIT )

    ! Check for reference time unit '(days, hours, minutes) since ...'
    ! Set beginning of reference date according to the unit and define
    ! minimum string length required by unit.

    ! 'days since YYYY-M-D'
    IF ( MIRRUNIT(1:10) == 'days since' ) THEN
       TTYPE  = 1
       L1     = 12
       MINLEN = 19

    ! 'hours since YYYY-M-D h:m:s'
    ELSEIF ( MIRRUNIT(1:11) == 'hours since' ) THEN
       TTYPE  = 2
       L1     = 13
       MINLEN = 26

    ! 'minutes since YYYY-M-D h:m:s'
    ELSEIF ( MIRRUNIT(1:13) == 'minutes since' ) THEN
       TTYPE  = 3
       L1     = 15
       MINLEN = 28

    ! 'seconds since YYYY-M-D h:m:s'
    ELSEIF ( MIRRUNIT(1:13) == 'seconds since' ) THEN
       TTYPE  = 4
       L1     = 15
       MINLEN = 28

    ! Return w/ error otherwise
    ELSE
       PRINT *, 'Invalid time unit: ' // TRIM(tUnit)
       RC = -999; RETURN
    ENDIF

    ! Check if time string is long enough or not
    STRLEN = LEN(tUnit)
    IF ( STRLEN < MINLEN ) THEN
       PRINT *, 'Time unit string too short: ' // TRIM(tUnit)
       RC = -999; RETURN
    ENDIF

    ! ----------------------------------------------------------------------
    ! Determine reference time/date
    ! Get the year, month, day and hour from the string
    ! '... since YYYY-MM-DD hh:mm:ss

    ! Read reference year, i.e. from beginning of date string until
    ! first separator sign (-).
    DO I=L1,STRLEN
       IF(tUnit(I:I) == '-') EXIT
    ENDDO
    L2 = I-1

    READ( tUnit(L1:L2),'(i4)', IOSTAT=STAT ) tYr
    IF ( STAT /= 0 ) THEN
       PRINT *, 'Invalid year in ' // TRIM(tUnit)
       RC = -999; RETURN
    ENDIF

    ! Advance in date string: now read reference month.
    L1 = L2 + 2
    DO I=L1,STRLEN
       IF(tUnit(I:I) == '-') EXIT
    ENDDO
    L2 = I-1
    READ( tUnit(L1:L2), '(i2)', IOSTAT=STAT ) tMt
    IF ( STAT /= 0 ) THEN
       PRINT *, 'Invalid month in ' // TRIM(tUnit)
       RC = -999; RETURN
    ENDIF

    ! Advance in date string: now read reference day.
    L1 = L2 + 2
    DO I=L1,STRLEN
       IF(tUnit(I:I) == ' ') EXIT
    ENDDO
    L2 = I-1
    READ( tUnit(L1:L2), '(i2)', IOSTAT=STAT ) tDy
    IF ( STAT /= 0 ) THEN
       PRINT *, 'Invalid day in ' // TRIM(tUnit)
       RC = -999; RETURN
    ENDIF

    ! Get reference hour only if 'hours/minutes/seconds since'.
    IF ( TTYPE > 1 ) THEN

       ! Reference hour
       L1 = L2 + 2
       DO I=L1,STRLEN
          IF(tUnit(I:I) == ':') EXIT
       ENDDO
       L2 = I-1
       READ( tUnit(L1:L2), '(i2)', IOSTAT=STAT ) tHr
       IF ( STAT /= 0 ) THEN
          PRINT *, 'Invalid hour in ', TRIM(tUnit)
          RC = -999; RETURN
       ENDIF

    ELSE
       ! Set reference hour to -1
       tHr = -1
    ENDIF

    ! Get reference minute only if 'minutes since...'
    IF ( TTYPE>2 ) THEN

       ! Reference minute
       L1 = L2 + 2
       DO I=L1,STRLEN
          IF(tUnit(I:I) == ':') EXIT
       ENDDO
       L2 = I-1
       READ( tUnit(L1:L2), '(i2)', IOSTAT=STAT ) tMn
       IF ( STAT /= 0 ) THEN
          PRINT *, 'Invalid minute in ', TRIM(tUnit)
          RC = -999; RETURN
       ENDIF

    ELSE
       ! Set reference minute to -1
       tMn = -1
    ENDIF

    ! Get reference minute only if 'seconds since...'
    IF ( TTYPE>3 ) THEN

       ! Reference second
       L1 = L2 + 2
       DO I=L1,STRLEN
          IF(tUnit(I:I) == ':') EXIT
       ENDDO
       L2 = I-1
       READ( tUnit(L1:L2), '(i2)', IOSTAT=STAT ) tSc
       IF ( STAT /= 0 ) THEN
          PRINT *, 'Invalid second in ', TRIM(tUnit)
          RC = -999; RETURN
       ENDIF

    ELSE
       ! Set reference second to -1
       tSc = -1
    ENDIF

    ! Return w/ success
    RC = 0

  END SUBROUTINE NC_GET_REFDATETIME
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_Grid_Edges_Sp
!
! !DESCRIPTION: Routine to get the longitude or latitude edges. If the edge
! cannot be read from the netCDF file, they are calculated from the provided
! grid midpoints. Use the axis input argument to discern between longitude
! (axis 1) and latitude (axis 2).
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_GET_GRID_EDGES_SP( fID, AXIS, MID, NMID, EDGE, NEDGE, RC )
!
! !USES:
!
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID             ! PIO File descriptor
    INTEGER,          INTENT(IN   ) :: AXIS            ! 1=lon, 2=lat
    INTEGER,          INTENT(IN   ) :: NMID            ! # of midpoints
    REAL*4,           INTENT(IN   ) :: MID(NMID)       ! midpoints
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*4,           POINTER       :: EDGE(:)         ! edges
    INTEGER,          INTENT(INOUT) :: NEDGE           ! # of edges
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    !======================================================================
    ! NC_GET_GRID_EDGES_SP begins here
    !======================================================================

    CALL NC_GET_GRID_EDGES_C( fID, AXIS, NMID, NEDGE, RC, &
                              MID4=MID,  EDGE4=EDGE )

  END SUBROUTINE NC_GET_GRID_EDGES_SP
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_Grid_Edges_Dp
!
! !DESCRIPTION: Routine to get the longitude or latitude edges. If the edge
! cannot be read from the netCDF file, they are calculated from the provided
! grid midpoints. Use the axis input argument to discern between longitude
! (axis 1) and latitude (axis 2).
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_GET_GRID_EDGES_DP( fID, AXIS, MID, NMID, EDGE, NEDGE, RC )
!
! !USES:
!
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID             ! PIO File descriptor
    INTEGER,          INTENT(IN   ) :: AXIS            ! 1=lon, 2=lat
    INTEGER,          INTENT(IN   ) :: NMID            ! # of midpoints
    REAL*8,           INTENT(IN   ) :: MID(NMID)       ! midpoints
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*8,           POINTER       :: EDGE(:)         ! edges
    INTEGER,          INTENT(INOUT) :: NEDGE           ! # of edges
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    !======================================================================
    ! NC_GET_GRID_EDGES_DP begins here
    !======================================================================

    CALL NC_GET_GRID_EDGES_C( fID, AXIS, NMID, NEDGE, RC, &
                              MID8=MID,  EDGE8=EDGE )

  END SUBROUTINE NC_GET_GRID_EDGES_DP
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_Grid_Edges_C
!
! !DESCRIPTION: Routine to get the longitude or latitude edges. If the edge
! cannot be read from the netCDF file, they are calculated from the provided
! grid midpoints. Use the axis input argument to discern between longitude
! (axis 1) and latitude (axis 2).
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_GET_GRID_EDGES_C( fID, AXIS, NMID, NEDGE, RC, &
                                  MID4, MID8, EDGE4, EDGE8 )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID             ! PIO File descriptor
    INTEGER,          INTENT(IN   ) :: AXIS            ! 1=lon, 2=lat
    INTEGER,          INTENT(IN   ) :: NMID            ! # of midpoints
    REAL*4, OPTIONAL, INTENT(IN   ) :: MID4(NMID)       ! midpoints
    REAL*8, OPTIONAL, INTENT(IN   ) :: MID8(NMID)       ! midpoints
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*4, OPTIONAL, POINTER       :: EDGE4(:)         ! edges
    REAL*8, OPTIONAL, POINTER       :: EDGE8(:)         ! edges
    INTEGER,          INTENT(INOUT) :: NEDGE           ! # of edges
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL              :: PoleMid
    INTEGER              :: I, AS
    CHARACTER(LEN=255)   :: ncVar, ThisUnit

    !======================================================================
    ! NC_GET_GRID_EDGES_C begins here
    !======================================================================

    ! Error trap: edge and mid must be same kind
    IF ( PRESENT(EDGE4) ) THEN
       IF ( .NOT. PRESENT(MID4) ) THEN
          PRINT *, 'If you provide EDGE4, you must also provide MID4'
          RC = -999
          RETURN
       ENDIF
    ELSEIF ( PRESENT(EDGE8) ) THEN
       IF ( .NOT. PRESENT(MID8) ) THEN
          PRINT *, 'If you provide EDGE8, you must also provide MID8'
          RC = -999
          RETURN
       ENDIF
    ELSE
       PRINT *, 'EDGE4 or EDGE8 must be given'
       RC = -999
       RETURN
    ENDIF

    ! Try to read edges from ncdf file
    IF ( AXIS == 1 ) THEN
       ncVar = 'lon_edge'
    ELSEIF ( AXIS == 2 ) THEN
       ncVar = 'lat_edge'
    ENDIF

    IF ( PRESENT(EDGE4) ) THEN
       CALL NC_READ_VAR( fID, TRIM(ncVar), nEdge, ThisUnit, Edge4, RC )
    ELSE
       CALL NC_READ_VAR( fID, TRIM(ncVar), nEdge, ThisUnit, Edge8, RC )
    ENDIF
    IF ( RC /= 0 ) RETURN

    ! Also try 'XXX_edges'
    IF ( nEdge == 0 ) THEN
       IF ( AXIS == 1 ) THEN
          ncVar = 'lon_edges'
       ELSEIF ( AXIS == 2 ) THEN
          ncVar = 'lat_edges'
       ENDIF
       IF ( PRESENT(EDGE4) ) THEN
          CALL NC_READ_VAR( fID, TRIM(ncVar), nEdge, ThisUnit, Edge4, RC )
       ELSE
          CALL NC_READ_VAR( fID, TRIM(ncVar), nEdge, ThisUnit, Edge8, RC )
       ENDIF
       IF ( RC /= 0 ) RETURN
    ENDIF

    ! Sanity check if edges are read from files: dimension must be nlon + 1!
    IF ( nEdge > 0 ) THEN
       IF ( nEdge /= (nMid + 1) ) THEN
          PRINT *, 'Edge has incorrect length!'
          RC = -999; RETURN
       ENDIF

    ! If not read from file, calculate from provided lon midpoints.
    ELSE

       nEdge = nMid + 1
       IF ( PRESENT(EDGE4) ) THEN
          IF ( ASSOCIATED ( Edge4 ) ) DEALLOCATE( Edge4 )
          ALLOCATE ( Edge4(nEdge), STAT=AS )
          IF ( AS /= 0 ) THEN
             PRINT *, 'Edge alloc. error in NC_GET_LON_EDGES (hco_pio_mod.F90)'
             RC = -999; RETURN
          ENDIF
          Edge4 = 0.0
       ELSE
          IF ( ASSOCIATED ( Edge8 ) ) DEALLOCATE( Edge8 )
          ALLOCATE ( Edge8(nEdge), STAT=AS )
          IF ( AS /= 0 ) THEN
             PRINT *, 'Edge alloc. error in NC_GET_LON_EDGES (hco_pio_mod.F90)'
             RC = -999; RETURN
          ENDIF
          Edge8 = 0.0d0
       ENDIF

       ! Get leftmost edge by extrapolating from first two midpoints.
       ! Error trap: for latitude axis, first edge must not be below -90!
       IF ( PRESENT(EDGE4) ) THEN
          Edge4(1) = Mid4(1) - ( (Mid4(2) - Mid4(1) ) / 2.0 )
          IF ( Edge4(1) < -90.0 .AND. AXIS == 2 ) Edge4(1) = -90.0
       ELSE
          Edge8(1) = Mid8(1) - ( (Mid8(2) - Mid8(1) ) / 2.0d0 )
          IF ( Edge8(1) < -90.0d0 .AND. AXIS == 2 ) Edge8(1) = -90.0d0
       ENDIF

       ! Calculate second edge. We need to catch the case where the first
       ! latitude mid-point is -90 (this is the case for GEOS-5 generic
       ! grids...). In that case, the second edge is put in the middle of
       ! the first two mid points (e.g. between -90 and -89). In all other
       ! case, we calculate it from the previously calculated left edge.
       IF ( PRESENT(EDGE4) ) THEN
          IF ( Mid4(1) == Edge4(1) ) THEN
             Edge4(2) = Mid4(1) + ( Mid4(2) - Mid4(1) ) / 2.0
             PoleMid  = .TRUE.
          ELSE
             Edge4(2) = Mid4(1) + Mid4(1) - Edge4(1)
             PoleMid  = .FALSE.
          ENDIF

          ! Sequentially calculate the right edge from the previously
          ! calculated left edge.
          DO I = 2, nMid
             Edge4(I+1) = Mid4(I) + Mid4(I) - Edge4(I)
          ENDDO

          ! Error check: max. lat edge must not exceed +90!
          IF ( Edge4(nMId+1) > 90.01 .AND. AXIS == 2 ) THEN
             IF ( PoleMid ) THEN
                Edge4(nMid+1) = 90.0
             ELSE
                PRINT *, 'Uppermost latitude edge above 90 deg north!'
                PRINT *, Edge4
                RC = -999; RETURN
             ENDIF
          ENDIF

       ! Real8
       ELSE
          IF ( Mid8(1) == Edge8(1) ) THEN
             Edge8(2) = Mid8(1) + ( Mid8(2) - Mid8(1) ) / 2.0d0
             PoleMid  = .TRUE.
          ELSE
             Edge8(2) = Mid8(1) + Mid8(1) - Edge8(1)
             PoleMid  = .FALSE.
          ENDIF

          ! Sequentially calculate the right edge from the previously
          ! calculated left edge.
          DO I = 2, nMid
             Edge8(I+1) = Mid8(I) + Mid8(I) - Edge8(I)
          ENDDO

          ! Error check: max. lat edge must not exceed +90!
          IF ( Edge8(nMId+1) > 90.01d0 .AND. AXIS == 2 ) THEN
             IF ( PoleMid ) THEN
                Edge8(nMid+1) = 90.0d0
             ELSE
                PRINT *, 'Uppermost latitude edge above 90 deg north!'
                PRINT *, Edge8
                RC = -999; RETURN
             ENDIF
          ENDIF
       ENDIF
    ENDIF

    ! Return w/ success
    RC = 0

  END SUBROUTINE NC_GET_GRID_EDGES_C
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_Sigma_Levels_Sp
!
! !DESCRIPTION: Wrapper routine to get the sigma levels in single precision.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_GET_SIGMA_LEVELS_SP( fID,  ncFile, levName, lon1, lon2, lat1, &
                                     lat2, lev1,   lev2,    time, SigLev, dir, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID             ! PIO File descriptor
    CHARACTER(LEN=*), INTENT(IN   ) :: ncFile          ! ncFile
    CHARACTER(LEN=*), INTENT(IN   ) :: levName         ! variable name
    INTEGER,          INTENT(IN   ) :: lon1            ! lon lower bound
    INTEGER,          INTENT(IN   ) :: lon2            ! lon upper bound
    INTEGER,          INTENT(IN   ) :: lat1            ! lat lower bound
    INTEGER,          INTENT(IN   ) :: lat2            ! lat upper bound
    INTEGER,          INTENT(IN   ) :: lev1            ! lev lower bound
    INTEGER,          INTENT(IN   ) :: lev2            ! lev upper bound
    INTEGER,          INTENT(IN   ) :: time            ! time index
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*4,           POINTER       :: SigLev(:,:,:)   ! sigma levels
    INTEGER,          INTENT(INOUT) :: dir             ! axis direction (1=up;-1=down)
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

  CALL NC_GET_SIGMA_LEVELS_C( fID,  ncFile, levName, lon1, lon2, lat1, &
                              lat2, lev1,   lev2,    time, dir,  RC,   &
                              SigLev4=SigLev )

  END SUBROUTINE NC_GET_SIGMA_LEVELS_SP
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_Sigma_Levels_Dp
!
! !DESCRIPTION: Wrapper routine to get the sigma levels in double precision.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_GET_SIGMA_LEVELS_DP( fID,  ncFile, levName, lon1, lon2, lat1, &
                                     lat2, lev1,   lev2,    time, SigLev, dir, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID             ! PIO File descriptor
    CHARACTER(LEN=*), INTENT(IN   ) :: ncFile          ! ncFile
    CHARACTER(LEN=*), INTENT(IN   ) :: levName         ! variable name
    INTEGER,          INTENT(IN   ) :: lon1            ! lon lower bound
    INTEGER,          INTENT(IN   ) :: lon2            ! lon upper bound
    INTEGER,          INTENT(IN   ) :: lat1            ! lat lower bound
    INTEGER,          INTENT(IN   ) :: lat2            ! lat upper bound
    INTEGER,          INTENT(IN   ) :: lev1            ! lev lower bound
    INTEGER,          INTENT(IN   ) :: lev2            ! lev upper bound
    INTEGER,          INTENT(IN   ) :: time            ! time index
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*8,           POINTER       :: SigLev(:,:,:)   ! sigma levels
    INTEGER,          INTENT(INOUT) :: dir             ! axis direction (1=up;-1=down)
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

  CALL NC_GET_SIGMA_LEVELS_C( fID,  ncFile, levName, lon1, lon2, lat1, &
                              lat2, lev1,   lev2,    time, dir,  RC,   &
                              SigLev8=SigLev )

  END SUBROUTINE NC_GET_SIGMA_LEVELS_DP
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_Sigma_Levels_C
!
! !DESCRIPTION: Routine to get the sigma levels from the netCDF file
! within the given grid bounds and for the given time index. This routine
! attempts to construct the 3D sigma values from provided variable levName.
! The vertical coordinate system is determined based upon the variable
! attribute "standard\_name".
!\\
!\\
! For now, only hybrid sigma coordinate systems are supported, and the
! standard\_name attribute must follow CF conventions and be set to
! "atmosphere\_hybrid\_sigma\_pressure\_coordinate".
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NC_GET_SIGMA_LEVELS_C( fID,  ncFile, levName, lon1, lon2, lat1, &
                                    lat2, lev1,   lev2,    time, dir,  RC,   &
                                    SigLev4, SigLev8 )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID             ! PIO File descriptor
    CHARACTER(LEN=*), INTENT(IN   ) :: ncFile          ! ncFile
    CHARACTER(LEN=*), INTENT(IN   ) :: levName         ! variable name
    INTEGER,          INTENT(IN   ) :: lon1            ! lon lower bound
    INTEGER,          INTENT(IN   ) :: lon2            ! lon upper bound
    INTEGER,          INTENT(IN   ) :: lat1            ! lat lower bound
    INTEGER,          INTENT(IN   ) :: lat2            ! lat upper bound
    INTEGER,          INTENT(IN   ) :: lev1            ! lev lower bound
    INTEGER,          INTENT(IN   ) :: lev2            ! lev upper bound
    INTEGER,          INTENT(IN   ) :: time            ! time index
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT) :: dir             ! axis direction (1=up;-1=down)
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
    REAL*4, OPTIONAL, POINTER       :: SigLev4(:,:,:)  ! sigma levels w/in
    REAL*8, OPTIONAL, POINTER       :: SigLev8(:,:,:)  ! specified boundaries
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    LOGICAL            :: found
    INTEGER            :: a_type    ! netCDF attribute type

    ! Strings
    CHARACTER(LEN=255) :: stdname
    CHARACTER(LEN=255) :: a_name    ! netCDF attribute name
    CHARACTER(LEN=255) :: a_val     ! netCDF attribute value

    ! PIO variables
    INTEGER            :: tmp_varid, tmp_ierr
    INTEGER(PIO_OFFSET_KIND) :: tmp_len

    !========================================================================
    ! NC_GET_SIGMA_LEVELS begins here
    !========================================================================

    ! Initialize
    RC = 0

    !------------------------------------------------------------------------
    ! Test that the level index variable exists
    !------------------------------------------------------------------------
    tmp_ierr = pio_inq_varid(fID, TRIM(levName), tmp_varid)
    found = (tmp_ierr == PIO_NOERR)
    IF ( .not. found ) THEN
       WRITE(*,*) 'Cannot find level variable ',                             &
                  TRIM(levName), ' in ', TRIM(ncFile), '!'
       RC = -999
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Look for the "standard_name" or "long_name" attribute,
    ! which will be used to identify the vertical coordinate
    !------------------------------------------------------------------------

    ! First look for "standard_name"
    a_name = "standard_name"
    tmp_ierr = pio_inq_varid(fID, TRIM(levName), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       found = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       found = (tmp_ierr == PIO_NOERR)
    ENDIF

    ! If not found, then look for "long_name"
    IF ( .not. found ) THEN
       a_name = "long_name"
       tmp_ierr = pio_inq_varid(fID, TRIM(levName), tmp_varid)
       IF (tmp_ierr /= PIO_NOERR) THEN
          found = .FALSE.
       ELSE
          tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
          found = (tmp_ierr == PIO_NOERR)
       ENDIF

       ! If neither attribute is found, then exit with error
       IF ( .not. found ) THEN
          WRITE(*,*) 'Cannot find level attribute ', TRIM(a_name),           &
               ' in variable ', TRIM(levName), ' - File: ', TRIM(ncFile), '!'
          RC = -999
          RETURN
       ENDIF
    ENDIF

    ! Read the "standard_name" or "long_name" attribute (whichever is found)
    tmp_ierr = pio_inq_varid(fID, TRIM(levName), tmp_varid)
    tmp_ierr = pio_get_att(fID, tmp_varid, TRIM(a_name), a_val)

    !------------------------------------------------------------------------
    ! Call functions to calculate sigma levels depending on the coordinate
    ! system.
    !------------------------------------------------------------------------
    IF ( TRIM(a_val) == 'atmosphere_hybrid_sigma_pressure_coordinate' ) THEN

       IF ( PRESENT( SigLev4 ) ) THEN

          ! Return 4-byte real array
          CALL NC_GET_SIG_FROM_HYBRID( fID,  levName, lon1, lon2,            &
                                       lat1, lat2,    lev1, lev2,            &
                                       time, dir,     RC,   SigLev4=SigLev4 )
       ELSE IF ( PRESENT( SigLev8 ) ) THEN

          ! Return 8-byte real array
          CALL NC_GET_SIG_FROM_HYBRID( fID,  levName, lon1, lon2,            &
                                       lat1, lat2,    lev1, lev2,            &
                                       time,  dir,    RC,   SigLev8=SigLev8 )
       ELSE

          ! Othrwise exit with error
          WRITE(*,*) 'SigLev array is missing!'
          RC = -999
          RETURN
       ENDIF
       IF ( RC /= 0 ) RETURN

    ELSE

       ! NOTE: for now, only hybrid sigma coordinates are supported!
       ! So exit with error if we get this far
       WRITE(*,*) 'Invalid level standard name: ', TRIM(a_val),              &
            ' in ', TRIM(ncFile)
       RC = -999
       RETURN
    ENDIF

    ! Return w/ success
    RC = 0

  END SUBROUTINE NC_GET_SIGMA_LEVELS_C
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nc_Get_Sig_From_Hybrid
!
! !DESCRIPTION: Calculates the sigma level field for a hybrid sigma coordinate
! system:
!
! sigma(i,j,l,t) = ( a(l) * p0 + b(l) * ps(i,j,t) ) / ps(i,j,t)
!
! or (p0=1):
!
! sigma(i,j,l,t) = ( ap(l) + b(l) * ps(i,j,t) ) / ps(i,j,t)
!
! where sigma are the sigma levels, ap and bp are the hybrid sigma coordinates,
! p0 is the constant reference pressure, and ps is the surface pressure. The
! variable names of ap, p0, bp, and ps are taken from level attribute
! `formula\_terms`.
!\\
!\\
! The direction of the vertical coordinate system is determined from attribute
! `positive` (up or down) or - if not found - from the b values, whereby it is
! assumed that the higher b value is found at the surface. The return argument
! dir is set to 1 for upward coordinates (level 1 is surface level) and -1 for
! downward coordinates (level 1 is top of atmosphere).
!\\
!\\
! !REMARKS:
! Example of valid netCDF meta-data: The attributes `standard\_name` and
! `formula\_terms` are required, as is the 3D surface pressure field.
!
! double lev(lev) ;\\
!        lev:standard_name = "atmosphere_hybrid_sigma_pressure_coordinate" ;\\
!        lev:units = "level" ;\\
!        lev:positive = "down" ;\\
!        lev:formula_terms = "ap: hyam b: hybm ps: PS" ;\\
! double hyam(nhym) ;\\
!        hyam:long_name = "hybrid A coefficient at layer midpoints" ;\\
!        hyam:units = "hPa" ;\\
! double hybm(nhym) ;\\
!        hybm:long_name = "hybrid B coefficient at layer midpoints" ;\\
!        hybm:units = "1" ;\\
! double time(time) ;\\
!        time:standard_name = "time" ;\\
!        time:units = "days since 2000-01-01 00:00:00" ;\\
!        time:calendar = "standard" ;\\
! double PS(time, lat, lon) ;\\
!        PS:long_name = "surface pressure" ;\\
!        PS:units = "hPa" ;\\
!
! !INTERFACE:
!
  SUBROUTINE NC_GET_SIG_FROM_HYBRID ( fID,  levName, lon1,   lon2, lat1,     &
                                      lat2, lev1,    lev2,   time, dir,      &
                                      RC,   sigLev4, sigLev8                )
!
! !INPUT PARAMETERS:
!
    TYPE(file_desc_t), INTENT(INOUT) :: fID             ! PIO File descriptor
    CHARACTER(LEN=*), INTENT(IN   ) :: levName         ! variable name
    INTEGER,          INTENT(IN   ) :: lon1            ! lon lower bound
    INTEGER,          INTENT(IN   ) :: lon2            ! lon upper bound
    INTEGER,          INTENT(IN   ) :: lat1            ! lat lower bound
    INTEGER,          INTENT(IN   ) :: lat2            ! lat upper bound
    INTEGER,          INTENT(IN   ) :: lev1            ! lev lower bound
    INTEGER,          INTENT(IN   ) :: lev2            ! lev upper bound
    INTEGER,          INTENT(IN   ) :: time            ! time index
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*4, OPTIONAL, POINTER       :: SigLev4(:,:,:)  ! sigma levels w/in
    REAL*8, OPTIONAL, POINTER       :: SigLev8(:,:,:)  ! specified boundaries
    INTEGER,          INTENT(  OUT) :: dir             ! axis direction (1=up;-1=down)
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER              :: I, J, l1, l2, AS
    INTEGER              :: nlev, nlat, nlon
    INTEGER              :: nlevs
    INTEGER              :: st1d(1), ct1d(1)
    LOGICAL              :: ok
    REAL*4, POINTER      :: a(:)
    REAL*4, POINTER      :: b(:)
    REAL*4, POINTER      :: ps(:,:,:,:)
    REAL*8               :: p0
    CHARACTER(LEN=255)   :: formula, ThisUnit
    CHARACTER(LEN=255)   :: aname, bname, psname, p0name
    CHARACTER(LEN=255)   :: a_name    ! netCDF attribute name
    INTEGER              :: a_type    ! netCDF attribute type

    ! PIO variables
    INTEGER              :: pio_varid
    INTEGER              :: pio_ierr
    INTEGER              :: tmp_dimid, tmp_ierr
    INTEGER              :: tmp_varid
    INTEGER(PIO_OFFSET_KIND) :: tmp_len
    REAL*8               :: p0_arr(1)

    !======================================================================
    ! NC_GET_SIG_FROM_HYBRID begins here
    !======================================================================

    ! Init
    p0 = -999.d0
    a  => NULL()
    b  => NULL()
    ps => NULL()

    ! Get desired grid dimensions.
    nlon = lon2 - lon1 + 1
    nlat = lat2 - lat1 + 1
    nlev = lev2 - lev1 + 1

    ! Get dimension length
    tmp_ierr = pio_inq_dimid(fID, TRIM(LevName), tmp_dimid)
    tmp_ierr = pio_inq_dimlen(fID, tmp_dimid, nlevs)

    ! Sanity check
    IF ( nlevs < nlev ) THEN
       WRITE(*,*) TRIM(LevName), ' is only of length ', nlevs, ' - required is: ', nlev
       RC = -999
       RETURN
    ENDIF

    !------------------------------------------------------------------------
    ! Get formula and parse variable names (ap, bp, p0, ps)
    !------------------------------------------------------------------------

    ! Get formula
    a_name = "formula_terms"
    tmp_ierr = pio_inq_varid(fID, TRIM(levName), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       ok = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       ok = (tmp_ierr == PIO_NOERR)
    ENDIF
    IF ( .NOT. ok ) THEN
       WRITE(*,*) 'Cannot find attribute ', TRIM(a_name), ' in variable ', &
                  TRIM(levName)
       RC = -999
       RETURN
    ENDIF
    pio_ierr = pio_inq_varid(fID, TRIM(levName), pio_varid)
    pio_ierr = pio_get_att(fID, pio_varid, TRIM(a_name), formula)

    ! Get variable names
    !-------------------
    I = INDEX( formula, 'a:' )
    IF ( I > 0 ) THEN
       CALL GetVarFromFormula( formula, 'a:',  aname, RC )
       IF ( RC /= 0 ) RETURN
       CALL GetVarFromFormula( formula, 'p0:', p0name, RC )
       IF ( RC /= 0 ) RETURN
    ELSE
       CALL GetVarFromFormula( formula, 'ap:', aname, RC )
       IF ( RC /= 0 ) RETURN
       p0 = 1.0d0
    ENDIF
    IF ( RC /= 0 ) RETURN

    CALL GetVarFromFormula( formula, 'b:', bname, RC )
    IF ( RC /= 0 ) RETURN

    CALL GetVarFromFormula( formula, 'ps:', psname, RC )
    IF ( RC /= 0 ) RETURN

    !------------------------------------------------------------------------
    ! Read variables from file.
    !------------------------------------------------------------------------

    ALLOCATE ( a(nlevs), b(nlevs) )
    st1d = (/ 1     /)
    ct1d = (/ nlevs /)

    ! read a
    !-------
    tmp_ierr = pio_inq_varid(fID, TRIM(aname), tmp_varid)
    IF ( tmp_ierr /= PIO_NOERR ) THEN
       WRITE(*,*) 'Cannot find variable ', TRIM(aname), '!'
       RC = -999
       RETURN
    ENDIF
    pio_ierr = pio_inq_varid(fID, TRIM(aname), pio_varid)
    pio_ierr = pio_get_var(fID, pio_varid, st1d, ct1d, a)

    ! eventually read p0
    !-------------------
    IF ( p0 < 0.0d0 ) THEN
       tmp_ierr = pio_inq_varid(fID, TRIM(p0name), tmp_varid)
       IF ( tmp_ierr /= PIO_NOERR ) THEN
          WRITE(*,*) 'Cannot find variable ', TRIM(p0name), '!'
          RC = -999
          RETURN
       ENDIF
       pio_ierr = pio_inq_varid(fID, TRIM(p0name), pio_varid)
       pio_ierr = pio_get_var(fID, pio_varid, p0_arr)
       p0 = p0_arr(1)
    ENDIF

    ! read b
    !-------
    tmp_ierr = pio_inq_varid(fID, TRIM(bname), tmp_varid)
    IF ( tmp_ierr /= PIO_NOERR ) THEN
       WRITE(*,*) 'Cannot find variable ', TRIM(bname), '!'
       RC = -999
       RETURN
    ENDIF
    pio_ierr = pio_inq_varid(fID, TRIM(bname), pio_varid)
    pio_ierr = pio_get_var(fID, pio_varid, st1d, ct1d, b)

    ! Read ps
    !--------
    CALL NC_READ_ARR( fID, TRIM(psname), lon1, lon2, lat1, &
                      lat2, 0, 0, time,  time, ps, VarUnit=thisUnit, RC=RC )
    IF ( RC /= 0 ) RETURN

    !------------------------------------------------------------------------
    ! Determine positive axis ('up' or 'down')
    ! Try to read it from the netCDF meta data (attribute `positive`). If not
    ! found, determine it from b values (b value at surface higher than at
    ! top of atmosphere).
    !------------------------------------------------------------------------
    a_name = "positive"
    tmp_ierr = pio_inq_varid(fID, TRIM(levName), tmp_varid)
    IF (tmp_ierr /= PIO_NOERR) THEN
       ok = .FALSE.
    ELSE
       tmp_ierr = pio_inq_att(fID, tmp_varid, TRIM(a_name), a_type, tmp_len)
       ok = (tmp_ierr == PIO_NOERR)
    ENDIF
    IF ( ok ) THEN
       pio_ierr = pio_inq_varid(fID, TRIM(levName), pio_varid)
       pio_ierr = pio_get_att(fID, pio_varid, TRIM(a_name), formula)
       IF ( TRIM(formula) == 'up' ) THEN
          dir = 1
       ELSEIF ( TRIM(formula) == 'down' ) THEN
          dir = -1
       ELSE
          WRITE(*,*) 'level attribute `positive` must be `up` ', &
                     'or `down`, instead: ', TRIM(formula)
          RC = -999
          RETURN
       ENDIF

    ! determine direction from b values.
    ELSE

       IF ( b(1) > b(nlevs) ) THEN
          dir = 1
       ELSE
          dir = -1
       ENDIF
    ENDIF

    !------------------------------------------------------------------------
    ! Determine vertical indeces to be used. It is possible to calculate
    ! the pressure only for a given number of layers (as specified by input
    ! arguments lev1 and lev2). Assume those are always from bottom to top,
    ! i.e. counting `upwards`.
    !------------------------------------------------------------------------

    IF ( dir == -1 ) THEN
       l1 = nlevs - lev2 + 1
       l2 = nlevs - lev1 + 1
    ELSE
       l1 = lev1
       l2 = lev2
    ENDIF

    !------------------------------------------------------------------------
    ! Calculate sigma values at grid edges
    !------------------------------------------------------------------------

    IF ( PRESENT(SigLev4) ) THEN
       IF ( ASSOCIATED(SigLev4) ) DEALLOCATE(SigLev4)
       ALLOCATE(SigLev4(nlon,nlat,nlev),STAT=AS)
    ELSEIF ( PRESENT(SigLev8) ) THEN
       IF ( ASSOCIATED(SigLev8) ) DEALLOCATE(SigLev8)
       ALLOCATE(SigLev8(nlon,nlat,nlev),STAT=AS)
    ELSE
       WRITE(*,*) 'SigLev must be provided!'
       RC = -999
       RETURN
    ENDIF
    IF ( AS /= 0 ) THEN
       WRITE(*,*) 'Cannot allocate SigLev!'
       RC = -999
       RETURN
    ENDIF

    DO J=1,nlat
    DO I=1,nlon
       IF ( PRESENT(SigLev4) ) THEN
          SigLev4(i,j,:) = ( ( a(l1:l2) * p0 ) + ( b(l1:l2) * ps(i,j,1,1) ) ) &
                        / ps(i,j,1,1)
       ELSE
          SigLev8(i,j,:) = ( ( a(l1:l2) * p0 ) + ( b(l1:l2) * ps(i,j,1,1) ) ) &
                        / ps(i,j,1,1)
       ENDIF
    ENDDO
    ENDDO

    ! Cleanup
    IF ( ASSOCIATED(a ) ) DEALLOCATE(a )
    IF ( ASSOCIATED(b ) ) DEALLOCATE(b )
    IF ( ASSOCIATED(ps) ) DEALLOCATE(ps)

    ! Return w/ success
    RC = 0

  END SUBROUTINE NC_GET_SIG_FROM_HYBRID
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: GetVarFromFormula
!
! !DESCRIPTION: helper function to extract the variable name from a vertical
! coordinate formula.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GetVarFromFormula ( formula, inname, outname, RC )
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN   ) :: formula
    CHARACTER(LEN=*), INTENT(IN   ) :: inname
!
! !INPUT/OUTPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(  OUT) :: outname
    INTEGER,          INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER              :: I, J, IDX, LN

    !======================================================================
    ! GetVarFromFormula begins here
    !======================================================================

    ! maximum length
    LN = LEN(TRIM(formula))

    ! Get start index of string
    !--------------------------
    I = INDEX( TRIM(formula), TRIM(inname) )
    IF ( I <= 0 ) THEN
       WRITE(*,*) 'Cannot extract ', TRIM(inname), ' from ', TRIM(formula)
       RC = -999
       RETURN
    ENDIF

    ! The variable name follows the formula string plus one space!
    I = I + LEN(inname) + 1

    outname = ''
    IDX = 1
    DO J = I, LN
       IF ( formula(J:J) == ' ' ) EXIT
       outname(IDX:IDX) = formula(J:J)
       IDX = IDX + 1
    ENDDO

    ! Return w/ success
    RC = 0

  END SUBROUTINE GetVarFromFormula
!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get_Tau0
!
! !DESCRIPTION: Function GET\_TAU0\_6A returns the corresponding TAU0 value
!  for the first day of a given MONTH of a given YEAR.  This is necessary to
!  index monthly mean binary punch files, which are used as input to GEOS-Chem.
!\\
!\\
!  This function takes 3 mandatory arguments (MONTH, DAY, YEAR) and 3
!  optional arguments (HOUR, MIN, SEC).  It is intended to replace the current
!  2-argument version of GET\_TAU0.  The advantage being that GET\_TAU0\_6A
!  can compute a TAU0 for any date and time in the GEOS-Chem epoch, rather
!  than just the first day of each month.  Overload this w/ an interface so
!  that the user can also choose the version of GET\_TAU0 w/ 2 arguments
!  (MONTH, YEAR), which is the prior version.
!\\
!\\
! !INTERFACE:
!
  FUNCTION GET_TAU0( MONTH, DAY, YEAR, HOUR, MIN, SEC ) RESULT( THIS_TAU0 )
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)           :: MONTH
    INTEGER, INTENT(IN)           :: DAY
    INTEGER, INTENT(IN)           :: YEAR
    INTEGER, INTENT(IN), OPTIONAL :: HOUR
    INTEGER, INTENT(IN), OPTIONAL :: MIN
    INTEGER, INTENT(IN), OPTIONAL :: SEC
!
! !RETURN VALUE:
!
    REAL*8                        :: THIS_TAU0   ! TAU0 timestamp
!
! !REMARKS:
!  TAU0 is hours elapsed since 00:00 GMT on 01 Jan 1985.
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: TMP_HOUR, TMP_MIN, TMP_SEC
    REAL*8  :: DAYS

    !=======================================================================
    ! GET_TAU0 begins here!
    !=======================================================================

    ! Error checking
    IF ( MONTH < 1 .or. MONTH > 12 ) THEN
       WRITE( 6, 100 )
100    FORMAT( 'Invalid MONTH selection!  STOP in GET_TAU0 (hco_pio_mod.F90)!' )
       STOP
    ENDIF

    ! Error checking
    IF ( DAY < 1 .or. DAY > 31 ) THEN
       WRITE( 6, 110 )
110    FORMAT( 'Invalid DAY selection!  STOP in GET_TAU0 (hco_pio_mod.F90)!' )
       STOP
    ENDIF

    ! If HOUR isn't passed, default to 0
    IF ( PRESENT( HOUR ) ) THEN
       TMP_HOUR = HOUR
    ELSE
       TMP_HOUR = 0
    ENDIF

    ! If MIN isn't passed, default to 0
    IF ( PRESENT( MIN ) ) THEN
       TMP_MIN = MIN
    ELSE
       TMP_MIN = 0
    ENDIF

    ! If SEC isn't passed, default to 0
    IF ( PRESENT( SEC ) ) THEN
       TMP_SEC = SEC
    ELSE
       TMP_SEC = 0
    ENDIF

    ! Number of days since midnight on 1/1/1985
    THIS_TAU0 = JULDAY( YEAR, MONTH, DBLE( DAY ) ) - 2446066.5d0

    ! Multiply by 24 to get hours since 1/1/1985
    ! Also add in the hours elapsed since midnight on this date
    THIS_TAU0 = ( THIS_TAU0 * 24d0 ) + ( TMP_HOUR         ) + &
                ( TMP_MIN   / 60d0 ) + ( TMP_SEC / 3600d0 )

  END FUNCTION GET_TAU0
END MODULE HCO_PIO_MOD
#endif
