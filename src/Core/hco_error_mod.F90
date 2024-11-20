!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_error_mod.F90
!
! !DESCRIPTION: Module HCO\_Error\_Mod contains routines and variables
! for error handling and logfile messages in HEMCO. It also contains
! definitions of some globally used parameters, such as the single/double
! precision as well as the HEMCO precision definitions. The HEMCO precision
! is used for almost all HEMCO internal data arrays and can be changed below
! if required.
!\\
!\\
! The error settings are specified in the HEMCO configuration file and
! error handling is performed according to these settings. They include:
!
! 1.  HEMCO logfile: HEMCO information is written into one or two logfiles,
!     each with logical unit number, LUN, indicating output stream. LUNs for
!     the logfiles are stored in HcoState%Config%outLUN and Err%LUN. Err%LUN
!     corresponds to the LUN of HcoState%Config%LogFile is configured using
!     entry LogFile in HEMCO_Config.rc.
!
!     HcoState%Config%outLUN is necessary to allow prints prior to reading
!     HEMCO_Config.rc. It is always set to stdout unless HEMCO is
!     initialized with an LUN from an external model, as is the case for CESM
!     (atm.log). It may be used throughout HEMCO to call HCO_MSG, HCO_WARNING,
!     or HCO_ERROR without passing an Err object, and the result will be
!     writing to this file instead of LogFile configured in HEMCO_Config.rc.
!
!     Err%LUN contains the LUN for most HEMCO prints and can be std out, an
!     existing file from a separate model, or a dedicated HEMCO file opened
!     and closed locally, e.g. HEMCO.log. The 'LogFile' field stores the
!     name of this file.
!  
!     Use the LogFile setting in HEMCO_Config.rc to specify Err%Lun. An
!     asterisk sets it to stdout. A filename sets it to a dedicated file,
!     e.g. HEMCO.log. If using CESM then LogFile is omitted from the
!     configuration file and it is set internally to atm.log.
!
! 2.  Verbose: Logical of whether to enable verbose prints.
!
! 3.  VerboseOnCores: Set to 'root' to print verbose only on root core. Set to
!     anything else to be verbose on all cores.
!
! The error settings are set via subroutine HCO_ERROR_SET, called when
! reading section 'SETTINGS' of the HEMCO configuration file (subroutine
! Config_ReadFile in hco_config_mod.F90). The currently active verbose
! settings can be checked using subroutines HCO_IsVerb and
! HCO_VERBOSE_INQ. Messages can be written into the logfile using
! subroutine HCO_MSG. Note that the logfile must be open, either from
! external model, or locally with HCO_LOGFILE_OPEN, before writing to it.
!
! As of HEMCO v2.0, all HEMCO error variables are organized in derived
! type object HcoErr. HcoErr is a component of the HEMCO configuration
! object (type ConfigObj, see hco\_types\_mod.F90). It must be passed
! explicitly to all error routines. This design allows the invocation
! of multiple independent HEMCO instances at the same time (which may
! have different HEMCO error settings).
! Beware that different error print subroutines have different behaviors:
!   HCO_ERROR:
!     - Always prints from all cores, regardless of Verbose and VerboseOnRoot
!   HCO_WARNING:
!     - Only prints if verbose (incorporates VerboseOnRoot)
!   HCO_MSG:
!     - Only prints if verbose and on root
!   True for all three:
!     - Prints to 'LogFile' if passed Err object
!     - Prints to stdout or optional passed LUN if not passed Err object
!
! !INTERFACE:
!
MODULE HCO_Error_Mod
!
! !USES:
!
#if defined( MAPL_ESMF )
    USE MAPL_Base, ONLY: MAPL_UNDEF
#endif
  USE ISO_Fortran_Env, ONLY : INT32, INT64, REAL32, REAL64

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC           :: HCO_ERROR
  PUBLIC           :: HCO_WARNING
  PUBLIC           :: HCO_MSG
  PUBLIC           :: HCO_ENTER
  PUBLIC           :: HCO_LEAVE
  PUBLIC           :: HCO_ERROR_SET
  PUBLIC           :: HCO_ERROR_FINAL
  PUBLIC           :: HCO_IsVerb
  PUBLIC           :: HCO_LOGFILE_OPEN
  PUBLIC           :: HCO_LOGFILE_CLOSE
!
! !MODULE VARIABLES:
!
  ! Double and single precision definitions
  INTEGER, PARAMETER, PUBLIC  :: dp = REAL64         ! Double (r8)
  INTEGER, PARAMETER, PUBLIC  :: sp = REAL32         ! Single (r4)
#ifdef USE_REAL8
  INTEGER, PARAMETER, PUBLIC  :: hp = dp             ! HEMCO precision = r8
#else
  INTEGER, PARAMETER, PUBLIC  :: hp = sp             ! HEMCO precision = r4
#endif
  INTEGER, PARAMETER, PUBLIC  :: i4 = INT32          ! FourByteInt
  INTEGER, PARAMETER, PUBLIC  :: i8 = INT64          ! EightByteInt

  ! Error success/failure definitions
  INTEGER, PARAMETER, PUBLIC  :: HCO_SUCCESS = 0
  INTEGER, PARAMETER, PUBLIC  :: HCO_FAIL    = -999

  ! Tiny value for math operations:
  ! --> deprecated. Use TINY(1.0_hp) instead!
  REAL(hp), PARAMETER, PUBLIC :: HCO_TINY    = 1.0e-32_hp

  ! Missing value
  ! Note: define missing value as single precision because all data arrays
  ! are read/stored in single precision.
#if defined( MAPL_ESMF )
  REAL(sp), PARAMETER, PUBLIC :: HCO_MISSVAL = MAPL_UNDEF
#else
  REAL(sp), PARAMETER, PUBLIC :: HCO_MISSVAL = -1.e31_sp
#endif

  ! HEMCO version number.
  CHARACTER(LEN=12), PARAMETER, PUBLIC :: HCO_VERSION = '3.10.0'

  INTERFACE HCO_Error
     MODULE PROCEDURE HCO_ErrorNoErr
     MODULE PROCEDURE HCO_ErrorErr
  END INTERFACE HCO_Error

  INTERFACE HCO_Warning
     MODULE PROCEDURE HCO_WarningNoErr
     MODULE PROCEDURE HCO_WarningErr
  END INTERFACE HCO_Warning

  INTERFACE HCO_MSG
     MODULE PROCEDURE HCO_MsgNoErr
     MODULE PROCEDURE HCO_MsgErr
  END INTERFACE HCO_MSG

!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !PRIVATE VARIABLES:
!
  TYPE, PUBLIC :: HcoErr
     LOGICAL                     :: FirstOpen  ! First time using new log file?
     LOGICAL                     :: IsRoot     ! Is this root?
     LOGICAL                     :: LogIsOpen  ! Is log open?
     LOGICAL                     :: doVerbose  ! Verbose print?
     INTEGER                     :: nWarnings  ! Warning counter
     INTEGER                     :: CurrLoc    ! Call depth used for traceback
     CHARACTER(LEN=255), POINTER :: Loc(:)     ! Location array used for traceback
     CHARACTER(LEN=255)          :: LogFile    ! Log file string (*=stdout)
     INTEGER                     :: Lun        ! logical unit number of file to write to
  END TYPE HcoErr

  ! MAXNEST is the maximum accepted subroutines nesting level.
  ! This only applies to routines with activated error tracking,
  ! i.e. which use HCO_ENTER/HCO_LEAVE statements.
  INTEGER, PARAMETER       :: MAXNEST =  10

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Error
!
! !DESCRIPTION: Subroutine HCO\_Error promts an error message and sets RC to
! HCO\_FAIL. Note that this routine does not stop a run, but it will cause a
! stop at higher level (when RC gets evaluated). Prints are done via calls
! to HCO_MSG and pass the Err object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ErrorErr( Err, ErrMsg, RC, THISLOC )
!
! !INPUT PARAMETERS:
!
    TYPE(HcoErr),     POINTER                  :: Err
    CHARACTER(LEN=*), INTENT(IN   )            :: ErrMsg
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: THISLOC
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER :: I, J
    CHARACTER(LEN=1023) :: MSG

    !======================================================================
    ! HCO_ERROR (with Err object passed) begins here
    !======================================================================

    ! Print error message
    MSG =  'HEMCO ERROR: ' // TRIM(ErrMsg)
    CALL HCO_MSG ( Err, MSG, SEP1='!' )

    ! Print error location
    IF ( PRESENT(THISLOC) ) THEN
       MSG = 'ERROR LOCATION: ' // TRIM( THISLOC )
       CALL HCO_MSG ( Err, MSG )

    ! Traceback
    ELSE
       DO I = 0, Err%CurrLoc-1
          J = Err%CurrLoc-I
          MSG =  'ERROR LOCATION: ' // TRIM( Err%Loc(J) )
          CALL HCO_MSG ( Err, MSG )
       ENDDO
    ENDIF

    MSG = ''
    CALL HCO_MSG ( Err, MSG, SEP2='!' )

    ! Return w/ error
    RC = HCO_FAIL

  END SUBROUTINE HCO_ErrorErr
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Error
!
! !DESCRIPTION: Subroutine HCO\_Error promts an error message and sets RC to
! HCO\_FAIL. Note that this routine does not stop a run, but it will cause a
! stop at higher level (when RC gets evaluated). Error messages are
! printed by every core regardless of Verbose and VerboseOnRoot settings
! in HEMCO_Config.rc.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ErrorNoErr( ErrMsg, RC, THISLOC, LUN )
!
! !USES:
!
#if defined( ESMF_ )
#include "MAPL_Generic.h"
    USE ESMF
    USE MAPLBase_Mod
#endif
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN   )            :: ErrMsg
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: THISLOC
    INTEGER,          INTENT(IN   ), OPTIONAL  :: LUN
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER             :: I, J, outLUN
    CHARACTER(LEN=1023) :: MSG, MSG1, MSG2
#if defined( ESMF_)
    INTEGER             :: localPET, STATUS
    CHARACTER(4)        :: localPETchar
    TYPE(ESMF_VM)       :: VM
#endif

    !======================================================================
    ! HCO_ERROR (without Err object passed) begins here
    !======================================================================

    ! Specify where to write
    IF ( PRESENT(LUN) ) THEN
       outLUN = LUN
    ELSE
       outLUN = 6
    ENDIF

    ! Construct error message
#if defined( ESMF_ )
    ! Get current thread number
    CALL ESMF_VMGetCurrent(VM, RC=STATUS)
    CALL ESMF_VmGet( VM, localPET=localPET, __RC__ )
    WRITE(localPETchar,'(I4.4)') localPET
    MSG1 = 'HEMCO ERROR ['//TRIM(localPETchar)//']: '//TRIM(ErrMsg)
#else
    MSG1 = 'HEMCO ERROR: '//TRIM(ErrMsg)
#endif
    MSG2 = ''
    IF ( PRESENT(THISLOC) ) THEN
       MSG2 = NEW_LINE('a') // ' --> LOCATION: ' // TRIM( THISLOC )
    ENDIF
    MSG = NEW_LINE('a') // TRIM(MSG1) // TRIM(MSG2)

    ! Print error message
    WRITE(outLUN,*) TRIM(MSG)

    ! Return w/ error
    RC = HCO_FAIL

  END SUBROUTINE HCO_ErrorNoErr
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Warning
!
! !DESCRIPTION: Subroutine HCO\_Warning prompts a warning message without
! forcing HEMCO to stop, i.e. return code is set to HCO\_SUCCESS. It only
! prints on threads where verbose is true, as set in configuration file
! HEMCO_Config.rc entries Verbose (must be true to print warnings) and
! VerboseOnRoot (must be root to print warnings only on root core).
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_WarningErr( Err, ErrMsg, RC, THISLOC )
!
! !INPUT PARAMETERS"
!
    TYPE(HcoErr),     POINTER                  :: Err
    CHARACTER(LEN=*), INTENT(IN   )            :: ErrMsg
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: THISLOC
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER            :: WLEV
    CHARACTER(LEN=255) :: MSG

    !======================================================================
    ! HCO_WARNING (with Err object passed) begins here
    !======================================================================

    ! Return w/ success
    RC = HCO_SUCCESS

    ! Only print warnings when verbose output is requested
    IF ( .NOT. HCO_IsVerb(Err) ) RETURN

    ! Print warning
    MSG = 'HEMCO WARNING: ' // TRIM( ErrMsg )
    CALL HCO_MSG ( Err, MSG )

    ! Print location
    IF ( PRESENT(THISLOC) ) THEN
       MSG = '--> LOCATION: ' // TRIM(THISLOC)
       CALL HCO_MSG ( Err, MSG )
    ELSEIF ( Err%CurrLoc > 0 ) THEN
       MSG = '--> LOCATION: ' // TRIM(Err%Loc(Err%CurrLoc))
       CALL HCO_MSG ( Err, MSG )
    ENDIF

    ! Increase # of warnings
    Err%nWarnings = Err%nWarnings + 1

  END SUBROUTINE HCO_WarningErr
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Warning
!
! !DESCRIPTION: Subroutine HCO\_Warning prompts a warning message without
! forcing HEMCO to stop, i.e. return code is set to HCO\_SUCCESS. Error
! messages are printed by every core regardless of Verbose and VerboseOnRoot
! settings in HEMCO_Config.rc.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_WarningNoErr( ErrMsg, RC, THISLOC, LUN )
!
! !INPUT PARAMETERS"
!
    CHARACTER(LEN=*), INTENT(IN   )            :: ErrMsg
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: THISLOC
    INTEGER,          INTENT(IN   ), OPTIONAL  :: LUN
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)            :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER            :: WLEV, outLUN
    CHARACTER(LEN=255) :: MSG

    !======================================================================
    ! HCO_WARNING (with no Err object passed) begins here
    !======================================================================

    ! Specify where to write
    IF ( PRESENT(LUN) ) THEN
       outLUN = LUN
    ELSE
       outLUN = 6
    ENDIF

    ! Print warning
    MSG = 'HEMCO WARNING: ' // TRIM( ErrMsg )
    WRITE( outLUN, '(a)' ) TRIM(MSG)

    ! Print location
    IF ( PRESENT(THISLOC) ) THEN
       MSG = '--> LOCATION: ' // TRIM(THISLOC)
       WRITE( outLUN, '(a)' ) TRIM(MSG)
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE HCO_WarningNoErr
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_MSG
!
! !DESCRIPTION: Subroutine HCO\_MSG prints message to log.
! Sep1 and Sep2 denote line delimiters before and after the message,
! respectively. Messages are only printed if the Err object exists,
! only from the root thread, and only if verbose is true as defined
! in the HEMCO_Config.rc configuration file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_MSGErr( Err, Msg, Sep1, Sep2 )
!
! !INPUT PARAMETERS:
!
    TYPE(HcoErr),     POINTER                  :: Err
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: Msg
    CHARACTER(LEN=1), INTENT(IN   ), OPTIONAL  :: Sep1
    CHARACTER(LEN=1), INTENT(IN   ), OPTIONAL  :: Sep2
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER :: LUN

    !=======================================================================
    ! HCO_MSG (with Err object passed) begins here
    !=======================================================================

    ! If Err is NULL then print message without using Err object settings
    IF ( .NOT. ASSOCIATED( Err) ) CALL HCO_MSG( Msg )

    ! Exit if we are not on the root core
    IF ( .NOT. Err%IsRoot ) RETURN

    ! Exit if Verbose is turned off
    IF ( .NOT. Err%doVerbose ) RETURN

    !=======================================================================
    ! Write message
    !=======================================================================

    ! Get the log file unit. Set to stdout if log is not open
    IF ( Err%LogIsOpen ) THEN
       LUN = Err%LUN
    ELSE
       LUN = 6
    ENDIF

    ! If logfile is open then write to it
    IF ( PRESENT(SEP1) ) THEN
       WRITE( LUN,'(a)' ) REPEAT( SEP1, 79 )
    ENDIF
    IF ( PRESENT(MSG) ) THEN
       WRITE( LUN,'(a)' ) TRIM( MSG )
    ENDIF
    IF ( PRESENT(SEP2) ) THEN
       WRITE( LUN,'(a)' ) REPEAT( SEP2, 79 )
    ENDIF

  END SUBROUTINE HCO_MsgErr
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_MSG
!
! !DESCRIPTION: Subroutine HCO\_MSG prints the passed message to log.
! Sep1 and Sep2 denote line delimiters before and after the message.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_MSGnoErr( Msg, Sep1, Sep2, LUN )
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: Msg
    CHARACTER(LEN=1), INTENT(IN   ), OPTIONAL  :: Sep1
    CHARACTER(LEN=1), INTENT(IN   ), OPTIONAL  :: Sep2
    INTEGER,          INTENT(IN   ), OPTIONAL  :: LUN
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    INTEGER :: outLUN

    !======================================================================
    ! HCO_MSG (with no Err object passed) begins here
    !======================================================================

    ! Specify where to write
    IF ( PRESENT(LUN) ) THEN
       outLUN = LUN
    ELSE
       outLUN = 6
    ENDIF

    ! Print message and optional separator lines
    IF ( PRESENT( SEP1 ) ) THEN
       WRITE( outLUN, '(a)' ) REPEAT( SEP1, 79 )
    ENDIF
    IF ( PRESENT( msg ) ) THEN
       WRITE( outLUN, '(a)' ) TRIM( msg )
    ENDIF
    IF ( PRESENT( SEP2 ) ) THEN
       WRITE( outLUN, '(a)' ) REPEAT( SEP2, 79 )
    ENDIF

  END SUBROUTINE HCO_MsgNoErr
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Enter
!
! !DESCRIPTION: Subroutine HCO\_Enter is called upon entering a routine.
! It organizes the traceback handling. It is recommended to call this
! routine for 'big' routines but NOT for routines/functions that are
! frequently called, e.g. inside of loops!
!\\
!\\
! Note that all subroutines calling HCO\_Enter must also call HCO\_Leave!
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_Enter( Err, thisLoc, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(HcoErr),     POINTER       :: Err
    CHARACTER(LEN=*), INTENT(IN   ) :: thisLoc
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT) :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    CHARACTER(LEN=255)  :: Msg, Loc

    !======================================================================
    ! HCO_ENTER begins here
    !======================================================================

    IF ( .NOT. ASSOCIATED(Err) ) RETURN

    ! Increment position
    Err%CurrLoc = Err%CurrLoc + 1
    IF ( Err%CurrLoc > MaxNest ) THEN
       Msg = 'MaxNest too low, cannot enter ' // TRIM(thisLoc)
       CALL HCO_Error( Err, Msg, RC )
       RETURN
    ENDIF

    ! Error trap
    IF ( Err%CurrLoc <= 0 ) THEN
       Msg = 'CurrLoc is zero, cannot enter: ' // TRIM(thisLoc)
       CALL HCO_Error( Err, Msg, RC )
       RETURN
    ENDIF

    ! Register current routine
    Err%Loc(Err%CurrLoc) = thisLoc

    ! Track location if enabled
    IF ( HCO_IsVerb( Err ) ) THEN
       WRITE(MSG,100) TRIM(thisLoc), Err%CurrLoc
       CALL HCO_Msg( Err, MSG )
    ENDIF

    ! Set RC to success
    RC = HCO_SUCCESS

100 FORMAT( 'HEMCO: Entering ', a, ' (', i2, ')' )

  END SUBROUTINE HCO_Enter
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Leave
!
! !DESCRIPTION: Subroutine HCO\_Leave is called upon leaving a routine.
! It organizes the traceback handling. It is recommended to call this
! routine for 'big' routines but NOT for routines/functions that are
! frequently called, e.g. inside of loops!
!\\
!\\
! Note that all subroutines calling HCO\_Leave must also call HCO\_Enter!
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_Leave( Err, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HcoErr),     POINTER       :: Err
    INTEGER,          INTENT(INOUT) :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    CHARACTER(LEN=255)  :: MSG, LOC

    !======================================================================
    ! HCO_LEAVE begins here
    !======================================================================

    IF ( .NOT. ASSOCIATED(Err) ) RETURN

    ! Track location if enabled
    IF ( HCO_IsVerb( Err ) ) THEN
       WRITE(MSG,110) TRIM(Err%Loc(Err%CurrLoc)), Err%CurrLoc
       CALL HCO_MSG( Err, MSG )
    ENDIF

    ! Remove from list
    Err%Loc(Err%CurrLoc) = ''

    ! Remove current position
    Err%CurrLoc = Err%CurrLoc - 1

    ! Error trap
    IF ( Err%CurrLoc < 0 ) THEN
       Msg = 'CurrLoc is below zero, this should never happen!!'
       CALL HCO_ERROR ( Err, Msg, RC )
       RETURN
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS

110 FORMAT( 'HEMCO: Leaving ', a, ' (', i2, ')' )

  END SUBROUTINE HCO_Leave
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Error_Set
!
! !DESCRIPTION: Subroutine HCO\_Error\_Set defines the HEMCO error
! settings. This routine is called at the beginning of a HEMCO
! simulation. Its input parameter are directly taken from the
! HEMCO configuration file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ERROR_SET( am_I_Root, Err,             LogFile,             &
                            doVerbose, doVerboseOnRoot, RC,      customLUN  )

!
!  !INPUT PARAMETERS:
!
    LOGICAL,           INTENT(IN)     :: am_I_Root       ! Root CPU?
    TYPE(HcoErr),      POINTER        :: Err             ! Error object
    CHARACTER(LEN=*),  INTENT(IN)     :: LogFile         ! logfile path+name
    INTEGER, OPTIONAL, INTENT(IN)     :: customLUN       ! Optional LUN
!
! !INPUT/OUTPUT PARAMETERS:
!
    LOGICAL,           INTENT(INOUT)  :: doVerbose       ! Verbose output T/F?
    LOGICAL,           INTENT(INOUT)  :: doVerboseOnRoot ! =T: Verbose on root only
                                                         ! =F: Verbose on all cores
    INTEGER,           INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    INTEGER :: LUN

    !======================================================================
    ! HCO_ERROR_SET begins here
    !======================================================================

    ! Nothing to do if already defined
    RC = HCO_SUCCESS
    IF ( ASSOCIATED(Err) ) RETURN

    ! Allocate error type
    ALLOCATE(Err)
    ALLOCATE(Err%Loc(MAXNEST))
    Err%Loc(:) = ''

    ! Pass values
    Err%IsRoot  = am_I_Root
    Err%LogFile = TRIM(LogFile)

    ! Specify if verbose will be printed on the root core, or all cores
    IF ( doVerboseOnRoot ) THEN
       Err%doVerbose = ( doVerbose .and. am_I_Root )
    ELSE
       Err%doVerbose = doVerbose
    ENDIF

    ! Init misc. values
    Err%FirstOpen = .TRUE.
    Err%CurrLoc   = 0
    Err%nWarnings = 0

    ! If Logfile is set to '*', set LUN to 6 (writes to default, i.e. stdout)
    ! If customLUN is passed, set LUN to this (assume log is open)
    ! Otherwise, set LUN to 0 (writes to LogFile set in HEMCO_Config.rc)
    IF ( TRIM(Err%LogFile) == '*' ) THEN
       LUN = 6
       Err%LogIsOpen = .TRUE.
    ELSEIF ( PRESENT(customLUN) ) THEN
       LUN = customLUN
       Err%LogIsOpen = .TRUE.
    ELSE
       LUN = 0
       Err%LogIsOpen = .FALSE.
    ENDIF
    Err%Lun = LUN

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE HCO_ERROR_SET
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Error_Final
!
! !DESCRIPTION: Subroutine HCO\_Error\_Final finalizes the error type.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_Error_Final ( Err )
!
!  !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HcoErr),     POINTER        :: Err            ! Error object
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    INTEGER :: STAT

    !======================================================================
    ! HCO_ERROR_FINAL begins here
    !======================================================================

    ! Print summary
    MSG = ' '
    CALL HCO_MSG ( Err, MSG )
    MSG = 'HEMCO ' // TRIM(HCO_VERSION) // ' FINISHED.'
    CALL HCO_MSG ( Err, MSG, SEP1='-' )

    WRITE(MSG,'(A16,I1,A12,I6)') &
       'Warnings: ', Err%nWarnings
    CALL HCO_MSG ( Err, MSG, SEP2='-' )

#ifndef MODEL_CESM
    ! Close the log file
    CALL HCO_Logfile_Close( Err )
#endif

    IF ( ASSOCIATED(Err) ) THEN
       IF ( ASSOCIATED(Err%Loc) ) DEALLOCATE(Err%Loc)
       DEALLOCATE(Err)
    ENDIF
    Err => NULL()

  END SUBROUTINE HCO_Error_Final
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_IsVerb
!
! !DESCRIPTION: Returns true if the HEMCO verbose output is turned on for this core.
! Returns false if Err object is not associated.
!
!\\
!\\
! !INTERFACE:
!
  FUNCTION HCO_IsVerb( Err ) RESULT ( IsVerb )
!
! !INPUT PARAMETERS:
!
    TYPE(HcoErr),  POINTER :: Err            ! Error object
!
! !OUTPUT PARAMETERS:
!
    LOGICAL                :: isVerb
!EOP
!------------------------------------------------------------------------------
!BOC

    !======================================================================
    ! HCO_IsVerb begins here
    !======================================================================

    ! Initialize
    isVerb = .FALSE.

    ! Return if the Err object is null
    IF ( .not. ASSOCIATED( Err ) ) RETURN

    ! Check if "Verbose: .true." was set in the HEMCO_Config.rc file.
    ! If this is called from non-root then result will also reflect
    ! setting of VerboseOnRoot in HEMCO_Config.rc.
    isVerb = Err%doVerbose

  END FUNCTION HCO_IsVerb
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_LOGFILE_OPEN
!
! !DESCRIPTION: Subroutine HCO\_LOGFILE\_OPEN opens the HEMCO logfile
! if using and not yet open.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_LogFile_Open( Err, RC )
!
! !USES:
!
    USE HCO_inquireMod,   ONLY : findFreeLUN
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HcoErr),  POINTER    :: Err            ! Error object
    INTEGER,   INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    CHARACTER(LEN=255) :: MSG
    INTEGER            :: IOS, LUN, FREELUN
    LOGICAL            :: isopen, exists

    !======================================================================
    ! HCO_LOGFILE_OPEN begins here
    !======================================================================

    ! Init
    RC = HCO_SUCCESS

    ! Check if Err object exists
    IF ( .NOT. ASSOCIATED(Err)) THEN
       PRINT *, 'Cannot open logfile - Err object not defined!'
       RC = HCO_FAIL
       RETURN
    ENDIF

    ! Exit if not writing to dedicated HEMCO log
    IF ( TRIM(Err%LogFile) == '*' ) RETURN

#ifdef MODEL_CESM
    ! Exit if using CESM (for safety)
    RETURN
#endif

    ! Never open if we are not on the root CPU
    IF ( .NOT. Err%IsRoot ) RETURN

    ! Find free LUN for the log file
    FREELUN = findFreeLun()

    ! Inquire if file is already open
    INQUIRE( FILE=TRIM(Err%LogFile), OPENED=isOpen, EXIST=exists, NUMBER=LUN )

    ! File exists and is opened ==> nothing to do
    IF ( exists .AND. isOpen ) THEN
       Err%LUN       = LUN
       Err%LogIsOpen = .TRUE.

    ! File exists but not opened ==> reopen
    ELSEIF (exists .AND. .NOT. isOpen ) THEN

       ! Replace existing file on first call
       IF ( Err%FirstOpen ) THEN
          OPEN ( UNIT=FREELUN,   FILE=TRIM(Err%LogFile), STATUS='REPLACE', &
               ACTION='WRITE', FORM='FORMATTED',       IOSTAT=IOS         )
          IF ( IOS /= 0 ) THEN
             PRINT *, 'Cannot create logfile: ' // TRIM(Err%LogFile)
             RC = HCO_FAIL
             RETURN
          ENDIF

       ! Reopen otherwise
       ELSE
          ! NAG did not like ACCESS='APPEND' -- use standard-compliant position='append'
          OPEN ( UNIT=FREELUN,   FILE=TRIM(Err%LogFile), STATUS='OLD',     &
               ACTION='WRITE', POSITION='APPEND', FORM='FORMATTED',    &
               IOSTAT=IOS   )
          IF ( IOS /= 0 ) THEN
             PRINT *, 'Cannot reopen logfile: ' // TRIM(Err%LogFile)
             RC = HCO_FAIL
             RETURN
          ENDIF
       ENDIF

       Err%LUN       = FREELUN
       Err%LogIsOpen = .TRUE.

    ! File does not yet exist ==> open new file
    ELSE
       OPEN ( UNIT=FREELUN,    FILE=TRIM(Err%LogFile),      &
            STATUS='NEW',    ACTION='WRITE', IOSTAT=IOS,  &
            FORM='FORMATTED'                             )
       IF ( IOS /= 0 ) THEN
          PRINT *, 'Cannot create logfile: ' // TRIM(Err%LogFile)
          RC = HCO_FAIL
          RETURN
       ENDIF

       Err%LUN       = FREELUN
       Err%LogIsOpen = .TRUE.

    ENDIF

    ! Write header on first call
    IF ( Err%FirstOpen ) THEN
       LUN = Err%Lun                ! Log gets written to file

       ! Only write the version info if verbose output is requested
       IF ( Err%DoVerbose ) ) THEN

          ! Write header
          WRITE( LUN, '(a)'      ) REPEAT( '-', 79)
          WRITE( LUN, '(a12, a)' ) 'Using HEMCO ', HCO_VERSION
          WRITE( LUN, '(a)'        )
#ifdef USE_REAL8
          WRITE( LUN,  100       )
100       FORMAT('HEMCO precision (hp) is set to is 8-byte real (aka REAL*8)')
#else
          WRITE( LUN,  110       )
110       FORMAT('HEMCO precision (hp) is set to is 4-byte real (aka REAL*4)')
#endif
          WRITE( LUN, '(a)'      ) REPEAT( '-', 79)
       ENDIF

       Err%FirstOpen = .FALSE.
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE HCO_Logfile_Open
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_LogFile_Close
!
! !DESCRIPTION: Subroutine HCO\_LOGFILE\_CLOSE closes the HEMCO logfile.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_LogFile_Close( Err )
!
! !INPUT PARAMETERS:
!
    TYPE(HcoErr),  POINTER        :: Err            ! Error object
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER            :: IOS
    CHARACTER(LEN=255) :: MSG

    !======================================================================
    ! HCO_LOGFILE_CLOSE begins here
    !======================================================================

    ! Exit if object does not exist, log is closed, not root core, or writing
    ! to standard out
    IF ( .NOT. ASSOCIATED(Err)) RETURN
    IF ( .NOT. Err%LogIsOpen  ) RETURN
    IF ( .NOT. Err%IsRoot     ) RETURN
    IF ( TRIM(Err%LogFile) == '*' ) RETURN

#ifdef MODEL_CESM
    ! Exit if using CESM (for safety)
    RETURN
#endif

    ! Close logfile
    CLOSE ( UNIT=Err%Lun, IOSTAT=IOS )
    IF ( IOS/= 0 ) THEN
       MSG = 'Cannot close logfile: ' // TRIM(Err%LogFile)
       CALL HCO_MSG( MSG, RC )
    ENDIF
    Err%LogIsOpen = .FALSE.

  END SUBROUTINE HCO_LogFile_Close
!EOC
END MODULE HCO_Error_Mod
