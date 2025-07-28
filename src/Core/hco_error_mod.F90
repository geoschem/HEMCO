!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_error_mod.F90
!
! !DESCRIPTION: Module HCO\_Error\_Mod contains routines and variables
! for error handling and logfile messages in HEMCO. It alse contains
! definitions of some globally used parameters, such as single/double
! precision as well as the HEMCO precision which can be either single or
! double. The HEMCO precision is used for almost all HEMCO internal data
! arrays and can be changed if required.
!\\
!\\
! HEMCO error variables are organized in an object of derived type
! HcoErr. The HcoErr object is generally referred to as Err throughout
! HEMCO and this submodule. The Err object is a component of the HEMCO
! configuration object which is type ConfigObj (see module
! hco_types_mod.F90), and is referenced as HcoState%Config%Err. This
! design allows the invocation of multiple independent HEMCO instances
! at the same time which may have different HEMCO error settings.
!
! The Err object is initialized via subroutine HCO_ERROR_SET, called
! when reading section 'SETTINGS' of the HEMCO configuration file
! (subroutine Config_ReadFile in hco_config_mod.F90). If writing to
! a dedicated log file then it is further updated upon opening the file
! in HCO_LOGFILE_OPEN. Once instantiated, the Err object is stored in
! HcoState%Config%Err. Configurable run-time error settings set in
! HEMCO_Config.rc are stored in the Err object. Look for the following
! three settings in HEMCO_Config.rc:
!
! 1. LogFile:
!     Use the LogFile setting in HEMCO_Config.rc to specify where most HEMCO
!     log prints will go. If set to an asterisk (*) then HEMCO will
!     print to stdout. If a filename is provided then HEMCO will open and
!     write to that file. If using CESM then LogFile is ignored and it is
!     set to CAM log atm.log.
!
!     The logical unit number (LUN) is stored in Err%LogFile as well as
!     in HcoState%Config%hcoLogLUN. Generally Err%LogFile is used for
!     when a dedicated log file is specified in HEMCO_Config.rc, e.g.
!     HEMCO.log, such as when opening and closing the file. If opening
!     a new file then the new LUN is passed back from HCO_LOGFILE_OPEN
!     to update HcoState%Config%hcoLogLUN for use anywhere in HEMCO.
!     That variable can then be passed to all HEMCO message and error
!     handling subroutines in this module to direct messages to that file.
!     If not passed then messages are sent to stdout.
!
! 2. Verbose:
!     Logical for whether to print to log.
!
! 3. VerboseOnCores:
!     Logical for whether to update verbose to be true for all cores or
!     only root. This setting has no impact on GC-Classic prints and
!     only is relevant for models using MPI, e.g. GCHP. Set to 'root'
!     in HEMCO_Config.rc for verbose only on the root thread. Set to
!     'all' to be verbose on all cores.
!
! Settings for Verbose and VerboseOnCores are used to set
! HcoState%Config%Err%doVerbose (for use locally within this module)
! as well as HcoState%Config%doVerbose for global use in HEMCO.
!
! There are three message subroutines in this module: HCO_ERROR,
! HCO_WARNING, and HCO_MSG. All will print regardless of verbose
! settings unless called with an if HcoState%Config%doVerbose block.
! If using MPI then all messages will be printed by all cores unless
! enclosed in an HcoState%Config%doVerbose and/or
! HcoState%Config%amIRoot block. When using these subroutines
! we recommend the following:
!  1) Never limit HCO_ERROR prints to root thread or if verbose.
!  2) Limit HCO_WARNING and HCO_MSG messages to root unless debugging.
!  3) Decide whether to limit HCO_WARNING and HCO_MSG messages to verbose
!     on a case-by-case basis.
!  4) Print warnings to standard output (do not pass log LUN)
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
  CHARACTER(LEN=12), PARAMETER, PUBLIC :: HCO_VERSION = '3.11.2'

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
! !DESCRIPTION: Subroutine HCO_Error prompts an error message and sets RC to
! HCO_FAIL. Note that this routine does not stop a run, but it will cause a
! stop at a higher level when RC gets evaluated. Messages are printed
! to stdout unless optional argument LUN is passed. This subroutine is
! independent of verbose settings and will print from all threads
! unless called within an if block limiting it to verbose and/or root.
! Limiting error prints to verbose and/or root is not recommended.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_Error( ErrMsg, RC, THISLOC, LUN )
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
    INTEGER             :: I, J, hcoLogLUN
    CHARACTER(LEN=1023) :: MSG, MSG1, MSG2
#if defined( ESMF_)
    INTEGER             :: localPET, STATUS
    CHARACTER(4)        :: localPETchar
    TYPE(ESMF_VM)       :: VM
#endif

    !======================================================================
    ! HCO_ERROR begins here
    !======================================================================

    ! Specify where to write
    IF ( PRESENT(LUN) ) THEN
       hcoLogLUN = LUN
    ELSE
       hcoLogLUN = 6
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
    WRITE(hcoLogLUN,*) TRIM(MSG)

    ! Return w/ error
    RC = HCO_FAIL

  END SUBROUTINE HCO_Error
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Warning
!
! !DESCRIPTION: Subroutine HCO_Warning prompts a warning message without
! forcing HEMCO to stop. Default destination is stdout unless optional LUN
! argument is passed. This subroutine is independent of verbose settings and
! will print from all threads unless called within an if block limiting it
! to verbose and/or root.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_Warning( ErrMsg, THISLOC, LUN )
!
! !INPUT PARAMETERS"
!
    CHARACTER(LEN=*), INTENT(IN   )            :: ErrMsg
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: THISLOC
    INTEGER,          INTENT(IN   ), OPTIONAL  :: LUN
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER            :: WLEV, hcoLogLUN
    CHARACTER(LEN=255) :: MSG

    !======================================================================
    ! HCO_WARNING begins here
    !======================================================================

    ! Specify where to write
    IF ( PRESENT(LUN) ) THEN
       hcoLogLUN = LUN
    ELSE
       hcoLogLUN = 6
    ENDIF

    ! Print warning
    MSG = 'HEMCO WARNING: ' // TRIM( ErrMsg )
    WRITE( hcoLogLUN, '(a)' ) TRIM(MSG)

    ! Print location
    IF ( PRESENT(THISLOC) ) THEN
       MSG = '--> LOCATION: ' // TRIM(THISLOC)
       WRITE( hcoLogLUN, '(a)' ) TRIM(MSG)
    ENDIF

  END SUBROUTINE HCO_Warning
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_MSG
!
! !DESCRIPTION: Subroutine HCO_MSG prints message to log. Optional
! Sep1 and Sep2 denote line delimiters before and after the message,
! respectively. If optional argument logLUN is passed then message
! is printed to that file. Otherwise message is printed to stdout.
! This subroutine is independent of verbose settings and will print from
! all threads unless called within an if block limiting it to verbose
! and/or root.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_MSG( Msg, Sep1, Sep2, LUN )
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN   )            :: Msg
    CHARACTER(LEN=1), INTENT(IN   ), OPTIONAL  :: Sep1
    CHARACTER(LEN=1), INTENT(IN   ), OPTIONAL  :: Sep2
    INTEGER,          INTENT(IN   ), OPTIONAL  :: LUN
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    INTEGER :: hcoLogLUN

    !=======================================================================
    ! HCO_MSG begins here
    !=======================================================================

    ! Set where to write message
    hcoLogLUN = 6
    IF ( PRESENT(LUN) ) hcoLogLUN = LUN

    ! Write message
    IF ( PRESENT(SEP1) ) THEN
       WRITE( hcoLogLUN,'(a)' ) REPEAT( SEP1, 79 )
    ENDIF
    WRITE( hcoLogLUN,'(a)' ) TRIM( MSG )
    IF ( PRESENT(SEP2) ) THEN
       WRITE( hcoLogLUN,'(a)' ) REPEAT( SEP2, 79 )
    ENDIF

  END SUBROUTINE HCO_MSG
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_Enter
!
! !DESCRIPTION: Subroutine HCO_Enter is called upon entering a routine.
! It organizes the traceback handling. It is recommended to call this
! routine for 'big' routines but NOT for routines/functions that are
! frequently called, e.g. inside of loops!
!\\
!\\
! Note that all subroutines calling HCO_Enter must also call HCO_Leave!
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
       CALL HCO_Error( Msg, RC )
       RETURN
    ENDIF

    ! Error trap
    IF ( Err%CurrLoc <= 0 ) THEN
       Msg = 'CurrLoc is zero, cannot enter: ' // TRIM(thisLoc)
       CALL HCO_Error( Msg, RC )
       RETURN
    ENDIF

    ! Register current routine
    Err%Loc(Err%CurrLoc) = thisLoc

    ! Track location if enabled
    IF ( Err%doVerbose ) THEN
       WRITE(MSG,100) TRIM(thisLoc), Err%CurrLoc
       CALL HCO_Msg( MSG, LUN=Err%LUN )
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
! !DESCRIPTION: Subroutine HCO_Leave is called upon leaving a routine.
! It organizes the traceback handling. It is recommended to call this
! routine for 'big' routines but NOT for routines/functions that are
! frequently called, e.g. inside of loops!
!\\
!\\
! Note that all subroutines calling HCO_Leave must also call HCO_Enter!
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
    IF ( Err%doVerbose ) THEN
       WRITE(MSG,110) TRIM(Err%Loc(Err%CurrLoc)), Err%CurrLoc
       CALL HCO_MSG( MSG, LUN=Err%LUN )
    ENDIF

    ! Remove from list
    Err%Loc(Err%CurrLoc) = ''

    ! Remove current position
    Err%CurrLoc = Err%CurrLoc - 1

    ! Error trap
    IF ( Err%CurrLoc < 0 ) THEN
       Msg = 'CurrLoc is below zero, this should never happen!!'
       CALL HCO_ERROR ( Msg, RC )
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
! !DESCRIPTION: Subroutine HCO_Error_Set defines the HEMCO error
! settings. This routine is called at the beginning of a HEMCO
! simulation. Its input parameters are directly derived from the
! HEMCO configuration file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ERROR_SET( am_I_Root, Err,             LogFile,       &
                            isVerbose, isVerboseOnRoot, RC,      LUN  )

!
!  !INPUT PARAMETERS:
!
    LOGICAL,           INTENT(IN)     :: am_I_Root       ! Root CPU?
    TYPE(HcoErr),      POINTER        :: Err             ! Error object
    CHARACTER(LEN=*),  INTENT(IN)     :: LogFile         ! logfile path+name
    INTEGER, OPTIONAL, INTENT(IN)     :: LUN             ! Optional LUN for output log
!
! !INPUT/OUTPUT PARAMETERS:
!
    LOGICAL,           INTENT(INOUT)  :: isVerbose       ! Verbose output T/F?
    LOGICAL,           INTENT(INOUT)  :: isVerboseOnRoot ! =T: Verbose on root only
                                                         ! =F: Verbose on all cores
    INTEGER,           INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

    INTEGER :: hcoLogLUN

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
    IF ( isVerboseOnRoot ) THEN
       Err%doVerbose = ( isVerbose .and. am_I_Root )
    ELSE
       Err%doVerbose = isVerbose
    ENDIF

    ! Init misc. values
    Err%FirstOpen = .TRUE.
    Err%CurrLoc   = 0

    ! If Logfile is set to '*', set hcoLogLUN to 6 (writes to default, i.e. stdout)
    ! If optional LUN is passed, set hcoLogLUN to this (assume log is open).
    ! Otherwise, set LUN to 0 (writes to LogFile set in HEMCO_Config.rc)
    IF ( TRIM(Err%LogFile) == '*' ) THEN
       hcoLogLUN = 6
       Err%LogIsOpen = .TRUE.
    ELSEIF ( PRESENT(LUN) ) THEN
       hcoLogLUN = LUN
       Err%LogIsOpen = .TRUE.
    ELSE
       hcoLogLUN = 0
       Err%LogIsOpen = .FALSE.
    ENDIF
    Err%Lun = hcoLogLUN

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
! !DESCRIPTION: Subroutine HCO_Error_Final finalizes the error object including
! closing a dedicated HEMCO log file if using one.
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
    CHARACTER(LEN=255) :: MSG

    !======================================================================
    ! HCO_ERROR_FINAL begins here
    !======================================================================

    ! Print message if verbose and the Err object still exists
    IF ( ASSOCIATED(Err) ) THEN
       IF ( Err%doVerbose ) THEN
          MSG = ' '
          CALL HCO_MSG ( MSG, LUN=Err%LUN )
          MSG = 'HEMCO ' // TRIM(HCO_VERSION) // ' FINISHED.'
          CALL HCO_MSG ( MSG, SEP1='-', LUN=Err%LUN )
       ENDIF
    ENDIF

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
! !ROUTINE: HCO_LOGFILE_OPEN
!
! !DESCRIPTION: Subroutine HCO\_LOGFILE\_OPEN opens the HEMCO logfile
! if using and not yet open.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_LogFile_Open( Err, doVerbose, RC, logLUN )
!
! !USES:
!
    USE HCO_inquireMod,   ONLY : findFreeLUN
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HcoErr),  POINTER    :: Err            ! Error object
    LOGICAL,   INTENT(IN)     :: doVerbose
    INTEGER,   INTENT(INOUT)  :: RC
    INTEGER,   INTENT(OUT)    :: logLUN
!
! !REVISION HISTORY:
!  23 Sep 2013 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
    CHARACTER(LEN=255) :: MSG
    INTEGER            :: IOS, LUN, FREELUN
    LOGICAL            :: isopen, exists, verbose

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
       IF ( doVerbose ) THEN

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

    logLUN = Err%Lun

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
! !DESCRIPTION: Subroutine HCO_LOGFILE_CLOSE closes the HEMCO logfile if
! a dedicated HEMCO log file is used.
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
       CALL HCO_MSG( MSG )
    ENDIF
    Err%LogIsOpen = .FALSE.

  END SUBROUTINE HCO_LogFile_Close
!EOC
END MODULE HCO_Error_Mod
