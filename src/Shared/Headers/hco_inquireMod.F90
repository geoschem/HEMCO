#ifdef MAPL_ESMF
#ifdef MAPL3
#include "MAPL.h"
#else
#include "MAPL_Generic.h"
#endif
#endif
!------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1    !
!------------------------------------------------------------------------
!BOP
!
! !MODULE: HCO_inquireMod
!
! !DESCRIPTION: Module inquireMod contains functions to find free and
!  unopened logical file units (LUNs) for Fortran I/O.
!
! !INTERFACE:
!
MODULE HCO_inquireMod
!
! !USES:
!

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: findFreeLUN
!
! !REVI<SION HISTORY:
!  14 Jun 2012 - E. Nielsen  - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
  CONTAINS
!EOC
!------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1    !
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: findFreeLUN
!
! !DESCRIPTION: Inquire for an existing, but unopened, logical unit number
!\\
!\\
! !INTERFACE:
!
  FUNCTION findFreeLUN( b ) RESULT( lun )
!
! !USES:
!
#ifdef MAPL_ESMF
#ifdef MAPL3
    USE mapl_ErrorHandlingMod, only: MAPL_Verify
#else
    USE ESMF
    USE MAPLBase_Mod
#endif
#endif

    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN), OPTIONAL :: b   ! Not really used here
!
! !RETURN VALUE:
!
    INTEGER :: lun
!
! !REVISION HISTORY:
!  14 Jun 2012 - E. Nielsen  - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                    :: i, status
    LOGICAL                    :: exists        ! File existence
    LOGICAL                    :: found         ! Detect unused logical unit
    LOGICAL                    :: open          ! Is open?
#ifdef MAPL3
    LOGICAL                    :: rc
#else
    INTEGER                    :: rc

#ifdef MAPL_ESMF
    CHARACTER(LEN=ESMF_MAXSTR) :: Iam
#else
    CHARACTER(LEN=255)         :: Iam
#endif
#endif
!
! !DEFINED PARAMETERS
!
    INTEGER, PARAMETER         :: iTop = 199     ! Maximum LUN limit

    !======================================================================
    ! Initialization
    !======================================================================

    status = 0

#ifndef MAPL3
    Iam = "GEOSCHEMCHEM::findFreeLUN"
    rc     = 0
#endif

    !======================================================================
    ! Find an available logical unit
    !======================================================================
    found = .FALSE.
    i     = 11

    DO WHILE ( .NOT. found .AND. i <= iTop )
       INQUIRE( UNIT=i, EXIST=exists, OPENED=open )
       IF ( exists .AND. .NOT. open ) THEN
          found = .TRUE.
          lun = i
       ENDIF
       i = i + 1
    ENDDO

    IF ( .NOT. found ) THEN
       status = 1
       PRINT *, "findFreeLUN in hco_inquireMod.F90: No available logical units"
    ENDIF

#ifdef MAPL_ESMF
#ifdef MAPL3
    ! Comment out for now
    !_VERIFY(status)
#else
    VERIFY_(status)
#endif
#endif

  END FUNCTION findFreeLUN
!EOC
END MODULE HCO_inquireMod
