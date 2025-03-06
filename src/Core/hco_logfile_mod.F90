!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_logfile_mod
!
! !DESCRIPTION: Module HCO\_LOGFILE\_MOD contains some wrapper routines to
! write data into the HEMCO logfile.
!\\
!\\
! !INTERFACE:
!
MODULE HCO_LOGFILE_MOD
!
! !USES:
!
  USE HCO_ERROR_MOD

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: HCO_Spec2Log
  PUBLIC  :: HCO_PrintList
  PUBLIC  :: HCO_PrintDataCont
!
! !REVISION HISTORY:
!  27 May 2014 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: hco_spec2log
!
! !DESCRIPTION: Subroutine HCO\_Spec2Log writes information of a species
! to the logfile.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_Spec2Log( HcoState, ID )
!
! !USES:
!
    USE HCO_STATE_MOD, ONLY : HCO_State
!
!
! !INPUT PARAMETER
!
    TYPE(HCO_State),  POINTER        :: HcoState   ! HEMCO state object
    INTEGER,          INTENT(IN)     :: ID         ! HEMCO species ID
!
! !REVISION HISTORY:
!  27 May 2014 - C. Keller   - Initialization
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: MSG

    !=================================================================
    ! HCO_Spec2Log begins here
    !=================================================================

    MSG = 'Species ' // TRIM(HcoState%Spc(ID)%SpcName)
    CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
    IF ( HcoState%Config%doVerbose ) THEN
       write(MSG,*) '--> HcoID         : ', HcoState%Spc(ID)%HcoID
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
       write(MSG,*) '--> ModID         : ', HcoState%Spc(ID)%ModID
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
       write(MSG,*) '--> MW (g/mol)    : ', HcoState%Spc(ID)%MW_g
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
       write(MSG,*) '--> Henry constant: ', HcoState%Spc(ID)%HenryK0
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
       write(MSG,*) '--> Henry temp.   : ', HcoState%Spc(ID)%HenryCR
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
       write(MSG,*) '--> Henry pKA     : ', HcoState%Spc(ID)%HenryPKA
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
    ENDIF

  END SUBROUTINE HCO_Spec2Log
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_PrintList
!
! !DESCRIPTION: Subroutine HCO\_PrintList displays the content of List.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_PrintList ( HcoState, List )
!
! !USES:
!
      USE HCO_STATE_MOD,     ONLY : HCO_State
      USE HCO_TYPES_MOD,     ONLY : ListCont
!
! !INPUT ARGUMENTS:
!
      TYPE(HCO_STATE),POINTER    :: HcoState
      TYPE(ListCont), POINTER    :: List
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !ARGUMENTS:
!
      TYPE(ListCont), POINTER   :: TmpLct
      CHARACTER(LEN=255)        :: MSG

      ! ================================================================
      ! HCO_PrintList begins here
      ! ================================================================

      ! Point to first element
      TmpLct => List
      DO WHILE ( ASSOCIATED(TmpLct) )
         IF ( ASSOCIATED(TmpLct%Dct) ) THEN
            CALL HCO_PrintDataCont( HcoState,TmpLct%Dct )
         ENDIF
         TmpLct => TmpLct%NextCont
      ENDDO

      TmpLct => NULL()

      END SUBROUTINE HCO_PrintList
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_PrintDataCont
!
! !DESCRIPTION: Subroutine HCO\_PrintDataCont displays the content of the
! data container Dct.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_PrintDataCont ( HcoState, Dct )
!
! !USES
!
      USE HCO_STATE_MOD,     ONLY : HCO_State
      USE HCO_TYPES_MOD,     ONLY : DataCont
      USE HCO_TYPES_MOD,     ONLY : HCO_DCTTYPE_BASE, HCO_DCTTYPE_MASK
!
! !INPUT ARGUMENTS:
!
      TYPE(HCO_STATE),POINTER    :: HcoState
      TYPE(DataCont), POINTER    :: Dct
!
! !REVISION HISTORY:
!  20 Apr 2013 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !ARGUMENTS:
!
      CHARACTER(LEN=255) :: MSG
      INTEGER            :: nx, ny, nz, nt
      REAL(sp)           :: sm, mn, mx

      ! ================================================================
      ! HCO_PrintDataCont begins here
      ! ================================================================

      sm = 0.0_sp
      mn = 0.0_sp
      mx = 0.0_sp
      sm = 0.0_sp
      nx = 0
      ny = 0
      nz = 0
      nt = Dct%Dta%nt
      IF ( nt > 0 ) THEN
         IF ( Dct%Dta%spaceDim<=2 ) THEN
            IF ( ASSOCIATED(Dct%Dta%V2) ) THEN
               nx = SIZE(Dct%Dta%V2(1)%Val,1)
               ny = SIZE(Dct%Dta%V2(1)%Val,2)
               sm = SUM(Dct%Dta%V2(1)%Val)
               mn = MINVAL(Dct%Dta%V2(1)%Val)
               mx = MAXVAL(Dct%Dta%V2(1)%Val)
            ENDIF
         ELSE
            IF ( ASSOCIATED(Dct%Dta%V3) ) THEN
               nx = SIZE(Dct%Dta%V3(1)%Val,1)
               ny = SIZE(Dct%Dta%V3(1)%Val,2)
               nz = SIZE(Dct%Dta%V3(1)%Val,3)
               sm = SUM(Dct%Dta%V3(1)%Val)
               mn = MINVAL(Dct%Dta%V3(1)%Val)
               mx = MAXVAL(Dct%Dta%V3(1)%Val)
            ENDIF
         ENDIF
      ENDIF

      ! Print name for verbose > 0
      IF ( HcoState%Config%doVerbose ) THEN
         MSG = 'Container ' // TRIM(Dct%cName)
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
      ENDIF

      ! Eventually add details
      IF ( HcoState%Config%doVerbose ) THEN

         ! General information
         write(MSG,*) '   -->Data type       : ', Dct%DctType
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Container ID    : ', Dct%cID
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Target ID       : ', Dct%targetID
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->File data home?   ', Dct%DtaHome
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Source file     : ', TRIM(Dct%Dta%ncFile)
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->ncRead?           ', Dct%Dta%ncRead
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Shared data file? ', Dct%Dta%DoShare
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         IF ( Dct%Dta%ncRead ) THEN
            write(MSG,*) '   -->Source parameter: ', TRIM(Dct%Dta%ncPara)
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Year range      : ', Dct%Dta%ncYrs
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Month range     : ', Dct%Dta%ncMts
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Day range       : ', Dct%Dta%ncDys
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Hour range      : ', Dct%Dta%ncHrs
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->SpaceDim        : ', Dct%Dta%SpaceDim
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         ENDIF
         IF ( NZ > 0 ) THEN
            write(MSG,*) '   -->Array dimension : ', nx,ny,nz
         ELSE
            write(MSG,*) '   -->Array dimension : ', nx,ny
         ENDIF
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Array sum       : ', sm
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Array min & max : ', mn,mx
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Time dimension  : ', nt
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Delta t[h]      : ', Dct%Dta%DeltaT
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Local time?       ', Dct%Dta%IsLocTime
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         IF ( ASSOCIATED(Dct%Dta%tIDx) ) THEN
            write(MSG,*) '   -->Tempres         : ', &
               TRIM(Dct%Dta%tIDx%TempRes)
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         ENDIF
         write(MSG,*) '   -->OrigUnit        : ',TRIM(Dct%Dta%OrigUnit)
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Concentration?    ', Dct%Dta%IsConc
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         write(MSG,*) '   -->Coverage        : ', Dct%Dta%Cover
         CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)

         ! For masks
         IF ( Dct%DctType == HCO_DCTTYPE_MASK ) THEN
            write(MSG,*) '   -->Lon range       : ', Dct%Dta%Lons
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Lat range       : ', Dct%Dta%Lats
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         ENDIF

         ! For base emissions
         IF ( Dct%DctType==HCO_DCTTYPE_BASE ) THEN
            write(MSG,*) '   -->Extension Nr    : ', Dct%ExtNr
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Species name    : ',TRIM(Dct%SpcName)
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->HEMCO species ID: ', Dct%HcoID
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Category        : ', Dct%Cat
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Hierarchy       : ', Dct%Hier
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            IF ( nz == 0 ) THEN
               write(MSG,*) '   -->2D emitted into : ', Dct%Dta%EmisL1, &
                            ' and ', Dct%Dta%EmisL2
               CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            ENDIF

         ! For scale factors
         ELSE
            write(MSG,*) '   -->Scal ID         : ', Dct%ScalID
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            write(MSG,*) '   -->Operator        : ', Dct%Oper
            CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
         ENDIF
      ENDIF

      END SUBROUTINE HCO_PrintDataCont
!EOC
END MODULE HCO_LOGFILE_MOD
