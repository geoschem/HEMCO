!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HCO_m_netcdf_io_open.F90
!
! !INTERFACE:
!
module HCO_m_netcdf_io_open
!
  implicit none
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public  Ncop_Rd
  public  Ncop_Wr
!
! !DESCRIPTION: Routines to open a netCDF file.
!\\
!\\
! !AUTHOR:
!  Jules Kouatchou
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!-----------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Ncop_Rd
!
! !INTERFACE:
!
  subroutine Ncop_Rd (ncid, filname, rc)
!
! !USES:
!
    USE netCDF
    use m_do_err_out
!
! !INPUT PARAMETERS:
!!  filname : name of netCDF file to open for reading
    character (len=*), intent (in)    :: filname
!
! !OUTPUT PARAMETERS:
!!  ncid    : opened netCDF file id
    integer          , intent (out)   :: ncid
    integer, optional                 :: rc
!
! !DESCRIPTION: Opens a netCDF file for reading and does some error checking.
!\\
!\\
! !AUTHOR:
!  John Tannahill (LLNL) and Jules Kouatchou
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
    character(len=512) :: err_msg
    integer            :: ierr
!
    ierr = Nf90_Open( filname, NF90_NOWRITE, ncid )

    if (ierr /= NF90_NOERR) then
       err_msg = 'In Ncop_Rd, cannot open:  ' // Trim (filname)
       call Do_Err_Out (err_msg, .true., 0, 0, 0, 0, 0.0d0, 0.0d0)
       return
    end if

    return

  end subroutine Ncop_Rd
!EOC
!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Ncop_Wr
!
! !INTERFACE:
!
  subroutine Ncop_Wr (ncid, filname)
!
! !USES:
!
    USE netCDF
    use m_do_err_out
!
! !INPUT PARAMETERS:
!!  filname : name of netCDF file to open for reading
    character (len=*), intent (in)    :: filname
!
! !OUTPUT PARAMETERS:
!!  ncid    : opened netCDF file id
    integer          , intent (out)   :: ncid
!
! !DESCRIPTION: Opens a netCDF file for reading/writing and does some
!  error checking.
!\\
!\\
! !AUTHOR:
!  John Tannahill (LLNL) and Jules Kouatchou
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
    character (len=512) :: err_msg
    integer             :: ierr
!
    ierr = Nf90_Open (filname, NF90_WRITE, ncid)

    if (ierr /= NF90_NOERR) then
       err_msg = 'In Ncop_Rd, cannot open:  ' // Trim (filname)
       call Do_Err_Out (err_msg, .true., 0, 0, 0, 0, 0.0d0, 0.0d0)
    end if

    return

  end subroutine Ncop_Wr
!EOC
!------------------------------------------------------------------------
end module HCO_m_netcdf_io_open

