!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE:  HCO_m_netcdf_io_close.F90
!
! !INTERFACE:
!
module HCO_m_netcdf_io_close
!
  implicit none
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public  Nccl
  public  Nccl_Noerr
!
! !DESCRIPTION: Routines to close a netCDF file.
!\\
!\\
! !AUTHOR:
!  Jules Kouatchou
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/ncdfutil for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nccl
!
! !INTERFACE:
!
  subroutine Nccl (ncid)
!
! !USES:
!
    use netCDF
    use m_do_err_out
!
! !INPUT PARAMETERS:
!!  ncid : netCDF file id
    integer, intent (in)   :: ncid
!
! !DESCRIPTION: Closes a netCDF file with file id ncid.
!\\
!\\
! !AUTHOR:
!  John Tannahill (LLNL) and Jules Kouatchou
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/ncdfutil for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
    character (len=512) :: err_msg
    integer             :: ierr
!
    ierr = Nf90_Close (ncid)

    if (ierr /= NF90_NOERR) then
       err_msg = 'In Nccl:  ' // Nf90_Strerror (ierr)
       call Do_Err_Out (err_msg, .true., 1, ncid, 0, 0, 0.0d0, 0.0d0)
    end if

    return

  end subroutine Nccl
!EOC
!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Nccl_Noerr
!
! !INTERFACE:
!
  subroutine Nccl_Noerr (ncid)
!
    use netCDF
!
! !INPUT PARAMETERS:
!!  ncid : netCDF file id
    integer, intent (in)   :: ncid
!
! !DESCRIPTION: Closes a netCDF file (with file id ncid) if it is open and
!  suppresses Ncclos error messages/exit if it is not.
!\\
!\\
! !AUTHOR:
!  John Tannahill (LLNL) and Jules Kouatchou
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/ncdfutil for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
    integer             :: ierr
!
    ierr = Nf90_Close (ncid)

    return

  end subroutine Nccl_Noerr
!EOC
end module HCO_m_netcdf_io_close

