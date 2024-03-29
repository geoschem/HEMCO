!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HCO_m_netcdf_io_get_dimlen
!
! !INTERFACE:
!
module HCO_m_netcdf_io_get_dimlen
!
  implicit none
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public  Ncget_Dimlen
  public  Ncget_Unlim_Dimlen
!
! !DESCRIPTION: Provides routines to obtain the length of a given dimension.
!\\
!\\
! !AUTHOR:
!  Jules Kouatchou
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/hemco for complete history
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
! !IROUTINE: Ncget_Dimlen
!
! !INTERFACE:
!
  subroutine Ncget_Dimlen(ncid, dim_name, dim_len)
!
! !USES:
!
    use netCDF
    use m_do_err_out
!
! !INPUT PARAMETERS:
!!  dim_name : netCDF dimension name
!!  ncid     : netCDF file id
    character (len=*), intent(in) :: dim_name
    integer,           intent(in) :: ncid
!
! !OUTPUT PARAMETERS:
!!  dim_len: netCDF dimension length
    integer,           intent(out)   :: dim_len
!
! !DESCRIPTION: Returns the length of a given netCDF dimension.
!               If err\_stop is set to FALSE, -1 is returned if
!               the given dimension cannot be found. Otherwise,
!               an error is prompted and the program stops.
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
    integer             :: dimid
    integer             :: ierr

    ierr = NF90_Inq_Dimid(ncid, dim_name, dimid)

    if (ierr /= NF90_NOERR ) then
       err_msg = 'In Ncget_Dimlen #1:  ' // Trim (dim_name) // &
                 ', ' // NF90_Strerror (ierr)
       call Do_Err_Out (err_msg, .true., 1, ncid, 0, 0, 0.0d0, 0.0d0)
    end if

    ierr = NF90_Inquire_Dimension(ncid, dimid, len=dim_len)

    if (ierr /= NF90_NOERR) then
       err_msg = 'In Ncget_Dimlen #2:  ' // NF90_Strerror (ierr)
       call Do_Err_Out (err_msg, .true., 2, ncid, dimid, 0, 0.0d0, 0.0d0)
    end if

    return
  end subroutine Ncget_Dimlen
!EOC
!------------------------------------------------------------------------------
!       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
!                      and NASA/GSFC, SIVO, Code 610.3                        !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Ncget_Unlim_Dimlen
!
! !INTERFACE:
!
  subroutine Ncget_Unlim_Dimlen (ncid, udim_len)
!
! !USES:
!
    use netCDF
    use m_do_err_out
!
! !INPUT PARAMETERS:
!!  ncid     : netCDF file id
    integer,           intent(in) :: ncid
!
! !OUTPUT PARAMETERS:
!!  udim_len : netCDF unlimited dimension length
    integer,           intent(out) :: udim_len
!
! !DESCRIPTION: Returns the length of the unlimited netCDF dimension.
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
    integer            :: ierr, udim_id

    udim_len = -1
    ierr = NF90_Inquire(ncid, unlimitedDimId=udim_id)
    IF ( ierr /= NF90_NOERR ) THEN
       ierr = NF90_Inquire_Dimension( ncid, udim_id, len=udim_len )
    ENDIF

  end subroutine Ncget_Unlim_Dimlen
!EOC
end module HCO_m_netcdf_io_get_dimlen
