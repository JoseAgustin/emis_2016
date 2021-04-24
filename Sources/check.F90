!      _               _
!  ___| |__   ___  ___| | __
! / __| '_ \ / _ \/ __| |/ /
!| (__| | | |  __/ (__|   <
! \___|_| |_|\___|\___|_|\_\
!
!>  @brief Verifies no error in netcdf function call
!>   @param status NetCDF functions return a non-zero status codes on error.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/24/2020
subroutine check(status)
  use netcdf
  integer, intent ( in) :: status
    if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
  return
end subroutine check

