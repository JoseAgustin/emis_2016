! testing count
!>  @brief Testing count subroutine
!>   Returns number of different municipalities
!>   count: Identifies the different municipalities in the VIALIDADES.CSV/CARRETERAS.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  19/12/2022
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>
program test_count
use mobile_mod
integer :: i
nm=15
allocate(grid(nm),icve(nm),rlm(nm),rlc(nm))
! Crea valores dummy
do i=1,nm
    grid(i)=mod(mn,i)+1
    icve(i)=1200+mod(mn,i)*10+grid(i)
    rlm(i)=grid(i)/40.
    rlc(i)=grid(i)/50.
end do

call count

end program test_count
