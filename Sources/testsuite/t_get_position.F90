! testing get_position
!>  @brief Testing get_position subroutine
!>   Returns 1 if the date is within daysaving time period
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  12/17/2022
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
!>   @param idgrid  day in the month
!>   @param mes  month of the year
program test_get_position
    use master, only:get_position
    integer*8:: idgrid
    integer :: columna, renglon
    integer :: i,j,nx=3,ny=5
    do j =1,ny    ! renglon
        do i=1,nx ! columna
            idgrid=i+(j-1)*nx
            call get_position(idgrid,nx,renglon,columna)
            write (6,'(5I4)')idgrid,renglon,columna,j,i
        end do
    end do
endprogram
