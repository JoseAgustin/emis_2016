! testing kverano
!>  @brief Testing kverano function
!>   Returns 1 if the date is within daysaving time period
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param ida  day in the month
!>   @param mes  month of the year
program t_kverano
    use master
    implicit none
    integer :: mes
    integer :: dia
    call lee_namelist

       do mes=2,10,2
         do dia =3,29,3
         write(6,*) anio,mes,dia, kverano(dia,mes)
         end do
        end do
233 format("******  HORARIO de VERANO *******",/,3x,"Abril ",I2,x,"a Octubre ",I2)

end program

