!
!  Programa suma Vialidades
!
!  Creado por Jose Agustin Garcia Reynoso el 19/12/2022
!
!  ifort -O2 -axAVX master_mod.F90 mobile_mod.F90 vialidades.F90 -o vial.exe
!
! Proposito:
! Este programa identifica las diferentes vialidades en la celda y las suma.
! y obtiene la fraccion de vialidad en la celda con respecto al municipio
!
!> @brief for vialidades.F90 program. For aggregation street fractions in cells.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  19/12/2022
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
program vialidades
use master, only:lee_namelist
use mobile_mod

	call lee_namelist

    call areas_read('VIALIDADES.csv ')

    call vial_fraction_calculation

    call vial_fraction_saving('salida2.csv ')

end program vialidades
