!
!  Programa SUMA Carreteras
!
!  Creado por Jose Agustin Garcia Reynoso el 19/12/2022
!
! ifort -O2 -axAVX  master_mod.F90 mobile_mod.F90 carreteras.F90 -o carr.exe
!
! Proposito:
! Este programa identifica las diferentes carreteras en la celda y las suma.
! y obtiene la fraccion de carretera en la celda con respecto al municipio
!
!  Modificaciones
!
!   2/Ago/2012  se considera la vialidad en todo el municipio rlm
!   9/Abr/2020  usa namelist global
!  18/Dic/2022  Use model highway_vars
!> @brief for suma_carretera.F90 program. For aggregation Highway fractions in cells.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  19/12/2022
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
program suma_carreteras
use master, only:lee_namelist
use mobile_mod

	call lee_namelist

    call areas_read('CARRETERAS.csv ')

    call vial_fraction_calculation

    call vial_fraction_saving('salida.csv  ')

end program suma_carreteras
