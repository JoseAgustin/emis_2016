!> @brief for agrega.f90 progam in mobile spatial allocation.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  19/12/2022
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!   Programa agrega.f90
!
!  Creado por Jose Agustin Garcia Reynoso el 19/12/2022
!
!  ifort -O2 -axAVX master_mod.F90 mobile_mod.F90 agrega.F90 -o agrega.exe
! Proposito:
!  Lee las fracciones de vialidad y carretera (salida.csv) y genera un archivo
!  combinado de ambas
!
!  Modificaciones
!
!   9/Sep/2014  se actualiza salida para varios municipios
!   2/Ago/2012  se incluye en la salida la clave del municipio
!   19/12/2022  se emplea el modulo mobile_mod
program adds_street_highway_fractions
use mobile_mod

    call highway_street_fractions_read

    call highway_street_fractions_sum_up

    call storing_highway_street_total

end program adds_street_highway_fractions
