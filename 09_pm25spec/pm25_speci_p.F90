!
! Proposito:
!          Especiacion de PM2.5 en diferentes categorias para
!          emisiones provenientes de fuentes de industriales fijas
!
!  compile: ifort -O2 -axAVX e_pm25_mod.F90 test_p.F90 -o spm25p.exe
!
!>  @brief does POINT emissions PM2.5 speciation
!>
!> Speciation of PM2.5 in different categories
!>
!> Profile #  Profile Name  SPEC  POA  PEC  GSO4  PNO3  OTHER
!>
!>  POA Primary organic aerosol
!>
!>  PEC elemental carbon
!>
!>  PNO3 primary nitrate
!>
!>  GSO4  primary sulfate
!>
!>  OTHER PMFINE, generally crustal
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/11/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program pm25_speciation
#ifdef _OPENMP
    use omp_lib
#endif
use var_spm25
integer :: isource= 3 ! 1 area 2 mobile 3 point

	call lee(isource)

   call calculos

	call guarda(isource)

end program pm25_speciation
