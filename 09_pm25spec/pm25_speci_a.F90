!
! Proposito:
!          Especiacion de PM2.5 en diferentes categorias para
!          emisiones provenientes de fuentes de area
!
!  compile: ifort -O2 -axAVX e_pm25_mod.F90 test_a.F90 -o spm25a.exe
!
!>  @brief do AREA emissions PM2.5 speciation
!>
!> Speciation of PM2.5 in different categories
!> @par Profile #  Profile Name  SPEC  POA  PEC  GSO4  PNO3  OTHER
!> @par POA Primary organic aerosol
!> @par PEC elemental carbon
!> @par PNO3 primary nitrate
!> @par GSO4  primary sulfate
!> @par OTHER PMFINE, generally crustal
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/11/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program pm25_speciation
#ifdef _OPENMP
    use omp_lib
#endif
use var_spm25
integer :: isource= 1 ! 1 area 2 mobile 3 point

	call lee(isource)

	call calculos

	call guarda(isource)

end program pm25_speciation
