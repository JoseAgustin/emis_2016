!
! Proposito:
!          Especiacion de PM2.5 en diferentes categorias para
!          emisiones provenientes de fuentes de vehiculares moviles
!
!  compile: ifort -O2 -axAVX e_pm25_mod.F90 pm25_speci_m.F90 -o spm25m.exe
!
!>  @brief does MOBILE emissions PM2.5 speciation
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
!>   @date  05/31/2022
!>   @version  3.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
program pm25_speciation
#ifdef _OPENMP
    use omp_lib
#endif
use PM25_speciation_mod
use master

integer :: isource= 2 ! 1 area 2 mobile 3 point

   call lee_namelist
   if(trim(mecha).eq."ghg") then
    write(6,*) " ***** Skipping Speciation ****"
  else
	 call lee(isource)

	 call calculos

	 call guarda(isource)
  end if

end program pm25_speciation
