!
! Proposito:
!          Especiacion de PM2.5 en diferentes categorias para
!          emisiones provenientes de fuentes de industriales fijas
!
!  compile: ifort -O2 -axAVX e_pm25_mod.F90 pm25_speci_p.F90 -o spm25p.exe
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
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program pm25_speciation
#ifdef _OPENMP
    use omp_lib
#endif
use PM25_speciation_mod
use master

integer :: isource= 3 ! 1 area 2 mobile 3 point

   call lee_namelist

   if(trim(mecha).eq."ghg") then
    write(6,*) " ***** Skipping Speciation ****"
  else
	  call lee(isource)

    call calculos

	  call guarda(isource)
  end if
end program pm25_speciation
