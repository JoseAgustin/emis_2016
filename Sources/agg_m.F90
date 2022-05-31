!>  @brief does MOBILE emissions VOC speciation and aggregation
!>
!>  splits VOC in chemical species and agregates those chemical species  into classes
!>  for a specific mechanism selected in namelist_emis.nml
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/31/2022
!>   @version  3.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
program mobile_speciation
   use voc_split
   use master
    call lee_namelist

    if(trim(mecha).eq."ghg") then
      write(6,*)   " ***** Skipping Speciation ****"
   else
    call lee_voc(2)

    call voc_agregation(2)

    call guarda_voc(2)
   end if
end program mobile_speciation
