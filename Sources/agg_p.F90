!>  @brief does AREA emissions VOC speciation and aggregation
!>
!>  splits VOC in chemical species and agregates those chemical species  into classes
!>  for a specific mechanism selected in namelist_emis.nml
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/31/2022
!>   @version  3.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
program point_speciation
   use voc_split
   use master

    call lee_namelist

  if(trim(mecha).eq."ghg") then
      write(6,*)   " ***** Skipping Speciation ****"
   else
     call lee_voc(3)

     call voc_agregation(3)

     call guarda_voc(3)
   end if
end program point_speciation
