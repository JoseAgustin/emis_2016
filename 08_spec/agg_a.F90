!>  @brief does AREA emissions VOC speciation and aggregation.
!>
!>  splits VOC in chemical species and agregates those chemical species  into classes
!>  for a specific mechanism selected in namelist_emis.nml
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program area_speciation
   use voc_split

    call lee_namelist

    call lee_voc(1)

    call voc_agregation(1)

    call guarda_voc(1)

end program area_speciation

