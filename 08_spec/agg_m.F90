!>  @brief does MOBILE emissions VOC speciation and aggregation to the different
!>   split in chemical species and agregates into classes for an specific mechanism
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program mobile_speciation
   use voc_split

    call lee_namelist

    call lee_voc(2)

    call voc_agregation(2)

    call guarda_voc(2)

end program mobile_speciation

