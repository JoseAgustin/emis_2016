! Testing lee_namelist
!
!>  @brief Test global namelist input file
!>
!>   for setting up geographical, temporal and chemical mechamism settings
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2021
!
program test_rnml
use master
  
   call crea_nml

   call lee_namelist

contains
subroutine crea_nml
    integer:: unit_f=10
    NAMELIST /region_nml/ zona
    NAMELIST /fecha_nml/ idia,month,anio,periodo
    NAMELIST /verano_nml/ lsummer
    NAMELIST /chem_nml/ mecha,model
    zona="palenque"
    idia=31
    month=3
    anio=2021
    periodo =2
    lsummer=.false.
    mecha="radm2"
    model=0
    !  Opening the file.
    open ( FILE   = './namelist_emis.nml' ,      &
    UNIT   =  unit_f          ,      &
    STATUS = 'UNKNOWN'        ,      &
    FORM   = 'FORMATTED'      ,      &
    ACTION = 'WRITE'           ,      &
    ACCESS = 'SEQUENTIAL'     )
    write(unit_f , NML = region_nml )
    write(unit_f , NML = fecha_nml  )
    write(unit_f , NML = verano_nml )
    write(unit_f , NML = chem_nml   )
    close(unit_f)
end subroutine
end program
