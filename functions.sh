#!/bin/bash
#
#: Title       : corre_2016.sh
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Functios to run emissions
#: Options     : None

#: Title       : hace_namelist
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : generates the namelist.nml
#: Options     : $dia $dia2
hace_namelist()
{
 if [ $1 -eq $2 ]
 then
  echo  "  *** Creating namelist.nml for "$dominio"   *****"
 else
  echo  "  *** Creating namelist.nml for day "$1"   *****"
 fi
cat << End_Of_File > namelist_emis.nml
!
!   Definicion de variables para calculo del Inventario
!
&region_nml
zona ="$dominio"
!
! bajio bajio3 cdjuarez   colima    ecacor  ecaim ecaim3
! guadalajara  jalisco    mexicali  mexico  mexico9
! monterrey    monterrey3 queretaro tijuana
/
&fecha_nml
! Se indica el dia y el mes a calcular
! se proporciona el anio
! month jan =1 to dec=12
! day in the month (from 1 to 28,30 or 31)
! anio a modelar validos: 2014 a 2020
! Si se quiere un archvio de 24 hr periodo=1
!    o dos archivos de 12 hr peridodo=2
idia=$dia
month=$mes
anio=$nyear
periodo=$nfile
/
!Horario de verano
&verano_nml
! .true. o .false. para considerar o no el cambio cuando se
!  esta en horario de verano
!
lsummer = .true.
/
! Quimica a utilizar
! Los mecanismos a usar:
!  cbm04 cbm05 mozart racm2 radm2 saprc99 saprc07
!  if saprc07  model= 0 WRF 1 CHIMERE
&chem_nml
mecha='$MECHA'
model=$AQM_SELECT
/
End_Of_File

}
#: Title       : check_domain
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Identifies if the domain are for emissions exists
#: Options     : domain
check_domain ()
{
cd 01_datos
existe=0
if [ -d $1 ]
then
   echo "  *** Relizando para "$dominio"   *****"
else
   echo "  Dominio "$dominio" no existe en el directorio *****"
   exit 1
fi
cd ..
}
#: Title       : make_tmpdir
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Creates the working directory for emissions
#: Options     : name of directory
make_tmpdir ()
{
if [ -d $1 ]
then
   if [ $HacerArea -eq 1 ]; then
     echo "     Making New Directory"
     rm -rf $1
     mkdir $1
   fi
   echo "  *** In directory "$1"   *****"
   cd $1
else
   echo "  *** Creating directory "$1"   *****"
   mkdir $1
   cd $1
fi
 ln -fs ../01_datos/$dominio .
 ln -fs ../01_datos/chem .
 ln -fs ../01_datos/time .
 ln -fs ../01_datos/emis .
 ln -fs ../bin .

}
#: Title       : hace_area
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Apatial distribution area emissions
#: Options     : none
hace_area(){
if [ $HacerArea -eq 1 ]; then
echo "     Making Spatial distribution in Area sources"
bin/ASpatial.exe >./area.log
fi

}
#: Title       : hace_movil
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Spatial distribution mobile emissions
#: Options     : none
hace_movil(){
if [ $HacerArea -eq 1 ]; then
echo "     Making Spatial distribution in Mobile sources"
bin/vial.exe > ./movil.log &
bin/carr.exe >> ./movil.log&
wait
bin/agrega.exe > ./movil.log
bin/MSpatial.exe > ./movil.log &
fi
}
#: Title       : emis_area
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Area emissions processing
#: Options     : none
emis_area(){
echo 'Area Temporal distribution'
../bin/Atemporal.exe  > ../area.log
echo 'Area Speciation distribution PM2.5'
../bin/spm25a.exe >> ../area.log &
echo 'Area Speciation distribution VOCs'
echo '** Area '$MECHA' *****'
 ln -fs ../chem/profile_${MECHA}.csv .
../bin/spa.exe >> ../area.log
}
#: Title       : emis_fijas
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Point emissions processing
#: Options     : none
emis_fijas(){
echo 'Point Temporal distribution'
../bin/Puntual.exe > ../puntual.log
echo 'Point Speciation distribution PM2.5'
../bin/spm25p.exe >> ../puntual.log &
echo 'Point Speciation distribution VOCs'
#
echo '** Point '$MECHA' *****'
../bin/spp.exe >> ../puntual.log
}
#: Title       : emis_movil
#: Date        : 25/04/2021
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2021 Actualizacion para IE del 2016
#: Description : Movile emissions processing
#: Options     : none
emis_movil(){
pwd
echo 'Movil Temporal distribution'
../bin/Mtemporal.exe > ../movil.log
echo 'Movil Speciation distribution PM2.5'
../bin/spm25m.exe >> ../movil.log&
echo '** Movil '$MECHA' *****'
../bin/spm.exe >> ../movil.log
}