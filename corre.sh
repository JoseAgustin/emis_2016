#!/bin/bash
#
#: Title       : corre.sh
#: Date        : 26/04/2020
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  26/04/2020 Actualizacion para IE del 2014
#: Description : Programa de emisiones con funciones
#: Options     : None
#
ProcessDir=$PWD
echo "Directorio actual "$ProcessDir
# cdjuarez    colima      mexicali    monterrey   monterrey3
# bajio       ecaim       guadalajara mexico      tijuana
# bajio3      ecaim3      jalisco     mexico9     queretaro

variables_set () {
dominio=$1
HacerArea=1
#
# Selecciona mecanismo
# Los mecanismos a usar cbm04 cbm05 mozart racm2 radm2 saprc99
#
MECHA=saprc99
#  Build the namelist_emis.nml file
# Cambiar aqui la fecha
mes=4
dia=10
dia2=10
dia1=$dia
#
#    Aqui cambiar el año a modelar
#
nyear=2020
#
#   Si se desea un archivo de 24 hrs  nfile=1
#              dos archivos de 12 hrs nfile=2
nfile=2

}
hace_area(){
cd 02_aemis
if [ $HacerArea -eq 1 ]; then
echo "     Haciendo distribucion espacial en Fuentes de area"
./ASpatial.exe >../area.log
fi
cd ..
}
hace_movil(){
cd 03_movilspatial
if [ $HacerArea -eq 1 ]; then
echo "     Haciendo distribucion espacial en Fuentes de Moviles"
./vial.exe > ../movil.log &
./carr.exe >> ../movil.log&
wait
./agrega.exe > ../movil.log
cd ../05_semisM
./MSpatial.exe > ../movil.log &
cd ..
fi
}
emis_area(){
cd $ProcessDir/04_temis
echo 'Area Temporal distribution'
./Atemporal.exe  > ../area.log
echo 'Area Speciation distribution PM2.5'
cd ../09_pm25spec
./spm25a.exe >> ../area.log &
echo 'Area Speciation distribution VOCs'
cd ../08_spec
echo '** Area '$MECHA' *****'
./spa.exe >> ../area.log
cd $ProcessDir
}
emis_movil(){
echo 'Movil Temporal distribution'
cd $ProcessDir/06_temisM/
./Mtemporal.exe > ../movil.log
echo 'Movil Speciation distribution PM2.5'
cd ../09_pm25spec
./spm25m.exe >> ../movil.log&
cd ../08_spec
echo '** Movil '$MECHA' *****'
./spm.exe >> ../movil.log
cd $ProcessDir
}
emis_fijas(){
echo 'Point Temporal distribution'
cd $ProcessDir/07_puntual/
./Puntual.exe > ../puntual.log
echo 'Point Speciation distribution PM2.5'
cd ../09_pm25spec
./spm25p.exe >> ../puntual.log &
echo 'Point Speciation distribution VOCs'
#
cd ../08_spec
echo '** Point '$MECHA' *****'
./spp.exe >> ../puntual.log
cd $ProcessDir
}
#
#   ___ _   _ ___ ____ ___    _
#  |_ _| \ | |_ _/ ___|_ _|  / \
#   | ||  \| || | |    | |  / _ \
#   | || |\  || | |___ | | / ___ \
#  |___|_| \_|___\____|___/_/   \_\
#
variables_set ecaim3
#  Check if domain exist
cd 01_datos
existe=0
for dir in $(ls)
 do
if [ "$dir" = "$dominio" ]; then
  existe=1
fi
done
cd ..
if [ $existe -eq 0 ];then
echo "  Dominio "$dominio" no existe en el directorio *****"
exit
else
echo "  Relizando para "$dominio"   *****"
cat << End_Of_File > namelist_emis.nml
!
!   Definicion de variables para calculo del Inventario
!
&region_nml
zona ="$dominio"
!
! cdjuarez    colima      mexicali    monterrey   monterrey3
! bajio       ecaim       guadalajara mexico      tijuana
! bajio3      ecaim3      jalisco     mexico9     queretaro
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
!  cbm04 cbm05 mozart racm2 radm2 saprc99
!
&chem_nml
mecha='$MECHA'
/
End_Of_File
fi

hace_area   &

hace_movil  &

wait
#
# Inicia Loop de Tiempo
# exit
while [ $dia -le $dia2 ] ;do
cd $ProcessDir
echo "directorio de trabajo "$PWD
echo ' '
echo '  Mes ='$mes 'DIA '$dia
#
cat << End_Of_File > namelist_emis.nml
!
!   Definicion de variables para calculo del Inventario
!
&region_nml
zona ="$dominio"
!
! cdjuarez    colima      mexicali    monterrey   monterrey3
! bajio       ecaim       guadalajara mexico      tijuana
! bajio3      ecaim3      jalisco     mexico9     queretaro
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
!  cbm04 cbm05 mozart racm2 radm2 saprc99
!
&chem_nml
mecha='$MECHA'
/
End_Of_File

emis_area &

emis_fijas &

emis_movil &


wait
echo ' Guarda'

cd $ProcessDir/10_storage
./emis2.exe  > ../${MECHA}.log
 let dia=dia+1
done
mv wrfchemi?d01* ../inventario/$dominio/
#ncrcat -O wrfchemi.d01.radm2.2019-0${mes}-1* wrfchemi_d01_2019-0${mes}-${dia}_00:00:00
#mv wrfchemi_d01_2019-02-05_00:00:00 ../../DOMAIN/mecanismos/emisiones
echo "DONE  Guarda "$MECHA
exit
