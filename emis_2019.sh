#!/bin/bash
#
# bsub -q q_hpc -oo salida_postwrf -n2 -R "span[hosts=1]" './febrero_2019.csh'
#  febrero_2019.csh
#
#
#  Creado por Jose Agustin Garcia Reynoso el 26/07/17.
#
#  Proposito:
#         Realiza la secuencia de pasos para generar diferentes fechas
#         del inventario de emisiones.
#  Modificaciones:
#         14/08/2013 Actualizacion para IE del 2014
#         14/10/2017 para bash
#SBATCH -J emi_2016
#SBATCH -o emi_2016%j.o
#SBATCH -n 4
#SBATCH --ntasks-per-node=24
#SBATCH -p id
ProcessDir=$PWD
echo "Directorio actual "$ProcessDir
# Selecciona area de modelacion
# cdjuarez    colima      mexicali    monterrey   monterrey3
# bajio       ecaim       guadalajara mexico      tijuana
# bajio3      ecaim3      jalisco     mexico9     queretaro
#
dominio=ecaim3
HacerArea=1
#
# Selecciona mecanismo
# Los mecanismos a usar cbm04 cbm05 mozart racm2 radm2 sapcr99
#
MECHA=radm2
#  Build the namelist_emis.nml file
# Cambiar aqui la fecha
mes=5
dia=21
dia2=21
dia1=$dia 
#
#    Aqui cambiar el año a modelar
#
nyear=2017
#
#   Si se desea un archivo de 24 hrs  nfile=1
#              dos archivos de 12 hrs nfile=2
nfile=1
#
# ******* No cambiar hacia abajo   ******
#
#  Revisa que exista el dominio
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
#
if [ $HacerArea -eq 1 ]; then
echo "     Haciendo distribucion espacial en Fuentes de area"
cd $ProcessDir/02_aemis
./ASpatial.exe >../area.log &
PIDa=$!
echo "     Haciendo distribucion espacial en Fuentes de Moviles"
cd $ProcessDir/03_movilspatial
./vial.exe > ../movil.log &
PIDm=$!
./carr.exe >> ../movil.log&
wait $PIDm
./agrega.exe > ../movil.log
cd ../05_semisM
./MSpatial.exe > ../movil.log
wait
fi
fi
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

cd $ProcessDir/04_temis
echo 'Area  Temporal distribution'
./Atemporal.exe  > ../area.log &
PIDa=$!
echo 'Movil Temporal distribution'
cd ../06_temisM/
./Mtemporal.exe > ../movil.log &
PIDm=$!
echo 'Point Temporal distribution'
cd ../07_puntual/
./Puntual.exe > ../puntual.log &
PIDp=$!


#
#echo 'Biogenic'
#cd ../12_biogenic
#./Btemporal.exe > biog.log&
#
echo 'Speciation distribution'
echo '   '$MECHA' *****'
#
cd ../09_pm25spec
wait $PIDm
echo 'Movile PM2.5'
./spm25m.exe >> ../movil.log &
cd ../08_spec
echo 'Movile VOC'
./spm.exe >> ../movil.log &
wait $PIDp
echo 'Puntual PM2.5'
cd ../09_pm25spec
./spm25p.exe >> ../puntual.log &
echo 'Puntual VOC'
cd ../08_spec
./spp.exe >> ../puntual.log  &
wait $PIDa
echo 'Area  PM2.5'
cd ../09_pm25spec
./spm25a.exe >> ../area.log&
cd ../08_spec
echo 'Area  VOC'
./spa.exe >> ../area.log &
#
wait
echo ' Guarda'

cd ../10_storage
./emiss.exe  > ../${MECHA}.log
 let dia=dia+1
done
mv wrfchemi?d01* ../inventario/$dominio/
#ncrcat -O wrfchemi.d01.radm2.2019-0${mes}-1* wrfchemi_d01_2019-0${mes}-${dia}_00:00:00
#mv wrfchemi_d01_2019-02-05_00:00:00 ../../DOMAIN/mecanismos/emisiones
echo "DONE  Guarda "$MECHA
exit

