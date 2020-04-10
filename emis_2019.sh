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
#SBATCH -J emi_2019
#SBATCH -o emi_2096%j.o
#SBATCH -n 4
#SBATCH --ntasks-per-node=24
#SBATCH -p id
ProcessDir=$PWD
echo "Directorio actual "$ProcessDir
# Selecciona area de modelacion
#  bajio     cdjuarez     colima      
#  ecaim    guadalajara  mexicali
#  mexico    monterrey    queretaro   tijuana
#
dominio=mexicali
HacerArea=0
#
# Selecciona mecanismo
# Los mecanismos a usar cbm04 cbm05 mozart racm2 radm2 saprc99
#
MECHA=radm2
#  Build the namelist_emis.nml file
# Cambiar aqui la fecha
mes=5
dia=9
dia2=9
dia1=$dia 
#
#    Aqui cambiar el año a modelar
#
nyear=2019
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
! bajio     cdjuarez     colima
! ecaim     guadalajara  mexicali
! mexico    monterrey    queretaro   tijuana
/
&fecha_nml
! Se indica el dia y el mes a calcular
! se proporciona el anio
! month jan =1 to dec=12
! day in the month (from 1 to 28,30 or 31)
idia=$dia
month=$mes
anio=$nyear
/
!Horario de verano
&verano_nml
! .true. o .false. para considerar o no el horario de verano
! inicia  dia de inicio en abril del horario de verano 
! termina dia de termino en octubre del Horario de verano 
lsummer = .true.
/
! Quimica a utilizar
&chem_nml
mecha='$MECHA'
! Los mecanismos a usar cbm04 cbm05 mozart racm2 radm2 sapcr99
/
End_Of_File

cd 02_aemis
if [ $HacerArea -eq 1 ]; then
echo "     Haciendo distribucion espacial en Fuentes de area"
./ASpatial.exe >../area.log &
fi
cd ../03_movilspatial
if [ $HacerArea -eq 1 ]; then
echo "     Haciendo distribucion espacial en Fuentes de Moviles"
./vial.exe > ../movil.log &
./carr.exe >> ../movil.log&
wait
./agrega.exe > ../movil.log 
cd ../05_semisM
./MSpatial.exe > ../movil.log
fi

fi
#
# Inicia Loop de Tiempo
# exit
while [ $dia -le $dia2 ] ;do
echo " Procesando el dia " $dia
cd $ProcessDir/04_temis
echo "directorio de trabajo "$PWD
#
#    Aqui cambiar el año a modelar
#
echo ' '
echo '  Mes ='$mes 'DIA '$dia
#
echo 'Area Temporal distribution'
./Atemporal.exe  > ../area.log &
echo 'Point Temporal distribution'
cd ../07_puntual/
./Puntual.exe >& ../puntual.log 
echo 'Movil Temporal distribution'
cd ../06_temisM/
./Mtemporal.exe > ../movil.log &
wait
#
#echo 'Biogenic'
#cd ../12_biogenic
#./Btemporal.exe > biog.log&
#
echo 'Speciation distribution PM2.5'
#
cd ../09_pm25spec
./spm25p.exe >> ../puntual.log &
./spm25m.exe >> ../movil.log &
./spm25a.exe >> ../area.log&
#

echo 'Speciation distribution VOCs'
#
cd ../08_spec
echo '   '$MECHA' *****'
echo 'Movile'
./spm.exe >> ../movil.log &
echo 'Puntual'
./spp.exe >> ../puntual.log  &
echo 'Area '
./spa.exe >> ../area.log
wait

echo ' Guarda'

cd ../10_storage
./emiss.exe  > ../${MECHA}.log
 let dia=dia+1
done
mv wrfchemi.d01* ../inventario/$dominio/
#ncrcat -O wrfchemi.d01.radm2.2019-0${mes}-1* wrfchemi_d01_2019-0${mes}-${dia}_00:00:00
#mv wrfchemi_d01_2019-02-05_00:00:00 ../../DOMAIN/mecanismos/emisiones
echo "DONE  Guarda "$MECHA
exit

