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
#  bajio     cdjuarez     colima      
#  ecaim    guadalajara  mexicali
#  mexico    monterrey    queretaro   tijuana
#
dominio=mexico
HacerArea=1
#
#  Build the fecha.txt file
# Cambiar aqui la fecha
mes=2
dia=8
dia2=14
dia1=$dia 
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
cd 02_aemis
ln -sf ../01_datos/$dominio/aeropuerto.csv
ln -sf ../01_datos/$dominio/agricola.csv
ln -sf ../01_datos/$dominio/bosque.csv
ln -sf ../01_datos/$dominio/centrales.csv
ln -sf ../01_datos/$dominio/ffcc.csv
ln -sf ../01_datos/$dominio/gri_pav.csv
ln -sf ../01_datos/$dominio/gri_pob.csv
ln -sf ../01_datos/$dominio/gri_ter.csv
ln -sf ../01_datos/$dominio/puertos.csv
if [ $HacerArea -eq 1 ]; then
echo "     Haciendo distribucion espacial en Fuentes de area"
./ASpatial.exe >../area.log &
fi
cd ../03_movilspatial
ln -sf ../01_datos/$dominio/CARRETERAS.csv
ln -sf ../01_datos/$dominio/VIALIDADES.csv
if [ $HacerArea -eq 1 ]; then
echo "     Haciendo distribucion espacial en Fuentes de Moviles"
./vial.exe > ../movil.log &
./carr.exe >> ../movil.log&
wait
./agrega.exe > ../movil.log 
cd ../05_semisM
./MSpatial.exe > ../movil.log
fi
cd ../07_puntual
ln -sf ../01_datos/$dominio/localiza.csv
cd ../10_storage
ln -sf ../01_datos/$dominio/localiza.csv
cd ../12_cmaq
ln -sf ../01_datos/$dominio/localiza.csv
fi
#
# Inicia Loop de Tiempo
# exit
while [ $dia -le $dia2 ] ;do
echo " Procesando el dia " $dia
cd $ProcessDir/04_temis
echo "directorio de trabajo "$PWD
#
if [ -e fecha.txt ]; then
rm fecha.txt
fi
#    Aqui cambiar el año a modelar
#
ln -sf anio2014.csv.org  anio2014.csv
#
cat << End_Of_File > fecha.txt
$mes       ! month jan =1 to dec=12
$dia       ! day in the month (from 1 to 28,30 or 31)
End_Of_File
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
wait
echo 'Speciation distribution VOCs'
#
cd ../08_spec
echo '   RADM2 *****'
ln -sf profile_radm2.csv profile_mech.csv
echo 'Movile'
./spm.exe >> ../movil.log &
echo 'Puntual'
./spp.exe >> ../puntual.log  &
echo 'Area '
./spa.exe >> ../area.log
#
#echo '    CBM05 *****'
#ln -sf profile_cbm05.csv profile_mech.csv
#echo 'Movile'
#./spm.exe >> ../movil.log &
#echo 'Puntual'
#./spp.exe >> ../puntual.log  &
#echo 'Area '
#./spa.exe >> ../area.log
#echo '    SAPRC99 *****'
#ln -sf profile_saprc99.csv profile_mech.csv
#echo 'Movile'
#./spm.exe >> ../movil.log &
#echo 'Puntual'
#./spp.exe >> ../puntual.log  &
#echo 'Area '
#./spa.exe >> ../area.log
#
#echo '    RACM2 *****'
#ln -sf profile_racm2.csv profile_mech.csv
#echo 'Movile'
#./spm.exe >> ../movil.log &
#echo 'Puntual'
#./spp.exe >> ../puntual.log  &
#echo 'Area '
#./spa.exe >> ../area.log

echo ' Guarda'

cd ../10_storage
./radm2.exe  > ../radm2.log
#./saprc.exe > ../saprc.log
#./cbm5.exe  > ../cbm5.log
#./racm2.exe > ../racm2.log
 let dia=dia+1
done
mv wrfchemi.d01* ../inventario/$dominio/
#ncrcat -O wrfchemi.d01.radm2.2019-0${mes}-1* wrfchemi_d01_2019-0${mes}-${dia}_00:00:00
#mv wrfchemi_d01_2019-02-05_00:00:00 ../../DOMAIN/mecanismos/emisiones
echo "DONE  guarda_RADM"
