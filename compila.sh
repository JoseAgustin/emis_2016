#!/bin/bash
#
#  Creado por Jose Agustin Garcia Reynoso el 26/07/17.
#
#  Proposito:
#         Compila los programas que se requieren para hacer la conversion
#
#  Modificaciones:
#         27/07/2017 Actualizacion para IE del 2014
#         10/12/2019 Actualizado para 2016
#
export ProcessDir=$PWD
echo $ProcessDir
#
#  
cd  02_aemis/
    ifort -o ASpatial.exe -O3 area_espacial.F90 &
    wait
    ./ASpatial.exe > ../ejecuta.log &
cd ../03_movilspatial/
    ifort -O2 -axAVX suma_carretera.F90 -o carr.exe &
    ifort -O2 -axAVX suma_vialidades.F90 -o vial.exe &
    ifort -O2 -axAVX agrega.f90 -o agrega.exe &
    wait
    ./carr.exe > ../ejecuta.log &
    ./vial.exe >> ../ejecuta.log &
     wait
    ./agrega.exe >>../ejecuta.log &
cd ../04_temis
   ifort -O3 -axAVX atemporal.f90 -o Atemporal.exe &
cd ../05_semisM
    ifort -O3 -axAVX -o MSpatial.exe movil_spatial.F90 
    ./MSpatial.exe >> ../ejecuta.log &
cd ../06_temisM
    ifort -O3 movil_temp.f90 -o Mtemporal.exe &
cd ../07_puntual
    ifort -O3 -axAVX  t_puntal.F90 -o Puntual.exe &
cd ../08_spec
    ifort -O2 -axAVX  agg_a.f90 -o spa.exe
    ifort -O2 -axAVX  agg_m.f90 -o spm.exe
    ifort -O2 -axAVX  agg_p.f90 -o spp.exe &
cd ../09_pm25spec
ifort -O2 -axAVX  pm25_speci_a.F90 -o spm25a.exe &
ifort -O2 -axAVX  pm25_speci_m.F90 -o spm25m.exe &
ifort -O2 -axAVX  pm25_speci_p.F90 -o spm25p.exe &
cd ../10_storage/
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include g_2016_racm.f90 -o racm2.exe &
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include  g_cbm5_2016.f90 -o cbm5.exe &
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include g_radm_2016.f90 -o radm2.exe &
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include g_saprc_2016.f90 -o saprc.exe
cd ..
echo "Done Compila"
exit 0

