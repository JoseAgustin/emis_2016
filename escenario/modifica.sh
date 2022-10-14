#!/bin/bash
#
#: Title       : modifica.sh
#: Date        : 09/10/2022
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  09/10/2020 Modificacion de inventario de emisiones
#: Description : Programa de emisiones con funciones
#: Options     : None
#
ProcessDir=$PWD
echo "------"
echo "   Directorio actual "$ProcessDir
echo "   Emisiones de fuentes móviles"
#awk ' NR>1 {print $1}' municipio.txt
# Evalua si existe el archivo de salida para borrar
if [ -e tol_base_movil.csv ]
then
rm tol_base_movil.csv
fi
IEMI=../01_datos/emis/movil/emiss_2016.csv
head -1 $IEMI > tol_base_movil.csv
# Escenario 1
#  VOC    CO     NOx    NO2    NH3    PM10  PM2.5   CN     CO2   SO2     CH4
#  2.4    4       1      1      1     5      5     1      1     10      1
#   3     4       5     6       7      8     9      10     11     12      13
#0.7668 0.7793 0.8213 0.8295 0.7786 0.7956 0.7771 0.7881 0.8018 0.8225  0.8466
awk -F, 'NR>1 {
if ($1==15005 || $1==15018 || $1==15027 || $1==15051 || $1==15054 ||
    $1==15055 || $1==15062 || $1==15067 || $1==15072 || $1==15073 ||
    $1==15076 || $1==15087 || $1==15090 || $1==15106 || $1==15115 || $1==15118){
  printf ("%s,%s,", $1, $2) ;
  printf("%f,%f,%f,%f,", $3*2.4, $4*4, $5*1, $6*1);
  printf("%f,%f,%f,%f,",$7*1, $8*5, $9*5,$10*1);
  printf("%f,%f,%f,",$11*1,$12*10.,$13*1);
  printf ("%s",$NF)
  printf ("\n");
}else
	print $0;
}' $IEMI  >> tol_base_movil.csv
IEMI=tol_base_movil.csv
#  Escenario 2
printf "\t Escenario 2 fuentes Moviles\n"
head -1 $IEMI > tol_escII.csv
# Escenario 2
# VOC     CO    NOx    NO2    NH3    SO2    PM10  PM2.5  CO2     CH4   CN
# 20.8%  18.6%  14.9%  14.7%  19.4%  17.9%  20.8%  20.1%  17.5%  15.6%  14.5%
#   3     4       5     6       7     12     8     9      11     13      10
#0.7921  0.8139 0.8506 0.8529 0.8060 0.8213 0.7915 0.7988 0.8253 0.8435 0.8551
awk -F, 'NR>1 {
if ($1==15005 || $1==15018 || $1==15027 || $1==15051 || $1==15054 ||
    $1==15055 || $1==15062 || $1==15067 || $1==15072 || $1==15073 ||
    $1==15076 || $1==15087 || $1==15090 || $1==15106 || $1==15115 || $1==15118)
{
printf ("%s,%s,", $1, $2) ;
printf("%f,%f,%f,%f,", $3*0.7921, $4*0.8139, $5*0.8506, $6*0.8529);
printf("%f,%f,%f,%f,", $7*0.8060, $8*0.7915, $9*0.7988,$10*0.8551);
printf("%f,%f,%f,",$11*0.8253,$12*0.8213,$13*0.8435);
printf ("%s\n",$NF);
}else
print $0 ;
}' $IEMI  >> tol_escII.csv
#
echo Emisiones de VOC fuentes de área x2.4
IEMI=../01_datos/emis/area/IVOC_2016.csv
head -3 $IEMI > tol_base_IVOC.csv
awk -F, 'NR>3 {
if ($3==15005 || $3==15018 || $3==15027 || $3==15051 || $3==15054 ||
    $3==15055 || $3==15062 || $3==15067 || $3==15072 || $3==15073 ||
    $3==15076 || $3==15087 || $3==15090 || $3==15106 || $3==15115 || $3==15118)
    {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*2.4) ;
      printf("%f\n", $NF*2.4);
    }
    else print $0}' $IEMI >>tol_base_IVOC.csv
#
echo Emisiones de SO2 fuentes de área x10
IEMI=../01_datos/emis/area/ISO2_2016.csv
head -3 $IEMI > tol_base_ISO2.csv
awk -F, 'NR>3 {
if ($3==15005 || $3==15018 || $3==15027 || $3==15051 || $3==15054 ||
    $3==15055 || $3==15062 || $3==15067 || $3==15072 || $3==15073 ||
    $3==15076 || $3==15087 || $3==15090 || $3==15106 || $3==15115 || $3==15118)
    {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf("%f," ,$i*10) ;
      printf("%f\n", $NF*10);
    }
    else print $0 }' $IEMI >> tol_base_ISO2.csv
#
echo Emisiones de CO fuentes de área x4
IEMI=../01_datos/emis/area/ICO__2016.csv
head -3 $IEMI > tol_base_ICO.csv
awk -F, 'NR>3 {
if ($3==15005 || $3==15018 || $3==15027 || $3==15051 || $3==15054 ||
    $3==15055 || $3==15062 || $3==15067 || $3==15072 || $3==15073 ||
    $3==15076 || $3==15087 || $3==15090 || $3==15106 || $3==15115 || $3==15118)
    {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf("%f,", $i*4) ;
      printf("%f\n", $NF*4);
    }
    else  print $0 }' $IEMI >> tol_base_ICO.csv
    
# Evalua si existe el archivo de salida para borrar
if [ -e tol_baseI.csv ]
then
rm tol_escII.csv
fi
echo "TERMINA"
