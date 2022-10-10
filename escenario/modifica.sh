#!/bin/bash
#
#: Title       : modifica.sh
#: Date        : 09/10/2020
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  09/10/2020 Modificacion de inventario de emisiones
#: Description : Programa de emisiones con funciones
#: Options     : None
#
ProcessDir=$PWD
echo "Directorio actual "$ProcessDir
#awk ' NR>1 {print $1}' municipio.txt
# Evalua si existe el archivo de salida para borrar
if [ -e tol_escI.csv ]
then
rm tol_escI.csv
fi
IEMI=../01_datos/emis/movil/emiss_2016.csv
head -1 $IEMI > tol_escI.csv
# Escenario 1
#  VOC    CO     NOx    NO2    NH3    PM10  PM2.5   CN     CO2   SO2     CH4
# 23.3%  22.1%  17.9%  17.1%  22.1%  22.3%  21.2%  15.3%  19.8%  20.4%  17.7%
#   3     4       5     6       7      8     9      10     11     12      13
#0.7668 0.7793 0.8213 0.8295 0.7786 0.7956 0.7771 0.7881 0.8018 0.8225  0.8466
awk -F, 'NR>1 {
if ($1==15005 || $1==15018 || $1==15027 || $1==15051 || $1==15054 ||
    $1==15055 || $1==15062 || $1==15067 || $1==15072 || $1==15073 ||
    $1==15076 || $1==15087 || $1==15090 || $1==15106 || $1==15115 || $1==15118){
  printf ("%s,%s,", $1, $2) ;
  printf("%f,%f,%f,%f,", $3*0.7668, $4*0.7793, $5*0.8213, $6*0.8295);
  printf("%f,%f,%f,%f,", $7*0.7786, $8*0.7956, $9*0.7771,$10*0.7881);
  printf("%f,%f,%f,",$11*0.8018,$12*0.8225,$13*0.8466);
  printf ("%s",$NF)
  printf ("\n");
}else
	print $0;
}' $IEMI  >> tol_escI.csv

# Evalua si existe el archivo de salida para borrar
if [ -e tol_escII.csv ]
then
rm tol_escII.csv
fi
head -1 $IEMI > tol_escII.csv
# Escenario 2
# VOC     CO    NOx    NO2    NH3    SO2    PM10  PM2.5  CO2     CH4   CN
# 20.8%  18.6%  14.9%  14.7%  19.4%  17.9%  20.8%  20.1%  17.5%  15.6%  14.5%
#   3     4       5     6       7     12     8     9      11     13      10
#0.7921  0.8139 0.8506 0.8529 0.8060 0.8213 0.7915 0.7988 0.8253 0.8435 0.8551
awk -F, 'NR>1 {
if ($1==15005 || $1==15018 || $1==15027 || $1==15051 || $1==15054 ||
    $1==15055 || $1==15062 || $1==15067 || $1==15072 || $1==15073 ||
    $1==15076 || $1==15087 || $1==15090 || $1==15106 || $1==15115 || $1==15118){
  printf ("%s,%s,", $1, $2) ;
  printf("%f,%f,%f,%f,", $3*0.7921, $4*0.8139, $5*0.8506, $6*0.8529);
  printf("%f,%f,%f,%f,", $7*0.8060, $8*0.7915, $9*0.7988,$10*0.8551);
  printf("%f,%f,%f,",$11*0.8253,$12*0.8213,$13*0.8435);
  printf ("%s",$NF)
  printf ("\n");
}else
  print $0;
}' $IEMI  >> tol_escII.csv
echo "TERMINA"
