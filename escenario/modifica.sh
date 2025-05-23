#!/bin/bash
#
#: Title       : modifica.sh
#: Date        : 09/10/2022
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.1  09/10/2020 Modificacion de inventario de emisiones
#: Description : Actualizacion a 2019 de emisiones de la CAME Toluca, Pachuca, Pue-Tlax y Qro
#: Options     : None
#
ProcessDir=$PWD
echo "------"
echo "   Directorio actual "$ProcessDir
echo "   Emisiones de fuentes móviles"
#awk ' NR>1 {print $1}' municipio.txt
# Evalua si existe el archivo de salida para borrar
if [ -e came_base_movil.csv ]
then
rm came_base_movil.csv
fi
IEMI=../01_datos/emis/movil/emiss_2016.csv
# Para Toluca
head -1 $IEMI > came_base_movil.csv
# Escenario 1
#  VOC    CO     NOx    NO2    NH3    PM10  PM2.5   CN     CO2   SO2     CH4
#  2.4    4       1      1      1      5      5      1      1     10       1
#   3     4       5     6       7      8      9     10     11     12      13
#0.7668 0.7793 0.8213 0.8295 0.7786 0.7956 0.7771 0.7881 0.8018 0.8225  0.8466
# Puebla-tlaxcala
#  2.0    2.8     1      1      1      1      1      1       1      1      1
# Pachuca
#  1.5    3.0     1      1      1      1      1      1       1      1      1
# Qro
#  1.6    2.0     1      1      1      1      1      1       1      1      1
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
} else if ($1==21001|| $1==21015|| $1==21034|| $1==21041|| $1==21048|| $1==21060||
           $1==21074|| $1==21090|| $1==21106|| $1==21114|| $1==21119|| $1==21122||
           $1==21125|| $1==21132|| $1==21136|| $1==21140|| $1==21143|| $1==21163||
           $1==21181|| $1==29015|| $1==29017|| $1==29019|| $1==29022|| $1==29023||
           $1==29025|| $1==29027|| $1==29028|| $1==29029|| $1==29032|| $1==29041||
           $1==29042|| $1==29044|| $1==29051|| $1==29053|| $1==29054|| $1==29056||
           $1==29057|| $1==29058|| $1==29059) {
  printf ("%s,%s,", $1, $2) ;
  printf("%f,%f,%f,%f,", $3*2, $4*2.8, $5*1, $6*1);
  printf("%f,%f,%f,%f,",$7*1, $8*1, $9*1,$10*1);
  printf("%f,%f,%f,",$11*1,$12*1.,$13*1);
  printf ("%s",$NF)
  printf ("\n");
  } else if ($1==13022|| $1==13039|| $1==13048|| $1==13051|| $1==13052||
            $1==13082|| $1==13083) {
  printf ("%s,%s,", $1, $2) ;
  printf("%f,%f,%f,%f,", $3*1.5, $4*3, $5*1, $6*1);
  printf("%f,%f,%f,%f,",$7*1, $8*1, $9*1,$10*1);
  printf("%f,%f,%f,",$11*1,$12*1.,$13*1);
  printf ("%s",$NF)
  printf ("\n");
  } else if ($1==11004|| $1==22006|| $1==22008|| $1==22011|| $1==22014){
  printf ("%s,%s,", $1, $2) ;
  printf("%f,%f,%f,%f,", $3*1.6, $4*2, $5*1, $6*1);
  printf("%f,%f,%f,%f,",$7*1, $8*1, $9*1,$10*1);
  printf("%f,%f,%f,",$11*1,$12*1.,$13*1);
  printf ("%s",$NF)
  printf ("\n");
  } else
	print $0;
}' $IEMI  >> came_base_movil.csv
IEMI=came_base_movil.csv
#  Escenario 2
printf "\t Escenario 2 fuentes Moviles\n"
head -1 $IEMI > came_escII.csv
# Escenario Toluca
# VOC     CO    NOx    NO2    NH3    SO2    PM10  PM2.5  CO2     CH4   CN
# 20.8%  18.6%  14.9%  14.7%  19.4%  17.9%  20.8%  20.1%  17.5%  15.6%  14.5%
#   3     4       5     6       7     12     8     9      11     13      10
#0.7921  0.8139 0.8506 0.8529 0.8060 0.8213 0.7915 0.7988 0.8253 0.8435 0.8551
awk -F, 'NR>1 {
if ($1==15005 || $1==15018 || $1==15027 || $1==15051 || $1==15054 ||
    $1==15055 || $1==15062 || $1==15067 || $1==15072 || $1==15073 ||
    $1==15076 || $1==15087 || $1==15090 || $1==15106 || $1==15115 ||
    $1==15118 || $1==21001|| $1==21015|| $1==21034|| $1==21041||
    $1==21048 || $1==21060|| $1==21074|| $1==21090|| $1==21106||
    $1==21114|| $1==21119|| $1==21122|| $1==21125|| $1==21132||
    $1==21136|| $1==21140|| $1==21143|| $1==21163|| $1==21181||
    $1==29015|| $1==29017|| $1==29019|| $1==29022|| $1==29023||
    $1==29025|| $1==29027|| $1==29028|| $1==29029|| $1==29032||
    $1==29041|| $1==29042|| $1==29044|| $1==29051|| $1==29053||
    $1==29054|| $1==29056|| $1==29057|| $1==29058|| $1==29059||
    $1==13022|| $1==13039|| $1==13048|| $1==13051|| $1==13052||
    $1==13082|| $1==13083|| $1==11004|| $1==22006|| $1==22008||
    $1==22011|| $1==22014)
{
printf ("%s,%s,", $1, $2) ;
printf("%f,%f,%f,%f,", $3*0.7921, $4*0.8139, $5*0.8506, $6*0.8529);
printf("%f,%f,%f,%f,", $7*0.8060, $8*0.7915, $9*0.7988,$10*0.8551);
printf("%f,%f,%f,",$11*0.8253,$12*0.8213,$13*0.8435);
printf ("%s\n",$NF);
}else
print $0 ;
}' $IEMI  >> came_escII.csv
#
echo Emisiones de VOC fuentes de área Toluca x2.4, Pachucax1.5,Puebla-Tlax2, Qro2.5
IEMI=../01_datos/emis/area/IVOC_2016.csv
head -3 $IEMI > came_base_IVOC.csv
# Para Toluca
awk -F, 'NR>3 {
if ($3==15005 || $3==15018 || $3==15027 || $3==15051 || $3==15054 ||
    $3==15055 || $3==15062 || $3==15067 || $3==15072 || $3==15073 ||
    $3==15076 || $3==15087 || $3==15090 || $3==15106 || $3==15115 || $3==15118)
    {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*2.4) ;
      printf("%f\n", $NF*2.4);
    }
    else if($3==21001|| $3==21015|| $3==21034|| $3==21041|| $3==21048|| $3==21060||
           $3==21074|| $3==21090|| $3==21106|| $3==21114|| $3==21119|| $3==21122||
           $3==21125|| $3==21132|| $3==21136|| $3==21140|| $3==21143|| $3==21163||
           $3==21181|| $3==29015|| $3==29017|| $3==29019|| $3==29022|| $3==29023||
           $3==29025|| $3==29027|| $3==29028|| $3==29029|| $3==29032|| $3==29041||
           $3==29042|| $3==29044|| $3==29051|| $3==29053|| $3==29054|| $3==29056||
           $3==29057|| $3==29058|| $3==29059) {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*2) ;
      printf("%f\n", $NF*2);
    }
    else  if ($3==13022|| $3==13039|| $3==13048|| $3==13051|| $3==13052||
            $3==13082|| $3==13083) {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*1.5) ;
      printf("%f\n", $NF*1.5);
    }
    else if ($3==11004|| $3==22006|| $3==22008|| $3==22011|| $3==22014){
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*1.6) ;
      printf("%f\n", $NF*1.6);
    }
    else
     print $0}' $IEMI >>came_base_IVOC.csv
#
echo Emisiones de SO2 fuentes de área x10
IEMI=../01_datos/emis/area/ISO2_2016.csv
head -3 $IEMI > came_base_ISO2.csv
# Para Toluca
awk -F, 'NR>3 {
if ($3==15005 || $3==15018 || $3==15027 || $3==15051 || $3==15054 ||
    $3==15055 || $3==15062 || $3==15067 || $3==15072 || $3==15073 ||
    $3==15076 || $3==15087 || $3==15090 || $3==15106 || $3==15115 || $3==15118)
    {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf("%f," ,$i*10) ;
      printf("%f\n", $NF*10);
    }
    else print $0 }' $IEMI >> came_base_ISO2.csv
#
echo Emisiones de CO fuentes de área x4
IEMI=../01_datos/emis/area/ICO__2016.csv
head -3 $IEMI > came_base_ICO.csv
# Para Toluca
awk -F, 'NR>3 {
if ($3==15005 || $3==15018 || $3==15027 || $3==15051 || $3==15054 ||
    $3==15055 || $3==15062 || $3==15067 || $3==15072 || $3==15073 ||
    $3==15076 || $3==15087 || $3==15090 || $3==15106 || $3==15115 || $3==15118)
    {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf("%f,", $i*4) ;
      printf("%f\n", $NF*4);
    }
        else if($3==21001|| $3==21015|| $3==21034|| $3==21041|| $3==21048|| $3==21060||
           $3==21074|| $3==21090|| $3==21106|| $3==21114|| $3==21119|| $3==21122||
           $3==21125|| $3==21132|| $3==21136|| $3==21140|| $3==21143|| $3==21163||
           $3==21181|| $3==29015|| $3==29017|| $3==29019|| $3==29022|| $3==29023||
           $3==29025|| $3==29027|| $3==29028|| $3==29029|| $3==29032|| $3==29041||
           $3==29042|| $3==29044|| $3==29051|| $3==29053|| $3==29054|| $3==29056||
           $3==29057|| $3==29058|| $3==29059) {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*2.8) ;
      printf("%f\n", $NF*2.8);
    }
    else  if ($3==13022|| $3==13039|| $3==13048|| $3==13051|| $3==13052||
            $3==13082|| $3==13083) {
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*3) ;
      printf("%f\n", $NF*3);
    }
    else if ($3==11004|| $3==22006|| $3==22008|| $3==22011|| $3==22014){
      printf ("%s,%s,%s,",$1,$2,$3) ;
      for (i = 4; i < NF; i++) printf ("%g," ,$i*2) ;
      printf("%f\n", $NF*2);
    }
    else
      print $0 }' $IEMI >> came_base_ICO.csv
echo "TERMINA"
echo "------"
