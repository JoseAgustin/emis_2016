#!/bin/sh

#: Title       :suma_nc.sh
#: Date        :10/07/23.
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total  emissions afrom netcdf file
#: Options     : None
if [ -f sumas.txt ]
then
rm sumas.txt
fi
for VAR in  BC CH4 CO CO2 NH3 NOx NO NO2 PM10 PM25 SO2 VOC
do
 for FIL in $(ls ../tmpmexico9/dia11/*$VAR.nc|grep POINT )
 do
  echo $FIL
  for VAR2 in $(ncdump -h $FIL |grep float| grep $VAR | awk '{print $2}'|awk -F'(' '{print $1}')
  do
       ncdump -v $VAR2 $FIL >sal
       NL=$(awk '{if ($1=="data:" ) {print NR}}' sal )
       awk -v val=$VAR -v va2=$VAR2 -v lin=$NL -f suma_sal.awk sal >> sumas.txt
  done
 done
done
