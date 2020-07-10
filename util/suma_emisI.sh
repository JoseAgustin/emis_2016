#!/bin/bash
#: Title       : Suma emisiones del inventario
#: Date        : 06-05-2020
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total area emissions before spatial distribution
#: Options     : None#
#
cd 02_aemis
ainv=(I*csv imet*)
cd ..
num=${#ainv[@]}
num=$((num-1))
for (( i=0; i<=$num ;i++))
do
 echo "${ainv[$i]:1:4}"
 awk -F"," -f suma_I.awk 02_aemis/${ainv[$i]}
done
