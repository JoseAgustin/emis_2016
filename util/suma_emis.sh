#!/bin/bash
#: Title       :Suma emisiones
#: Date        : 6-6-2020
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total area emissions after spatial distribution
#: Options     : None
#cd 02_aemis
ainv=(A*csv)
num=${#ainv[@]}
num=$((num-1))
for (( i=0; i<=$num ;i++))
do
 echo "${ainv[$i]:1:4}"
 awk -F"," -f ../util/suma_A.awk ${ainv[$i]}
done
