#!/bin/bash
#
#  Suma emisiones 
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
