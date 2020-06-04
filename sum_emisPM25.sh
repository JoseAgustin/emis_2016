#!/bin/bash
#
# BC analysis
echo Area CN
awk -F"," -f suma_Apm25s.awk 04_temis/TACN__2016.csv
echo Mobile CN 
awk -F"," -f suma_Apm25s.awk 06_temisM/TMCN__2016.csv
echo Puntual CN
awk -F"," -f suma_Apm25s.awk 07_puntual/T_ANNCN.csv

# PM10 analysis
echo Area PM10
awk -F"," -f suma_Apm25s.awk 04_temis/TAPM102016.csv
echo Mobile PM10
awk -F"," -f suma_Apm25s.awk 06_temisM/TMPM102016.csv
echo Puntual PM10
awk -F"," -f suma_Apm25s.awk 07_puntual/T_ANNPM10.csv
#
#  Analysis of PM2.5 emissions
#
echo " Emissions per day of PM2.5 Area"
awk -F"," -f suma_Apm25.awk 04_temis/TAPM2_2016.csv
echo " Emissions speciation for Area Sources"
cd 09_pm25spec
ainv=(*_A.txt)
cd ..
num=${#ainv[@]}
num=$((num-1))
for (( i=0; i<=$num ;i++))
do
echo "${ainv[$i]:0:4}"
awk  -f suma_Apm25s.awk 09_pm25spec/${ainv[$i]}
done
#####################
echo " Emissions per day of PM2.5 Mobile"
awk -F"," -f suma_Apm25.awk 06_temisM/TMPM2_2016.csv
echo " Emissions speciation for Mobile Sources"
cd 09_pm25spec
ainv=(*_M.txt)
cd ..
num=${#ainv[@]}
num=$((num-1))
for (( i=0; i<=$num ;i++))
do
echo "${ainv[$i]:0:4}"
awk  -f suma_Apm25s.awk 09_pm25spec/${ainv[$i]}
done
######################
echo " Emissions per day of PM2.5 Puntual"
awk -F"," -f suma_Apm25p.awk 07_puntual/T_ANNPM25.csv
echo " Emissions speciation for Puntual Sources"
cd 09_pm25spec
ainv=(*_P.txt)
cd ..
num=${#ainv[@]}
num=$((num-1))
for (( i=0; i<=$num ;i++))
do
echo "${ainv[$i]:0:4}"
awk  -f suma_Apm25s.awk 09_pm25spec/${ainv[$i]}
done

