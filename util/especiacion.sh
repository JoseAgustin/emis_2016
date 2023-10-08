#!/bin/bash
#: Title       :Suma especies quimicas
#: Date        : 10-7-2023
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total emissions fros speciation
#: Options     : None

# avalable:
# CBM04 cbm05 mozart racm2 radm2 saprc99 saprc07 ghg
MECHA=MOZA
spec=(${MECHA}*_M.txt)
num=${#spec[@]}
for (( i=0; i<$num ;i++))
do
 printf "%s"  "${spec[$i]:7:4}, "
 awk -F"," -f ../../util/suma_spc.awk ${spec[$i]}
done
