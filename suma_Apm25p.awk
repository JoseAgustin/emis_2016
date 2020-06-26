#: Title       : suma_Apm25p.awk
#: Date        : 6-6-2020
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total PM2.5 point emissions after temporal distribution
#: Options     : None
NR>2 { for (i=4;i<=NF-1;i++) a+=$i }
END{ printf a/1000000 OFS; printf "\n"}
# awk -F"," -f suma_Apm25p.awk 07_puntual/T_ANNPM25.csv
