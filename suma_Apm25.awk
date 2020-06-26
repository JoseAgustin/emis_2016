#: Title       :Suma emisiones PM2.5
#: Date        : 6-6-2020
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total PM2.5 area emissions after spatial distribution
#: Options     : None
NR>2 { for (i=3;i<=NF;i++) a+=$i }
END{ printf a/1000000 OFS; printf "\n"}
# awk -F"," -f suma_Apm25.awk 09_pm25spec/GSO4_A.txt
