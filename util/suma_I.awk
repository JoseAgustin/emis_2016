#: Title       : sumaI.awk
#: Date        : 6-6-2020
#: Author      : "Agustin Garcia" <agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total area emissions before spatial distribution
#: Options     : None
NR>3 { for (i=4;i<=NF;i++) a+=$i }
END{ printf a OFS; printf "\n"}
# awk -F"," -f sumaI.awk 02_aemis/INOx_2014.csv
