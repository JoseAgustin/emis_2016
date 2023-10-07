#: Title       : suma_A.awk
#: Date        : 17-05-2020
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total area emissions after spatial distribution
#: Options     : None
NR>2 { for (i=2;i<=NF;i++) a+=$i }
END{ printf a/1000000 OFS; printf "\n"}

