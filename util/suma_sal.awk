#: Title       : suma_sal.awk
#: Date        : 10-07-2023
#: Author      : "Agustin Garcia"<agustin@atmosfera.unam.mx>
#: Version     : 1.0
#: Description : Obtain the total area emissions after spatial distribution
#: Options     : None
BEGIN{ printf val"\t"va2"\t"}
NR>lin { for (i=1;i<=NF;i++) a+=$i }
END{ printf a/1000000 OFS; printf "\n"}
