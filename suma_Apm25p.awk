NR>2 { for (i=4;i<=NF-1;i++) a+=$i }
END{ printf a/1000000 OFS; printf "\n"}
# awk -F"," -f suma_Apm25p.awk 07_puntual/T_ANNPM25.csv
