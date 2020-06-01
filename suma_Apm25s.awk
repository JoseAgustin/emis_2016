NR>2 { for (i=2;i<=NF;i++) a+=$i }
END{ printf a/1000000 OFS; printf "\n"}
# awk -F"," -f suma_Apm25.awk 09_pm25spec/GSO4_A.txt
