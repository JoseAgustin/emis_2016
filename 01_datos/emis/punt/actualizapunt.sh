tr -d '\r' < Puntuales2016.csv > puntual_unix.csv
gawk -F',' '
BEGIN{
    OFS=","
    n_tula_pm10=0
    n_tula_pm25=0
}

NR==1{
    print
    next
}

{
    lat=$1+0
    lon=$2+0

    zona="OTROS"
    fpm10=1
    fpm25=1

    if(lat>=25.51 && lat<=26.00 && lon>=-100.4 && lon<=-99.87){
        zona="MONTERREY"; fpm10=4; fpm25=3
    }
    else if(lat>=19.23 && lat<=19.39 && lon>=-99.72 && lon<=-99.50){
        zona="TOLUCA"; fpm10=5; fpm25=2
    }
    else if(lat>=18.95 && lat<=19.12 && lon>=-98.32 && lon<=-98.10){
        zona="PUEBLA"; fpm10=2; fpm25=1.3
    }
    else if(lat>=19.29 && lat<=19.36 && lon>=-98.26 && lon<=-98.15){
        zona="TLAXCALA"; fpm10=2; fpm25=1.3
    }
    else if(lat>=20.03 && lat<=20.13 && lon>=-98.80 && lon<=-98.67){
        zona="PACHUCA"; fpm10=10; fpm25=10
    }
    else if(lat>=18.89 && lat<=18.98 && lon>=-99.26 && lon<=-99.14){
        zona="CUERNAVACA"; fpm10=4; fpm25=3
    }
    else if(lat>=19.8920 && lat<=20.1825 && lon>=-99.4447 && lon<=-99.0890){
        zona="TULA"
        if($4!="" && ($4+0)>100){ $4=sprintf("%.6f",$4*0.5); n_tula_pm10++ }
        if($5!="" && ($5+0)>74){  $5=sprintf("%.6f",$5*0.2); n_tula_pm25++ }
        print
        next
    }

    if(zona!="OTROS")
        contador[zona]++

    if($4!="") $4=sprintf("%.6f",$4*fpm10)
    if($5!="") $5=sprintf("%.6f",$5*fpm25)

    print
}

END{
    print "=================================" > "/dev/stderr"
    print "Fuentes modificadas" > "/dev/stderr"
    print "=================================" > "/dev/stderr"
    for(i in contador) print i, contador[i] > "/dev/stderr"
    print "Fuentes Tula modificadas PM10:", n_tula_pm10 > "/dev/stderr"
    print "Fuentes Tula modificadas PM25:", n_tula_pm25 > "/dev/stderr"
}
' puntual_unix.csv > Puntuales2016_mod.csv
rm puntual_unix.csv