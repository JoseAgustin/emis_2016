#!/bin/bash
# -----------------------------------------------------------------------------
# ARCHIVO:      functions.sh (Revisado con nuevas funciones)
#
# DESCRIPCIÓN:  Biblioteca de funciones de shell para procesar emisiones.
#               Incluye una función para verificar la salida de cada programa
#               y detener la ejecución en caso de error.
#
# COMPATIBILIDAD MULTIPLATAFORMA (Linux / macOS):
#               El comando `date` de GNU/Linux (coreutils) y el `date` nativo
#               de macOS (BSD) NO comparten la misma sintaxis para fechas
#               relativas ni para parsear fechas arbitrarias:
#
#                 GNU/Linux:  date -d "2024-03-05" "+%a"
#                             date -d "+1 days"    "+%Y"
#                             date -d "-1 days"    "+%m"
#
#                 macOS/BSD:  date -j -f "%Y-%m-%d" "2024-03-05" "+%a"
#                             date -v+1d            "+%Y"
#                             date -v-1d            "+%m"
#
#               Para evitar duplicar lógica en cada función, este archivo
#               define `date_compat()`, un wrapper que detecta el sistema
#               operativo (`uname`) y traduce la llamada al dialecto correcto.
#               Todas las funciones de este archivo usan `date_compat` en
#               lugar de invocar `date -d` directamente.
#
#               Alternativa recomendada en macOS: instalar GNU coreutils
#               vía Homebrew (`brew install coreutils`), lo que provee
#               `gdate` con sintaxis GNU idéntica a Linux. Si `gdate` está
#               disponible en el PATH, `date_compat` lo usará automáticamente
#               y evita la traducción de sintaxis (más robusto).
# -----------------------------------------------------------------------------

# --- Definición de Códigos de Color ---
COLOR_INFO='\033[0;36m'
COLOR_SUCCESS='\033[0;32m'
COLOR_WARNING='\033[1;33m'
COLOR_ERROR='\033[1;41m'
COLOR_RESET='\033[0m'

# =============================================================================
# FUNCIÓN: date_compat (NUEVA)
#
# Propósito:   Wrapper multiplataforma sobre `date` que oculta las diferencias
#              de sintaxis entre GNU/Linux (coreutils) y macOS (BSD).
#
# Modos de uso:
#   date_compat offset <±N> <formato>
#       Calcula una fecha relativa a HOY en días y la formatea.
#       Equivale a: date -d "+N days" "+formato"   (GNU)
#                   date -v+Nd        "+formato"   (BSD)
#       Ejemplos:
#         date_compat offset 1  "+%Y"   # mañana, año en formato YYYY
#         date_compat offset -1 "+%d"   # ayer, día del mes
#
#   date_compat parse <AAAA/MM/DD o AAAA-MM-DD> <formato>
#       Parsea una fecha arbitraria (no relativa a hoy) y la formatea.
#       Equivale a: date -d "$fecha" "+formato"               (GNU)
#                   date -j -f "%Y-%m-%d" "$fecha" "+formato" (BSD)
#       Ejemplo:
#         date_compat parse "2024/03/05" "+%a"   # día de la semana abreviado
#
#   date_compat valid <AAAA/MM/DD o AAAA-MM-DD>
#       Solo valida que la fecha exista (sin imprimir nada).
#       Devuelve código de salida 0 si es válida, distinto de 0 si no.
#
# Detección de plataforma:
#   - Si existe `gdate` en el PATH (GNU coreutils instalado vía Homebrew en
#     macOS), se usa directamente con sintaxis GNU: es la opción más fiable.
#   - Si `uname` reporta "Darwin" y no hay `gdate`, se traduce a sintaxis BSD.
#   - En cualquier otro caso (Linux y similares), se usa `date -d` (GNU).
# =============================================================================
date_compat() {
    local modo="$1"; shift

    # Preferir gdate (GNU coreutils) si está disponible, sin importar el OS.
    local DATE_BIN="date"
    if command -v gdate &>/dev/null; then
        DATE_BIN="gdate"
    fi

    local es_macos_bsd=0
    if [ "$DATE_BIN" = "date" ] && [ "$(uname)" = "Darwin" ]; then
        es_macos_bsd=1
    fi

    case "$modo" in
        offset)
            local valor="$1" formato="$2"
            if [ "$es_macos_bsd" -eq 1 ]; then
                # BSD: date -v+Nd / date -v-Nd (sin espacio entre signo y número)
                if [[ "$valor" == -* ]]; then
                    date -v"${valor}d" "$formato"
                else
                    date -v+"${valor}d" "$formato"
                fi
            else
                # GNU (Linux o gdate en macOS): date -d "+N days" / "-N days"
                if [[ "$valor" == -* ]]; then
                    "$DATE_BIN" -d "${valor} days" "$formato"
                else
                    "$DATE_BIN" -d "+${valor} days" "$formato"
                fi
            fi
            ;;
        parse)
            local fecha="$1" formato="$2"
            # Normaliza separadores '/' a '-' para máxima compatibilidad.
            local fecha_normalizada="${fecha//\//-}"
            if [ "$es_macos_bsd" -eq 1 ]; then
                # BSD requiere el formato de entrada explícito con -f.
                date -j -f "%Y-%m-%d" "$fecha_normalizada" "$formato"
            else
                "$DATE_BIN" -d "$fecha_normalizada" "$formato"
            fi
            ;;
        valid)
            local fecha="$1"
            local fecha_normalizada="${fecha//\//-}"
            if [ "$es_macos_bsd" -eq 1 ]; then
                date -j -f "%Y-%m-%d" "$fecha_normalizada" &>/dev/null
            else
                "$DATE_BIN" -d "$fecha_normalizada" &>/dev/null
            fi
            ;;
        *)
            echo -e "${COLOR_ERROR} ERROR (date_compat): modo '$modo' no reconocido. Usa: offset | parse | valid ${COLOR_RESET}"
            return 1
            ;;
    esac
}

# =============================================================================
# FUNCIÓN: run_and_check (NUEVA)
#
# Propósito:   Ejecuta un comando y verifica su código de salida. Si el código
#              no es 0 (error), imprime un mensaje y aborta el script.
# Parámetros:
#   $@:        El comando completo a ejecutar (ej. bin/ASpatial.exe).
# =============================================================================
run_and_check() {
    # Ejecuta todos los argumentos pasados como un solo comando.
    "$@"
    local exit_code=$? # Captura el código de salida del comando anterior.
    
    if [ $exit_code -ne 0 ]; then
        echo -e "${COLOR_ERROR}ERROR: El comando '$*' falló con el código de salida $exit_code. Abortando.${COLOR_RESET}"
        # Salir del script con el mismo código de error del programa que falló.
        exit $exit_code
    fi
}

# =============================================================================
# BLOQUE DE FUNCIONES DE PROCESAMIENTO (ACTUALIZADO)
#
# Propósito:   Funciones que ejecutan los binarios. Ahora usan 'run_and_check'
#              para garantizar que el script se detenga si un paso falla.
#              La ejecución ahora es secuencial para permitir esta verificación.
# =============================================================================
hace_area() {
    echo "Ejecutando distribución espacial para fuentes de área..."
    run_and_check bin/ASpatial.exe > ./area.log
}

hace_movil() {
    echo "Ejecutando distribución espacial para fuentes móviles..."
    run_and_check bin/vial.exe > ./movil.log
    run_and_check bin/carr.exe >> ./movil.log
    run_and_check bin/agrega.exe >> ./movil.log
    run_and_check bin/MSpatial.exe >> ./movil.log
}

emis_area() {
    echo "Procesando emisiones de ÁREA (Temporal y Especiación)..."
    ln -fs ../chem/profile_${MECHA}.csv .
    run_and_check ../bin/Atemporal.exe > ../area.log
    run_and_check ../bin/spm25a.exe >> ../area.log
    run_and_check ../bin/spa.exe >> ../area.log
}

emis_fijas() {
    echo "Procesando emisiones de FUENTES FIJAS (Temporal y Especiación)..."
    run_and_check ../bin/Puntual.exe > ../puntual.log
    run_and_check ../bin/spm25p.exe >> ../puntual.log
    run_and_check ../bin/spp.exe >> ../puntual.log
}

emis_movil() {
    echo "Procesando emisiones MÓVILES (Temporal y Especiación)..."
    run_and_check ../bin/Mtemporal.exe > ../movil.log
    run_and_check ../bin/spm25m.exe >> ../movil.log
    run_and_check ../bin/spm.exe >> ../movil.log
}

# =============================================================================
# FUNCIÓN: procesar_dia_pronostico
#
# Propósito:   Encapsula la lógica para procesar un día. Ahora las llamadas
#              a emis_* son secuenciales y seguras.
# =============================================================================
procesar_dia_pronostico() {
    local offset="$1"
    local etiqueta_dia
    case $offset in
        0) etiqueta_dia="Hoy";;
        1) etiqueta_dia="Mañana";;
        2) etiqueta_dia="Pasado Mañana";;
        *) echo "Offset inválido"; return;;
    esac

    # Compatibilidad: usa `date_compat offset` en lugar de `date -d "+N days"`,
    # que es sintaxis GNU/Linux y falla en macOS (BSD). Ver definición de
    # date_compat al inicio de este archivo.
    export dia=$(date_compat offset "$offset" +%d)
    export mes=$(date_compat offset "$offset" +%m)
    export nyear=$(date_compat offset "$offset" +%Y)
    
    local fecha_str="${nyear}-${mes}-${dia}"
    echo -e "\n${COLOR_INFO}--- Procesando día: $etiqueta_dia ($fecha_str) ---${COLOR_RESET}"

    local archivo_salida="${DOMAINS}/interpolaD01/wrfchemi_d01_${MECHA}_${dominio:0:8}_${fecha_str}_00:00:00"
    if [ -f "$archivo_salida" ]; then
        echo -e "${COLOR_WARNING}---> Archivo de salida ya existe. Saltando día.${COLOR_RESET}"
        return
    fi
    
    local dir_dia="dia${dia}"
    mkdir -p "$dir_dia"
    cd "$dir_dia"
    
    echo "Directorio de trabajo: $(pwd)"
    echo "Creando archivos de configuración para el ${dia}/${mes}/${nyear}..."
    hace_namelist
    crea_anio_csv "$nyear" "$mes" "$dia"
    
    echo "Ejecutando procesamiento de emisiones (secuencialmente)..."
    # Estas funciones ahora se detendrán si hay un error interno.
    emis_area
    emis_fijas
    emis_movil
    
    echo "Combinando emisiones y generando archivo final..."
    ln -fs ../chem/namelist.* .
    run_and_check ../bin/emiss.exe > ../${MECHA}_${dia}.log
    
    local inv_dir="../../inventario/${dominio}"
    mkdir -p "$inv_dir"
    mv ./*00:00 "$inv_dir/"
    
    echo -e "${COLOR_SUCCESS}---> Día $etiqueta_dia procesado exitosamente.${COLOR_RESET}"
    cd ..
}


# =============================================================================
# OTRAS FUNCIONES (sin cambios)
# =============================================================================
check_domain() {
    echo -e "${COLOR_INFO}      ___  _ ___ _____ ___   ${COLOR_RESET}"
    echo -e "${COLOR_INFO}     |   \\(_) __|_   _| __|  ${COLOR_RESET}"
    echo -e "${COLOR_INFO}     | |) | | _|  | | | _|   ${COLOR_RESET}"
    echo -e "${COLOR_INFO}     |___/|_|___| |_| |___|  ${COLOR_RESET}"
    echo
    local domain_path="01_datos/$dominio"
    if [ -d "$domain_path" ]; then
        echo -e "${COLOR_SUCCESS}---> Dominio '$dominio' encontrado. Continuando...${COLOR_RESET}"
    else
        echo -e "${COLOR_ERROR} ERROR: El dominio '$dominio' no existe en '01_datos/'. ${COLOR_RESET}"
        exit 1
    fi
}

make_tmpdir() {
    local dir_name="$1"
    if [ -z "$dir_name" ]; then
        echo -e "${COLOR_ERROR} ERROR (make_tmpdir): No se proporcionó un nombre de directorio. ${COLOR_RESET}"
        exit 1
    fi
    if [ -d "$dir_name" ]; then
        if [ "$HacerArea" -eq 1 ]; then
            echo -e "${COLOR_WARNING}Directorio '$dir_name' existe. Eliminando y recreando...${COLOR_RESET}"
            rm -rf "$dir_name"
            mkdir -p "$dir_name"
        fi
    else
        echo "Creando directorio '$dir_name'..."
        mkdir -p "$dir_name"
    fi
    cd "$dir_name"
    echo "Cambiado al directorio de trabajo: $(pwd)"
    ln -fs ../01_datos/"$dominio" .
    ln -fs ../01_datos/chem .
    ln -fs ../01_datos/time .
    ln -fs ../01_datos/emis .
    ln -fs ../bin .
}

crea_anio_csv() {
    local anio mes dia fecha
    if [ $# -eq 3 ]; then
        anio="$1"; mes="$2"; dia="$3"
        fecha="$anio/$mes/$dia"
        # Validar si la fecha es correcta.
        # Compatibilidad: date_compat encapsula la diferencia entre
        # `date -d` (GNU) y `date -j -f` (BSD/macOS) en una sola llamada.
        if ! date_compat valid "$fecha"; then
            echo -e "${COLOR_ERROR} ERROR: La fecha '$fecha' no es válida. ${COLOR_RESET}"; exit 1
        fi
        
        # Obtener el día de la semana (0=Domingo, 1=Lunes, ..., 6=Sábado)
        local dow=$(date_compat parse "$fecha" "+%w")
        
        # Si es Domingo (0), cambiar su valor a 7 como se requiere.
        if [ "$dow" -eq 0 ]; then
            dow=6
        fi
        
        # Obtener el resto de los componentes de la fecha
        local mes_csv=$(date_compat parse "$fecha" "+%m")
        local dia_csv=$(date_compat parse "$fecha" "+%d")
        local nomdia_csv=$(date_compat parse "$fecha" "+%a")

        # Reconstruir la línea con el día de la semana corregido (formato: mes,dia,n_dia_semana,nomdia_semana)
        local linea="${mes_csv},${dia_csv},${dow},${nomdia_csv}"
        
        local csv_file="anio${anio}.csv"
        echo "mes,dia,n_dia_semana,nomdia_semana" > "$csv_file"
        echo "$linea" >> "$csv_file"
        mv "$csv_file" ../time/
    else
        echo -e "${COLOR_ERROR} USO INCORRECTO: crea_anio_csv AAAA MM DD ${COLOR_RESET}"; exit 1
    fi
}

hace_namelist() {
    cat > namelist_emis.nml <<- End_Of_File
	!
	!   Definicion de variables para calculo del Inventario
	!
	&region_nml
	zona ="$dominio"
	/
	&fecha_nml
	idia=$dia
	month=$mes
	anio=$nyear
	periodo=$nfile
	/
	&verano_nml
	lsummer = .false.
	/
	&chem_nml
	mecha='$MECHA'
	model=$AQM_SELECT
	/
End_Of_File
}

limpiar_archivos_viejos() {
    # Compatibilidad: se calcula la fecha de "ayer" con date_compat en vez
    # de `date -d "-1 days" ...`, por lo que funciona igual en Linux y macOS.
    local ayer=$(date_compat offset -1 +%d)
    local ames=$(date_compat offset -1 +%m)
    local ayear=$(date_compat offset -1 +%Y)
    local fayer1="${DOMAINS}/interpolaD01/wrfchemi_d01_${MECHA}_${dominio:0:8}_${ayear}-${ames}-${ayer}_00:00:00"
    local fayer2="${DOMAINS}/interpolaD01/wrfchemi_d01_${MECHA}_${dominio:0:8}_${ayear}-${ames}-${ayer}_12:00:00"
    if [ -f "$fayer1" ] || [ -f "$fayer2" ]; then
        echo "Borrando archivos de ayer: ${ayear}-${ames}-${ayer}"
        rm -f "$fayer1" "$fayer2"
    else
       echo "No se encontraron archivos en ${DOMAINS}/interpolaD01"
    fi
}
