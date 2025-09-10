#!/bin/bash

usuario(){
    echo "1"
}

ingProd(){
    echo "2"
}

vendProd(){
    echo "3"
}

filterProd(){
    echo "4"
}

crearRepo(){
    echo "5"
}


opcion=0
while [[ $opcion -ne 6 ]]; do

    echo -e "Seleccionar una opciÓn: \n1) Usuario \n2) Ingresar producto \n3) Vender producto \n4) Filtro de productos \n5) Crear reporte de pinturas \n6) Salir"
    read -r opcion

    if [[ $opcion -eq 1 ]]; then
        usuario
    elif [[ $opcion -eq 2 ]]; then
        ingProd
    elif [[ $opcion -eq 3 ]]; then
        vendProd
    elif [[ $opcion -eq 4 ]]; then
        filterProd
    elif [[ $opcion -eq 5 ]]; then
        crearRepo
    elif [[ $opcion -eq 6 ]]; then
        echo "Sesión finalizada"
    else
        echo "Opcion incorrecta, seleccione un valor válido"
    fi
done
