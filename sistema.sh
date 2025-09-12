#!/bin/bash

########################################################################
#                            ZONA USUARIO                              #
########################################################################

#pre:
#post: retorna 0 si el usuario existe, y 1 en caso contrario
existeUsuario(){
    grep -q "^$1\ " users.txt
}

crearUser(){
    opcion=0
    echo -e "Ingresar un nombre de usuario: "
    read -r usuario
    existeUsuario "$usuario"
    # $? toma el ultimo valor obtenido
    existe=$?

    while [[ existe -eq 0 ]]; do
        echo -e "Usuario inválido, ingresar un nombre de usuario distinito"
        read -r usuario
        existeUsuario "$usuario"
        existe=$?
    done

    echo -e "Ingresar una contraseña: "
    read -r pass

    while [[ "$pass" =~ [\ ] ]]; do
        echo -e "Contraseña inválida, no puede contener espacios"
        read -r pass
    done

    echo "$usuario $pass" >> users.txt
}

usuario(){
    opcion=0
    while [[ $opcion -ne 5 ]]; do
        echo -e "Seleccionar una opciÓn: \n1) Crear usuario \n2) Cambiar contraseña \n3) Login \n4) Logout \n5) Salir"
        read -r opcion

        if [[ $opcion -eq 1 ]]; then
            crearUser
        elif [[ $opcion -eq 2 ]]; then
            cambiarPass
        elif [[ $opcion -eq 3 ]]; then
            login
        elif [[ $opcion -eq 4 ]]; then
            logout
        elif [[ $opcion -eq 5 ]]; then
            echo "Sesión finalizada"
        else
            echo "Opcion incorrecta, seleccione un valor válido"
        fi
    done
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

while [[ $opcion -ne 6 ]]; do
    opcion=0
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
