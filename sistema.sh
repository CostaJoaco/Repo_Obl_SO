#!/bin/bash

user="  "

########################################################################
#                            ZONA USUARIO                              #
########################################################################

#pre:
#post: retorna 0 si el usuario existe, y 1 en caso contrario
existeUsuario(){
    grep -q "^$1 " users.txt
}

passCorrecta(){
    grep -q "^$1 $2" users.txt
}

crearUser(){
    opcion=0
    echo -e "Ingresar un nombre de usuario: "
    read -r usuario
    existeUsuario "$usuario"
    # $? toma el ultimo valor obtenido
    existe=$?

    while [[ "$usuario" =~ [\ ] ]] || [[ $existe -eq 0 ]]; do
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
    clear
    echo "Usuario agregado exitosamente"
}

cambiarPass(){
    echo -e "¿Desea cambiar una contraseña? \n1) Si \nOtro) No"
    read -r quiere
    while [[ "$quiere" -eq 1 ]]; do
        login
        ingreso=$?

        if [[ "$ingreso" -eq 0 ]]; then
            echo -e "Ingresar la nueva contraseña: "
            read -r nueva

            sed -i "/^$user/c\\$user $nueva" users.txt
        fi

        echo -e "¿Desea cambiar una contraseña? \n1) Si \nOtro) No"
        read -r quiere
    done
    clear
}

login(){
    echo -e "Ingrese nombre de usuario: "
    read -r usuario

    existeUsuario $usuario
    existe=$?

    if [[ "$existe" -eq 0 ]]; then
        echo -e "Ingrese la constraseña: "
        read -r pass

        passCorrecta $usuario $pass
        valido=$?

        if [[ "$valido" -eq 0 ]]; then
            user=$usuario
            echo "Ingreso exitoso"
            return 0
        else 
            clear
            echo "Contraseña incorrecta"
            return 1
        fi
    else
        clear
        echo "Usuario no encontrado"
        return 1
    fi
}

usuario(){
    opcion=0
    while [[ $opcion -ne 5 ]]; do
        echo -e "Seleccionar una opción: \n1) Crear usuario \n2) Cambiar contraseña \n3) Login \n4) Logout \n5) Menú"
        read -r opcion

        if [[ $opcion -eq 1 ]]; then
            clear
            crearUser
        elif [[ $opcion -eq 2 ]]; then
            clear
            cambiarPass
        elif [[ $opcion -eq 3 ]]; then
            clear
            login
        elif [[ $opcion -eq 4 ]]; then
            clear
            logout
        elif [[ $opcion -eq 5 ]]; then
            echo "Voler al menú"
        else
            echo "Opción incorrecta, seleccione un valor válido"
        fi
    done
    clear
}

########################################################################
#                         FIN ZONA USUARIO                             #
########################################################################

ingProd(){
    echo "########################################################################"
    echo "#                              PRODUCTO                                #"
    echo "########################################################################"
    echo "Ingrese El Tipo de producto: "
    read -r tipo
    echo "Ingrese El Modelo del producto: "
    read -r modelo
    echo "Ingrese La Descripcion del producto: "
    read -r desc
    echo "Ingrese La Cantidad del producto: "
    read -r cant
    echo "Ingrese El Precio del producto: "
    read -r precio
    codigo=$(echo "$tipo:0:3" | tr '[:lower:]' '[:upper:]') # Extraer las 3 primeras letras de tipo y convertirlas a mayúsculas (Codigo consegudido con ayuda de IA)
    echo "$codigo - $tipo - $modelo - $desc - $cant -$ $precio" >> productos.csv
    echo "Producto ingresado exitosamente"
}

vendProd(){
    echo "########################################################################"
    echo "#                               VENTA                                 #"
    echo "########################################################################"
    iter=1
    while IFS= read -r line; do
        tipo=$(echo "$line" | awk -F'-' '{print $2}' | xargs) # xargs funciona como trim en java y awk extrae una columna del csv
        modelo=$(echo "$line" | awk -F'-' '{print $3}' | xargs)
        precio=$(echo "$line" | awk -F'-' '{print $6}' | xargs)
        echo "$iter) $tipo - $modelo - $precio"
        ((iter++))
    done < productos.csv
}



filterProd(){
    echo "4"
}

crearRepo(){
    echo "5"
}


########################################################################
#                          MENU PRINCIPAL                              #
########################################################################


while [[ $opcion -ne 6 ]]; do
    opcion=0
    echo -e "Seleccionar una opciÓn: \n1) Usuario \n2) Ingresar producto \n3) Vender producto \n4) Filtro de productos \n5) Crear reporte de pinturas \n6) Salir"
    read -r opcion

    if [[ $opcion -eq 1 ]]; then
        clear
        usuario
    elif [[ $opcion -eq 2 ]]; then
        clear
        ingProd
    elif [[ $opcion -eq 3 ]]; then
        clear
        vendProd
    elif [[ $opcion -eq 4 ]]; then
        clear
        filterProd
    elif [[ $opcion -eq 5 ]]; then
        clear
        crearRepo
    elif [[ $opcion -eq 6 ]]; then
        clear
        echo "Sesión finalizada"
    else
        clear
        echo "Opcion incorrecta, seleccione un valor válido"
    fi
done
