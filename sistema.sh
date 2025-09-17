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

logout(){
    clear
    if [[ "$user" -ne "  " ]]; then
        user="  "
        echo "Sesión cerrada exitosamente"
    else
        echo "Debe ingresar sesión previamente"
    fi
}

usuario(){
    opcion=0
    echo "########################################################################"
    echo "#                           MENÚ USUARIO                               #"
    echo "########################################################################"
    echo 
    
    while [[ "$opcion" -ne 5 ]]; do
        echo -e "Seleccionar una opción: \n1) Crear usuario \n2) Cambiar contraseña \n3) Login \n4) Logout \n5) Menú"
        read -r opcion

        if [[ "$opcion" -eq 1 ]]; then
            clear
            crearUser
        elif [[ "$opcion" -eq 2 ]]; then
            clear
            cambiarPass
        elif [[ "$opcion" -eq 3 ]]; then
            clear
            login
        elif [[ "$opcion" -eq 4 ]]; then
            clear
            logout
        elif [[ "$opcion" -eq 5 ]]; then
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


########################################################################
#                          MENU PRINCIPAL                              #
########################################################################


validarLogin() {
    if [[ $user -ne "  " ]]; then
        return 0  # éxito
    else
        return 1  # error
    fi
}


while [[ "$opcion" -ne 6 ]]; do
echo -e "\e[1;32m########################################################################"
echo "#                          MENU PRINCIPAL                              #"
echo -e "########################################################################\e[0m"
echo 
    opcion=0
    echo -e "Seleccionar una opción: \n\n\e[1;32m 1) \e[0m Usuario 
                                     \n\e[1;32m 2) \e[0m Ingresar producto 
                                     \n\e[1;32m 3) \e[0m Vender producto 
                                     \n\e[1;32m 4) \e[0m Filtro de productos 
                                     \n\e[1;32m 5) \e[0m Crear reporte de pinturas 
                                     \n\e[1;32m 6) \e[0m Salir"
    read -r opcion

    if [[ "$opcion" -eq 1 ]]; then
        clear
        usuario
    elif [[ "$opcion" -eq 6 ]]; then
        clear
        echo "Sesión finalizada"
    elif validarLogin ; then
            if [[ "$opcion" -eq 2 ]]; then
                clear
                ingProd
            elif [[ "$opcion" -eq 3 ]]; then
                clear
                vendProd
            elif [[ "$opcion" -eq 4 ]]; then
                clear
                filterProd
            elif [[ "$opcion" -eq 5 ]]; then
                clear
                crearRepo
            fi
    else
        clear
        if validarLogin ; then
            echo "Opcion incorrecta, seleccione un valor válido"
        else
            echo -e "\e[1;31m--------------------------"
            echo "|   usuario incorrecto   |"
            echo -e "--------------------------\e[0m"
        fi
    fi
done
