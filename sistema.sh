#!/bin/bash

user=" 1233 "

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
    echo -e "\e[1;32m########################################################################"
    echo "#                            MENU USUARIO                              #"
    echo -e "########################################################################\e[0m"
    echo 
    
    while [[ "$opcion" -ne 5 ]]; do
        echo -e "Seleccionar una opción: \n\n\e[1;32m 1) \e[0m Crear usuario 
                                         \n\e[1;32m 2) \e[0m Cambiar contraseña 
                                         \n\e[1;32m 3) \e[0m Login 
                                         \n\e[1;32m 4) \e[0m Logout 
                                         \n\e[1;32m 5) \e[0m Menú"
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
    echo -e "\e[1;32m########################################################################"
    echo "#                              PRODUCTO                                #"
    echo -e "########################################################################\e[0m"
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
   
    while IFS= read -r line; do
        tipo2=$("$line" | awk -F'-' '{print $2}' | xargs) 
        modelo2=$("$line" | awk -F'-' '{print $3}' | xargs)
        precio2=$("$line" | awk -F'-' '{print $6}' | xargs)
        if [[ "$tipo2" == "$tipo" && "$modelo2" == "$modelo" ]]; then
            echo "El producto ya existe, se actualizo la cantidad"
            # Codigo de actualizacion de cantidad
            return
        fi
        ((iter++))
    done < productos.csv
    echo "$codigo - $tipo - $modelo - $desc - $cant -$ $precio" 
    funcIng ( $codigo, $tipo, $modelo, $desc, $cant, $precio)
    echo "Producto ingresado exitosamente"
}
funcIng($codigo, $tipo, $modelo, $desc, $cant, $precio){
    echo " $codigo , $tipo , $modelo , $desc , $cant , $precio " >> productos.csv

}

vendProd(){
    echo -e "\e[1;32m########################################################################"
    echo "#                               VENTA                                 #"
    echo -e "########################################################################\e[0m"
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
    echo -e "\e[1;32m########################################################################"
    echo "#                          BUSCAR PRODUCTO                              #"
    echo -e "########################################################################\e[0m"
    echo

    echo -e "Seleccionar un filtro: \n\n\e[1;32m 0) \e[0m Sin filtro
                                    \n\e[1;32m 1) \e[0m Base 
                                    \n\e[1;32m 2) \e[0m Layer 
                                    \n\e[1;32m 3) \e[0m Shade
                                    \n\e[1;32m 4) \e[0m Dry 
                                    \n\e[1;32m 5) \e[0m Contrast
                                    \n\e[1;32m 6) \e[0m Technical
                                    \n\e[1;32m 7) \e[0m Texture
                                    \n\e[1;32m 8) \e[0m Mediums"
    read -r opcion
    
    filtro=''
    if [[ "$opcion" -eq 1 ]]; then
        filtro="Base"
    elif [[ "$opcion" -eq 2 ]]; then
        filtro="Layer"
    elif [[ "$opcion" -eq 3 ]]; then
        filtro="Shade"
    elif [[ "$opcion" -eq 4 ]]; then
        filtro="Dry"
    elif [[ "$opcion" -eq 5 ]]; then
        filtro="Contrast"
    elif [[ "$opcion" -eq 6 ]]; then
        filtro="Technical"
    elif [[ "$opcion" -eq 7 ]]; then
        filtro="Texture"
    elif [[ "$opcion" -eq 8 ]]; then
        filtro="Mediums"
    fi

    if [[ "$filtro" -eq '' ]]; then
        while IFS= read -r line; do
            tipo=$(echo "$line" | awk -F'-' '{print $2}' | xargs) # xargs funciona como trim en java y awk extrae una columna del csv
            modelo=$(echo "$line" | awk -F'-' '{print $3}' | xargs)
            precio=$(echo "$line" | awk -F'-' '{print $6}' | xargs)
            echo "$iter) $tipo - $modelo - $precio"
            ((iter++))
        done < productos.csv
    else
        while IFS= read -r line; do
            tipo=$(echo "$line" | awk -F'-' '{print $2}' | xargs) # xargs funciona como trim en java y awk extrae una columna del csv
            if [[ "$filtro" -eq "$tipo" ]]; then
                modelo=$(echo "$line" | awk -F'-' '{print $3}' | xargs)
                precio=$(echo "$line" | awk -F'-' '{print $6}' | xargs)
                echo "$iter) $tipo - $modelo - $precio"
                ((iter++))
            fi
        done < productos.csv
    fi


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

menuPrincipal(){
    
    clear
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
            echo -e "\e[1;31m--------------------------"
                echo "|   sesion finalizada    |"
                echo -e "--------------------------\e[0m"
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
}

menuPrincipal;