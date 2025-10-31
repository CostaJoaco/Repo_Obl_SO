#!/bin/bash

trap 'rm -f session.tmp' EXIT
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
    echo -e "\e[1;32m---------------------"
    echo "|   Crear Usuario   |"
    echo -e "---------------------\e[0m"
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
    echo -e "\e[1;32m----------------------"
               echo "|   Iniciar sesion   |"
            echo -e "----------------------\e[0m"
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
            echo "$usuario" > session.tmp
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
    if [[ -s session.tmp ]]; then   # hay sesión activa
        rm -f session.tmp .session.tmp
        echo "Sesión cerrada exitosamente"
    else
        echo "Debe ingresar sesión previamente"
    fi
}

usuario(){
    opcion=0
    
    while [[ "$opcion" -ne 5 ]]; do
        clear
        echo -e "\e[1;32m########################################################################"
        echo "#                            MENU USUARIO                              #"
        echo -e "########################################################################\e[0m"
        echo 
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
            sleep 2
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

ingProd() {
    echo -e "\e[1;32m########################################################################"
    echo "#                              PRODUCTO                                #"
    echo -e "########################################################################\e[0m"

    echo "Ingrese el tipo de producto: "
    read -r tipo
    echo "Ingrese el modelo del producto: "
    read -r modelo
    echo "Ingrese la descripcion del producto: "
    read -r desc

    # Validacion de cantidad
    cant=0  
    while [[ "$cant" -le 0 ]]; do
        echo "Ingrese la cantidad del producto: "
        read -r cant
        if [[ "$cant" -le 0 ]]; then
            echo -e "\e[1;31mCantidad invalida, debe ser mayor a 0\e[0m"
        fi
    done

    
    precio=0
    while [[ "$precio" -le 0 ]]; do
        echo "Ingrese El Precio del producto: "
        read -r precio
        if [[ "$precio" -le 0 ]]; then
            echo -e "\e[1;31mPrecio invalido, debe ser mayor a 0\e[0m"
        fi
    done

    codigo=$(echo "${tipo:0:3}" | tr '[:lower:]' '[:upper:]' | xargs) # Extraer las 3 primeras letras de tipo y convertirlas a mayúsculas (Codigo consegudido con ayuda de IA)
   
    existe=0
    while IFS= read -r line; do
    tipo2=$(echo "$line"   | awk -F',' '{print $2}' | xargs)
    modelo2=$(echo "$line" | awk -F',' '{print $3}' | xargs)
    if [[ "$tipo2" == "$tipo" && "$modelo2" == "$modelo" ]]; then
        echo "El producto ya existe, se actualizo la cantidad"
        # TODO: actualizar cantidad
        existe=1
        break
    fi
    done < productos.csv

    if [[ "$existe" -eq 0 ]]; then
        echo -e "\e[1;32m------------------------------------"
        echo "|   Producto ingresado con exito   |"
        echo -e "------------------------------------\e[0m"
        echo "$codigo - $tipo - $modelo - $desc - $cant - \$$precio"
        funcIng "$codigo" "$tipo" "$modelo" "$desc" "$cant" "$precio"
    fi
}

funcIng (){
    echo "$1,$2,$3,$4,$5,$6" >> productos.csv
}

vendProd(){
    echo -e "\e[1;32m########################################################################"
    echo "#                                VENTA                                 #"
    echo -e "########################################################################\e[0m"
    iter=1
    while IFS= read -r line; do
        tipo=$(echo "$line" | awk -F',' '{print $2}' | xargs) # xargs funciona como trim en java y awk extrae una columna del csv
        modelo=$(echo "$line" | awk -F',' '{print $3}' | xargs)
        precio=$(echo "$line" | awk -F',' '{print $6}' | xargs)
        echo "$iter) $tipo - $modelo - \$${precio}"
        ((iter++))
    done < productos.csv
    echo -e "seleccione el numero de producto que desea comprar"
    read -r num
    echo -e "seleccione la cantidad a comprar"
    read -r cant
    linea=$(sed -n "${num}p" productos.csv)
    IFS=',' read -r codigo tipo modelo descripcion cantidad precio <<< "$linea"
    while [[ "$cant" -gt "$cantidad" || "$cant" -le 0 ]]; do
        clear
        echo -e "\e[1;31m-----------------------------------------------------------"
        echo "|   Cantidad invalida, La cantidad debe ser menor a $cantidad    |"
        echo -e "-----------------------------------------------------------\e[0m"
        echo
        echo "Inserte nuevamente:"
        read -r cant
    done
    
    col=5 
    valor=$((cantidad - cant)) 
    # linea hecha con IA
    awk -v r="$num" -v c="$col" -v v="$valor" 'BEGIN{FS=OFS=","} NR==r{$c=v}1' \
    productos.csv > productos.csv.tmp && mv productos.csv.tmp productos.csv

    # TODO: cuando cant es mayor a stock y restar stock
    clear
    echo -e "\e[1;32m----------------------------------"
    echo "|   Compra Realizada con exito   |"
    echo -e "----------------------------------\e[0m"
    total=$((cant * precio))   
    echo -e "\e[1;32m Tipo: $tipo \e[0m"
    echo -e "\e[1;32m Modelo: $modelo \e[0m"
    echo -e "\e[1;32m Cantidad: $cant \e[0m"
    echo -e "\e[1;32m Precio: $precio \e[0m"
    echo -e "\e[1;32m Total: $total \e[0m"
    sleep 1
    

}

filterProd() {
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

    iter=1
    if [[ "$filtro" == '' ]]; then
        while IFS= read -r line; do
            tipo=$(echo "$line" | awk -F',' '{print $2}' | xargs)
            modelo=$(echo "$line" | awk -F',' '{print $3}' | xargs)
            precio=$(echo "$line" | awk -F',' '{print $6}' | xargs)
            echo "$iter) $tipo - $modelo - \$${precio}"
            ((iter++))
        done < productos.csv
    else
        while IFS= read -r line; do
            tipo=$(echo "$line" | awk -F',' '{print $2}' | xargs)
            if [[ "$filtro" == "$tipo" ]]; then
                modelo=$(echo "$line" | awk -F',' '{print $3}' | xargs)
                precio=$(echo "$line" | awk -F',' '{print $6}' | xargs)
                echo "$iter) $tipo - $modelo - \$${precio}"
                ((iter++))
            fi
        done < productos.csv
    fi
}

# TODO: Implementar la funcion crearRepo
crearRepo(){
    echo "5"
}


########################################################################
#                          MENU PRINCIPAL                              #
########################################################################


validarLogin() {
    if [[ -s session.tmp ]]; then
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