#include <pthread.h> 
#include <stdio.h> 
#include <stdlib.h> 
#include <time.h> 
#include <semaphore.h>

//Creo una variable compartida para leer y modificar del tipo "vuelo"
struct vuelo {
    time_t hora
    char destination[20]
    char flight[6]
    int gate
    char remarks[10]
};

void* agregarVuelo(vuelo** cartel, int cant , int size, char* tipos[]){
    // ejemplo de vuelo(hora=09:45, destination="New York", flight="AA101", gate=5, remarks="On Time")
    for (int i = 0; i < cant ; i++) {
        vuelo* v = (vuelo*)malloc(sizeof(vuelo));
        v->hora = rand() % 24 * 100 + rand() % 60; // hora random entre 00:00 y 23:59
        sprintf(v->destination, "City%d", rand() % 100); // destino random
        sprintf(v->flight, "FL%03d", rand() % 1000); // vuelo random
        v->gate = rand() % 20 + 1; // gate random entre 1 y 20
        sprintf(v->remarks, tipos[i % 3] ); // comentarios fijos por simplicidad
        cartel[i] = v;
    }
}
void* modificarVuelo(vuelo** cartel, int cant, int size, char* tipos[]){
    // modifico un vuelo random del cartel
   for(int i =0; i < cant ; i++){
        int index = rand() % size;
        cartel[index]->hora = rand() % 24 * 100 + rand() % 60; // nueva hora random
        cartel[index]->gate = rand() % 20 + 1;
        int atraso = rand() % 3;
        cartel[index]->remarks = tipos[atraso];
   }
}

void* lector (void * x){
    int id = (int)x;
    printf("pasajero %id esta mirando el cartel");
}

void* escritor (void * x){ 
    int id = (int)x;
    printf(" oficinista %id esta modificando el cartel");
    modificarVuelo(cartel, cant);
}

int main(){
    pthread_t escritores[5];
    pthread_t lectores[100];
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    
    for (int i = 0; i < 100; i++) {
        lectores[i] = i;

    }
    for (int i = 0; i < 5; i++) {
        escritores[i] = i;
    }
    char* tipos = {"On Time", "Delayed", "Cancelled"};
    vuelo** cartel = new vuelo*[20];
    agregarVuelo(cartel, 20, 20, tipos);
    for (int i = 0; i < 100; i++) {
        pthread_create(&lectores[i], &attr, lector, NULL);
        pthread_join(lectores[i], NULL);
    }
    for (int i = 0; i < 5; i++) {
        pthread_create(&escritores[i], &attr, escritor, NULL);
        pthread_join(escritores[i], NULL);
    }
}