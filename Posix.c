#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <semaphore.h>
#include <unistd.h>

#define CANT_LECTORES 100
#define CANT_ESCRITORES 5
#define SIZE_CARTEL 20

// struct vuelo, ref de IA prompt:"para crear un struct en c es similar a c++? como lo creo?"
typedef struct {
    int hora;
    char destination[20];
    char flight[6];
    int gate;
    char remarks[10];
} vuelo;

// Variables compartidas
vuelo* cartel[SIZE_CARTEL];
char* tipos[] = {"On Time", "Delayed", "Cancelled"};

// Sincronizacion
sem_t sem_escritor;
pthread_mutex_t mutex_lectores;
int lectores_activos = 0;

// Funcion para agregar vuelos
void* agregarVuelo(vuelo** cartel, int cant) {
    for (int i = 0; i < cant; i++) {
        vuelo* v = (vuelo*)malloc(sizeof(vuelo));
        v->hora = (rand() % 24) * 100 + (rand() % 60);
        sprintf(v->destination, "City%d", rand() % 100);
        sprintf(v->flight, "FL%03d", rand() % 1000);
        v->gate = rand() % 20 + 1;
        strcpy(v->remarks, tipos[i % 3]);
        cartel[i] = v;
    }
    return NULL;
}

// Funcion para modificar vuelos
void* modificarVuelo(vuelo** cartel, int cant) {
    for (int i = 0; i < cant; i++) {
        int index = rand() % SIZE_CARTEL;
        cartel[index]->hora = (rand() % 24) * 100 + (rand() % 60);
        cartel[index]->gate = rand() % 20 + 1;
        strcpy(cartel[index]->remarks, tipos[rand() % 3]);
    }
    return NULL;
}

// Funcion lector
void* lector(void* x) {
    int id = *(int*)x;

    pthread_mutex_lock(&mutex_lectores);
    lectores_activos++;
    if (lectores_activos == 1) {
        sem_wait(&sem_escritor);
    }
    pthread_mutex_unlock(&mutex_lectores);

    printf("Pasajero %d esta mirando el cartel\n", id);
    sleep(1); // Simula lectura

    pthread_mutex_lock(&mutex_lectores);
    lectores_activos--;
    if (lectores_activos == 0) {
        sem_post(&sem_escritor);
    }
    pthread_mutex_unlock(&mutex_lectores);

    return NULL;
}

// Funcion escritor
void* escritor(void* x) {
    int id = *(int*)x;

    sem_wait(&sem_escritor);
    printf("Oficinista %d esta modificando el cartel\n", id);
    modificarVuelo(cartel, rand() % 5 + 1);
    sleep(1); // Simula escritura
    sem_post(&sem_escritor);

    return NULL;
}

int main() {
    srand(time(NULL));
    agregarVuelo(cartel, SIZE_CARTEL);

    pthread_t lectores[CANT_LECTORES], escritores[CANT_ESCRITORES];
    int id_lectores[CANT_LECTORES], id_escritores[CANT_ESCRITORES];

    pthread_mutex_init(&mutex_lectores, NULL);
    sem_init(&sem_escritor, 0, 1);

    for (int i = 0; i < CANT_LECTORES; i++) {
        id_lectores[i] = i;
        pthread_create(&lectores[i], NULL, lector, &id_lectores[i]);
    }

    for (int i = 0; i < CANT_ESCRITORES; i++) {
        id_escritores[i] = i;
        pthread_create(&escritores[i], NULL, escritor, &id_escritores[i]);
    }

    for (int i = 0; i < CANT_LECTORES; i++) {
        pthread_join(lectores[i], NULL);
    }

    for (int i = 0; i < CANT_ESCRITORES; i++) {
        pthread_join(escritores[i], NULL);
    }

    return 0;
}

//! En la Bibliografia agregar:
//! Link de consulta por escritores[i] para darles nombre id
//! NOTA: se uso #define, en vez de una variable para no generar errores con numeros fijos