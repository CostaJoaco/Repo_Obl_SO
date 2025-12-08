
// Lectores-Escritores: Pasajeros (lectores) y Oficinistas (escritores)
// Versión mínima: sólo imprime mensajes para validar concurrencia.
// Compilar: gcc -pthread -o aeropuerto Posix.c

#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#define CANT_PASAJEROS        100
#define CANT_OFICINISTAS      5
#define MODS_POR_OFICINISTA   3  // cada oficinista "escribe" 3 veces

// Semáforos y estado compartido
sem_t wrt;        // controla acceso exclusivo de escritores
sem_t mutex;      // protege la variable contadora de lectores
int lectores = 0; // cantidad de lectores activos

// --- Hilos ---

// Pasajero = Lector
void* pasajero(void* arg) {
    int id = *(int*)arg;

    // Entrada de lector
    sem_wait(&mutex);
    lectores++;
    if (lectores == 1) {
        // primer lector bloquea a los escritores
        sem_wait(&wrt);
    }
    sem_post(&mutex);

    // Lectura concurrente (solo mensaje)
    printf("Pasajero %d está viendo el cartel\n", id);

    // Salida de lector
    sem_wait(&mutex);
    lectores--;
    if (lectores == 0) {
        // último lector libera a los escritores
        sem_post(&wrt);
    }
    sem_post(&mutex);

    return NULL;
}

// Oficinista = Escritor
void* oficinista(void* arg) {
    int id = *(int*)arg;

    // Cada oficinista realiza varias "escrituras" (solo mensaje)
    for (int m = 1; m <= MODS_POR_OFICINISTA; m++) {
        sem_wait(&wrt);  // sección crítica exclusiva
        printf("Oficinista %d está escribiendo (mod %d)\n", id, m);
        sem_post(&wrt);
        // Breve pausa para dar variabilidad al scheduler (opcional)
        // usleep((rand() % 5 + 1) * 1000); // descomentar si querés más desorden
    }
    return NULL;
}

int main(void) {
    srand((unsigned)time(NULL));

    // Inicialización de semáforos
    sem_init(&wrt,   0, 1);
    sem_init(&mutex, 0, 1);

    pthread_t h_pasajeros[CANT_PASAJEROS];
    pthread_t h_oficinistas[CANT_OFICINISTAS];
    int id_pasajeros[CANT_PASAJEROS];
    int id_oficinistas[CANT_OFICINISTAS];

    // Creación de hilos (sin aleatorizar): 100 pasajeros + 5 oficinistas
    for (int i = 0; i < CANT_PASAJEROS; i++) {
        id_pasajeros[i] = i + 1;
        pthread_create(&h_pasajeros[i], NULL, pasajero, &id_pasajeros[i]);
    }
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        id_oficinistas[j] = j + 1;
        pthread_create(&h_oficinistas[j], NULL, oficinista, &id_oficinistas[j]);
    }

    // Espera a que terminen todos
    for (int i = 0; i < CANT_PASAJEROS; i++) {
        pthread_join(h_pasajeros[i], NULL);
    }
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        pthread_join(h_oficinistas[j], NULL);
    }

    // Limpieza
    sem_destroy(&wrt);
    sem_destroy(&mutex);

    printf("Fin: %d pasajeros y %d oficinistas (cada uno %d veces).\n",
           CANT_PASAJEROS, CANT_OFICINISTAS, MODS_POR_OFICINISTA);
    return 0;
}
