
// Lectores-Escritores justo: prioridad a escritores con "torniquete".
// Compilar: gcc -pthread -o po po.c

#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>

#define CANT_PASAJEROS        100
#define CANT_OFICINISTAS      5
#define MODS_POR_OFICINISTA   3

// Semáforos y estado
sem_t torniquete;   // cierra el paso de nuevos lectores cuando hay escritores
sem_t roomEmpty;    // exclusión total del recurso compartido (lectores/escritores)
sem_t mutex;        // protege el contador 'lectores'
int lectores = 0;

// --- Hilos ---

// Pasajero = Lector (justo, bloqueado por torniquete si hay escritor esperando)
void* pasajero(void* arg) {
    int id = *(int*)arg;

    // Paso por el torniquete: si un escritor lo tomó, me bloqueo
    sem_wait(&torniquete);
    sem_post(&torniquete);

    // Entrada de lector (patrón clásico)
    sem_wait(&mutex);
    lectores++;
    if (lectores == 1) {
        // primer lector ocupa la sala para impedir escritores
        sem_wait(&roomEmpty);
    }
    sem_post(&mutex);

    // Lectura concurrente (solo mensaje)
    printf("Pasajero %d está viendo el cartel\n", id);

    // Salida de lector
    sem_wait(&mutex);
    lectores--;
    if (lectores == 0) {
        // último lector libera la sala
        sem_post(&roomEmpty);
    }
    sem_post(&mutex);

    return NULL;
}

// Oficinista = Escritor (toma el torniquete para impedir nuevos lectores)
void* oficinista(void* arg) {
    int id = *(int*)arg;
    for (int m = 1; m <= MODS_POR_OFICINISTA; m++) {
        // Cierro el torniquete: nadie nuevo entra, se vacía la sala de lectores
        sem_wait(&torniquete);
        // Espero a que la sala quede vacía
        sem_wait(&roomEmpty);

        // Escritura exclusiva (solo mensaje)
        printf("Oficinista %d está escribiendo (mod %d)\n", id, m);

        // Libero la sala y abro el torniquete
        sem_post(&roomEmpty);
        sem_post(&torniquete);
    }
    return NULL;
}

int main(void) {
    // Inicialización
    sem_init(&torniquete, 0, 1);  // abierto inicialmente
    sem_init(&roomEmpty, 0, 1);   // sala vacía
    sem_init(&mutex, 0, 1);       // protege 'lectores'

    pthread_t h_pasajeros[CANT_PASAJEROS];
    pthread_t h_oficinistas[CANT_OFICINISTAS];
    int id_pasajeros[CANT_PASAJEROS];
    int id_oficinistas[CANT_OFICINISTAS];

    // Crear 100 pasajeros
    for (int i = 0; i < CANT_PASAJEROS; i++) {
        id_pasajeros[i] = i + 1;
        pthread_create(&h_pasajeros[i], NULL, pasajero, &id_pasajeros[i]);
    }
    // Crear 5 oficinistas
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        id_oficinistas[j] = j + 1;
        pthread_create(&h_oficinistas[j], NULL, oficinista, &id_oficinistas[j]);
    }

    // Esperar a que terminen
    for (int i = 0; i < CANT_PASAJEROS; i++) pthread_join(h_pasajeros[i], NULL);
    for (int j = 0; j < CANT_OFICINISTAS; j++) pthread_join(h_oficinistas[j], NULL);

    // Limpieza
    sem_destroy(&torniquete);
    sem_destroy(&roomEmpty);
    sem_destroy(&mutex);

    printf("Fin (justo): %d pasajeros y %d oficinistas (cada uno %d veces).\n",
           CANT_PASAJEROS, CANT_OFICINISTAS, MODS_POR_OFICINISTA);
    return 0;
