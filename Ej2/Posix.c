
// Lectores-Escritores: pausa de lectores mientras escribe un oficinista.
// Compilar: gcc -pthread -o po po.c

#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>

#define CANT_PASAJEROS        100
#define CANT_OFICINISTAS      5
#define MODS_POR_OFICINISTA   3

// Semáforos y estado compartido
sem_t pausaLectores; // el escritor lo toma para pausar el ingreso de nuevos lectores
sem_t roomEmpty;     // exclusión total del recurso (sala vacía) para escribir
sem_t mutex;         // protege el contador de lectores
int lectores = 0;

// Pasajero = Lector
void* pasajero(void* arg) {
    int id = *(int*)arg;

    // Paso rápido por la "pausa": si hay escritor, me bloqueo; si no, sigo
    sem_wait(&pausaLectores);
    sem_post(&pausaLectores);

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

// Oficinista = Escritor
void* oficinista(void* arg) {
    int id = *(int*)arg;

    for (int m = 1; m <= MODS_POR_OFICINISTA; m++) {
        // Antes de escribir: bloqueo el paso de nuevos lectores
        sem_wait(&pausaLectores);

        // Espero a que la sala quede vacía (sin lectores) para escribir
        sem_wait(&roomEmpty);

        // Escritura exclusiva (solo mensaje)
        printf("Oficinista %d está escribiendo (mod %d)\n", id, m);

        // Libero la sala y reanudo el paso de lectores
        sem_post(&roomEmpty);
        sem_post(&pausaLectores);
    }
    return NULL;
}

int main(void) {
    // Inicialización
    sem_init(&pausaLectores, 0, 1); // abierto al inicio (lectores pasan)
    sem_init(&roomEmpty,     0, 1); // sala vacía disponible
    sem_init(&mutex,         0, 1); // protege 'lectores'

    pthread_t h_pasajeros[CANT_PASAJEROS];
    pthread_t h_oficinistas[CANT_OFICINISTAS];
    int id_pasajeros[CANT_PASAJEROS];
    int id_oficinistas[CANT_OFICINISTAS];

    // Crear hilos (105 en total): 100 pasajeros y 5 oficinistas
    for (int i = 0; i < CANT_PASAJEROS; i++) {
        id_pasajeros[i] = i + 1;
        pthread_create(&h_pasajeros[i], NULL, pasajero, &id_pasajeros[i]);
    }
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        id_oficinistas[j] = j + 1;
        pthread_create(&h_oficinistas[j], NULL, oficinista, &id_oficinistas[j]);
    }

    // Esperar a que terminen
    for (int i = 0; i < CANT_PASAJEROS; i++) pthread_join(h_pasajeros[i], NULL);
    for (int j = 0; j < CANT_OFICINISTAS; j++) pthread_join(h_oficinistas[j], NULL);

    // Limpieza
    sem_destroy(&pausaLectores);
    sem_destroy(&roomEmpty);
    sem_destroy(&mutex);

    printf("Fin: %d pasajeros y %d oficinistas (cada uno %d veces).\n",
           CANT_PASAJEROS, CANT_OFICINISTAS, MODS_POR_OFICINISTA);
    return 0;
}
