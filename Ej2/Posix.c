
// Lectores-Escritores: versión mínima en español, sin trabajo adicional (solo printf).
// Estructura clásica del apunte + pausa de lectores al escribir + semáforo 3 por oficinista.
// Compilar: gcc -pthread -o po po.c
// Ejecutar: ./po

#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>

#define CANT_PASAJEROS        100
#define CANT_OFICINISTAS      5
#define CUPOS_POR_OFICINISTA  3  // cada oficinista puede escribir 3 veces

// --- Semáforos y estado compartido (patrón del apunte) ---
sem_t wrt;      // exclusión para escribir (sala compartida)
sem_t mutex;    // protege el contador de lectores
int   rdr = 0;  // cantidad de lectores activos

// --- Semáforo extra para evitar starving de escritores ---
sem_t pausaLectores;   // el escritor lo toma para pausar ingreso de nuevos lectores

// --- Semáforos por oficinista (capacidad 3 cada uno) ---
sem_t cuposEscritura[CANT_OFICINISTAS];

// ------------------ HILOS ------------------

// Lector (idéntico al pseudocódigo del apunte)
void* Lector(void* arg) {
    int id = *(int*)arg;

    // Paso rápido por la "pausa de lectores": si un escritor la tiene, me bloqueo.
    sem_wait(&pausaLectores);
    sem_post(&pausaLectores);

    // Wait(mutex)
    sem_wait(&mutex);
    rdr++;                         // rdr++
    if (rdr == 1) {
        // If rdr == 1 -> Wait(wrt)
        sem_wait(&wrt);            // primer lector bloquea a los escritores
    }
    // Signal(mutex)
    sem_post(&mutex);

    // Leer(b)
    printf("Pasajero %d está viendo el cartel\n", id);

    // Wait(mutex)
    sem_wait(&mutex);
    rdr--;                         // rdr--
    if (rdr == 0) {
        // If rdr == 0 -> Signal(wrt)
        sem_post(&wrt);            // último lector libera a los escritores
    }
    // Signal(mutex)
    sem_post(&mutex);

    return NULL;
}

// Escritor (consume hasta 3 cupos; antes de escribir pausa lectores)
void* Escritor(void* arg) {
    int id = *(int*)arg;

    while (1) {
        // ¿Quedan cupos? si no, termino este oficinista
        if (sem_trywait(&cuposEscritura[id - 1]) != 0) break;

        // Pauso el ingreso de nuevos lectores mientras me preparo para escribir
        sem_wait(&pausaLectores);

        // Patrón del apunte:
        // Wait(wrt) // Pido acceso exclusivo a escribir
        sem_wait(&wrt);

        // b.InsertarBuffer(p) -> (solo mensaje)
        printf("Oficinista %d está escribiendo\n", id);

        // Signal(wrt) // Aviso que ya escribí
        sem_post(&wrt);

        // Reanudo el paso de lectores
        sem_post(&pausaLectores);

        // (opcional) ceder CPU para favorecer mezcla:
        // sched_yield();  // #include <sched.h> si lo activás
    }
    return NULL;
}

// ------------------ MAIN ------------------

int main(void) {
    // Inicialización según el apunte (y el semáforo extra)
    sem_init(&wrt,           0, 1);  // sala disponible para escribir
    sem_init(&mutex,         0, 1);  // protege 'rdr'
    sem_init(&pausaLectores, 0, 1);  // abierto inicialmente (lectores pasan)

    // Cupos por oficinista: 3 cada uno
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        sem_init(&cuposEscritura[j], 0, CUPOS_POR_OFICINISTA);
    }

    pthread_t hLectores[CANT_PASAJEROS];
    pthread_t hEscritores[CANT_OFICINISTAS];
    int idL[CANT_PASAJEROS];
    int idE[CANT_OFICINISTAS];

    // Creación intercalada: 1 Escritor + 20 Lectores (×5) = 5E + 100L
    int idxL = 0;
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        idE[j] = j + 1;
        pthread_create(&hEscritores[j], NULL, Escritor, &idE[j]);

        for (int k = 0; k < (CANT_PASAJEROS / CANT_OFICINISTAS); k++) { // 20
            idL[idxL] = idxL + 1;
            pthread_create(&hLectores[idxL], NULL, Lector, &idL[idxL]);
            idxL++;
        }
    }

    // Esperar a que terminen
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        pthread_join(hEscritores[j], NULL);
    }
    for (int i = 0; i < CANT_PASAJEROS; i++) {
        pthread_join(hLectores[i], NULL);
    }

    // Limpieza
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        sem_destroy(&cuposEscritura[j]);
    }
    sem_destroy(&pausaLectores);
    sem_destroy(&wrt);
    sem_destroy(&mutex);

    printf("Fin: %d pasajeros y %d oficinistas (cada uno hasta %d escrituras).\n",
           CANT_PASAJEROS, CANT_OFICINISTAS, CUPOS_POR_OFICINISTA);
    return 0;
}
