
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>

#define CANT_PASAJEROS 100
#define CANT_OFICINISTAS 15

sem_t wrt;
sem_t mutex;
int rdr = 0;

void* Lector(void* arg) {
    int id = *(int*)arg;
    sem_wait(&mutex);
    rdr++;
    if (rdr == 1) sem_wait(&wrt);
    sem_post(&mutex);
    printf("Pasajero %d está viendo el cartel\n", id);
    sem_wait(&mutex);
    rdr--;
    if (rdr == 0) sem_post(&wrt);
    sem_post(&mutex);
    return NULL;
}

void* Escritor(void* arg) {
    int id = *(int*)arg;
    sem_wait(&wrt);
    printf("Oficinista %d está escribiendo\n", id);
    sem_post(&wrt);
    return NULL;
}

int main(void) {
    sem_init(&wrt, 0, 1);
    sem_init(&mutex, 0, 1);

    pthread_t hLectores[CANT_PASAJEROS];
    pthread_t hEscritores[CANT_OFICINISTAS];
    int idL[CANT_PASAJEROS];
    int idE[CANT_OFICINISTAS];

    int idxL = 0;
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        idE[j] = j + 1;
        pthread_create(&hEscritores[j], NULL, Escritor, &idE[j]);
    }
    for (int k = 0; k < CANT_PASAJEROS; k++) {
            idL[idxL] = idxL + 1;
            pthread_create(&hLectores[idxL], NULL, Lector, &idL[idxL]);
            idxL++;
    }

    while (idxL < CANT_PASAJEROS) {
        idL[idxL] = idxL + 1;
        pthread_create(&hLectores[idxL], NULL, Lector, &idL[idxL]);
        idxL++;
    }

    for (int j = 0; j < CANT_OFICINISTAS; j++) pthread_join(hEscritores[j], NULL);
    for (int i = 0; i < CANT_PASAJEROS; i++) pthread_join(hLectores[i], NULL);

    sem_destroy(&wrt);
    sem_destroy(&mutex);

    printf("Fin: %d pasajeros y %d oficinistas.\n", CANT_PASAJEROS, CANT_OFICINISTAS);
    return 0;
}
