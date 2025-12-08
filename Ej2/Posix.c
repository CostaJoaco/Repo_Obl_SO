
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>

#define CANT_PASAJEROS 100
#define CANT_OFICINISTAS 5
#define ESCRITURAS_POR_OFICINISTA 3

sem_t wrt;
sem_t mutex;
sem_t pasoLectores;
sem_t sinLectores;
int rdr = 0;

void* Lector(void* arg) {
    int id = *(int*)arg;
    sem_wait(&pasoLectores);
    sem_post(&pasoLectores);
    sem_wait(&mutex);
    rdr++;
    sem_post(&mutex);
    printf("Pasajero %d está viendo el cartel\n", id);
    sem_wait(&mutex);
    rdr--;
    if (rdr == 0) sem_post(&sinLectores);
    sem_post(&mutex);
    return NULL;
}

void* Escritor(void* arg) {
    int id = *(int*)arg;
    for (int k = 1; k <= ESCRITURAS_POR_OFICINISTA; k++) {
        sem_wait(&wrt);
        sem_wait(&pasoLectores);
        sem_wait(&mutex);
        if (rdr == 0) {
            sem_post(&mutex);
        } else {
            sem_post(&mutex);
            sem_wait(&sinLectores);
        }
        printf("Oficinista %d está escribiendo (mod %d)\n", id, k);
        sem_post(&pasoLectores);
        sem_post(&wrt);
    }
    return NULL;
}

int main(void) {
    sem_init(&wrt, 0, 1);
    sem_init(&mutex, 0, 1);
    sem_init(&pasoLectores, 0, 1);
    sem_init(&sinLectores, 0, 0);

    pthread_t hLectores[CANT_PASAJEROS];
    pthread_t hEscritores[CANT_OFICINISTAS];
    int idL[CANT_PASAJEROS];
    int idE[CANT_OFICINISTAS];

    int idxL = 0;
    for (int j = 0; j < CANT_OFICINISTAS; j++) {
        idE[j] = j + 1;
        pthread_create(&hEscritores[j], NULL, Escritor, &idE[j]);
        for (int k = 0; k < (CANT_PASAJEROS / CANT_OFICINISTAS); k++) {
            idL[idxL] = idxL + 1;
            pthread_create(&hLectores[idxL], NULL, Lector, &idL[idxL]);
            idxL++;
        }
    }

    for (int j = 0; j < CANT_OFICINISTAS; j++) pthread_join(hEscritores[j], NULL);
    for (int i = 0; i < CANT_PASAJEROS; i++) pthread_join(hLectores[i], NULL);

    sem_destroy(&sinLectores);
    sem_destroy(&pasoLectores);
    sem_destroy(&wrt);
    sem_destroy(&mutex);

    printf("Fin: %d pasajeros y %d oficinistas (cada uno %d escrituras).\n",
           CANT_PASAJEROS, CANT_OFICINISTAS, ESCRITURAS_POR_OFICINISTA);
    return 0;
}
