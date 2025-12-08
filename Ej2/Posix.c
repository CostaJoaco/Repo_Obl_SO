#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#define CANT_LECTORES 100
#define CANT_ESCRITORES 5
#define MODS_POR_ESCRITOR 3

#include <semaphore.h>

sem_t wrt;
sem_t mutex_lectores;
sem_t mutex_escritores;

int cant_lectores = 0;
int cant_escritores = 0;

void* lector(void* arg) {
    int id = *(int*)arg;

    sem_wait(&mutex_escritores);
    while (cant_escritores > 0) {
        sem_post(&mutex_escritores);
        sem_wait(&mutex_escritores);
    }
    sem_post(&mutex_escritores);

    sem_wait(&mutex_lectores);
    cant_lectores++;
    if (cant_lectores == 1) {
        sem_wait(&wrt);
    }
    sem_post(&mutex_lectores);

    printf("Pasajero %d esta mirando el cartel\n", id);
    sleep(rand() % 3);
    sem_wait(&mutex_lectores);
    cant_lectores--;
    if (cant_lectores == 0) {
        sem_post(&wrt);
    }
    sem_post(&mutex_lectores);

    return NULL;
}

void* escritor(void* arg) {
    int id = *(int*)arg;

    sem_wait(&mutex_escritores);
    cant_escritores++;
    sem_post(&mutex_escritores);

    sem_wait(&wrt);

    printf("Oficinista %d esta modificando el cartel\n", id);
    sleep(rand() % 5);
    sem_post(&wrt);

    sem_wait(&mutex_escritores);
    cant_escritores--;
    sem_post(&mutex_escritores);

    return NULL;
}

int main() {
    srand(time(NULL));

    sem_init(&wrt, 0, 1);
    sem_init(&mutex_lectores, 0, 1);
    sem_init(&mutex_escritores, 0, 1);

    int total_escrituras = CANT_ESCRITORES * MODS_POR_ESCRITOR;
    int total_hilos = CANT_LECTORES + total_escrituras;

    pthread_t hilos[total_hilos];
    char roles[total_hilos];
    int ids_lectores[CANT_LECTORES];
    int ids_escritores[total_escrituras];

    for (int i = 0; i < CANT_LECTORES; i++) roles[i] = 'L';
    for (int i = CANT_LECTORES; i < total_hilos; i++) roles[i] = 'E';

    for (int i = total_hilos - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        char temp = roles[i];
        roles[i] = roles[j];
        roles[j] = temp;
    }

    int lector_idx = 0;
    int escritor_idx = 0;

    for (int i = 0; i < total_hilos; i++) {
        if (roles[i] == 'L') {
            ids_lectores[lector_idx] = lector_idx + 1;
            pthread_create(&hilos[i], NULL, lector, &ids_lectores[lector_idx]);
            lector_idx++;
        } else {
            ids_escritores[escritor_idx] = (escritor_idx / MODS_POR_ESCRITOR) + 1;
            pthread_create(&hilos[i], NULL, escritor, &ids_escritores[escritor_idx]);
            escritor_idx++;
        }
        sleep(1);
    }

    for (int i = 0; i < total_hilos; i++) {
        pthread_join(hilos[i], NULL);
    }

    return 0;
}
