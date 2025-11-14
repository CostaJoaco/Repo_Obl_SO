#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <semaphore.h>
#include <time.h>

#define CANT_LECTORES 100
#define CANT_ESCRITORES 5
#define TOTAL_HILOS 105

sem_t turno;
sem_t rw_mutex;
pthread_mutex_t mutex;
int lectores_activos = 0;

void* lector(void* arg) {
    int id = *(int*)arg;

    sem_wait(&turno);
    pthread_mutex_lock(&mutex);
    lectores_activos++;
    if (lectores_activos == 1) {
        sem_wait(&rw_mutex);
    }
    pthread_mutex_unlock(&mutex);
    sem_post(&turno);

    printf("Pasajero %d esta leyendo el cartel\n", id);
    sleep(rand() % 3 + 1);

    pthread_mutex_lock(&mutex);
    lectores_activos--;
    if (lectores_activos == 0) {
        sem_post(&rw_mutex);
    }
    pthread_mutex_unlock(&mutex);

    return NULL;
}

void* escritor(void* arg) {
    int id = *(int*)arg;

    sem_wait(&turno);
    sem_wait(&rw_mutex);
    sem_post(&turno);

    printf("Oficinista %d esta modificando el cartel\n", id);
    sleep(rand() % 4 + 1);

    sem_post(&rw_mutex);

    return NULL;
}

int main() {
    srand(time(NULL));

    pthread_t hilos[TOTAL_HILOS];
    int ids[TOTAL_HILOS];

    pthread_mutex_init(&mutex, NULL);
    sem_init(&turno, 0, 1);
    sem_init(&rw_mutex, 0, 1);

    char roles[TOTAL_HILOS];
    for (int i = 0; i < CANT_LECTORES; i++) roles[i] = 'L';
    for (int i = CANT_LECTORES; i < TOTAL_HILOS; i++) roles[i] = 'E';

    for (int i = TOTAL_HILOS - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        char temp = roles[i];
        roles[i] = roles[j];
        roles[j] = temp;
    }

    int lector_id = 1, escritor_id = 1;
    for (int i = 0; i < TOTAL_HILOS; i++) {
        if (roles[i] == 'L') {
            ids[i] = lector_id++;
            pthread_create(&hilos[i], NULL, lector, &ids[i]);
        } else {
            ids[i] = escritor_id++;
            pthread_create(&hilos[i], NULL, escritor, &ids[i]);
        }
        usleep(50000); // ! Agregue este sleep para que no se mezclen los hilos incorrectamente y respetar un orden
    }

    for (int i = 0; i < TOTAL_HILOS; i++) {
        pthread_join(hilos[i], NULL);
    }

    return 0;
}