#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#define CANT_LECTORES 100
#define CANT_ESCRITORES 5
#define MODS_POR_ESCRITOR 3
#define TOTAL_ESCRITURAS (CANT_ESCRITORES * MODS_POR_ESCRITOR)
#define TOTAL_HILOS (CANT_LECTORES + TOTAL_ESCRITURAS)

// ------------------ 

int next_ticket = 0;
int turno_actual = 0;
pthread_mutex_t ticket_mutex = PTHREAD_MUTEX_INITIALIZER;

void esperar_turno(int* mi_turno) {
    pthread_mutex_lock(&ticket_mutex);
    *mi_turno = next_ticket++;
    pthread_mutex_unlock(&ticket_mutex);

    while (turno_actual != *mi_turno) {
        sched_yield();
    }
}

void liberar_turno() {
    __sync_fetch_and_add(&turno_actual, 1);
}

// ------------------

void* lector(void* arg) {
    int id = *(int*)arg;
    int ticket;

    esperar_turno(&ticket);

    printf("Pasajero %d esta mirando el cartel\n", id);
    sleep(rand() % 3 + 1);

    liberar_turno();
    return NULL;
}


void* escritor(void* arg) {
    int id = *(int*)arg;
    int ticket;

    esperar_turno(&ticket);

    printf("Oficinista %d esta modificando el cartel\n", id);
    sleep(rand() % 5 + 1);
    
    liberar_turno();
    return NULL;
}

// ------------------

int main() {
    srand(time(NULL));

    pthread_t hilos[TOTAL_HILOS];
    int lector_ids[CANT_LECTORES];
    int escritores_ids[TOTAL_ESCRITURAS];

    char roles[TOTAL_HILOS];
    for (int i = 0; i < CANT_LECTORES; i++) roles[i] = 'L';
    for (int i = CANT_LECTORES; i < TOTAL_HILOS; i++) roles[i] = 'E';

    for (int i = TOTAL_HILOS - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        char t = roles[i];
        roles[i] = roles[j];
        roles[j] = t;
    }

    int lector_idx = 0, escritor_idx = 0;

    for (int i = 0; i < TOTAL_HILOS; i++) {
        if (roles[i] == 'L') {
            lector_ids[lector_idx] = lector_idx + 1;
            pthread_create(&hilos[i], NULL, lector, &lector_ids[lector_idx]);
            lector_idx++;
        } else {
            escritores_ids[escritor_idx] = (escritor_idx / MODS_POR_ESCRITOR) + 1;
            pthread_create(&hilos[i], NULL, escritor, &escritores_ids[escritor_idx]);
            escritor_idx++;
        }
    }

    for (int i = 0; i < TOTAL_HILOS; i++) {
        pthread_join(hilos[i], NULL);
    }

    return 0;
}
