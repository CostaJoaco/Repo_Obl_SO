#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#define CANT_LECTORES 100
#define CANT_ESCRITORES 5
#define MODS_POR_ESCRITOR 3

// ==================== SEMÁFOROS POSIX ====================
#include <semaphore.h>

sem_t wrt;              // Control de escritura (exclusión mutua escritores)
sem_t mutex_lectores;   // Protege contador de lectores
sem_t mutex_escritores; // Protege contador de escritores en espera

int cant_lectores = 0;      // Lectores activos
int cant_escritores = 0;    // Escritores esperando/escribiendo

// ==================== FUNCIONES ====================

void* lector(void* arg) {
    int id = *(int*)arg;
    
    // Simular tiempo random antes de mirar (0-3 segundos)
    usleep((rand() % 3000000));
    
    // VERIFICAR SI HAY ESCRITORES ESPERANDO (Prioridad escritores)
    sem_wait(&mutex_escritores);
    while (cant_escritores > 0) {
        sem_post(&mutex_escritores);
        usleep(50000); // Espera breve
        sem_wait(&mutex_escritores);
    }
    sem_post(&mutex_escritores);
    
    // ENTRAR A LEER
    sem_wait(&mutex_lectores);
    cant_lectores++;
    if (cant_lectores == 1) {
        sem_wait(&wrt); // Primer lector bloquea escritores
    }
    sem_post(&mutex_lectores);
    
    // LEER (mirar cartel)
    printf("Pasajero %d esta mirando el cartel\n", id);
    usleep((rand() % 2000000)); // Simula tiempo mirando (0-2 seg)
    
    // SALIR DE LEER
    sem_wait(&mutex_lectores);
    cant_lectores--;
    if (cant_lectores == 0) {
        sem_post(&wrt); // Último lector libera escritores
    }
    sem_post(&mutex_lectores);
    
    free(arg);
    return NULL;
}

void* escritor(void* arg) {
    int id = *(int*)arg;
    
    // Simular tiempo random antes de modificar (0-3 segundos)
    usleep((rand() % 3000000));
    
    // ANUNCIAR INTENCIÓN DE ESCRIBIR
    sem_wait(&mutex_escritores);
    cant_escritores++;
    sem_post(&mutex_escritores);
    
    // PEDIR ACCESO EXCLUSIVO
    sem_wait(&wrt);
    
    // ESCRIBIR (modificar cartel)
    printf("Oficinista %d esta modificando el cartel\n", id);
    usleep((rand() % 2000000)); // Simula tiempo modificando (0-2 seg)
    
    // LIBERAR ACCESO
    sem_post(&wrt);
    
    // SALIR DE LA COLA DE ESCRITORES
    sem_wait(&mutex_escritores);
    cant_escritores--;
    sem_post(&mutex_escritores);
    
    free(arg);
    return NULL;
}

// ==================== MAIN ====================

int main() {
    srand(time(NULL));
    
    // INICIALIZAR SEMÁFOROS
    sem_init(&wrt, 0, 1);
    sem_init(&mutex_lectores, 0, 1);
    sem_init(&mutex_escritores, 0, 1);
    
    // CALCULAR TOTAL DE HILOS
    int total_escrituras = CANT_ESCRITORES * MODS_POR_ESCRITOR;
    int total_hilos = CANT_LECTORES + total_escrituras;
    
    pthread_t* hilos = malloc(total_hilos * sizeof(pthread_t));
    
    // CREAR ARRAY DE ROLES (L = Lector, E = Escritor)
    char* roles = malloc(total_hilos * sizeof(char));
    for (int i = 0; i < CANT_LECTORES; i++) {
        roles[i] = 'L';
    }
    for (int i = CANT_LECTORES; i < total_hilos; i++) {
        roles[i] = 'E';
    }
    
    // MEZCLAR ROLES (Fisher-Yates shuffle)
    for (int i = total_hilos - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        char temp = roles[i];
        roles[i] = roles[j];
        roles[j] = temp;
    }
    
    // CREAR HILOS EN ORDEN ALEATORIO
    int lector_idx = 0;
    int escritor_idx = 0;
    
    for (int i = 0; i < total_hilos; i++) {
        if (roles[i] == 'L') {
            int* id = malloc(sizeof(int));
            *id = ++lector_idx;
            pthread_create(&hilos[i], NULL, lector, id);
        } else {
            int* id = malloc(sizeof(int));
            *id = (escritor_idx / MODS_POR_ESCRITOR) + 1;
            pthread_create(&hilos[i], NULL, escritor, id);
            escritor_idx++;
        }
        usleep(10000); // Pequeño delay para distribuir llegadas
    }
    
    // ESPERAR A QUE TERMINEN TODOS
    for (int i = 0; i < total_hilos; i++) {
        pthread_join(hilos[i], NULL);
    }
    
    // LIBERAR MEMORIA
    free(hilos);
    free(roles);
    
    // DESTRUIR SEMÁFOROS
    sem_destroy(&wrt);
    sem_destroy(&mutex_lectores);
    sem_destroy(&mutex_escritores);
    
    printf("\n=== FIN ===\n");
    printf("Total: %d pasajeros y %d oficinistas\n", CANT_LECTORES, CANT_ESCRITORES);
    printf("Cada oficinista hizo %d modificaciones\n", MODS_POR_ESCRITOR);
    
    return 0;
}