#include <iostream>
#include <pthread.h>

using namespace std;

const int num_students = 5;
const int slices_per_pizza = 8;
const int max_pizzas = 3;

int pizzas = 0;
int remaining_slices = 0;
pthread_mutex_t mutex;

void* student(void* arg) {
    int id = ((int) arg);
    while (pizzas < max_pizzas) {
        pthread_mutex_lock(&mutex);
        if (remaining_slices == 0) {
            cout << "Student " << id << " calls the pizza shop to order a new pizza." << endl;
            pizzas++;
            remaining_slices = slices_per_pizza;
            cout << "New pizza delivered (#" << pizzas << "). Total slices: " << remaining_slices << endl;
        }
        else {
            remaining_slices--;
            cout << "Student " << id << " eats a slice. Remaining slices: " << remaining_slices << endl;
        }
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

int main() {
    pthread_t threads[num_students];
    int ids[num_students];

    pthread_mutex_init(&mutex, NULL);

    for (int i = 0; i < num_students; i++) {
        ids[i] = i+1;
        pthread_create(&threads[i], NULL, student, (void*) &ids[i]);
    }

    for (int i = 0; i < num_students; i++) {
        pthread_join(threads[i], NULL);
    }

    pthread_mutex_destroy(&mutex);

    return 0;
}