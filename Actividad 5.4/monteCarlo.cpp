// Monte Carlo simulation to compute Pi using pthreads
// pi = number of points in circle / total number of points * 4
// (x,y) 
// -1 <= x <= 1
// -1 <= y <= 1
// radius = 1
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <time.h>


using namespace std;

#define NUM_THREADS 4
#define NUM_POINTS 1000000

int numPointsInCircle = 0;

void *computePi(void *threadId) {
    int tid = (long)threadId;
    int pointsInCircle = 0;
    double x, y, distance;

    // Seed the random number generator
    srand(time(NULL) + tid);

    for (int i = 0; i < NUM_POINTS; i++) {
        // Generate random (x,y) points
        x = (double)rand() / RAND_MAX * 2.0 - 1.0;
        y = (double)rand() / RAND_MAX * 2.0 - 1.0;

        // Check if (x,y) is in the circle
        distance = x*x + y*y;
        if (distance <= 1) {
            pointsInCircle++;
        }
    }

    // Critical section
    numPointsInCircle += pointsInCircle;

    pthread_exit(NULL);
}

int main(int argc, char *argv[]) {
    pthread_t threads[NUM_THREADS];
    int rc;
    long t;

    for (t = 0; t < NUM_THREADS; t++) {
        rc = pthread_create(&threads[t], NULL, computePi, (void *)t);
        if (rc) {
            cout << "Error: unable to create thread, " << rc << endl;
            exit(-1);
        }
    }

    for (t = 0; t < NUM_THREADS; t++) {
        pthread_join(threads[t], NULL);
    }

    cout << "Pi: " << 4.0 * numPointsInCircle / (NUM_POINTS * NUM_THREADS) << endl;

    pthread_exit(NULL);
}

// Pi: 3.1416