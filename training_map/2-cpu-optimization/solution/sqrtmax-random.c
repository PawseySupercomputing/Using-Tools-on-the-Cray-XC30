#include "mpi.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define ITERATIONS 500
#define ARRAY_SIZE 1000000

int main(int argc, char *argv[]){
    int myid, numprocs, sIndex, subsize, i, j;
    double results[ITERATIONS];
    double max, result;
    double* numbers;
    double* maxes;
    MPI_Status status;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);

    numbers = malloc(sizeof(double) * ARRAY_SIZE);
    maxes = malloc(sizeof(double)*numprocs);

    for(j = 0; j < ITERATIONS; j++){
        /* split the array into sections and send these to workers */
        if(myid == 0){
            subsize = (int)floor(ARRAY_SIZE/numprocs);  
            sIndex = subsize + ARRAY_SIZE%numprocs;

            /* send the size of each section to the workers */
            for(i = 1; i < numprocs; i++){
                MPI_Send(&subsize, 1, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
                MPI_Send((numbers+sIndex+(subsize*(i-1))), subsize, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
            }
            for(i = 1; i < numprocs; i++){
                MPI_Recv(&result, 1, MPI_DOUBLE,  i, 0, MPI_COMM_WORLD, &status);
                maxes[i] = result;
            }
            max = maxes[1];
            for(i = 2; i < numprocs; i++){
                if(max < maxes[i]){
                    max = maxes[i];
                }
            }
            results[j] = max;
        }
        else{ /* receive a subsection of the array and find the max in it */
            MPI_Recv(&subsize, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD, &status);
            MPI_Recv(numbers, subsize, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD, &status);

            /* fill the input data with random numbers ourselves */
            srand(time(NULL));
            for(i = 0; i<subsize; i++)
            {
                double x = random();
                numbers[i] = sqrt(x);
            }

            result = numbers[0];
            for(i = 1; i<subsize; i++){
                if(result < numbers[i])
                    result = numbers[i];
            }
            MPI_Send(&result, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
        }
    }

    //Calculates the average, removes the largest and smallest from all the runs
    if(myid == 0){
        max = results[0];
        for(i = 0; i<ITERATIONS; i++){
            if(max < results[i])
                max = results[i];
        }
        printf("Max after %d iterations:%g\n", ITERATIONS, max);
    }

    MPI_Finalize();
}
