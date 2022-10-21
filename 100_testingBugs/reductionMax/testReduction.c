#include <stdlib.h>
#include <stdio.h>
#include <math.h>

// grid size
#define GRIDY    2048
#define GRIDX    2048

int main(int argc, char *argv[]) {

    int i, j;
    double dt=0.0;
    double sum=0.0;
    double T[GRIDX][GRIDY];

    // Initialising
    for(i = 0; i < GRIDX; i++){
        for (j = 0; j < GRIDY; j++){
            T[i][j] = i+1;
        }
    }
    
   // testing max reduction
   #pragma omp target teams distribute parallel for collapse(2) reduction(max:dt)
   for(i = 0; i < GRIDX; i++){
       for(j = 0; j < GRIDY; j++){
         dt = fmax( fabs(T[i][j]), dt);
       }
   }
   // testing + reduction    
   #pragma omp target teams distribute parallel for collapse(2) reduction(+:sum)
   for(i = 0; i < GRIDX; i++){
       for(j = 0; j < GRIDY; j++){
         sum += T[i][j];
       }
   }

   // Results should be:
   //dt= 2048.000000, sum=4297064448.000000
   printf("dt= %f, sum=%f\n",dt,sum);
       
   return 0;
}
