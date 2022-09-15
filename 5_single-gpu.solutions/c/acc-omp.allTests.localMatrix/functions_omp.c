#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "functions_omp.h"
#include "globals.h"

#if !defined (_PGI_) && !defined (_NVC_)
   #define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#endif

// --------------------------------------------------------------------
void getAverage_omp(double *restrict U, double *restrict U_new)
{
   int i,j;
   //#pragma omp target
       //:gcc11:justomp:static(non-managedMemory):fails at execution: wrong results
       //:gcc11:justomp:dynamic(non-managedMemory):fails at execution: wrong results
   //#pragma omp target map(to:U) map(from:U_new)
       //:gcc11:justomp:static(non-managedMemory):fails at runtime:illegal memory access encountered
       //:gcc11:justomp:dynamic(non-managedMemory):fails at runtime:illegal memory access encountered
   #pragma omp target map(to:U[:(GRIDX+2)*(GRIDY+2)]) map(from:U_new[:(GRIDX+2)*(GRIDY+2)])
       //:gcc11:justomp:static(non-managedMemory):works (fast: preloaded data works fine)
       //:gcc11:justomp:dynamic(non-managedMemory):works (fast: preloaded data works fine)
   #pragma omp teams distribute parallel for collapse(2) //Note: Always active and always together with one of the above
   for(i = 1; i <= GRIDX; i++)
      for(j = 1; j <= GRIDY; j++)
         U_new[OFFSET(i,j)] = 0.25 * (U[OFFSET(i+1,j)] + U[OFFSET(i-1,j)] +
                                      U[OFFSET(i,j+1)] + U[OFFSET(i,j-1)]);

   return;
}

// --------------------------------------------------------------------
double updateT_omp(double *restrict U, double *restrict U_new,double dt_old)
{
   double dt=dt_old;
   int i,j;
   
   // compute the largest change and copy U_new to U
   //#pragma omp target
   //#pragma omp target map(tofrom:dt,U) map(to:U_new)
   #pragma omp target map(tofrom:dt,U[:(GRIDX+2)*(GRIDY+2)]) map(to:U_new[:(GRIDX+2)*(GRIDY+2)])
   #pragma omp teams distribute parallel for collapse(2) reduction(max:dt) //Note: Always active and always together with one of the above
   for(i = 1; i <= GRIDX; i++){
      for(j = 1; j <= GRIDY; j++){
         #if defined (_PGI_) || defined (_NVC_)
         dt = fmax( fabs(U_new[OFFSET(i,j)]-U[OFFSET(i,j)]), dt);
         #else
         dt = MAX( fabs(U_new[OFFSET(i,j)]-U[OFFSET(i,j)]), dt);
         #endif
         U[OFFSET(i,j)] = U_new[OFFSET(i,j)];
      }
   }
   return dt; 
}

// --------------------------------------------------------------------
void loadGPU_omp(double *restrict U,double *restrict U_new)
{
   #pragma omp target enter data map(to:U[:(GRIDX+2)*(GRIDY+2)]) map(alloc:U_new[:(GRIDX+2)*(GRIDY+2)])
   return;
}

// --------------------------------------------------------------------
void copy2HOST_omp(double *restrict U,double *restrict U_new)
{
   #pragma omp target exit data map(from:U[:(GRIDX+2)*(GRIDY+2)]) 
   #pragma omp target exit data map(delete:U,U_new) 
   return;
}
