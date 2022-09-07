#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "functions_omp.h"

#ifndef _PGI_
   #define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#endif

// --------------------------------------------------------------------
void getAverage_omp(int gridx,int gridy,double T[gridx+2][gridy+2],
                          double T_new[gridx+2][gridy+2])
{
   int i,j;
   //#pragma omp target
       //:gcc11:justomp:static(non-managedMemory):fails:libgomp:cuCtxSynchronize: illegal memory access encountered
       //:gcc11:justomp:dynamic(non-managedMemory):fails:libgomp:cuCtxSynchronize: illegal memory access encountered
   //#pragma omp target map(to:T) map(from:T_new)
       //:gcc11:justomp:static(non-managedMemory):fails:libgomp:cuCtxSynchronize: illegal memory access encountered
       //:gcc11:justomp:dynamic(non-managedMemory):fails:libgomp:cuCtxSynchronize: illegal memory access encountered
   #pragma omp target map(to:T[:gridx+2][:gridy+2]) map(from:T_new[:gridx+2][:gridy+2])
       //:gcc11:justomp:static(non-managedMemory):works (slow:this naive-version has data transfers in every call)
       //:gcc11:justomp:dynamic(non-managedMemory):works (slow:this naive-version has data transfers in every call)
   #pragma omp teams distribute parallel for collapse(2)
   for(i = 1; i <= gridx; i++)
      for(j = 1; j <= gridy; j++)
         T_new[i][j] = 0.25 * (T[i+1][j] + T[i-1][j] +
                               T[i][j+1] + T[i][j-1]);
}

// --------------------------------------------------------------------
double updateT_omp(int gridx,int gridy,double T[gridx+2][gridy+2],
                   double T_new[gridx+2][gridy+2],double dt_old)
{
   double dt=dt_old;
   int i,j;
   
   // compute the largest change and copy T_new to T
   //#pragma omp target
   //#pragma omp target map(tofrom:dt,T) map(to:T_new)
   #pragma omp target map(tofrom:dt,T[:gridx+2][:gridy+2]) map(to:T_new[:gridx+2][:gridy+2])
   #pragma omp teams distribute parallel for collapse(2) reduction(max:dt)
   for(i = 1; i <= gridx; i++){
      for(j = 1; j <= gridy; j++){
#ifdef _PGI_
         dt = fmax( fabs(T_new[i][j]-T[i][j]), dt);
#else
         dt = MAX( fabs(T_new[i][j]-T[i][j]), dt);
#endif
         T[i][j] = T_new[i][j];
      }
   }
   return dt; 
}
