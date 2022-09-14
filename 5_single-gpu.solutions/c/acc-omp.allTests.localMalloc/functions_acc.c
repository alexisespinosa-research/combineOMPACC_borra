#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "functions_acc.h"
#include "globals.h"

#if !defined (_PGI_) && !defined (_NVC_)
   #define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#endif

// --------------------------------------------------------------------
void getAverage_acc(double *restrict U,double *restrict U_new)
{
   int i,j;
   //#pragma acc kernels
   //   #pragma acc loop independent //to activate together with kernels above
        //:pgi:toBeTested
   //#pragma acc parallel loop collapse(2)
        //:pgi:toBeTested
   #pragma acc parallel loop copyin(U[:(GRIDX+2)*(GRIDY+2)]) copyout(U_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
        //:pgi:justacc:static(non-managedMemory):works (fast:preloaded data works fine:avoids the indicated copy)
        //:pgi:justacc:dynamic(non-managedMemory):works (fast:preloaded data works fine:avoids the indicated copy)
        //:gcc11:justacc:static(non-managedMemory):works (fast:preloaded data works fine:avoids the indicated copy)
        //:gcc11:justacc:dynamic(non-managedMemory):works (fast:preloaded data works fine:avoids the indicated copy)
   //#pragma acc parallel loop pcopyin(U[:(GRIDX+2)*(GRIDY+2)]) pcopyout(U_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
        //:pgi:justacc:static(non-managedMemory):works (fast:preloaded data works fine simply works as above)
        //:pgi:justacc:dynamic(non-managedMemory):works (fast:preloaded data works fine simply works as above)
        //:gcc11:justacc:static(non-managedMemory):works (fast:preloaded data works fine simply works as above)
        //:gcc11:justacc:dynamic(non-managedMemory):works (fast:preloaded data works fine simply works as above)
   //#pragma acc parallel loop present(U) present(U_new) collapse(2)
        //:pgi:justacc:static(non-managedMemory):works (fast:preloaded data works fine)
        //:pgi:justacc:dynamic(non-managedMemory):works (fast:preloaded data works fine)
        //:gcc11:justacc:static(non-managedMemory):fails at runtime: present clause error
        //:gcc11:justacc:dynamic(non-managedMemory):fails at runtime: present clause error
   //#pragma acc parallel loop present(U[:(GRIDX+2)*(GRIDY+2)]) present(U_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
        //:pgi:justacc:static(non-managedMemory):works (fast:preloaded data works fine)
        //:pgi:justacc:dynamic(non-managedMemory):works (fast:preloaded data works fine)
        //:gcc11:justacc:static(non-managedMemory):works (fast:preloaded data works fine)
        //:gcc11:justacc:dynamic(non-managedMemory):works (fast:preloaded data works fine)
   for(i = 1; i <= GRIDX; i++) 
      //#pragma acc loop independent //to activate together with kernels above
      for(j = 1; j <= GRIDY; j++) 
         U_new[OFFSET(i,j)] = 0.25 * (U[OFFSET(i+1,j)] + U[OFFSET(i-1,j)] +
                                      U[OFFSET(i,j+1)] + U[OFFSET(i,j-1)]);

   return;
}

// --------------------------------------------------------------------
double updateT_acc(double *restrict U, double *restrict U_new, double dt_old)
{
   double dt=dt_old;
   int i,j;
   // compute the largest change and copy U_new to U
   //#pragma acc kernels
   //   #pragma acc loop independent //to activate together with kernels above
   //#pragma acc parallel loop reduction(max:dt) collapse(2)
   #pragma acc parallel loop copy(U[:(GRIDX+2)*(GRIDY+2)]) copyin(U_new[:(GRIDX+2)*(GRIDY+2)]) reduction(max:dt) collapse(2)
   //#pragma acc parallel loop pcopy(U[:(GRIDX+2)*(GRIDY+2)]) pcopyin(U_new[:(GRIDX+2)*(GRIDY+2)]) reduction(max:dt) collapse(2)
   //#pragma acc parallel loop present(U) present(U_new) reduction(max:dt) collapse(2)
   //#pragma acc parallel loop present(U[:(GRIDX+2)*(GRIDY+2)]) present(U_new[:(GRIDX+2)*(GRIDY+2)]) reduction(max:dt) collapse(2)
   for(i = 1; i <= GRIDX; i++){
      //#pragma acc loop independent //to activate together with kernels above
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
void loadGPU_acc(double *restrict U,double *restrict U_new)
{
   #pragma acc enter data copyin(U[:(GRIDX+2)*(GRIDY+2)]) create(U_new[:(GRIDX+2)*(GRIDY+2)])
   return;
}

// --------------------------------------------------------------------
void copy2HOST_acc(double *restrict U,double *restrict U_new)
{
   #pragma acc exit data copyout(U[:(GRIDX+2)*(GRIDY+2)])
   #pragma acc exit data delete(U,U_new)
   return;
}
