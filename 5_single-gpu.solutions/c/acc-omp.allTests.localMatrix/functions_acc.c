#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "functions_acc.h"
#include "globals.h"  //definition of size of matrices and needed externs

#ifndef _PGI_
   #define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#endif

// --------------------------------------------------------------------
void getAverage_acc(int gridx,int gridy,double T[gridx+2][gridy+2],
                    double T_new[gridx+2][gridy+2])
{
   int i,j;
   //#pragma acc kernels
   //   #pragma acc loop independent //to activate together with kernels above
        //:pgi:noBeTested
   //#pragma acc parallel loop collapse(2)
        //:pgi:noBeTested
   //#pragma acc parallel loop copyin(T[:gridx+2][:gridy+2]) copyout(T_new[:gridx+2][:gridy+2]) collapse(2)
        //:pgi:justacc:static(non-managedMemory):works (fast:preloaded data works fine:avoids the indicated copy)
        //:pgi:justacc:dynamic(non-managedMemory):works (slow:has data transfers in every call)
   //#pragma acc parallel loop pcopyin(T[:gridx+2][:gridy+2]) pcopyout(T_new[:gridx+2][:gridy+2]) collapse(2)
        //:pgi:justacc:static(non-managedMemory):works (fast:preloaded data works fine
        //                                              simply works as above)
        //:pgi:justacc:dynamic(non-managedMemory):works (slow:still has data transfers in every call)
        //                                               simply works as above)  
   #pragma acc parallel loop present(T[:gridx+2][:gridy+2]) present(T_new[:gridx+2][:gridy+2]) collapse(2)
        //:pgi:justacc:static(non-managedMemory):works (fast:preloaded data works fine)
        //:pgi:justacc:dynamic(non-managedMemory):fails: data in PRESENT not found
   for(i = 1; i <= gridx; i++) 
      //#pragma acc loop independent //to activate together with kernels above
      for(j = 1; j <= gridy; j++) 
         T_new[i][j] = 0.25 * (T[i+1][j] + T[i-1][j] +
                               T[i][j+1] + T[i][j-1]);
}

// --------------------------------------------------------------------
double updateT_acc(int gridx,int gridy,double T[gridx+2][gridy+2],
                   double T_new[gridx+2][gridy+2],double dt_old)
{
   double dt=dt_old;
   int i,j;

   // compute the largest change and copy T_new to T
   //#pragma acc kernels
   //   #pragma acc loop independent //to activate together with kernels above
   //#pragma acc parallel loop reduction(max:dt) collapse(2)
   //#pragma acc parallel loop copy(T[:gridx+2][:gridy+2]) copyin(T_new[:gridx+2][:gridy+2]) reduction(max:dt) collapse(2)
   //#pragma acc parallel loop pcopy(T[:gridx+2][:gridy+2]) pcopyin(T_new[:gridx+2][:gridy+2]) reduction(max:dt) collapse(2)
   #pragma acc parallel loop present(T[:gridx+2][:gridy+2]) present(T_new[:gridx+2][:gridy+2]) reduction(max:dt) collapse(2)
   for(i = 1; i <= gridx; i++){
      //#pragma acc loop independent //to activate together with kernels above
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
