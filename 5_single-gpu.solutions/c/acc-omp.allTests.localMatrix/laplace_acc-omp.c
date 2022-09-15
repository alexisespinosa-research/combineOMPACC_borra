#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include "globals.h" //contains array sizes and needed externs
#include "functions_cpu.h"
#ifndef _JUSTOMP_
   #include "functions_acc.h"
#endif
#ifndef _JUSTACC_
   #include "functions_omp.h"
#endif


#if defined (_UPDATE_INTERNAL_) || defined (_ALL_INTERNAL_)
   #if !defined (_PGI_) && !defined (_NVC_)
      #define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
   #endif
#endif

// smallest permitted change in temperature
#define MAX_TEMP_ERROR 0.02

// data to report in prints
#define FACDATA 0.98

// Global arrays
// //double T_new[GRIDX+2][GRIDY+2]; // temperature grid
// //double T[GRIDX+2][GRIDY+2];     // temperature grid from last iteration

int main(int argc, char *argv[]) {

    int i, j;                                            // grid indexes
    int max_iterations;                                  // maximal number of iterations
    int iteration=1;                                     // iteration
    double dt=100;                                       // largest change in temperature
    struct timeval start_time, stop_time, elapsed_time;  // timers
    double T_new[GRIDX+2][GRIDY+2]; // temperature grid
    double T[GRIDX+2][GRIDY+2];     // temperature grid from last iteration


    if(argc!=2) {
      printf("Usage: %s number_of_iterations\n",argv[0]);
      exit(1);
    } else {
      max_iterations=atoi(argv[1]);
    }

    gettimeofday(&start_time,NULL); 

    //---- Still initialising on the host
    init(T);

    //---- Section of preloading arrays to the GPU. Default is to use OpenACC function
    //     And for the internal preloading, decided to also use "enter data" as in functions
    #ifndef _NOPRELOAD_
       #if defined (_PRELOAD_UNSTRUCTURED_) && defined (_ALL_INTERNAL_)
          #if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
             #pragma omp target enter data map(to:T[:(GRIDX+2)*(GRIDY+2)]) map(alloc:T_new[:(GRIDX+2)*(GRIDY+2)])
          #else
             #pragma acc enter data copyin(T[:(GRIDX+2)*(GRIDY+2)]) create(T_new[:(GRIDX+2)*(GRIDY+2)])
          #endif
       #elif defined (_PRELOAD_STRUCTURED_) && defined (_ALL_INTERNAL_)
          #if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
             //#pragma omp target data map(tofrom:T) map(alloc:T_new)
             #pragma omp target data map(tofrom:T[:(GRIDX+2)*(GRIDY+2)]) map(alloc:T_new[:(GRIDX+2)*(GRIDY+2)])
          #else
             //#pragma acc data copy(T) create(T_new)
             #pragma acc data copy(T[:(GRIDX+2)*(GRIDY+2)]) create(T_new[:(GRIDX+2)*(GRIDY+2)])
          #endif
       #else
          #if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
             loadGPU_omp(T,T_new);
          #else
             loadGPU_acc(T,T_new);
          #endif
       #endif
    #endif

    // ---- simulation iterations in a while loop
    while ( dt > MAX_TEMP_ERROR && iteration <= max_iterations ) {
    /*for ( iteration=1; iteration <=max_iterations; iteration++){
    if (dt > MAX_TEMP_ERROR) {*/
       
        //---- reset dt
        dt = 0.0;

        //---- Average over neighbours. Default is to use OpenACC function
        #if defined (_AVERAGE_INTERNAL_) || defined (_ALL_INTERNAL_)
           #ifndef _JUSTOMP_
              //#pragma acc kernels
              //   #pragma acc loop independent //together with kernels above
              //#pragma acc parallel loop collapse(2)
              //#pragma acc parallel loop copyin(T) copyout(T_new) collapse(2)
              #pragma acc parallel loop copyin(T[:(GRIDX+2)*(GRIDY+2)]) copyout(T_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
              //#pragma acc parallel loop pcopyin(T[:(GRIDX+2)*(GRIDY+2)]) pcopyout(T_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
              //#pragma acc parallel loop present(T) present(T_new) collapse(2)
              //#pragma acc parallel loop present(T[:(GRIDX+2)*(GRIDY+2)]) present(T_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
           #else
              //#pragma omp target
              //#pragma omp target map(to:T) map(from:T_new)
              #pragma omp target map(to:T[:(GRIDX+2)*(GRIDY+2)]) map(from:T_new[:(GRIDX+2)*(GRIDY+2)])
              #pragma omp teams distribute parallel for collapse(2) private(i,j)
           #endif
           for(i = 1; i <= GRIDX; i++)
              #ifndef _JUSTOMP_
              //   #pragma acc loop independent //together with kernels above
              #endif
              for(j = 1; j <= GRIDY; j++)
                 T_new[i][j] = 0.25 * (T[i+1][j] + T[i-1][j] +
                                       T[i][j+1] + T[i][j-1]);
        #else
           #ifndef _JUSTOMP_
           getAverage_acc(T,T_new);
           #else
           getAverage_omp(T,T_new);
           #endif
        #endif
           
        //---- Copy T_new into T and compute largest change. Default is to use OpenMP function
        #if defined (_UPDATE_INTERNAL_) || defined (_ALL_INTERNAL_)
           #ifndef _JUSTACC_
              //#pragma omp target map(tofrom:dt)
              //#pragma omp target map(tofrom:T) map(tofrom:dt) map(to:T_new)
              #pragma omp target map(tofrom:T[:(GRIDX+2)*(GRIDY+2)],dt) map(to:T_new[:(GRIDX+2)*(GRIDY+2)])
              #pragma omp teams distribute parallel for collapse(2) reduction(max:dt) private(i,j)
           #else
              //#pragma acc kernels
              //   #pragma acc loop independent //together with kernels above
              //#pragma acc parallel loop reduction(max:dt) collapse(2)
              //#pragma acc parallel loop copy(T) copyin(T_new) reduction(max:dt) collapse(2)
              #pragma acc parallel loop copy(T[:(GRIDX+2)*(GRIDY+2)]) copyin(T_new[:(GRIDX+2)*(GRIDY+2)]) reduction(max:dt) collapse(2)
              //#pragma acc parallel loop pcopy(T[:(GRIDX+2)*(GRIDY+2)]) pcopyin(T_new[:(GRIDX+2)*(GRIDY+2)]) reduction(max:dt) collapse(2)
              //#pragma acc parallel loop present(T) present(T_new) reduction(max:dt) collapse(2)
              //#pragma acc parallel loop present(T[:(GRIDX+2)*(GRIDY+2)]) present(T_new[:(GRIDX+2)*(GRIDY+2)]) reduction(max:dt) collapse(2)
           #endif
           for(i = 1; i <= GRIDX; i++){
              #ifndef _JUSTACC_
                 #define foo 0 
              #else
              //   #pragma acc loop independent //together with kernels above
              #endif
              for(j = 1; j <= GRIDY; j++){
                 #if defined (_PGI_) || defined (_NVC_)
                 dt = fmax( fabs(T_new[i][j]-T[i][j]), dt);
                 #else
                 dt = MAX( fabs(T_new[i][j]-T[i][j]), dt);
                 #endif
                 T[i][j] = T_new[i][j];
              }
           }
        #else
           #ifndef _JUSTACC_
           dt = updateT_omp(T,T_new,dt);
           #else
           dt = updateT_acc(T,T_new,dt);
           #endif
        #endif
        // periodically print largest change
        if((iteration % 100) == 0) 
            printf("Iteration %4.0d, dt %f, T[Fac*GX][Fac*GY]=%f\n",iteration,dt,
                      T[(int)(FACDATA*(float)GRIDX)][(int)(FACDATA*(float)GRIDY)]);
        
	     iteration++;
    /*}else
    {
       break;
    }*/
    }
    //---- Section for copying arrays back to host. Default is to use OpenMP except when STRUCTURED,
    //     where end is implicit at the end of the while loop (c logic)
    //     (Needed because loading functions,and decided statement in internal preload, use "enter data")
    #ifndef _NOPRELOAD_
       #if defined (_PRELOAD_UNSTRUCTURED_) && defined (_ALL_INTERNAL_)
          #if defined(_JUSTACC_) || defined(_PRELOADACC_)
             #pragma acc exit data copyout(T[:(GRIDX+2)*(GRIDY+2)])
             #pragma acc exit data delete(T,T_new)
          #else
             #pragma omp target exit data map(from:T[:(GRIDX+2)*(GRIDY+2)])
             #pragma omp target exit data map(delete:T,T_new)
          #endif
       #elif defined (_PRELOAD_STRUCTURED_) && defined (_ALL_INTERNAL_)
          #if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
             //#pragma omp end target data //Not needed in c
          #else
             //#pragma acc end data //Not needed in c
          #endif
       #else
          #if defined(_JUSTACC_) || defined(_PRELOADACC_)
             copy2HOST_acc(T,T_new);
          #else
             copy2HOST_omp(T,T_new);
          #endif
       #endif
    #endif

    //------ Do we have T in the host ready to be saved?
    printf("Final values, iteration %4.0d, dt %f, T[Fac*GX][Fac*GY]=%f\n",iteration,dt,
              T[(int)(FACDATA*(float)GRIDX)][(int)(FACDATA*(float)GRIDY)]);

    gettimeofday(&stop_time,NULL);
    timersub(&stop_time, &start_time, &elapsed_time); // measure time

    printf("Total time was %f seconds.\n", elapsed_time.tv_sec+elapsed_time.tv_usec/1000000.0);

    return 0;
}
