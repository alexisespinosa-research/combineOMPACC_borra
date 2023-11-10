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

// Offset from corner to report in prints
#define CORNEROFFSET 10

// Global arrays
//double *restrict T_new; // temperature grid
//double *restrict T; // temperature grid from last iteration

int main(int argc, char *argv[]) {

    int i, j;                                            // grid indexes
    int max_iterations;                                  // maximal number of iterations
    int iteration=1;                                     // iteration
    double dt=100;                                       // largest change in temperature
    struct timeval start_iterations, stop_iterations, stop_one, elapsed_time;  // timers
    struct timeval start_host2device, stop_host2device;  // timers
    struct timeval start_device2host, stop_device2host;  // timers

    //----These are for the estimates of performance
    size_t theoreticalFetchSizeInAvg = ((GRIDX+2)*(GRIDY+2)-4)*sizeof(double);
    size_t theoreticalWriteSizeInAvg = GRIDX*GRIDY*sizeof(double);
    size_t dataSizeInAvg = theoreticalFetchSizeInAvg + theoreticalWriteSizeInAvg;
    size_t theoreticalFetchSizeInUpd = GRIDX*GRIDY*2*sizeof(double);
    size_t theoreticalWriteSizeInUpd = GRIDX*GRIDY*sizeof(double);
    size_t dataSizeInUpd = theoreticalFetchSizeInUpd + theoreticalWriteSizeInUpd;

    printf("Theoretical:\n");
    printf("Fetch size in average operation per iter: %g GB\n", theoreticalFetchSizeInAvg * 1E-9);
    printf("Write size in average operation per iter: %g GB\n", theoreticalWriteSizeInAvg * 1E-9);
    printf("Data size in average operation per iter: %g GB\n", dataSizeInAvg * 1E-9);
    printf("Fetch size in update operation per iter: %g GB\n", theoreticalFetchSizeInUpd * 1E-9);
    printf("Write size in update operation per iter: %g GB\n", theoreticalWriteSizeInUpd * 1E-9);
    printf("Data size in update operation per iter: %g GB\n", dataSizeInUpd * 1E-9);

    //----Restricted pointers are easier for managed memory for the compilers:
    double *restrict T_new=(double*)malloc(sizeof(double)*(GRIDX+2)*(GRIDY+2)); // temperature grid
    double *restrict T=(double*)malloc(sizeof(double)*(GRIDX+2)*(GRIDY+2)); // temperature grid from last iteration
    //----Use of normal pointers:
    //double *T_new=(double*)malloc(sizeof(double)*(GRIDX+2)*(GRIDY+2)); // temperature grid
    //double *T=(double*)malloc(sizeof(double)*(GRIDX+2)*(GRIDY+2)); // temperature grid from last iteration


    if(argc!=2) {
      printf("Usage: %s number_of_iterations\n",argv[0]);
      exit(1);
    } else {
      max_iterations=atoi(argv[1]);
    }


    //---- Still initialising on the host
    init(T);

    //---- Section of preloading arrays to the GPU. Default is to use OpenACC function
    //     And for the internal preloading, decided to also use "enter data" as in functions
    gettimeofday(&start_host2device,NULL); 
    #ifndef _NOPRELOAD_
       #if defined (_PRELOAD_UNSTRUCTURED_) && defined (_ALL_INTERNAL_)
          #if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
             #pragma omp target enter data map(to:T[:(GRIDX+2)*(GRIDY+2)]) map(alloc:T_new[:(GRIDX+2)*(GRIDY+2)])
                     //:gcc11:works
          #else
             #pragma acc enter data copyin(T[:(GRIDX+2)*(GRIDY+2)]) create(T_new[:(GRIDX+2)*(GRIDY+2)])
                     //:pgi:works
                     //:gcc11:works
          #endif
       #elif defined (_PRELOAD_STRUCTURED_) && defined (_ALL_INTERNAL_)
          #if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
             //#pragma omp target data map(tofrom:T) map(alloc:T_new)
                     //:gcc11:fails in runtime: illegal memory access
             #pragma omp target data map(tofrom:T[:(GRIDX+2)*(GRIDY+2)]) map(alloc:T_new[:(GRIDX+2)*(GRIDY+2)])
                     //:gcc11:works
          #else
             //#pragma acc data copy(T) create(T_new)
                     //:pgi:fails in compilation: error says "cannot determine bounds"
                     //:gcc11:fails in runtime: illegal memory access
             #pragma acc data copy(T[:(GRIDX+2)*(GRIDY+2)]) create(T_new[:(GRIDX+2)*(GRIDY+2)])
                     //:pgi:works
                     //:gcc11:works
          #endif
       #else
          #if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
             loadGPU_omp(T,T_new);
          #else
             loadGPU_acc(T,T_new);
          #endif
       #endif
    #endif
    gettimeofday(&stop_host2device,NULL); 

    // ---- simulation iterations in a while loop
    gettimeofday(&start_iterations,NULL); 
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
                   //:pgi:justacc:(internal):works (fast:only copies data outside the while when preloaded)
              //#pragma acc parallel loop collapse(2)
                   //:pgi:justacc:(internal):works (fast:only copies data outside the while when preloaded)
                   //:gcc11:justacc:(internal):works (fast:only copies data outside the while when preloaded)
              //#pragma acc parallel loop copyin(T) copyout(T_new) collapse(2)
                   //:pgi:justacc:(internal):works (fast:only copies data outside the while when preloaded)
                   //:gcc11:justacc:(internal):fails at runtime:illegal memory access
              #pragma acc parallel loop copyin(T[:(GRIDX+2)*(GRIDY+2)]) copyout(T_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
                   //:pgi:justacc:(internal):works (fast:only copies data outside the while when preloaded)
                   //:gcc11:justacc:(internal):works (fast:only copies data outside the while when preloaded)
              //#pragma acc parallel loop pcopyin(T[:(GRIDX+2)*(GRIDY+2)]) pcopyout(T_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
                   //:pgi:justacc:(internal):works (fast:only copies data outside the while when preloaded)
                   //:gcc11:justacc:(internal):works (fast:only copies data outside the while when preloaded)
              //#pragma acc parallel loop present(T) present(T_new) collapse(2)
                   //:pgi:justacc:(internal):works (fast:only copies data outside the while when preloaded)
                   //:gcc11:justacc:(internal):fails at runtime:present clause error
              //#pragma acc parallel loop present(T[:(GRIDX+2)*(GRIDY+2)]) present(T_new[:(GRIDX+2)*(GRIDY+2)]) collapse(2)
                   //:pgi:justacc:(internal):works (fast:only copies data outside the while when preloaded)
                   //:gcc11:justacc:(internal):works (fast:only copies data outside the while when preloaded)
           #else
              //#pragma omp target
                   //:gcc11:justomp:(internal):works (fast:only copies data outside the while when preloaded)
              //#pragma omp target map(to:T) map(from:T_new)
                   //:gcc11:justomp:(internal):fails at execution time: illegal memory access
              #pragma omp target map(to:T[:(GRIDX+2)*(GRIDY+2)]) map(from:T_new[:(GRIDX+2)*(GRIDY+2)])
                   //:gcc11:justomp:(internal):works (fast:only copies data outside the while when preloaded)
              #pragma omp teams distribute parallel for simd collapse(2) private(i,j)
           #endif
           for(i = 1; i <= GRIDX; i++)
              #ifndef _JUSTOMP_
              //   #pragma acc loop independent //together with kernels above
              #endif
              for(j = 1; j <= GRIDY; j++)
                 T_new[OFFSET(i,j)] = 0.25 * (T[OFFSET(i+1,j)] + T[OFFSET(i-1,j)] +
                                       T[OFFSET(i,j+1)] + T[OFFSET(i,j-1)]);
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
                 dt = fmax( fabs(T_new[OFFSET(i,j)]-T[OFFSET(i,j)]), dt);
                 #else
                 dt = MAX( fabs(T_new[OFFSET(i,j)]-T[OFFSET(i,j)]), dt);
                 #endif
                 T[OFFSET(i,j)] = T_new[OFFSET(i,j)];
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
            printf("Iteration %4.0d, dt %f, T[GX-CO][GY-CO]=%f\n",iteration,dt,
                      T[OFFSET(GRIDX-CORNEROFFSET,GRIDY-CORNEROFFSET)]);
        if (iteration == 1) 
           gettimeofday(&stop_one,NULL);

        iteration++;
    /*}else
    {
       break;
    }*/
    }
    gettimeofday(&stop_iterations,NULL);
    iteration--;
    gettimeofday(&start_device2host,NULL);
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
    gettimeofday(&stop_device2host,NULL);

    //------ Do we have T in the host ready to be saved?
    printf("Final values, iteration %4.0d, dt %f, T[GX-CO][GY-CO]=%f\n",iteration,dt,
              T[OFFSET(GRIDX-CORNEROFFSET,GRIDY-CORNEROFFSET)]);

    timersub(&stop_host2device, &start_host2device, &elapsed_time); // measure time
    double elapsedHere=elapsed_time.tv_sec+elapsed_time.tv_usec*1E-6;
    printf("Total time for initial host2device transfer was %f seconds.\n", elapsedHere);

    timersub(&stop_one, &start_iterations, &elapsed_time); // measure time
    elapsedHere=elapsed_time.tv_sec+elapsed_time.tv_usec*1E-6;
    printf("Total time for first iteration was %f seconds.\n", elapsedHere);

    timersub(&stop_iterations, &stop_one, &elapsed_time); // measure time
    elapsedHere=elapsed_time.tv_sec+elapsed_time.tv_usec*1E-6;
    printf("Total time for mesh GRID(X,Y)=(%i,%i) rest %i iterations was %f seconds.\n", GRIDX,GRIDY,
           iteration-1,elapsedHere);
    printf("Effective memory bandwidth for rest %i iterations was %g GB/s.\n", iteration-1,
           (dataSizeInAvg + dataSizeInUpd)*1E-9*(iteration-1)/elapsedHere);
           
    timersub(&stop_device2host, &start_device2host, &elapsed_time); // measure time
    elapsedHere=elapsed_time.tv_sec+elapsed_time.tv_usec*1E-6;
    printf("Total time for final device2host transfer was %f seconds.\n", elapsedHere);

    return 0;
}
