#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <omp.h>

// grid size
#define GRIDS    8192

int main(int argc, char *argv[]) {

    double T_new[GRIDS][GRIDS]; // temperature grid
    double T[GRIDS][GRIDS];     // temperature grid from last iteration
    int i, j, i_start, i_end;   // grid indexes
    int num_threads = 1;	// number of OpenMP threads
    int thread_id  = 0;		// thread ID
    int num_devices = 1;	// number of GPU devices
    int device_id  = 0;		// device ID
    int chunk_size = 0;		// grid size per GPU (X direction)

    // Initialising
    for(i = 0; i < GRIDS; i++)
        for (j = 0; j < GRIDS; j++)
            T[i][j] = i+1;

    // Obtaining information about devices
    //num_devices = omp_get_num_devices();

    #pragma omp parallel default(shared) firstprivate(num_threads, thread_id, num_devices, device_id, i_start, i_end, chunk_size,i,j)
    {
       //Obtaining number of threads and setting device to use
       num_threads = omp_get_num_threads();
       thread_id  = omp_get_thread_num();
       num_devices = omp_get_num_devices();
       device_id  = thread_id % num_devices;
       omp_set_default_device(device_id);
   
       //Simple calculation of the chunk size (use exact numbers for the test)
       chunk_size=ceil((1.0*GRIDS)/num_threads);
   
       //Calculate boundaries indexes
       i_start = thread_id * chunk_size;
       i_end   = i_start + chunk_size;
   
       //Transferring data to the devices
       #pragma omp target enter data map(alloc:T,T_new)
       #pragma omp target update to(T[i_start:chunk_size][0:GRIDS])

       //Operating with the arrays
       #pragma omp target teams distribute parallel for collapse(2)
       for(i = i_start; i < i_end; i++) 
           for(j = 0; j < GRIDS; j++) 
               T_new[i][j] = T[i][j] ;

       //Trasferring data back to the host
       #pragma omp target update from(T_new[i_start:chunk_size][0:GRIDS])

       //Printing results, value in the array should be identical to the i_end index
       printf("ThreadID %4i, i_end=%4i, T_new[i_end-1][GRIDS-1]=%f\n",
              thread_id,i_end,T_new[i_end-1][GRIDS-1]);
    }
}
