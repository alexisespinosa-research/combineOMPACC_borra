#include <iostream>
#include <thread>
#include <math.h>
#include <omp.h>
using namespace std;

// grid size
#define GRIDS    8192
#define NUM_THREADS 2
#define NUM_DEVICES 2

struct thread_data
{
   int thread_id;
   int num_devices;
   int num_threads;
   double *T;
   double *T_new;
   int n;
   int m;
};

void *operateMatrix(void *threadarg)
{
   //Argument
   struct thread_data *my_data;
   my_data = (struct thread_data *) threadarg; 
   //In practice arguments
   int thread_id=my_data->thread_id;
   int num_devices=my_data->num_devices;
   int num_threads=my_data->num_threads;
   double *T=my_data->T;
   double *T_new=my_data->T_new;
   int n=my_data->n;
   int m=my_data->m;
   cout << "Thread:" << thread_id << ", num_devices=" << num_devices << ", num_threads=" << num_threads << endl;
   //Variables in the function
   int i, j, i_start, i_end;   // grid indexes
   int device_id  = thread_id % num_devices;
   int chunk_size = 0;		// grid size per GPU (X direction)
   omp_set_default_device(device_id);
   cout << "Thread=" << thread_id << ", device=" << device_id << endl;

   //Simple calculation of the chunk size (use exact numbers for the test)
   chunk_size=ceil((1.0*n)/num_threads);

   //Calculate boundaries indexes
   i_start = thread_id * chunk_size;
   i_end   = i_start + chunk_size;

   //Transferring data to the devices
   #pragma omp target enter data map(alloc:T[0:n*m],T_new[0:n*m])
   #pragma omp target update to(T[i_start*m:chunk_size*m])

   //Operating with the arrays
   #pragma omp target teams distribute parallel for collapse(2)
   for(i = i_start; i < i_end; i++) 
       for(j = 0; j < m; j++) 
           //T_new[i][j] = T[i][j] ;
           *((T_new+i*m)+j)=*((T+i*m)+j);

   //Trasferring data back to the host
   #pragma omp target update from(T_new[i_start*m:chunk_size*m])

   //Printing results, value in the array should be identical to the i_end index
   printf("ThreadID %4i, i_end=%4i, T_new[i_end-1][m-1]=%f\n",
          //thread_id,i_end,T_new[i_end-1][m-1]);
          thread_id,i_end,*((T_new+(i_end-1)*m)+(n-1)));
   pthread_exit(NULL);
   //printf("Finished matrix operations\n");
}

int main(int argc, char *argv[]) {
    int n=GRIDS, m=GRIDS;
    double T_new[n][m]; // temperature grid
    double T[n][m];     // temperature grid from last iteration
    int i, j;   // grid indexes
    int num_devices=1;
    int num_threads=1;

    //For dealing with pthreads stuff
    pthread_t threads[NUM_THREADS];
    pthread_attr_t attr;
    void *status;
    struct thread_data td[NUM_THREADS];
    int rc=0;

    // Initialising
    for(i = 0; i < n; i++)
        for (j = 0; j < m; j++)
            T[i][j] = i+1;

    // Obtaining information about devices
    //num_devices = NUM_DEVICES;
    num_devices = omp_get_num_devices();
    num_threads = NUM_THREADS;
    cout << "main() : num_devices=" << num_devices << ", num_threads=" << num_threads << endl;

    // Initialise and set thread joinable
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

    //Threads loop
    for (i=0; i< num_threads; i++)
    {
       //Preparing the argument
       td[i].thread_id=i;
       td[i].num_devices=num_devices;
       td[i].num_threads=num_threads;
       td[i].T=(double *)T;
       td[i].T_new=(double *)T_new;
       td[i].n=n;
       td[i].m=m;
       //Creating the thread
       cout << "main() : creating thread, " << i << endl;
       rc = pthread_create(&threads[i], NULL, operateMatrix, (void *)&td[i]); 
       cout << "main() : Finished creating thread, " << i << endl;

       //If error
       if (rc) {
          cout << "Error:unable to create thread," << rc << endl;
          exit(-1);
       }
    }

    // free attribute and wait for the other threads
    pthread_attr_destroy(&attr);
    for( i = 0; i < NUM_THREADS; i++ ) {
       rc = pthread_join(threads[i], &status);
       if (rc) {
          cout << "Error:unable to join," << rc << endl;
          exit(-1);
       }
       cout << "Main(): completed thread id :" << i ;
       cout << "  exiting with status :" << status << endl;
    }

    cout << "Main(): program exiting." << endl;
    //pthread_exit(NULL);
}
