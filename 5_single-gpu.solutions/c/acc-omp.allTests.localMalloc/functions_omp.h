// Headers of functions that will make use of OpenMP
void getAverage_omp(double *restrict U, double *restrict U_new);

double updateT_omp(double *restrict U, double *restrict U_new,double dt_old);

void loadGPU_omp(double *restrict U,double *restrict U_new);

void copy2HOST_omp(double *restrict U,double *restrict U_new);
