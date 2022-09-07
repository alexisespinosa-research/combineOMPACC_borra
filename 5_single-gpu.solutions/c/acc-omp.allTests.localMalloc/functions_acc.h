// Headers of functions that will make use of openacc
void getAverage_acc(double * restrict U, double *restrict U_new);

double updateT_acc(double * restrict U, double *restrict U_new,double dt_old);

void loadGPU_acc(double * restrict U, double *restrict U_new);
