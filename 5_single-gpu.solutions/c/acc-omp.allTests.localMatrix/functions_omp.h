// Headers of functions that will make use of OpenMP
void getAverage_omp(int gridx,int gridy,double T[gridx+2][gridy+2],
                    double T_new[gridx+2][gridy+2]);

double updateT_omp(int gridx,int gridy,double T[gridx+2][gridy+2],
                   double T_new[gridx+2][gridy+2],double old_dt);
