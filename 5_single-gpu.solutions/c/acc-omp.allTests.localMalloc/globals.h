// grid size
#define GRIDY    2048
#define GRIDX    2048

// global arrays
//extern double *restrict T_new; // temperature grid
//extern double *restrict T; // temperature grid from last iteration

// indexing of arrays
#define OFFSET(i, j) (((i)*(GRIDY+2)) + (j))
