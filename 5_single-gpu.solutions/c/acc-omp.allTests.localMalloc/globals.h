// grid size
#define GRIDX    2048
//#define GRIDX    8192
//#define GRIDX    16384
//#define GRIDX    32768

#define GRIDY    2048
//#define GRIDY    8192
//#define GRIDY    16384
//#define GRIDY    32768


// global arrays
//extern double *restrict T_new; // temperature grid
//extern double *restrict T; // temperature grid from last iteration

// indexing of arrays
#define OFFSET(i, j) (((i)*(GRIDY+2)) + (j))
