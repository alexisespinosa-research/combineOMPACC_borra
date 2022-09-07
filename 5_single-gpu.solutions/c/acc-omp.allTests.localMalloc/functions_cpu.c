#include <stdlib.h>
//#include <stdio.h>
//#include <math.h>
#include "functions_cpu.h"
#include "globals.h"

// initialize grid and boundary conditions
void init(double *restrict T){
   int i,j;

   for (i = 0; i <= GRIDX+1; i++){
      for (j = 0; j <= GRIDY+1; j++){
         T[OFFSET(i,j)] = 0.0;
      }
   }

   // these boundary conditions never change throughout run

   // set left side to 0 and right to a linear increase
   for (i = 0; i <= GRIDX+1; i++) {
      T[OFFSET(i,0)] = 0.0;
      T[OFFSET(i,GRIDY+1)] = (128.0/GRIDX)*i;
   }

   // set top to 0 and bottom to linear increase
   for (j = 0; j <= GRIDY+1; j++) {
      T[OFFSET(0,j)] = 0.0;
      T[OFFSET(GRIDX+1,j)] = (128.0/GRIDY)*j;
   }
}
