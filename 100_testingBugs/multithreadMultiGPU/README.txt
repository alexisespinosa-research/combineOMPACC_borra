Problem is that multiGPU is failing with the cray compiler
WORKAROUND1: use the call  num_devices = omp_get_num_devices(); outside the CPU parallel region
   - This workaround did not fully worked as the Cray compiler still gives bad answer to the function
     even when out of the loop
