Problem is that multiGPU is failing with the cray compiler
WORKAROUND1: use the call  num_devices = omp_get_num_devices(); outside the CPU parallel region
   - This workaround did not fully worked as the Cray compiler still gives bad answer to the function
     even when out of the loop
WORKAROUND2: hardcode the available number of devices in the code
   - This did not worked. Even with hardcoding the device indices, all the workload goes to a single (overloaded) GPU
