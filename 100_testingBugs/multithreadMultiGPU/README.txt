Problem is that multiGPU is failing with the cray compiler
WORKAROUND: use the call  num_devices = omp_get_num_devices(); outside the CPU parallel region
