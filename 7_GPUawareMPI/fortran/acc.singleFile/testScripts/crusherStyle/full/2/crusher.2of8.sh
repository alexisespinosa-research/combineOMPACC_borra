#!/bin/bash -l
#SBATCH --job-name=2of8CrusherLike
#SBATCH --partition=batch
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#--#SBATCH --cpus-per-task=8
#--#SBATCH --gpus-per-node=8
#--#SBATCH --gpus-per-task=1
#SBATCH --time=00:10:00
#--#SBATCH --exclusive
#--#SBATCH --distribution=*:block:block
#--#SBATCH --mem=60G

#----
echo "Checking slurm settings with:"
echo "env | grep SLURM"
env | grep SLURM
echo -e "\n\n"

#----
echo "Checking scontrol with"
echo "scontrol show job \$SLURM_JOB_ID"
scontrol show job $SLURM_JOB_ID
echo -e "\n\n"

#----
echo "Loading modules"
module load PrgEnv-cray
module load craype-accel-amd-gfx90a rocm
module list
echo -e "\n\n"

#----
exeDir="../../../.."
exeName=crusher.cray.ftn.laplace_acc.noblock_mpiGPU.twoParallelsBasic.exe
#exeName=joey.cray.ftn.laplace_acc.noblock_mpiGPU.twoParallelsBasic.exe
theExe=$exeDir/$exeName

#----
#Manual CPU binding settings
#: can't be used in partial requests

#----
#MPI & OMP settings
export MPICH_GPU_SUPPORT_ENABLED=1
export OMP_NUM_THREADS=1

#----
#Execution
echo "OMP_NUM_THREADS=$OMP_NUM_THREADS"
echo "Running with:"
echo "srun -l -u -N 1 -n 2 -c 8 --gpu-bind=closest \$theExe 4000"
srun -l -u -N 1 -n 2 -c 8 --gpu-bind=closest $theExe 4000
echo -e "\n\n"

#----
#rocm-smi check:
srun -n 1 rocm-smi --showhw
echo -e "\n\n"

#----
#Finalising
