#!/bin/bash -l
#SBATCH --job-name=1of8CrusherLike
#SBATCH --partition=gpu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --cpus-per-task=8
#SBATCH --gpus-per-node=8
#SBATCH --gpus-per-task=1
#SBATCH --time=00:10:00
#SBATCH --exclusive
#--#SBATCH --distribution=*:block:block
#--#SBATCH --mem=60G

#----
echo "Checking slurm settings:"
echo "SLURM_NTASKS=$SLURM_NTASKS"
echo "SLURM_NODELIST=$SLURM_NODELIST"
echo "SLURM_TASKS_PER_NODE=$SLURM_TASKS_PER_NODE"
echo "SLURM_GPUS=$SLURM_GPUS"
echo "SLURM_JOB_GPUS=$SLURM_JOB_GPUS"

#----
module load PrgEnv-cray
module load craype-accel-amd-gfx90a rocm
module list

#----
exeDir="../../../.."
exeName=joey.cray.ftn.laplace_acc.noblock_mpiGPU.twoParallelsBasic.exe
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
echo "Running:"
echo "srun -l -u -N 1 -n 1 -c 8 --gpu-bind=closest $theExe 4000"
srun -l -u -N 1 -n 1 -c 8 --gpu-bind=closest $theExe 4000

#----
#rocm-smi check:
srun -n 1 rocm-smi --showhw

#----
#Finalising
