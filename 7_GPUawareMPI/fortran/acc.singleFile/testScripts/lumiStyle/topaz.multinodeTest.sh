#!/bin/bash -l
#SBATCH -p gpuq-dev
#SBATCH -N 2
#SBATCH -n 4
#SBATCH -c 1
#SBATCH --sockets-per-node=2
#SBATCH --ntasks-per-socket=1
#SBATCH --ntasks-per-node=2
#SBATCH --gres=gpu:2 #Two gpus per node
#SBATCH --time=00:01:00

module load nvhpc/22.7
module list

echo "Checking slurm settings:"
echo "SLURM_NTASKS=$SLURM_NTASKS"
echo "SLURM_NODELIST=$SLURM_NODELIST"
echo "SLURM_TASKS_PER_NODE=$SLURM_TASKS_PER_NODE"
echo "SLURM_GPUS=$SLURM_GPUS"
echo "SLURM_JOB_GPUS=$SLURM_JOB_GPUS"

echo ""
echo "Executing code:"
mpiexec -n 4 topaz.nvhpc.mpifort.laplace_acc.noblock_mpiGPU.twoParallelsBasic.exe 4000
