#!/bin/bash -l
#SBATCH -p gpu
#SBATCH -N 2
#SBATCH -n 16
#SBATCH -c 1
#SBATCH --sockets-per-node=1
#SBATCH --ntasks-per-socket=8
#SBATCH --ntasks-per-node=8
#SBATCH --gres=gpu:8 #Eight gpus per node
#SBATCH --time=00:01:00

module load craype-accel-amd-gfx90a
module load rocm
module list

export MPICH_GPU_SUPPORT_ENABLED=1

echo "Checking slurm settings:"
echo "SLURM_NTASKS=$SLURM_NTASKS"
echo "SLURM_NODELIST=$SLURM_NODELIST"
echo "SLURM_TASKS_PER_NODE=$SLURM_TASKS_PER_NODE"
echo "SLURM_GPUS=$SLURM_GPUS"
echo "SLURM_JOB_GPUS=$SLURM_JOB_GPUS"

echo ""
echo "Executing code:"
srun -l -u -N 2 -n 16 -c 1 --gpus-per-node=8 --gpu-bind=closest joey.cray.ftn.laplace_acc.noblock_mpiHOST.twoParallelsBasic.exe 4000
