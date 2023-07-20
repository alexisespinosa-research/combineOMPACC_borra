#!/bin/bash -l
#SBATCH --job-name=multiLumi
#SBATCH --partition=gpu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --gpus-per-node=1
#SBATCH --time=00:01:00
#--#SBATCH --exclusive
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
module load craype-accel-amd-gfx90a
module load rocm
module list

#----
exeDir="../.."
exeName=joey.cray.ftn.laplace_acc.noblock_mpiGPU.twoParallelsBasic.exe
theExe=$exeDir/$exeName

#----
#GPU_selection_wrapper
wName=select_gpu_${SLURM_JOBID}
cat << EOF > $wName
#!/bin/bash

export ROCR_VISIBLE_DEVICES=\$SLURM_LOCALID
exec \$*
EOF

chmod +x ./$wName

#----
#Manual CPU binding settings
#: can't be used in partial requests

#----
#MPI & OMP settings
export MPICH_GPU_SUPPORT_ENABLED=1
export OMP_NUM_THREADS=1

#----
#Execution
export FI_CXI_DEFAULT_VNI=$(od -vAn -N4 -tu < /dev/urandom)
echo "OMP_NUM_THREADS=$OMP_NUM_THREADS"
echo "Running:"
echo "srun -l -u -N 1 -n 1 -c 8 ./$wName $theExe 4000"
srun -l -u -N 1 -n 1 -c 8 ./$wName $theExe 4000

#----
#rocm-smi check:
export FI_CXI_DEFAULT_VNI=$(od -vAn -N4 -tu < /dev/urandom)
srun -n 1 rocm-smi --showhw

#----
#Finalising
rm -rf ./$wName
