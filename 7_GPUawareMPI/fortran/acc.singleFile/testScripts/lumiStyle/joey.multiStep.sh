#!/bin/bash -l
#SBATCH --job-name=multiLumi
#SBATCH --partition=gpu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --cpus-per-task=8
#SBATCH --gpus-per-node=8
#SBATCH --time=00:01:00
#SBATCH --exclusive

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
declare -a singles=("48" "56" "16" "24" "1" "8" "32" "40")
declare -a pairs=("48,56" "16,24" "1,8" "32,40")

#----
#MPI & OMP settings
export MPICH_GPU_SUPPORT_ENABLED=1
export OMP_NUM_THREADS=1

#----
#rocm-smi check:
export FI_CXI_DEFAULT_VNI=$(od -vAn -N4 -tu < /dev/urandom)
srun -N 1 -n 1 -c 8 --gres=gpu:8 rocm-smi --showhw

#----
#Execution
echo ""
echo "Executing code many times in single GPU:"
for i in $(seq 0 7); do
   export FI_CXI_DEFAULT_VNI=$(od -vAn -N4 -tu < /dev/urandom)
   salida=sal.$SLURM_JOBID.$i.out
   echo "OMP_NUM_THREADS=$OMP_NUM_THREADS" > $salida
   echo "Running:" >> $salida
   echo "srun -l -u -N 1 -n 1 -c 8 --cpu-bind=map_cpu:${singles[i]} --exact ./$wName $theExe 4000" >> $salida
   srun -l -u -N 1 -n 1 -c 8 --cpu-bind=map_cpu:${singles[i]} --exact ./$wName $theExe 4000 >> $salida 2>&1 &
done
wait

#----
#Finalising
rm -rf ./$wName
