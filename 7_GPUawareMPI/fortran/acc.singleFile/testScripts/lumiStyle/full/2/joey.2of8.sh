#!/bin/bash -l
#SBATCH --job-name=2of8LumiLike
#SBATCH --partition=gpu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --cpus-per-task=8
#SBATCH --gpus-per-node=8
#SBATCH --time=00:10:00
#SBATCH --exclusive

#----
echo "Checking slurm settings with:"
echo "env | grep SLURM | sort"
env | grep SLURM | sort
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
#GPU_selection_wrapper
wName=select_gpu_${SLURM_JOBID}
cat << EOF > $wName
#!/bin/bash

export ROCR_VISIBLE_DEVICES=\$SLURM_LOCALID
exec \$*
EOF

chmod +x ./$wName

#----
#Manual CPU binding
#CPU_BIND="map_cpu:48,56,16,24,1,8,32,40"
CPU_BIND="map_cpu:48,56"

#----
#MPI & OMP settings
export MPICH_GPU_SUPPORT_ENABLED=1
export OMP_NUM_THREADS=1

#----
exeDir="../../../.."
exeName=joey.cray.ftn.laplace_acc.noblock_mpiGPU.twoParallelsBasic.exe
theExe=$exeDir/$exeName

#----
#Setting the srun command to test:
theSrunCmd="srun -l -u -N 1 -n 2 -c 8 --cpu-bind=${CPU_BIND} --exact ./$wName"

#----
#Execution
echo "OMP_NUM_THREADS=$OMP_NUM_THREADS"
echo "Running with:"
echo "$theSrunCmd \$theExe 4000"
$theSrunCmd $theExe 4000
echo -e "\n\n"

#----
#rocm-smi check
echo "rocm-smi check using:"
echo "$theSrunCmd rocm-smi --showhw | sort"
$theSrunCmd rocm-smi --showhw | sort
echo -e "\n\n"

#----
#hello-step check
helloDir=$MYSOFTWARE/setonix_documentation_material/hello_jobstep
helloExe=$helloDir/hello_jobstep
echo "hello-step check using:"
echo "$theSrunCmd \$helloExe | sort"
$theSrunCmd $helloExe | sort
echo -e "\n\n"

#----
#Finalising
rm -rf ./$wName
