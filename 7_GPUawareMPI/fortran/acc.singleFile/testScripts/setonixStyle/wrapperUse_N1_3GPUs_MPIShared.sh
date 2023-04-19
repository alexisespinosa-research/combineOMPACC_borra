#!/bin/bash --login
#SBATCH --job-name=wrapperUseMPIShared
#SBATCH --partition=gpu
#SBATCH --nodes=1              #1 nodes in this example 
#SBATCH --ntasks-per-node=3    #3 tasks for the 3 GPUs in this job
#SBATCH --gpus-per-node=3      #3 GPUs in this job
#SBATCH --sockets-per-node=3   #Use 3 slurm-sockets in this job
#SBATCH --cpus-per-task=8      #IMPORTANT: Always reserve 8 CPU-cores per task. In this way,
#                                the whole slurm-socket (L3 cache group) will be reserved
#                                for each of the tasks, allowing for correct GPU binding.
#SBATCH --time=00:05:00
#SBATCH --account=<yourProject>-gpu #IMPORTANT: use your own project and the -gpu suffix
#--#SBATCH --mem=29440G           #Request for the right amount of memory (29440M per L3 cache region chiplet)

#----
#Tricks used only for the development/debugging of the script.
#This section should be commented or removed from a proper production script.
#shopt -s expand_aliases
#alias "generate_CPU_BIND.sh"="$MYSOFTWARE/pawseytools/generate_CPU_BIND.sh"
#alias

#----
#Loading needed modules
module load PrgEnv-cray
module load rocm craype-accel-amd-gfx90a
module list

#----
#Definition of the binding test executable (comment/uncomment as needed):
helloDir=$MYSCRATCH/hello_jobstep
helloName=hello_jobstep
theHello=$helloDir/$helloName

#----
#Definition of the user's own test executables (comment/uncomment as needed):
#exeDir=$MYSCRATCH/myCode
exeDir="../.."
preName="setonix.cray.ftn.laplace_acc.noblock"
postName="twoParallelsBasic"
exeName1=${preName}_mpiGPU.${postName}.exe #GPU-aware MPI code
exeName2=${preName}_mpiHOST.${postName}.exe #CPU MPI code
theExe1=$exeDir/$exeName1
theExe2=$exeDir/$exeName2

#----
#First preliminar "hack": create a selectGPU wrapper to be used for
#                         binding only 1 GPU per each task spawned by srun
wrapper="selectGPU_${SLURM_JOBID}.sh"
cat << EOF > $wrapper
#!/bin/bash

export ROCR_VISIBLE_DEVICES=\$SLURM_LOCALID
exec \$*
EOF
chmod +x ./$wrapper

#----
#Second preliminar "hack": generate an ordered list of CPU-cores (each on a different slurm-socket)
#                          to be matched with the correct GPU in the srun command using --cpu-bind option.
#                          Script "generate_CPU_BIND.sh" serves this purpose and is available to all users
#                          through the module pawseytools, which is loaded by default.
CPU_BIND=$(generate_CPU_BIND.sh map_cpu)
lastResult=$?
if [ $lastResult -ne 0 ]; then
   echo "Exiting as the map generation for CPU_BIND failed" 1>&2
   rm -f ./$wrapper #deleting the wrapper
   exit 1
fi
echo "CPU_BIND=$CPU_BIND"

#----
#MPI & OpenMP settings
export MPICH_GPU_SUPPORT_ENABLED=1 #This allows for GPU-aware MPI communication among GPUs
#export OMP_NUM_THREADS=1           #This controls the real CPU-cores per task for the executable

#---
#srun command to use in this script
srunCommand="srun -l -u -c 8 --cpu-bind=${CPU_BIND} ./$wrapper"

#----
#Execution of binding test (comment/uncomment as needed):
echo -e "\n\n#------------------------#"
echo "Binding test execution using:"
echo "${srunCommand} ${theHello} | sort -n"
${srunCommand} ${theHello} | sort -n

#----
#Check with rocm-smi (comment/uncomment as needed):
echo -e "\n\n#------------------------#"
echo "Check with rocm-smi using:"
echo "${srunCommand} ./$wrapper rocm-smi --showhw | sort -n"
${srunCommand} ./$wrapper rocm-smi --showhw | sort -n

#----
#Execution of First user's own tests (comment/uncomment as needed):
echo -e "\n\n#------------------------#"
echo "First user's own executable (GPU-aware MPI) using:"
echo "${srunCommand} ${theExe1} 1000 16384 16384 | sort -n"
${srunCommand} ${theExe1} 1000 16384 16384 | sort -n

#----
#Execution of Second user's own tests (comment/uncomment as needed):
echo -e "\n\n#------------------------#"
echo "Second user's own executable (CPU MPI) using:"
echo "${srunCommand} ${theExe2} 1000 16384 16384 | sort -n"
${srunCommand} ${theExe2} 1000 16384 16384 | sort -n

#----
#Finalising
rm -f ./$wrapper #deleting the wrapper
echo -e "\n\n#------------------------#"
echo "Done"
