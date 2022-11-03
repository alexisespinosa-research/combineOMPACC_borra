# --------------------
# reset this from the command line for easier making of other test code:
CLUSTER=joey
VENDOR=gnu
#MPISURNAME=noblock_mpiHOST
MPISURNAME=noblock_mpiGPU
#SURNAME=singleTargetBasic
SURNAME=twoTargetsBasic

# --------------------
GPU_ARCH=gfx90a
ROCM_PATH ?= /opt/rocm
ROCM_LLVM = $(ROCM_PATH)/llvm
ROCM_GPUTARGET ?= amdgcn-amd-amdhsa

INSTALLED_GPU = $(GPU_ARCH)
ROCM_GPU ?= $(INSTALLED_GPU)

ifeq ($(TARGETS),)
        TARGETS =-march=$(ROCM_GPU)
endif

# --------------------
CC=cc
F90=ftn
CFLAGS=-O3 -fopenmp -foffload=$(ROCM_GPUTARGET) $(TARGETS)
#FFLAGS=-O3 -fopenmp -foffload=$(ROCM_GPUTARGET) $(TARGETS)
FFLAGS=-O3 -fopenmp -foffload=default $(TARGETS)
INCLUDE=-I${ROCM_PATH}/include
LIBS=-L${ROCM_PATH}/lib -lamdhip64
COMPILER_TAG=-D_GNU_
OBJ=laplace_omp.$(MPISURNAME).$(SURNAME).o
TARGET=laplace_omp.$(MPISURNAME).$(SURNAME).exe

%.o: %.f90
	$(F90) $(FFLAGS) $(INCLUDE) $(COMPILER_TAG) -c -o $@ $<

$(TARGET): $(OBJ)
	$(F90) $(FFLAGS) $(INCLUDE) $(LIBS) -o $@ $^
	mv $(TARGET) $(CLUSTER).$(VENDOR).$(F90).$(TARGET)

cleanAll:
	rm -f *.exe *.o *.s *.lst

cleanThis:
	rm -f $(CLUSTER).$(VENDOR).$(F90).$(TARGET) $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s

leaveThis:
	rm -f $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s
