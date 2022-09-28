# --------------------
# reset this from the command line for easier making of other test code:
#CLUSTER=topaz
#CLUSTER=mulan
#CLUSTER=lumi
CLUSTER=amd

#VENDOR=gnu
#VENDOR=nvhpc
#VENDOR=cray
VENDOR=rocm

SURNAME=singleTargetBasic

# --------------------
#  General settings
CFLAGS=-O3
FFLAGS=-O3
LIBS=

# --------------------
#  Specific settings for topaz
ifeq ($(CLUSTER),topaz)
	GPU_ARCH=cc70
	ifeq ($(VENDOR),gnu)
		CC=gcc
		FC=gfortran
		OMPFLAGS=-fopenmp
		ACCFLAGS=-fopenacc
		MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
		MANFLAGSOMP=
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
		EXEFLAGS=$(OMPFLAGS)
		COMPILER_TAG=-D_GNU_
	endif
	ifeq ($(VENDOR),pgi)
		CC=pgcc
		FC=pgfortran
		OMPFLAGS=-mp #No GPU offloading supported yet on pgi, but set anyway
		ACCFLAGS=-acc -Minfo=accel -ta=tesla:$(GPU_ARCH)
		MANFLAGS=-ta=tesla:managed
		MANFLAGSOMP=
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		EXEFLAGS=-ta=tesla:$(GPU_ARCH)
		COMPILER_TAG=-D_PGI_
	endif
	ifeq ($(VENDOR),nvhpc)
		CC=nvc++
		FC=nvfortran
		OMPFLAGS=-mp=gpu -Minfo=accel,mp -gpu=$(GPU_ARCH)
		ACCFLAGS=-acc -Minfo=accel -gpu=$(GPU_ARCH)
		MANFLAGS=-gpu=managed
		MANFLAGSOMP=-gpu=managed
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		EXEFLAGS=$(OMPFLAGS)
		COMPILER_TAG=-D_NVHPC_
	endif
endif

# --------------------
#  Specific settings for mulan
ifeq ($(CLUSTER),mulan)
	GPU_ARCH=gfx908
	ifeq ($(VENDOR),cray)
		CC=cc
		FC=ftn
		OMPFLAGS=-homp
		ACCFLAGS=-hacc
		MANFLAGS=
		MANFLAGSOMP=
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
		EXEFLAGS=$(OMPFLAGS)
		COMPILER_TAG=-D_CRAY_
	endif
	ifeq ($(VENDOR),gnu)
		CC=cc
		FC=ftn
		OMPFLAGS=-fopenmp
		ACCFLAGS=-fopenacc
		MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
		MANFLAGSOMP=
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
		EXEFLAGS=$(OMPFLAGS)
		COMPILER_TAG=-D_GNU_
	endif
endif

# --------------------
#  Specific settings for lumi
ifeq ($(CLUSTER),lumi)
	GPU_ARCH=gfx90a
	ifeq ($(VENDOR),cray)
		CC=cc
		FC=ftn
		OMPFLAGS=-homp
		ACCFLAGS=-hacc
		MANFLAGS=
		MANFLAGSOMP=
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
		EXEFLAGS=$(OMPFLAGS)
		COMPILER_TAG=-D_CRAY_
	endif
	ifeq ($(VENDOR),gnu)
		CC=cc
		FC=ftn
		OMPFLAGS=-fopenmp
		ACCFLAGS=-fopenacc
		MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
		MANFLAGSOMP=
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
		EXEFLAGS=$(OMPFLAGS)
		COMPILER_TAG=-D_GNU_
	endif
endif

# --------------------
#  Specific settings for AMDnode
ifeq ($(CLUSTER),amd)
	ROCM_LLVM ?= /opt/rocm/llvm
	ROCM_GPUTARGET ?= amdgcn-amd-amdhsa

	INSTALLED_GPU = $(shell $(ROCM_LLVM)/../bin/rocm_agent_enumerator | grep -m 1 -E gfx[^0]{1})
	ROCM_GPU ?= $(INSTALLED_GPU)

	ifeq ($(TARGETS),)
	   TARGETS =-march=$(ROCM_GPU)
	endif

	LFLAGS =
	ifeq ($(VENDOR),rocm)
		CC = $(ROCM_LLVM)/bin/clang
		FC =$(ROCM_LLVM)/bin/flang
		OMPFLAGS =-fopenmp -fopenmp-targets=$(ROCM_GPUTARGET) -Xopenmp-target=$(ROCM_GPUTARGET) $(TARGETS)
		ACCFLAGS=-hacc
		MANFLAGS=
		MANFLAGSOMP=
		STALIBFLAGS=
		DYNLIBFLAGS=-shared -fPIC
		#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
		EXEFLAGS=$(OMPFLAGS)
		COMPILER_TAG=-D_FLANG_
	endif
endif

# --------------------
#  General tags
FCNAME1=$(subst /, ,$(FC))
FCNAME=$(lastword $(FCNAME1))
CCNAME1=$(subst /, ,$(FC))
CCNAME=$(lastword $(FCNAME1))
tagF=$(CLUSTER).$(VENDOR).$(FCNAME)
tagC=$(CLUSTER).$(VENDOR).$(CCNAME)

# --------------------
#cpu
.PHONY: dynamic
mixHere=acc-omp
tagHere=$(tagF).dynamic

dynamic: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so libfunctions_acc.$(tagHere).so 

globals.o: globals.F90
	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_cpu.$(tagHere).o: functions_cpu.F90 globals.o
	$(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_cpu--sub.$(tagHere).o: functions_cpu--sub.F90 globals.o functions_cpu.$(tagHere).o
	$(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

libfunctions_cpu.$(tagHere).so: functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(DYNLIBFLAGS) $^ -o $@

functions_omp.$(tagHere).o: functions_omp.F90 globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_omp--sub.$(tagHere).o: functions_omp--sub.F90 globals.o functions_omp.$(tagHere).o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

libfunctions_omp.$(tagHere).so: functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $^ -o $@

functions_acc.$(tagHere).o: functions_acc.F90 globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_acc--sub.$(tagHere).o: functions_acc--sub.F90 globals.o functions_acc.$(tagHere).o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

libfunctions_acc.$(tagHere).so: functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $^ -o $@

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so libfunctions_acc.$(tagHere).so
	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamic -lfunctions_omp.$(tagF).dynamic -lfunctions_acc.$(tagF).dynamic -o $@

.PHONY: dynamicManaged
mixHere=acc-omp
tagHere=$(tagF).dynamicManaged

dynamicManaged: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so libfunctions_acc.$(tagHere).so 

functions_cpu.$(tagHere).o: functions_cpu.F90 globals.o
	$(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

functions_cpu--sub.$(tagHere).o: functions_cpu--sub.F90 globals.o functions_cpu.$(tagHere).o
	$(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

libfunctions_cpu.$(tagHere).so: functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(DYNLIBFLAGS) $(MANFLAGS) $^ -o $@

functions_omp.$(tagHere).o: functions_omp.F90 globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -c $< -o $@

functions_omp--sub.$(tagHere).o: functions_omp--sub.F90 globals.o functions_omp.$(tagHere).o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -c $< -o $@

libfunctions_omp.$(tagHere).so: functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(MANFLAGSOMP) $^ -o $@

functions_acc.$(tagHere).o: functions_acc.F90 globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

functions_acc--sub.$(tagHere).o: functions_acc--sub.F90 globals.o functions_acc.$(tagHere).o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

libfunctions_acc.$(tagHere).so: functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(MANFLAGS) $^ -o $@

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so libfunctions_acc.$(tagHere).so
	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamicManaged -lfunctions_omp.$(tagF).dynamicManaged -lfunctions_acc.$(tagF).dynamicManaged -o $@

##!##.PHONY: dynamicAccPreload
##!##dynamicAccPreload: laplace_acc-omp.dynaccpre.exe libfunctions_omp.so libfunctions_acc.so 
##!##
##!##laplace_acc-omp.dynaccpre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_PRELOADACC_ -c -o $@ $<
##!##
##!##laplace_acc-omp.dynaccpre.exe: laplace_acc-omp.dynaccpre.o libfunctions_omp.so libfunctions_acc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -lfunctions_acc -o $@ $<
##!##
##!##.PHONY: dynamicOmpPreload
##!##dynamicOmpPreload: laplace_acc-omp.dynomppre.exe libfunctions_omp.so libfunctions_acc.so 
##!##
##!##laplace_acc-omp.dynomppre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_PRELOADOMP_ -c -o $@ $<
##!##
##!##laplace_acc-omp.dynomppre.exe: laplace_acc-omp.dynomppre.o libfunctions_omp.so libfunctions_acc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -lfunctions_acc -o $@ $<
##!##
##!##.PHONY: dynamicNoPreload
##!##dynamicNoPreload: laplace_acc-omp.dynnopre.exe libfunctions_omp.so libfunctions_acc.so 
##!##
##!##laplace_acc-omp.dynnopre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_acc-omp.dynnopre.exe: laplace_acc-omp.dynnopre.o libfunctions_omp.so libfunctions_acc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -lfunctions_acc -o $@ $<
##!##
##!##.PHONY: dynamicNoPreloadManaged
##!##dynamicNoPreloadManaged: laplace_acc-omp.dynnopre-man.exe libfunctions_omp-man.so libfunctions_acc-man.so 
##!##
##!##laplace_acc-omp.dynnopre-man.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_acc-omp.dynnopre-man.exe: laplace_acc-omp.dynnopre-man.o libfunctions_omp-man.so libfunctions_acc-man.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGS) -L. -lfunctions_omp-man -lfunctions_acc-man -o $@ $<
##!##
##!##.PHONY: libOmpGcc
##!##libOmpGcc: libfunctions_omp.gcc.so
##!##
##!##functions_omp.dyngcc.o: functions_omp.F90
##!##ifeq ($(FC),gcc)
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##libfunctions_omp.gcc.so: functions_omp.dyngcc.o
##!##ifeq ($(FC),gcc)
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##.PHONY: libOmpNvc
##!##libOmpNvc: libfunctions_omp.nvc.so
##!##
##!##functions_omp.dynnvc.o: functions_omp.F90
##!##ifeq ($(FC),nvc++)
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##libfunctions_omp.nvc.so: functions_omp.dynnvc.o
##!##ifeq ($(FC),nvc++)
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##.PHONY: libAccPgi
##!##libAccPgi: libfunctions_acc.pgi.so
##!##
##!##functions_acc.dynpgi.o: functions_acc.F90
##!##ifeq ($(FC),pgcc)
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##libfunctions_acc.pgi.so: functions_acc.dynpgi.o
##!##ifeq ($(FC),pgcc)
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##.PHONY: libAccGcc
##!##libAccGcc: libfunctions_acc.gcc.so
##!##
##!##functions_acc.dyngcc.o: functions_acc.F90
##!##ifeq ($(FC),gcc)
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##libfunctions_acc.gcc.so: functions_acc.dyngcc.o
##!##ifeq ($(FC),gcc)
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##.PHONY: libAccNvc
##!##libAccNvc: libfunctions_acc.nvc.so
##!##
##!##functions_acc.dynnvc.o: functions_acc.F90
##!##ifeq ($(FC),nvc++)
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##libfunctions_acc.nvc.so: functions_acc.dynnvc.o
##!##ifeq ($(FC),nvc++)
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
##!##else
##!##	$(info $@ cannot be built with current compiler: $(FC) but, if it already exists, things may have worked)
##!##endif
##!##
##!##.PHONY: dynamic2CompilersAccPreload_ompgcc_accpgi
##!##dynamic2CompilersAccPreload_ompgcc_accpgi: laplace_acc-omp.dyn2compaccpre.$(FC)main.ompgcc.accpgi.exe libfunctions_omp.gcc.so libfunctions_acc.pgi.so
##!##
##!##laplace_acc-omp.dyn2compaccpre.$(FC)main.ompgcc.accpgi.exe: laplace_acc-omp.dynaccpre.o libfunctions_omp.gcc.so libfunctions_acc.pgi.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.pgi -o $@ $<
##!##
##!##.PHONY: dynamic2CompilersAccPreload_ompgcc_accgcc
##!##dynamic2CompilersAccPreload_ompgcc_accgcc: laplace_acc-omp.dyn2compaccpre.$(FC)main.ompgcc.accgcc.exe libfunctions_omp.gcc.so libfunctions_acc.gcc.so
##!##
##!##laplace_acc-omp.dyn2compaccpre.$(FC)main.ompgcc.accgcc.exe: laplace_acc-omp.dynaccpre.o libfunctions_omp.gcc.so libfunctions_acc.gcc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.gcc -o $@ $<
##!##
##!##.PHONY: dynamic2CompilersOmpPreload_ompgcc_accpgi
##!##dynamic2CompilersOmpPreload_ompgcc_accpgi: laplace_acc-omp.dyn2compomppre.$(FC)main.ompgcc.accpgi.exe libfunctions_omp.gcc.so libfunctions_acc.pgi.so
##!##
##!##laplace_acc-omp.dyn2compomppre.$(FC)main.ompgcc.accpgi.exe: laplace_acc-omp.dynomppre.o libfunctions_omp.gcc.so libfunctions_acc.pgi.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.pgi -o $@ $<
##!##
##!##.PHONY: dynamic2CompilersOmpPreload_ompgcc_accgcc
##!##dynamic2CompilersOmpPreload_ompgcc_accgcc: laplace_acc-omp.dyn2compomppre.$(FC)main.ompgcc.accgcc.exe libfunctions_omp.gcc.so libfunctions_acc.gcc.so
##!##
##!##laplace_acc-omp.dyn2compomppre.$(FC)main.ompgcc.accgcc.exe: laplace_acc-omp.dynomppre.o libfunctions_omp.gcc.so libfunctions_acc.gcc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.gcc -o $@ $<
##!##
##!##.PHONY: dynamic2CompilersNoPreload_ompgcc_accpgi
##!##dynamic2CompilersNoPreload_ompgcc_accpgi: laplace_acc-omp.dyn2compnopre.$(FC)main.ompgcc.accpgi.exe libfunctions_omp.gcc.so libfunctions_acc.pgi.so
##!##
##!##laplace_acc-omp.dyn2compnopre.$(FC)main.ompgcc.accpgi.exe: laplace_acc-omp.dynnopre.o libfunctions_omp.gcc.so libfunctions_acc.pgi.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.pgi -o $@ $<
##!##
##!##.PHONY: dynamic2CompilersNoPreload_ompgcc_accgcc
##!##dynamic2CompilersNoPreload_ompgcc_accgcc: laplace_acc-omp.dyn2compnopre.$(FC)main.ompgcc.accgcc.exe libfunctions_omp.gcc.so libfunctions_acc.gcc.so
##!##
##!##laplace_acc-omp.dyn2compnopre.$(FC)main.ompgcc.accgcc.exe: laplace_acc-omp.dynnopre.o libfunctions_omp.gcc.so libfunctions_acc.gcc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.gcc -o $@ $<
##!##
##!##.PHONY: dynamic2CompilersJustAcc_accpgi
##!##dynamic2CompilersJustAcc_accpgi: laplace_acc-omp.dyn2compjustacc.$(FC)main.accpgi.exe libfunctions_acc.pgi.so
##!##
##!##laplace_acc-omp.dyn2compjustacc.$(FC)main.accpgi.exe: laplace_acc.dyn.o libfunctions_acc.pgi.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_acc.pgi -o $@ $<
##!##
##!##.PHONY: dynamic2CompilersJustAcc_accgcc
##!##dynamic2CompilersJustAcc_accgcc: laplace_acc-omp.dyn2compjustacc.$(FC)main.accgcc.exe libfunctions_acc.gcc.so
##!##
##!##laplace_acc-omp.dyn2compjustacc.$(FC)main.accgcc.exe: laplace_acc.dyn.o libfunctions_acc.gcc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_acc.gcc -o $@ $<
##!##

#cpu
.PHONY: dynamicNoGPU
mixHere=noGPU
tagHere=$(tagF).dynNoGPU
tagDyn=$(tagF).dynamic

dynamicNoGPU: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagDyn).so libfunctions_omp.$(tagHere).so libfunctions_acc.$(tagHere).so

functions_omp.$(tagHere).o: functions_omp.F90 globals.o
	   $(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_omp--sub.$(tagHere).o: functions_omp--sub.F90 globals.o functions_omp.$(tagHere).o
	   $(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

libfunctions_omp.$(tagHere).so: functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o
	   $(FC) $(FFLAGS) $(DYNLIBFLAGS) $^ -o $@

functions_acc.$(tagHere).o: functions_acc.F90 globals.o
	   $(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_acc--sub.$(tagHere).o: functions_acc--sub.F90 globals.o functions_acc.$(tagHere).o
	   $(FC) $(FFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c $< -o $@

libfunctions_acc.$(tagHere).so: functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
	   $(FC) $(FFLAGS) $(DYNLIBFLAGS) $^ -o $@

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagDyn).o functions_cpu--sub.$(tagDyn).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o
	   $(FC) $(FFLAGS) $(COMPILER_TAG) -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagDyn).so libfunctions_omp.$(tagHere).so libfunctions_acc.$(tagHere).so
	   $(FC) $(FFLAGS) $(LIBS) $< -L. -lfunctions_cpu.$(tagF).dynamic -lfunctions_omp.$(tagF).dynNoGPU -lfunctions_acc.$(tagF).dynNoGPU -o $@

#cpu
.PHONY: accDynamic
mixHere=acc
tagHere=$(tagF).dynamic

accDynamic: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagHere).so libfunctions_acc.$(tagHere).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagHere).so libfunctions_acc.$(tagHere).so
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamic -lfunctions_acc.$(tagF).dynamic -o $@

#cpu
.PHONY: accDynamicManaged
mixHere=acc
tagHere=$(tagF).dynamicManaged

accDynamicManaged: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagHere).so libfunctions_acc.$(tagHere).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagHere).so libfunctions_acc.$(tagHere).so
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(MANFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamicManaged -lfunctions_acc.$(tagF).dynamicManaged -o $@

#cpu
.PHONY: accDynamicNoPreload
mixHere=acc
tagHere=$(tagF).dynnopre
tagDyn=$(tagF).dynamic

accDynamicNoPreload: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagDyn).so libfunctions_acc.$(tagDyn).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagDyn).o functions_cpu--sub.$(tagDyn).o functions_acc.$(tagDyn).o functions_acc--sub.$(tagDyn).o globals.o
	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagDyn).so libfunctions_acc.$(tagDyn).so
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamic -lfunctions_acc.$(tagF).dynamic -o $@

#cpu
#Bad Notes: nvhpc/22.7 not improving with managed memory: probably a matter of the arrays
.PHONY: accDynamicNoPreloadManaged
mixHere=acc
tagHere=$(tagF).dynnopreManaged
tagDyn=$(tagF).dynamicManaged

accDynamicNoPreloadManaged: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagDyn).so libfunctions_acc.$(tagDyn).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagDyn).o functions_cpu--sub.$(tagDyn).o functions_acc.$(tagDyn).o functions_acc--sub.$(tagDyn).o globals.o
	$(FC) $(FFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagDyn).so libfunctions_acc.$(tagDyn).so
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(MANFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamicManaged -lfunctions_acc.$(tagF).dynamicManaged -o $@

#cpu
.PHONY: ompDynamic
mixHere=omp
tagHere=$(tagF).dynamic

ompDynamic: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamic -lfunctions_omp.$(tagF).dynamic -o $@

#cpu
.PHONY: ompDynamicManaged
mixHere=omp
tagHere=$(tagF).dynamicManaged

ompDynamicManaged: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTOMP_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagHere).so libfunctions_omp.$(tagHere).so libfunctions_omp.$(tagHere).so
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(MANFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamicManaged -lfunctions_omp.$(tagF).dynamicManaged -o $@

#cpu
.PHONY: ompDynamicNoPreload
mixHere=omp
tagHere=$(tagF).dynnopre
tagDyn=$(tagF).dynamic

ompDynamicNoPreload: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagDyn).so libfunctions_omp.$(tagDyn).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagDyn).o functions_cpu--sub.$(tagDyn).o functions_omp.$(tagDyn).o functions_omp--sub.$(tagDyn).o globals.o
	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagDyn).so libfunctions_omp.$(tagDyn).so
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamic -lfunctions_omp.$(tagF).dynamic -o $@

#cpu
#Bad Notes: nvhpc/22.7 not improving with managed memory: probably a matter of the arrays
.PHONY: ompDynamicNoPreloadManaged
mixHere=omp
tagHere=$(tagF).dynnopreManaged
tagDyn=$(tagF).dynamicManaged

ompDynamicNoPreloadManaged: laplace_$(mixHere).$(tagHere).exe libfunctions_cpu.$(tagDyn).so libfunctions_omp.$(tagDyn).so 

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagDyn).o functions_cpu--sub.$(tagDyn).o functions_omp.$(tagDyn).o functions_omp--sub.$(tagDyn).o globals.o
	$(FC) $(FFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTOMP_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o libfunctions_cpu.$(tagDyn).so libfunctions_omp.$(tagDyn).so
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(MANFLAGS) $< -L. -lfunctions_cpu.$(tagF).dynamicManaged -lfunctions_omp.$(tagF).dynamicManaged -o $@

#cpu
.PHONY: static
mixHere=acc-omp
tagHere=$(tagF).static

static: laplace_$(mixHere).$(tagHere).exe

functions_cpu.$(tagHere).o: functions_cpu.F90 globals.o
	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_cpu--sub.$(tagHere).o: functions_cpu--sub.F90 globals.o functions_cpu.$(tagHere).o
	$(FC) $(FFLAGS) $(STAIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_omp.$(tagHere).o: functions_omp.F90 globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_omp--sub.$(tagHere).o: functions_omp--sub.F90 globals.o functions_omp.$(tagHere).o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_acc.$(tagHere).o: functions_acc.F90 globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c $< -o $@

functions_acc--sub.$(tagHere).o: functions_acc--sub.F90 globals.o functions_acc.$(tagHere).o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c $< -o $@

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $^ -o $@

#cpu
.PHONY: staticManaged
mixHere=acc-omp
tagHere=$(tagF).staticManaged

staticManaged: laplace_$(mixHere).$(tagHere).exe

functions_cpu.$(tagHere).o: functions_cpu.F90 globals.o
	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

functions_cpu--sub.$(tagHere).o: functions_cpu--sub.F90 globals.o functions_cpu.$(tagHere).o
	$(FC) $(FFLAGS) $(STAIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

functions_omp.$(tagHere).o: functions_omp.F90 globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -c $< -o $@

functions_omp--sub.$(tagHere).o: functions_omp--sub.F90 globals.o functions_omp.$(tagHere).o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -c $< -o $@

functions_acc.$(tagHere).o: functions_acc.F90 globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

functions_acc--sub.$(tagHere).o: functions_acc--sub.F90 globals.o functions_acc.$(tagHere).o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o 
	$(FC) $(FFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagHere).o functions_cpu--sub.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGS) $^ -o $@

##!##.PHONY: staticAccPreload
##!##staticAccPreload: laplace_acc-omp.staaccpre.exe
##!##
##!##laplace_acc-omp.staaccpre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_PRELOADACC_ -c -o $@ $<
##!##
##!##laplace_acc-omp.staaccpre.exe: laplace_acc-omp.staaccpre.o functions_omp.sta.o functions_acc.sta.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^
##!##
##!##.PHONY: staticOmpPreload
##!##staticOmpPreload: laplace_acc-omp.staomppre.exe
##!##
##!##laplace_acc-omp.staomppre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_PRELOADOMP_ -c -o $@ $<
##!##
##!##laplace_acc-omp.staomppre.exe: laplace_acc-omp.staomppre.o functions_omp.sta.o functions_acc.sta.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^
##!##
##!##.PHONY: staticNoPreload
##!##staticNoPreload: laplace_acc-omp.stanopre.exe
##!##
##!##laplace_acc-omp.stanopre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
##!##
##!##laplace_acc-omp.stanopre.exe: laplace_acc-omp.stanopre.o functions_omp.sta.o functions_acc.sta.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^
##!##
##!##.PHONY: staticNoGPU
##!##tagHere=$(tagF).staticNoGPU
##!##
##!##staticNoGPU: laplace_acc-omp.$(tagHere).exe
##!##
##!##globals.$(tagHere).o globals.mod: globals.F90
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $*.o $<
##!##
##!##functions_acc.$(tagHere).o: functions_acc.F90 globals.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $*.o $<
##!##
##!##functions_acc--sub.$(tagHere).o: functions_acc--sub.F90 globals.$(tagHere).o functions_acc.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $*.o $<
##!##
##!##functions_omp.$(tagHere).o: functions_omp.F90 globals.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $*.o $<
##!##
##!##functions_omp--sub.$(tagHere).o: functions_omp--sub.F90 globals.$(tagHere).o functions_omp.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $*.o $<
##!##
##!##laplace_acc-omp.$(tagHere).o: laplace_acc-omp.F90 globals.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
##!##
##!##laplace_acc-omp.$(tagHere).exe: laplace_acc-omp.$(tagHere).o globals.$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(LIBS) -o $@ $^

##!##.PHONY: accStatic
##!##mixHere=acc
##!##tagHere=$(tagF).accStatic
##!##
##!##accStatic: laplace_$(mixHere).$(tagHere).exe
##!##
##!##functions_acc.$(tagHere).o: functions_acc.F90 globals.o
##!##	   $(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $*.o $<
##!##
##!##functions_acc--sub.$(tagHere).o: functions_acc--sub.F90 globals.o functions_acc.$(tagHere).o
##!##	   $(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $*.o $<
##!##
##!##laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $@ $<
##!##
##!##laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) -o $@ $^

#cpu
.PHONY: accStatic
mixHere=acc
tagHere=$(tagF).static
tagSta=$(tagF).static

accStatic: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o functions_acc.$(tagHere).o functions_acc--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) $^ -o $@


##!##.PHONY: accStaticManaged
##!##accStaticManaged: laplace_acc.staticManaged.exe
##!##
##!##laplace_acc.staticManaged.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -c -o $@ $<
##!##
##!##laplace_acc.staticManaged.exe: laplace_acc.staticManaged.o functions_acc.staticManaged.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) $(MANFLAGS) -o $@ $^
##!##
##!##.PHONY: accStaticNoPreload
##!##accStaticNoPreload: laplace_acc.stanopre.exe
##!##
##!##laplace_acc.stanopre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_acc.stanopre.exe: laplace_acc.stanopre.o functions_acc.sta.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) -o $@ $^
##!##
##!##.PHONY: accStaticNoPreloadManaged
##!##accStaticNoPreloadManaged: laplace_acc.stanopre-man.exe
##!##
##!##laplace_acc.stanopre-man.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_acc.stanopre-man.exe: laplace_acc.stanopre-man.o functions_acc.staticManaged.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) $(MANFLAGS) -o $@ $^
##!##

#cpu
.PHONY: ompStatic
mixHere=omp
tagHere=$(tagF).static
tagSta=$(tagF).static

ompStatic: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) $^ -o $@

##!##.PHONY: ompStaticManaged
##!##mixHere=omp
##!##tagHere=staticManaged
##!##
##!##ompStaticManaged: laplace_$(mixHere).$(tagHere).exe
##!##
##!##globals.$(tagHere).o: globals.F90
##!##	$(FC) $(FFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -c -o $*.o $<
##!##
##!##functions_omp.$(tagHere).o: functions_omp.F90 globals.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -c -o $*.o $<
##!##
##!##functions_omp--sub.$(tagHere).o: functions_omp--sub.F90 globals.$(tagHere).o functions_omp.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -c -o $*.o $<
##!##
##!##laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -c -o $@ $<
##!##
##!##laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_omp.$(tagHere).o functions_omp--sub.$(tagHere).o globals.$(tagHere).o
##!##	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) $(MANFLAGSOMP) -o $@ $^

##!##.PHONY: ompStaticNoPreload
##!##ompStaticNoPreload: laplace_omp.stanopre.exe
##!##
##!##laplace_omp.stanopre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_omp.stanopre.exe: laplace_omp.stanopre.o functions_omp.sta.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) -o $@ $^
##!##
##!##.PHONY: ompStaticNoPreloadManaged
##!##ompStaticNoPreloadManaged: laplace_omp.stanopre-man.exe
##!##
##!##laplace_omp.stanopre-man.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_omp.stanopre-man.exe: laplace_omp.stanopre-man.o functions_omp.staticManaged.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) $(MANFLAGSOMP) -o $@ $^
##!##
##!##.PHONY: averageInternalStatic
##!##averageInternalStatic: laplace_acc-omp.aveista.exe
##!##
##!##laplace_acc-omp.aveista.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.aveista.exe: laplace_acc-omp.aveista.o functions_omp.sta.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^
##!##
##!##.PHONY: averageInternalDynamic
##!##averageInternalDynamic: laplace_acc-omp.aveidyn.exe
##!##
##!##laplace_acc-omp.aveidyn.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.aveidyn.exe: laplace_acc-omp.aveidyn.o libfunctions_omp.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -o $@ $<
##!##
##!##.PHONY: averageInternalDynamicNvc
##!##averageInternalDynamicNvc: laplace_acc-omp.aveidynnvc.exe
##!##
##!##laplace_acc-omp.aveidynnvc.exe: laplace_acc-omp.aveidyn.o libfunctions_omp.nvc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.nvc -o $@ $<
##!##
##!##.PHONY: averageNoGPUInternalDynamic
##!##averageNoGPUInternalDynamic: laplace_acc-omp.avenogpuidyn.exe
##!##
##!##laplace_acc-omp.avenogpuidyn.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.avenogpuidyn.exe: laplace_acc-omp.avenogpuidyn.o libfunctions_omp.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -o $@ $<
##!##
##!##.PHONY: updateInternalStatic
##!##updateInternalStatic: laplace_acc-omp.updista.exe
##!##
##!##laplace_acc-omp.updista.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_UPDATE_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.updista.exe: laplace_acc-omp.updista.o functions_acc.sta.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) -o $@ $^
##!##
##!##.PHONY: updateInternalDynamic
##!##updateInternalDynamic: laplace_acc-omp.updidyn.exe
##!##
##!##laplace_acc-omp.updidyn.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_UPDATE_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.updidyn.exe: laplace_acc-omp.updidyn.o libfunctions_acc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $(OMPFLAGS) -L. -lfunctions_acc -o $@ $<
##!##
##!##.PHONY: updateNoGPUInternalDynamic
##!##updateNoGPUInternalDynamic: laplace_acc-omp.updnogpuidyn.exe
##!##
##!##laplace_acc-omp.updnogpuidyn.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_UPDATE_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.updnogpuidyn.exe: laplace_acc-omp.updnogpuidyn.o libfunctions_acc.so
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_acc -o $@ $<
##!##

#cpu
.PHONY: allInternalAcc
mixHere=acc
tagHere=$(tagF).allInternal
tagSta=$(tagF).static

allInternalAcc: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTACC_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $^ -o $@

#cpu
.PHONY: allInternalAccPreloadStructured
mixHere=acc
tagHere=$(tagF).allInternalPreStr
tagSta=$(tagF).static

allInternalAccPreloadStructured: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_PRELOAD_STRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTACC_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $^ -o $@

#cpu
.PHONY: allInternalAccNoPreload
mixHere=acc
tagHere=$(tagF).allInternalNoPreload
tagSta=$(tagF).static

allInternalAccNoPreload: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_JUSTACC_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $^ -o $@

##!##.PHONY: allInternalAccManaged
##!##allInternalAccManaged: laplace_acc.alli-man.exe
##!##
##!##laplace_acc.alli-man.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_ALL_INTERNAL_ -D_JUSTACC_ -c -o $@ $<
##!##
##!##laplace_acc.alli-man.exe: laplace_acc.alli-man.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(MANFLAGS) -o $@ $^
##!##

#cpu
#Bad Notes: nvhpc/22.7 not improving with managed memory: probably a matter of the arrays
.PHONY: allInternalAccNoPreloadManaged
mixHere=acc
tagHere=$(tagF).allInternalAccNoPreloadManaged
tagSta=$(tagF).staticManaged

allInternalAccNoPreloadManaged: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(ACCFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_ALL_INTERNAL_ -D_JUSTACC_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(ACCFLAGS) $(MANFLAGS) $^ -o $@

#cpu
.PHONY: allInternalOmp
mixHere=omp
tagHere=$(tagF).allInternalOmp
tagSta=$(tagF).static

allInternalOmp: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTOMP_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $^ -o $@

#cpu
.PHONY: allInternalOmpPreloadStructured
mixHere=omp
tagHere=$(tagF).allInternalOmpPreStr
tagSta=$(tagF).static

allInternalOmpPreloadStructured: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_PRELOAD_STRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTOMP_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $^ -o $@

#cpu
.PHONY: allInternalOmpNoPreload
mixHere=omp
tagHere=$(tagF).allInternalOmpNoPreload
tagSta=$(tagF).static

allInternalOmpNoPreload: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_JUSTOMP_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $^ -o $@

##!##.PHONY: allInternalOmpManaged
##!##allInternalOmpManaged: laplace_omp.alli-man.exe
##!##
##!##laplace_omp.alli-man.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_ALL_INTERNAL_ -D_JUSTOMP_ -c -o $@ $<
##!##
##!##laplace_omp.alli-man.exe: laplace_omp.alli-man.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(MANFLAGSOMP) -o $@ $^
##!##

#cpu
#Bad Notes: nvhpc/22.7 not improving with managed memory: probably a matter of the arrays
.PHONY: allInternalOmpNoPreloadManaged
mixHere=omp
tagHere=$(tagF).allInternalOmpNoPreloadManaged
tagSta=$(tagF).staticManaged

allInternalOmpNoPreloadManaged: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_ALL_INTERNAL_ -D_JUSTOMP_ -D_NOPRELOAD_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(MANFLAGSOMP) $^ -o $@

#cpu
.PHONY: allInternal
mixHere=acc-omp
tagHere=$(tagF).allInternal
tagSta=$(tagF).static

allInternal: laplace_$(mixHere).$(tagHere).exe

laplace_$(mixHere).$(tagHere).o: laplace_acc-omp.F90 functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(OMPFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -c $< -o $@

laplace_$(mixHere).$(tagHere).exe: laplace_$(mixHere).$(tagHere).o functions_cpu.$(tagSta).o functions_cpu--sub.$(tagSta).o globals.o
	$(FC) $(FFLAGS) $(LIBS) $(OMPFLAGS) $(ACCFLAGS) $^ -o $@

##!##.PHONY: allInternalManaged
##!##allInternalManaged: laplace_acc-omp.alli-man.exe
##!##
##!##laplace_acc-omp.alli-man.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_ALL_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.alli-man.exe: laplace_acc-omp.alli-man.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGSOMP) -o $@ $^
##!##
##!##.PHONY: allInternalNoPreload
##!##allInternalNoPreload: laplace_acc-omp.allinopre.exe
##!##
##!##laplace_acc-omp.allinopre.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_acc-omp.allinopre.exe: laplace_acc-omp.allinopre.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^
##!##
##!##.PHONY: allInternalNoPreloadManaged
##!##allInternalNoPreloadManaged: laplace_acc-omp.allinopre-man.exe
##!##
##!##laplace_acc-omp.allinopre-man.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(ACCFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_ALL_INTERNAL_ -D_NOPRELOAD_ -c -o $@ $<
##!##
##!##laplace_acc-omp.allinopre-man.exe: laplace_acc-omp.allinopre-man.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGSOMP) -o $@ $^
##!##
##!##.PHONY: allInternalNoGPU
##!##allInternalNoGPU: laplace_acc-omp.allinogpu.exe
##!##
##!##laplace_acc-omp.allinogpu.o: laplace_acc-omp.F90
##!##	$(FC) $(FFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -c -o $@ $<
##!##
##!##laplace_acc-omp.allinogpu.exe: laplace_acc-omp.allinogpu.o
##!##	$(FC) $(FFLAGS) $(LIBS) $(COMPILER_TAG) -o $@ $^

.PHONY: cleanAll
cleanAll:
	rm -f *.exe *.so *.o *.mod *.smod *.s

.PHONY: keepLibs
keepLibs:
	-rename so sokeep lib*gcc*.so
	-rename so sokeep lib*pgi*.so
	-rm -f *.exe *.so *.o *.mod *.smod *.s
	-rename sokeep so lib*.sokeep

