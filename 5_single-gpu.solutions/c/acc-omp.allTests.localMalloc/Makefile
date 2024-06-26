#cluster=topaz
#cluster=mulan
#cluster=lumi
cluster=setonix
#cluster=amd

#compiler?=gcc
#compiler?=pgcc
#compiler?=nvc
#compiler?=clang
#compiler?=prg-gnu
compiler?=prg-cray
#compiler?=prg-aocc
#compiler?=rocm-llvm-clang
#compiler?=rocm-amd-clang

CFLAGS=-O3

ifeq ($(cluster),topaz)
	GPU_ARCH=cc70
	GPU_TARGETS=nvptx64-nvidia-cuda
endif
ifeq ($(cluster),mulan)
	GPU_ARCH= #GPU settings are automatic with: "module load craype-accel-amd-gfx908 rocm" 
	GPU_TARGETS= #GPU settings are automatic with: "module load craype-accel-amd-gfx908 rocm" 
	UNAMEP= x86_64
	ROCM_CPUTARGET= $(UNAMEP)-pc-linux-gnu

	ROCM_PATH ?= /opt/rocm-4.5.0
	ROCM_LLVM ?= $(ROCM_PATH)/llvm
	ROCM_GPUTARGET ?= amdgcn-amd-amdhsa

	INSTALLED_GPU  = gfx908
	ROCM_GPU       ?= $(INSTALLED_GPU)
endif
ifeq ($(cluster),lumi)
	GPU_ARCH= #GPU settings are automatic with: "module load craype-accel-amd-gfx90a rocm" 
	GPU_TARGETS= #GPU settings are automatic with: "module load craype-accel-amd-gfx90a rocm" 
endif
ifeq ($(cluster),setonix)
	GPU_ARCH= #GPU settings are automatic with: "module load craype-accel-amd-gfx90a rocm" 
	GPU_TARGETS= #GPU settings are automatic with: "module load craype-accel-amd-gfx90a rocm" 
	UNAMEP= x86_64
	ROCM_CPUTARGET= $(UNAMEP)-pc-linux-gnu

	ROCM_PATH ?= /opt/rocm-5.2.3
	ROCM_LLVM ?= $(ROCM_PATH)/llvm
	ROCM_GPUTARGET ?= amdgcn-amd-amdhsa

	INSTALLED_GPU  = gfx90a
	ROCM_GPU       ?= $(INSTALLED_GPU)
endif
ifeq ($(cluster),amd)
	UNAMEP= $(shell uname -p)
	ROCM_CPUTARGET= $(UNAMEP)-pc-linux-gnu
	ifeq ($(UNAMEP),ppc64le)
	   ROCM_CPUTARGET= ppc64le-linux-gnu
	endif

	ROCM_PATH ?= /opt/rocm
	ROCM_LLVM = $(ROCM_PATH)/llvm
	ROCM_GPUTARGET ?= amdgcn-amd-amdhsa

	INSTALLED_GPU  = $(shell $(ROCM_LLVM)/../bin/rocm_agent_enumerator | grep -m 1 -E gfx[^0]{1})
	ROCM_GPU       ?= $(INSTALLED_GPU)
endif

ifeq ($(compiler),gcc)
	CC=$(compiler)
	LIBS=
	OMPFLAGS=-fopenmp
	ACCFLAGS=-fopenacc
	MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_GCC_
endif
ifeq ($(compiler),prg-gnu)
	CC=cc
	LIBS=
	OMPFLAGS=-fopenmp
	ACCFLAGS=-fopenacc
	MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_GCC_
endif
ifeq ($(compiler),pgcc)
	CC=$(compiler)
	LIBS=
	OMPFLAGS=-mp #No GPU offloading supported yet on pgi, but set anyway
	ACCFLAGS=-acc -Minfo=accel -ta=tesla:$(GPU_ARCH)
	MANFLAGS=-ta=tesla:managed
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	EXEFLAGS=-ta=tesla:$(GPU_ARCH)
	COMPILER_TAG=-D_PGI_
endif
ifeq ($(compiler),nvc)
	CC=$(compiler)
	LIBS=
	OMPFLAGS=-mp=gpu -Minfo=mp -gpu=$(GPU_ARCH)
	ACCFLAGS=-acc -Minfo=acc -gpu=$(GPU_ARCH)
	MANFLAGS=-gpu=managed
	MANFLAGSOMP=-gpu=managed
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_NVC_
endif
ifeq ($(compiler),clang)
	CC=$(compiler)
	LIBS=-lm
	OMPFLAGS=-fopenmp -fopenmp-targets=$(GPU_TARGETS)
	ACCFLAGS=-fopenacc -fopenacc-targets=$(GPU_TARGETS)
	MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_CLANG_
endif
ifeq ($(compiler),prg-cray)
	CC=cc
	LIBS=-lm
	CFLAGS+=-fsave-loopmark
	OMPFLAGS=-fopenmp
	ACCFLAGS=-fopenacc #OpenACC does not work on clang-cray compiler
	MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_CLANG_
endif
ifeq ($(compiler),prg-aocc)
	CC=cc
	LIBS=-lm
	OMPFLAGS=-fopenmp
	ACCFLAGS=-fopenacc #OpenACC does not work on clang-cray compiler
	MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_CLANG_
endif
ifeq ($(compiler),rocm-llvm-clang)
	CC = $(ROCM_LLVM)/bin/clang
	LIBS=-lm
	OMPFLAGS = -target $(ROCM_CPUTARGET) -fopenmp -fopenmp-targets=$(ROCM_GPUTARGET) -Xopenmp-target=$(ROCM_GPUTARGET) -march=$(ROCM_GPU)
	ACCFLAGS=-fopenacc -fopenacc-targets=$(GPU_TARGETS) #Not recognised by clang
	MANFLAGS=-managed #Not recognized by clang, but set to generate an error when tried
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_CLANG_
endif
ifeq ($(compiler),rocm-amd-clang)
	CC = $(ROCM_LLVM)/bin/amdclang
	LIBS=-lm
	OMPFLAGS = -target $(ROCM_CPUTARGET) -fopenmp -fopenmp-targets=$(ROCM_GPUTARGET) -Xopenmp-target=$(ROCM_GPUTARGET) -march=$(ROCM_GPU)
	ACCFLAGS=-fopenacc -fopenacc-targets=$(GPU_TARGETS) #Not recognised by clang
	MANFLAGS=-managed #Not recognized by clang, but set to generate an error when tried
	MANFLAGSOMP=
	STALIBFLAGS=
	DYNLIBFLAGS=-shared -fPIC
	#EXEFLAGS=-fopenacc #-foffload=nvptx-none #-fopenacc #-fopenmp #I need to find the right flag
	EXEFLAGS=$(OMPFLAGS)
	COMPILER_TAG=-D_CLANG_
endif


.PHONY: dynamic
dynamic: laplace_acc-omp.dyn.exe libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so 

laplace_acc-omp.dyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_PRELOADACC_ -c -o $@ $<

functions_cpu.dyn.o: functions_cpu.c
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_omp.dyn.o: functions_omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.dyn.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

libfunctions_cpu.so: functions_cpu.dyn.o
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) -o $@ $<

libfunctions_omp.so: functions_omp.dyn.o
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) -o $@ $<

libfunctions_acc.so: functions_acc.dyn.o
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) -o $@ $<

laplace_acc-omp.dyn.exe: laplace_acc-omp.dyn.o libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_cpu -lfunctions_omp -lfunctions_acc -o $@ $<


#Bad Notes: gcc/11:compilation error: managed memory not working
.PHONY: dynamicManaged
dynamicManaged: laplace_acc-omp.dyn-man.exe libfunctions_cpu-man.so libfunctions_omp-man.so libfunctions_acc-man.so

laplace_acc-omp.dyn-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_PRELOADACC_ -c -o $@ $<

functions_cpu.dyn-man.o: functions_cpu.c
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c -o $@ $<

functions_omp.dyn-man.o: functions_omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -c -o $@ $<

functions_acc.dyn-man.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c -o $@ $<

libfunctions_cpu-man.so: functions_cpu.dyn-man.o
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) -o $@ $<

libfunctions_omp-man.so: functions_omp.dyn-man.o
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(MANFLAGSOMP) -o $@ $<

libfunctions_acc-man.so: functions_acc.dyn-man.o
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(MANFLAGS) -o $@ $<

laplace_acc-omp.dyn-man.exe: laplace_acc-omp.dyn-man.o libfunctions_cpu-man.so libfunctions_omp-man.so libfunctions_acc-man.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGS) -L. -lfunctions_cpu-man -lfunctions_omp-man -lfunctions_acc-man -o $@ $<


.PHONY: dynamicAccPreload
dynamicAccPreload: laplace_acc-omp.dynaccpre.exe libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so 

laplace_acc-omp.dynaccpre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_PRELOADACC_ -c -o $@ $<

laplace_acc-omp.dynaccpre.exe: laplace_acc-omp.dynaccpre.o libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_cpu -lfunctions_omp -lfunctions_acc -o $@ $<


.PHONY: dynamicAccPreloadStructured
dynamicAccPreloadStructured: laplace_acc-omp.dynaccpre-str.exe libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so 

laplace_acc-omp.dynaccpre-str.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) $(ACCFLAGS) -D_PRELOAD_STRUCTURED_ -D_PRELOADACC_ -c -o $@ $<

laplace_acc-omp.dynaccpre-str.exe: laplace_acc-omp.dynaccpre-str.o libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_cpu -lfunctions_omp -lfunctions_acc -o $@ $<


#Bad Notes: topaz:gcc/11:runtime error: libgomp: device already active
.PHONY: dynamicOmpPreload
dynamicOmpPreload: laplace_acc-omp.dynomppre.exe libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so 

laplace_acc-omp.dynomppre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_PRELOADOMP_ -c -o $@ $<

laplace_acc-omp.dynomppre.exe: laplace_acc-omp.dynomppre.o libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_cpu -lfunctions_omp -lfunctions_acc -o $@ $<


.PHONY: dynamicOmpPreloadStructured
dynamicOmpPreloadStructured: laplace_acc-omp.dynomppre-str.exe libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so 

laplace_acc-omp.dynomppre-str.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) $(OMPFLAGS) -D_PRELOAD_STRUCTURED_ -D_PRELOADOMP_ -c -o $@ $<

laplace_acc-omp.dynomppre-str.exe: laplace_acc-omp.dynomppre-str.o libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_cpu -lfunctions_omp -lfunctions_acc -o $@ $<


.PHONY: dynamicNoPreload
dynamicNoPreload: laplace_acc-omp.dynnopre.exe libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so 

laplace_acc-omp.dynnopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_NOPRELOAD_ -c -o $@ $<

laplace_acc-omp.dynnopre.exe: laplace_acc-omp.dynnopre.o libfunctions_cpu.so libfunctions_omp.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_cpu -lfunctions_omp -lfunctions_acc -o $@ $<

#Bad Notes: gcc/11:compilation error: managed memory not working
.PHONY: dynamicNoPreloadManaged
dynamicNoPreloadManaged: laplace_acc-omp.dynnopre-man.exe libfunctions_omp-man.so libfunctions_acc-man.so 

laplace_acc-omp.dynnopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_NOPRELOAD_ -c -o $@ $<

laplace_acc-omp.dynnopre-man.exe: laplace_acc-omp.dynnopre-man.o libfunctions_cpu-man.so libfunctions_omp-man.so libfunctions_acc-man.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGS) -L. -lfunctions_cpu-man -lfunctions_omp-man -lfunctions_acc-man -o $@ $<


.PHONY: libOmpGcc
libOmpGcc: libfunctions_omp.gcc.so

functions_omp.dyngcc.o: functions_omp.c
ifeq ($(CC),gcc)
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif

libfunctions_omp.gcc.so: functions_omp.dyngcc.o
ifeq ($(CC),gcc)
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif


.PHONY: libOmpNvc
libOmpNvc: libfunctions_omp.nvc.so

functions_omp.dynnvc.o: functions_omp.c
ifeq ($(CC),nvc++)
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif

libfunctions_omp.nvc.so: functions_omp.dynnvc.o
ifeq ($(CC),nvc++)
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif


.PHONY: libAccPgi
libAccPgi: libfunctions_acc.pgi.so

functions_acc.dynpgi.o: functions_acc.c
ifeq ($(CC),pgcc)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif

libfunctions_acc.pgi.so: functions_acc.dynpgi.o
ifeq ($(CC),pgcc)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif


.PHONY: libAccGcc
libAccGcc: libfunctions_acc.gcc.so

functions_acc.dyngcc.o: functions_acc.c
ifeq ($(CC),gcc)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif

libfunctions_acc.gcc.so: functions_acc.dyngcc.o
ifeq ($(CC),gcc)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif


.PHONY: libAccNvc
libAccNvc: libfunctions_acc.nvc.so

functions_acc.dynnvc.o: functions_acc.c
ifeq ($(CC),nvc++)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif

libfunctions_acc.nvc.so: functions_acc.dynnvc.o
ifeq ($(CC),nvc++)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC) but, if it already exists, things may have worked)
endif


.PHONY: dynamic2CompilersAccPreload_ompgcc_accpgi
dynamic2CompilersAccPreload_ompgcc_accpgi: laplace_acc-omp.dyn2compaccpre.$(CC)main.ompgcc.accpgi.exe libfunctions_omp.gcc.so libfunctions_acc.pgi.so

laplace_acc-omp.dyn2compaccpre.$(CC)main.ompgcc.accpgi.exe: laplace_acc-omp.dynaccpre.o libfunctions_omp.gcc.so libfunctions_acc.pgi.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.pgi -o $@ $<


.PHONY: dynamic2CompilersAccPreload_ompgcc_accgcc
dynamic2CompilersAccPreload_ompgcc_accgcc: laplace_acc-omp.dyn2compaccpre.$(CC)main.ompgcc.accgcc.exe libfunctions_omp.gcc.so libfunctions_acc.gcc.so

laplace_acc-omp.dyn2compaccpre.$(CC)main.ompgcc.accgcc.exe: laplace_acc-omp.dynaccpre.o libfunctions_omp.gcc.so libfunctions_acc.gcc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.gcc -o $@ $<


.PHONY: dynamic2CompilersOmpPreload_ompgcc_accpgi
dynamic2CompilersOmpPreload_ompgcc_accpgi: laplace_acc-omp.dyn2compomppre.$(CC)main.ompgcc.accpgi.exe libfunctions_omp.gcc.so libfunctions_acc.pgi.so

laplace_acc-omp.dyn2compomppre.$(CC)main.ompgcc.accpgi.exe: laplace_acc-omp.dynomppre.o libfunctions_omp.gcc.so libfunctions_acc.pgi.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.pgi -o $@ $<


.PHONY: dynamic2CompilersOmpPreload_ompgcc_accgcc
dynamic2CompilersOmpPreload_ompgcc_accgcc: laplace_acc-omp.dyn2compomppre.$(CC)main.ompgcc.accgcc.exe libfunctions_omp.gcc.so libfunctions_acc.gcc.so

laplace_acc-omp.dyn2compomppre.$(CC)main.ompgcc.accgcc.exe: laplace_acc-omp.dynomppre.o libfunctions_omp.gcc.so libfunctions_acc.gcc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.gcc -o $@ $<


.PHONY: dynamic2CompilersNoPreload_ompgcc_accpgi
dynamic2CompilersNoPreload_ompgcc_accpgi: laplace_acc-omp.dyn2compnopre.$(CC)main.ompgcc.accpgi.exe libfunctions_omp.gcc.so libfunctions_acc.pgi.so

laplace_acc-omp.dyn2compnopre.$(CC)main.ompgcc.accpgi.exe: laplace_acc-omp.dynnopre.o libfunctions_omp.gcc.so libfunctions_acc.pgi.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.pgi -o $@ $<


.PHONY: dynamic2CompilersNoPreload_ompgcc_accgcc
dynamic2CompilersNoPreload_ompgcc_accgcc: laplace_acc-omp.dyn2compnopre.$(CC)main.ompgcc.accgcc.exe libfunctions_omp.gcc.so libfunctions_acc.gcc.so

laplace_acc-omp.dyn2compnopre.$(CC)main.ompgcc.accgcc.exe: laplace_acc-omp.dynnopre.o libfunctions_omp.gcc.so libfunctions_acc.gcc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.gcc -o $@ $<


.PHONY: dynamic2CompilersJustAcc_accpgi
dynamic2CompilersJustAcc_accpgi: laplace_acc-omp.dyn2compjustacc.$(CC)main.accpgi.exe libfunctions_acc.pgi.so

laplace_acc-omp.dyn2compjustacc.$(CC)main.accpgi.exe: laplace_acc.dyn.o libfunctions_acc.pgi.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_acc.pgi -o $@ $<


.PHONY: dynamic2CompilersJustAcc_accgcc
dynamic2CompilersJustAcc_accgcc: laplace_acc-omp.dyn2compjustacc.$(CC)main.accgcc.exe libfunctions_acc.gcc.so

laplace_acc-omp.dyn2compjustacc.$(CC)main.accgcc.exe: laplace_acc.dyn.o libfunctions_acc.gcc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_acc.gcc -o $@ $<


.PHONY: dynamicNoGPU
dynamicNoGPU: laplace_acc-omp.dynnogpu.exe libfunctions_cpu.so libfunctions_omp_nogpu.so libfunctions_acc_nogpu.so 

functions_omp.dynnogpu.o: functions_omp.c
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.dynnogpu.o: functions_acc.c
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

libfunctions_omp_nogpu.so: functions_omp.dynnogpu.o
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<

libfunctions_acc_nogpu.so: functions_acc.dynnogpu.o
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<

laplace_acc-omp.dynnogpu.exe: laplace_acc-omp.dyn.o libfunctions_cpu.so libfunctions_omp_nogpu.so libfunctions_acc_nogpu.so
	$(CC) $(CFLAGS) $(LIBS) -L. -lfunctions_cpu -lfunctions_omp_nogpu -lfunctions_acc_nogpu -o $@ $<


.PHONY: accDynamic
accDynamic: laplace_acc.dyn.exe libfunctions_cpu.so libfunctions_acc.so 

laplace_acc.dyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $@ $<

laplace_acc.dyn.exe: laplace_acc.dyn.o libfunctions_cpu.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(ACCFLAGS) -L. -lfunctions_cpu -lfunctions_acc -o $@ $<


#Bad Notes: gcc/11: Managed memory is not working
.PHONY: accDynamicManaged
accDynamicManaged: laplace_acc.dyn-man.exe libfunctions_cpu-man.so libfunctions_acc-man.so 

laplace_acc.dyn-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -c -o $@ $<

laplace_acc.dyn-man.exe: laplace_acc.dyn-man.o libfunctions_cpu-man.so libfunctions_acc-man.so
	$(CC) $(CFLAGS) $(ACCFLAGS) $(MANFLAGS) -L. -lfunctions_cpu-man -lfunctions_acc-man -o $@ $<


.PHONY: accDynamicNoPreload
accDynamicNoPreload: laplace_acc.dynnopre.exe libfunctions_cpu.so libfunctions_acc.so 

laplace_acc.dynnopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc.dynnopre.exe: laplace_acc.dynnopre.o libfunctions_cpu.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(ACCFLAGS) -L. -lfunctions_cpu -lfunctions_acc -o $@ $<


.PHONY: accDynamicNoPreloadManaged
accDynamicNoPreloadManaged: laplace_acc.dynnopre-man.exe libfunctions_acc-man.so 

laplace_acc.dynnopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc.dynnopre-man.exe: laplace_acc.dynnopre-man.o libfunctions_acc-man.so
	$(CC) $(CFLAGS) $(ACCFLAGS) $(MANFLAGS) -L. -lfunctions_acc-man -o $@ $<


.PHONY: ompDynamic
ompDynamic: laplace_omp.dyn.exe libfunctions_cpu.so libfunctions_omp.so 

laplace_omp.dyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -c -o $@ $<

laplace_omp.dyn.exe: laplace_omp.dyn.o libfunctions_cpu.so libfunctions_omp.so
	$(CC) $(CFLAGS) $(OMPFLAGS) -L. -lfunctions_cpu -lfunctions_omp -o $@ $<


.PHONY: ompDynamicManaged
ompDynamicManaged: laplace_omp.dyn-man.exe libfunctions_cpu-man.so libfunctions_omp-man.so 

laplace_omp.dyn-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -c -o $@ $<

laplace_omp.dyn-man.exe: laplace_omp.dyn-man.o libfunctions_cpu-man.so libfunctions_omp-man.so
	$(CC) $(CFLAGS) $(OMPFLAGS) $(MANFLAGSOMP) -L. -lfunctions_cpu-man -lfunctions_omp-man -o $@ $<


.PHONY: ompDynamicNoPreload
ompDynamicNoPreload: laplace_omp.dynnopre.exe libfunctions_cpu.so libfunctions_omp.so 

laplace_omp.dynnopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<

laplace_omp.dynnopre.exe: laplace_omp.dynnopre.o libfunctions_cpu.so libfunctions_omp.so
	$(CC) $(CFLAGS) $(OMPFLAGS) -L. -lfunctions_cpu -lfunctions_omp -o $@ $<


.PHONY: ompDynamicNoPreloadManaged
ompDynamicNoPreloadManaged: laplace_omp.dynnopre-man.exe libfunctions_omp-man.so 

laplace_omp.dynnopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<

laplace_omp.dynnopre-man.exe: laplace_omp.dynnopre-man.o libfunctions_omp-man.so
	$(CC) $(CFLAGS) $(OMPFLAGS) $(MANFLAGSOMP) -L. -lfunctions_omp-man -o $@ $<


#Bad Notes: gcc/11: Error during linking, complaining about the mix of omp+acc
.PHONY: static
static: laplace_acc-omp.sta.exe

laplace_acc-omp.sta.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_PRELOADACC_ -c -o $@ $<

functions_cpu.sta.o: functions_cpu.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_omp.sta.o: functions_omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.sta.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

laplace_acc-omp.sta.exe: laplace_acc-omp.sta.o functions_cpu.sta.o functions_omp.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^


#Bad Notes: gcc/11: Error during compilation. Can't do managed memory with gcc. And will also fail to mix.
.PHONY: staticManaged
staticManaged: laplace_acc-omp.sta-man.exe

laplace_acc-omp.sta-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_PRELOADACC_ -c -o $@ $<

functions_cpu.sta-man.o: functions_cpu.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -c -o $@ $<

functions_omp.sta-man.o: functions_omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -c -o $@ $<

functions_acc.sta-man.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c -o $@ $<

laplace_acc-omp.sta-man.exe: laplace_acc-omp.sta-man.o functions_cpu.sta-man.o functions_omp.sta-man.o functions_acc.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGS) -o $@ $^


#Bad Notes: gcc/11: Error during linking, complaining about the mix of omp+acc
.PHONY: staticAccPreload
staticAccPreload: laplace_acc-omp.staaccpre.exe

laplace_acc-omp.staaccpre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_PRELOADACC_ -c -o $@ $<

laplace_acc-omp.staaccpre.exe: laplace_acc-omp.staaccpre.o functions_cpu.sta.o functions_omp.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^


#Bad Notes: gcc/11: Error during linking, complaining about the mix of omp+acc
.PHONY: staticOmpPreload
staticOmpPreload: laplace_acc-omp.staomppre.exe

laplace_acc-omp.staomppre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_PRELOADOMP_ -c -o $@ $<

laplace_acc-omp.staomppre.exe: laplace_acc-omp.staomppre.o functions_cpu.sta.o functions_omp.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^


#Bad Notes: gcc/11: Error during linking, complaining about the mix of omp+acc
.PHONY: staticNoPreload
staticNoPreload: laplace_acc-omp.stanopre.exe

laplace_acc-omp.stanopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_NOPRELOAD_ -c -o $@ $<

laplace_acc-omp.stanopre.exe: laplace_acc-omp.stanopre.o functions_cpu.sta.o functions_omp.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^


.PHONY: staticNoGPU
staticNoGPU: laplace_acc-omp.stanogpu.exe

laplace_acc-omp.stanogpu.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_NOPRELOAD_ -c -o $@ $<

functions_cpu.stanogpu.o: functions_cpu.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_omp.stanogpu.o: functions_omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.stanogpu.o: functions_acc.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

laplace_acc-omp.stanogpu.exe: laplace_acc-omp.stanogpu.o functions_cpu.stanogpu.o functions_omp.stanogpu.o functions_acc.stanogpu.o
	$(CC) $(CFLAGS) $(LIBS) -o $@ $^


.PHONY: accStatic
accStatic: laplace_acc.sta.exe

laplace_acc.sta.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $@ $<

laplace_acc.sta.exe: laplace_acc.sta.o functions_cpu.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) -o $@ $^


#Bad Notes: gcc/11: Error during compilation. Can't do managed memory with gcc
.PHONY: accStaticManaged
accStaticManaged: laplace_acc.sta-man.exe

laplace_acc.sta-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -c -o $@ $<

laplace_acc.sta-man.exe: laplace_acc.sta-man.o functions_cpu.sta-man.o functions_acc.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) $(MANFLAGS) -o $@ $^


.PHONY: accStaticNoPreload
accStaticNoPreload: laplace_acc.stanopre.exe

laplace_acc.stanopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc.stanopre.exe: laplace_acc.stanopre.o functions_cpu.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) -o $@ $^


.PHONY: accStaticNoPreloadManaged
accStaticNoPreloadManaged: laplace_acc.stanopre-man.exe

laplace_acc.stanopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc.stanopre-man.exe: laplace_acc.stanopre-man.o functions_cpu.sta-man.o functions_acc.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) $(MANFLAGS) -o $@ $^


.PHONY: ompStatic
ompStatic: laplace_omp.sta.exe

laplace_omp.sta.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -c -o $@ $<

laplace_omp.sta.exe: laplace_omp.sta.o functions_cpu.sta.o functions_omp.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) -o $@ $^


#Bad Notes: gcc/11: Error during compilation. Can't do managed memory with gcc
.PHONY: ompStaticManaged
ompStaticManaged: laplace_omp.sta-man.exe

laplace_omp.sta-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -c -o $@ $<

laplace_omp.sta-man.exe: laplace_omp.sta-man.o functions_cpu.sta-man.o functions_omp.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) $(MANFLAGSOMP) -o $@ $^


.PHONY: ompStaticNoPreload
ompStaticNoPreload: laplace_omp.stanopre.exe

laplace_omp.stanopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<

laplace_omp.stanopre.exe: laplace_omp.stanopre.o functions_cpu.sta.o functions_omp.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) -o $@ $^


#Bad Notes: gcc/11: Error during compilation. Can't do managed memory with gcc
.PHONY: ompStaticNoPreloadManaged
ompStaticNoPreloadManaged: laplace_omp.stanopre-man.exe

laplace_omp.stanopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<

laplace_omp.stanopre-man.exe: laplace_omp.stanopre-man.o functions_cpu.sta-man.o functions_omp.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) $(MANFLAGSOMP) -o $@ $^

.PHONY: averageInternalStatic
averageInternalStatic: laplace_acc-omp.aveista.exe

laplace_acc-omp.aveista.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.aveista.exe: laplace_acc-omp.aveista.o functions_omp.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^

.PHONY: averageInternalDynamic
averageInternalDynamic: laplace_acc-omp.aveidyn.exe

laplace_acc-omp.aveidyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.aveidyn.exe: laplace_acc-omp.aveidyn.o libfunctions_omp.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -o $@ $<

.PHONY: averageInternalDynamicNvc
averageInternalDynamicNvc: laplace_acc-omp.aveidynnvc.exe

laplace_acc-omp.aveidynnvc.exe: laplace_acc-omp.aveidyn.o libfunctions_omp.nvc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.nvc -o $@ $<

.PHONY: averageNoGPUInternalDynamic
averageNoGPUInternalDynamic: laplace_acc-omp.avenogpuidyn.exe

laplace_acc-omp.avenogpuidyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.avenogpuidyn.exe: laplace_acc-omp.avenogpuidyn.o libfunctions_omp.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -o $@ $<

.PHONY: updateInternalStatic
updateInternalStatic: laplace_acc-omp.updista.exe

laplace_acc-omp.updista.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_UPDATE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.updista.exe: laplace_acc-omp.updista.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) -o $@ $^

.PHONY: updateInternalDynamic
updateInternalDynamic: laplace_acc-omp.updidyn.exe

laplace_acc-omp.updidyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_UPDATE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.updidyn.exe: laplace_acc-omp.updidyn.o libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) $(OMPFLAGS) -L. -lfunctions_acc -o $@ $<

.PHONY: updateNoGPUInternalDynamic
updateNoGPUInternalDynamic: laplace_acc-omp.updnogpuidyn.exe

laplace_acc-omp.updnogpuidyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_UPDATE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.updnogpuidyn.exe: laplace_acc-omp.updnogpuidyn.o libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_acc -o $@ $<


.PHONY: allInternalAcc
allInternalAcc: laplace_acc.alli.exe

laplace_acc.alli.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTACC_ -c -o $@ $<

laplace_acc.alli.exe: laplace_acc.alli.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) -o $@ $^


.PHONY: allInternalAccPreloadStructured
allInternalAccPreloadStructured: laplace_acc.alli-str.exe

laplace_acc.alli-str.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_PRELOAD_STRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTACC_ -c -o $@ $<

laplace_acc.alli-str.exe: laplace_acc.alli-str.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) -o $@ $^


.PHONY: allInternalAccNoPreload
allInternalAccNoPreload: laplace_acc.allinopre.exe

laplace_acc.allinopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc.allinopre.exe: laplace_acc.allinopre.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) -o $@ $^


.PHONY: allInternalAccManaged
allInternalAccManaged: laplace_acc.alli-man.exe

laplace_acc.alli-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTACC_ -c -o $@ $<

laplace_acc.alli-man.exe: laplace_acc.alli-man.o functions_cpu.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(MANFLAGS) -o $@ $^


.PHONY: allInternalAccNoPreloadManaged
allInternalAccNoPreloadManaged: laplace_acc.allinopre-man.exe

laplace_acc.allinopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_ALL_INTERNAL_ -D_JUSTACC_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc.allinopre-man.exe: laplace_acc.allinopre-man.o functions_cpu.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(MANFLAGS) -o $@ $^


.PHONY: allInternalOmp
allInternalOmp: laplace_omp.alli.exe

laplace_omp.alli.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTOMP_ -c -o $@ $<

laplace_omp.alli.exe: laplace_omp.alli.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) -o $@ $^


.PHONY: allInternalOmpPreloadStructured
allInternalOmpPreloadStructured: laplace_omp.alli-str.exe

laplace_omp.alli-str.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_PRELOAD_STRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTOMP_ -c -o $@ $<

laplace_omp.alli-str.exe: laplace_omp.alli-str.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) -o $@ $^


.PHONY: allInternalOmpNoPreload
allInternalOmpNoPreload: laplace_omp.allinopre.exe

laplace_omp.allinopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<

laplace_omp.allinopre.exe: laplace_omp.allinopre.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) -o $@ $^


.PHONY: allInternalOmpManaged
allInternalOmpManaged: laplace_omp.alli-man.exe

laplace_omp.alli-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -D_JUSTOMP_ -c -o $@ $<

laplace_omp.alli-man.exe: laplace_omp.alli-man.o functions_cpu.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) $(MANFLAGSOMP) -o $@ $^


.PHONY: allInternalOmpNoPreloadManaged
allInternalOmpNoPreloadManaged: laplace_omp.allinopre-man.exe

laplace_omp.allinopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_ALL_INTERNAL_ -D_JUSTOMP_ -D_NOPRELOAD_ -c -o $@ $<

laplace_omp.allinopre-man.exe: laplace_omp.allinopre-man.o functions_cpu.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) $(MANFLAGSOMP) -o $@ $^


.PHONY: allInternal
allInternal: laplace_acc-omp.alli.exe

laplace_acc-omp.alli.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -c -o $@ $<

laplace_acc-omp.alli.exe: laplace_acc-omp.alli.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^


.PHONY: allInternalManaged
allInternalManaged: laplace_acc-omp.alli-man.exe

laplace_acc-omp.alli-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_PRELOAD_UNSTRUCTURED_ -D_ALL_INTERNAL_ -c -o $@ $<

laplace_acc-omp.alli-man.exe: laplace_acc-omp.alli-man.o functions_cpu.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGSOMP) -o $@ $^


.PHONY: allInternalNoPreload
allInternalNoPreload: laplace_acc-omp.allinopre.exe

laplace_acc-omp.allinopre.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc-omp.allinopre.exe: laplace_acc-omp.allinopre.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^


.PHONY: allInternalNoPreloadManaged
allInternalNoPreloadManaged: laplace_acc-omp.allinopre-man.exe

laplace_acc-omp.allinopre-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(OMPFLAGS) $(COMPILER_TAG) $(MANFLAGSOMP) -D_ALL_INTERNAL_ -D_NOPRELOAD_ -c -o $@ $<

laplace_acc-omp.allinopre-man.exe: laplace_acc-omp.allinopre-man.o functions_cpu.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) $(MANFLAGSOMP) -o $@ $^


.PHONY: allInternalNoGPU
allInternalNoGPU: laplace_acc-omp.allinogpu.exe

laplace_acc-omp.allinogpu.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -c -o $@ $<

laplace_acc-omp.allinogpu.exe: laplace_acc-omp.allinogpu.o functions_cpu.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(COMPILER_TAG) -o $@ $^

.PHONY: cleanAll
cleanAll:
	rm -f *.exe *.so *.o *.lst

.PHONY: keepLibs
keepLibs:
	-rename so sokeep lib*gcc*.so
	-rename so sokeep lib*pgi*.so
	-rm -f *.exe *.so *.o
	-rename sokeep so lib*.sokeep

