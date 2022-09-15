CC=gcc
#CC=pgcc
CFLAGS=-O3
LIBS=
ifeq ($(CC),gcc)
	OMPFLAGS=-fopenmp
	ACCFLAGS=-fopenacc
	MANFLAGS=-managed #Not recognized by gcc, but set to generate an error when tried
	STALIBFLAGS=
   DYNLIBFLAGS=-shared -fPIC
	EXEFLAGS=
	COMPILER_TAG=-D_GCC_
endif
ifeq ($(CC),pgcc)
	OMPFLAGS=-mp #No GPU offloading supported yet on pgi, but set anyway
	ACCFLAGS=-acc -Minfo=accel -ta=tesla:cc70
	MANFLAGS=-ta=tesla:managed
	STALIBFLAGS=
   DYNLIBFLAGS=-shared -fPIC
   EXEFLAGS=-ta=tesla:cc70
	COMPILER_TAG=-D_PGI_
endif

.PHONY: dynamic
dynamic: laplace_acc-omp.dyn.exe libfunctions_omp.so libfunctions_acc.so 

laplace_acc-omp.dyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_omp.dyn.o: functions_omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.dyn.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

libfunctions_omp.so: functions_omp.dyn.o
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<

libfunctions_acc.so: functions_acc.dyn.o
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<

laplace_acc-omp.dyn.exe: laplace_acc-omp.dyn.o libfunctions_omp.so libfunctions_acc.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp -lfunctions_acc -o $@ $<

.PHONY: libOmpGcc
libOmpGcc: libfunctions_omp.gcc.so

functions_omp.dyngcc.o: functions_omp.c
ifeq ($(CC),gcc)
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC))
endif

libfunctions_omp.gcc.so: functions_omp.dyngcc.o
ifeq ($(CC),gcc)
	$(CC) $(CFLAGS) $(OMPFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC))
endif

.PHONY: libAccPgi
libAccPgi: libfunctions_acc.pgi.so

functions_acc.dynpgi.o: functions_acc.c
ifeq ($(CC),pgcc)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC))
endif

libfunctions_acc.pgi.so: functions_acc.dynpgi.o
ifeq ($(CC),pgcc)
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<
else
	$(info $@ cannot be built with current compiler: $(CC))
endif

.PHONY: dynamic2Compilers
dynamic2Compilers: laplace_acc-omp.dyn2comp.$(CC)main.exe libfunctions_omp.gcc.so libfunctions_acc.pgi.so 

laplace_acc-omp.dyn2comp.$(CC)main.exe: laplace_acc-omp.dyn.o libfunctions_omp.gcc.so libfunctions_acc.pgi.so
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -L. -lfunctions_omp.gcc -lfunctions_acc.pgi -o $@ $<

.PHONY: dynamicNoGPU
dynamicNoGPU: laplace_acc-omp.dynnogpu.exe libfunctions_omp_nogpu.so libfunctions_acc_nogpu.so 

functions_omp.dynnogpu.o: functions_omp.c
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.dynnogpu.o: functions_acc.c
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

libfunctions_omp_nogpu.so: functions_omp.dynnogpu.o
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<

libfunctions_acc_nogpu.so: functions_acc.dynnogpu.o
	$(CC) $(CFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) -o $@ $<

laplace_acc-omp.dynnogpu.exe: laplace_acc-omp.dyn.o libfunctions_omp_nogpu.so libfunctions_acc_nogpu.so
	$(CC) $(CFLAGS) $(LIBS) -L. -lfunctions_omp_nogpu -lfunctions_acc_nogpu -o $@ $<

.PHONY: accDynamic
accDynamic: laplace_acc.dyn.exe libfunctions_acc.so 

laplace_acc.dyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $@ $<

laplace_acc.dyn.exe: laplace_acc.dyn.o libfunctions_acc.so
	$(CC) $(CFLAGS) $(EXEFLAGS) -L. -lfunctions_acc -o $@ $<

.PHONY: accDynamicManaged
accDynamicManaged: laplace_acc.dyn-man.exe libfunctions_acc-man.so 

laplace_acc.dyn-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -c -o $@ $<

functions_acc.dyn-man.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c -o $@ $<

libfunctions_acc-man.so: functions_acc.dyn-man.o
	$(CC) $(CFLAGS) $(ACCFLAGS) $(DYNLIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -o $@ $<

laplace_acc.dyn-man.exe: laplace_acc.dyn-man.o libfunctions_acc-man.so
	$(CC) $(CFLAGS) $(EXEFLAGS) $(MANFLAGS) -L. -lfunctions_acc-man -o $@ $<

.PHONY: ompDynamic
ompDynamic: laplace_omp.dyn.exe libfunctions_omp.so 

laplace_omp.dyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -c -o $@ $<

laplace_omp.dyn.exe: laplace_omp.dyn.o libfunctions_omp.so
	$(CC) $(CFLAGS) $(EXEFLAGS) -D_JUSTOMP_ -L. -lfunctions_omp -o $@ $<

.PHONY: static
static: laplace_acc-omp.sta.exe

laplace_acc-omp.sta.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_omp.sta.o: functions_omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.sta.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

laplace_acc-omp.sta.exe: laplace_acc-omp.sta.o functions_omp.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(EXEFLAGS) -o $@ $^

.PHONY: staticNoGPU
staticNoGPU: laplace_acc-omp.stanogpu.exe

laplace_acc-omp.stanogpu.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_omp.stanogpu.o: functions_omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

functions_acc.stanogpu.o: functions_acc.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -c -o $@ $<

laplace_acc-omp.stanogpu.exe: laplace_acc-omp.stanogpu.o functions_omp.stanogpu.o functions_acc.stanogpu.o
	$(CC) $(CFLAGS) $(LIBS) -o $@ $^

.PHONY: accStatic
accStatic: laplace_acc.sta.exe

laplace_acc.sta.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTACC_ -c -o $@ $<

laplace_acc.sta.exe: laplace_acc.sta.o functions_acc.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) -o $@ $^

.PHONY: accStaticManaged
accStaticManaged: laplace_acc.sta-man.exe

laplace_acc.sta-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_JUSTACC_ -c -o $@ $<

functions_acc.sta-man.o: functions_acc.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) $(MANFLAGS) -c -o $@ $<

laplace_acc.sta-man.exe: laplace_acc.sta-man.o functions_acc.sta-man.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(STALIBFLAGS) $(MANFLAGS) -o $@ $^

.PHONY: ompStatic
ompStatic: laplace_omp.sta.exe

laplace_omp.sta.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(STALIBFLAGS) $(COMPILER_TAG) -D_JUSTOMP_ -c -o $@ $<

laplace_omp.sta.exe: laplace_omp.sta.o functions_omp.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) $(STALIBFLAGS) -o $@ $^

.PHONY: averageInternalStatic
averageInternalStatic: laplace_acc-omp.aveista.exe

laplace_acc-omp.aveista.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.aveista.exe: laplace_acc-omp.aveista.o functions_omp.sta.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) -o $@ $^

.PHONY: averageInternalDynamic
averageInternalDynamic: laplace_acc-omp.aveidyn.exe

laplace_acc-omp.aveidyn.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_AVERAGE_INTERNAL_ -c -o $@ $<

laplace_acc-omp.aveidyn.exe: laplace_acc-omp.aveidyn.o libfunctions_omp.so
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) -L. -lfunctions_omp -o $@ $<

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
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_JUSTACC_ -c -o $@ $<

laplace_acc.alli.exe: laplace_acc.alli.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) -o $@ $^

.PHONY: allInternalAccManaged
allInternalAccManaged: laplace_acc.alli-man.exe

laplace_acc.alli-man.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(ACCFLAGS) $(COMPILER_TAG) $(MANFLAGS) -D_ALL_INTERNAL_ -D_JUSTACC_ -c -o $@ $<

laplace_acc.alli-man.exe: laplace_acc.alli-man.o
	$(CC) $(CFLAGS) $(LIBS) $(ACCFLAGS) $(MANFLAGS) -o $@ $^

.PHONY: allInternalOmp
allInternalOmp: laplace_omp.alli.exe

laplace_omp.alli.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(OMPFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -D_JUSTOMP_ -c -o $@ $<

laplace_omp.alli.exe: laplace_omp.alli.o
	$(CC) $(CFLAGS) $(LIBS) $(OMPFLAGS) -o $@ $^

.PHONY: allInternalNoGPU
allInternalNoGPU: laplace_acc-omp.allinogpu.exe

laplace_acc-omp.allinogpu.o: laplace_acc-omp.c
	$(CC) $(CFLAGS) $(COMPILER_TAG) -D_ALL_INTERNAL_ -c -o $@ $<

laplace_acc-omp.allinogpu.exe: laplace_acc-omp.allinogpu.o
	$(CC) $(CFLAGS) $(LIBS) $(COMPILER_TAG) -o $@ $^

.PHONY: cleanAll
cleanAll:
	rm -f *.exe *.so *.o

.PHONY: keepLibs
keepLibs:
	-rename so sokeep lib*gcc*.so
	-rename so sokeep lib*pgi*.so
	-rm -f *.exe *.so *.o
	-rename sokeep so lib*.sokeep

