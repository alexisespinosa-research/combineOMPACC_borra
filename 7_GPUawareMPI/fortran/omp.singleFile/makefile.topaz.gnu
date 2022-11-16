# --------------------
# reset this from the command line for easier making of other test code:
CLUSTER=topaz
VENDOR=gnu
#MPISURNAME=noblock_mpiHOST
MPISURNAME=noblock_mpiGPU
#SURNAME=singleTargetBasic
SURNAME=twoTargetsBasic

# --------------------
CC=mpicc
F90=mpif90
CFLAGS=-O3 -fopenmp
FFLAGS=-O3 -fopenmp
INCLUDE=
LIBS=
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
