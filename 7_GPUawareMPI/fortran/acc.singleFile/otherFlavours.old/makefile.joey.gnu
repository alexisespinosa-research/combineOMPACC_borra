# --------------------
# reset this from the command line for easier making of other test code:
CLUSTER=joey
VENDOR=gnu
#MPISURNAME=noblock_mpiHOST
MPISURNAME=noblock_mpiGPU
#SURNAME=singleParallelBasic
SURNAME=twoParallelsBasic

# --------------------
CC=cc
F90=ftn
CFLAGS=-O3 -fopenacc 
FFLAGS=-O3 -fopenacc
LIBS=
COMPILER_TAG=-D_GNU_
OBJ=laplace_acc.$(MPISURNAME).$(SURNAME).o
TARGET=laplace_acc.$(MPISURNAME).$(SURNAME).exe

%.o: %.f90
	$(F90) $(FFLAGS) $(COMPILER_TAG) -c -o $@ $<

$(TARGET): $(OBJ)
	$(F90) $(FFLAGS) $(LIBS) -o $@ $^
	mv $(TARGET) $(CLUSTER).$(VENDOR).$(F90).$(TARGET)

cleanAll:
	rm -f *.exe *.o *.s *.lst

cleanThis:
	rm -f $(CLUSTER).$(VENDOR).$(F90).$(TARGET) $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s

leaveThis:
	rm -f $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s
