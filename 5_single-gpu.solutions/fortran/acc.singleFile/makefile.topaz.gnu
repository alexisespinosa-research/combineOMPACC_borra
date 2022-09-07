CLUSTER=topaz
VENDOR=gnu
CC=gcc
F90=gfortran
CFLAGS=-O3 -fopenacc
FFLAGS=-O3 -fopenacc
LIBS=
COMPILER_TAG=-D_GNU_
SURNAME=singleParallelBasic
OBJ=laplace_acc.$(SURNAME).o
TARGET=laplace_acc.$(SURNAME).exe

%.o: %.f90
	$(F90) $(FFLAGS) $(COMPILER_TAG) -c -o $@ $<

$(TARGET): $(OBJ)
	$(F90) $(FFLAGS) $(LIBS) -o $@ $^
	mv $(TARGET) $(CLUSTER).$(VENDOR).$(F90).$(TARGET)

cleanAll:
	rm -f *.exe *.o

cleanThis:
	rm -f $(CLUSTER).$(VENDOR).$(F90).$(TARGET) $(TARGET) *.$(SURNAME).o

leaveThis:
	rm -f $(TARGET) *.$(SURNAME).o
