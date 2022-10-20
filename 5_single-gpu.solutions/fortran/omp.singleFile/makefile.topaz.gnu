# --------------------
# reset this from the command line for easier making of other test code:
CLUSTER=topaz
VENDOR=gnu
#SURNAME=singleTargetBasic
SURNAME=twoTargetsBasic

# --------------------
CC=gcc
F90=gfortran
CFLAGS=-O3 -fopenmp
FFLAGS=-O3 -fopenmp
LIBS=
COMPILER_TAG=-D_GNU_
OBJ=laplace_omp.$(SURNAME).o
TARGET=laplace_omp.$(SURNAME).exe

%.o: %.f90
	$(F90) $(FFLAGS) $(COMPILER_TAG) -c -o $@ $<

$(TARGET): $(OBJ)
	$(F90) $(FFLAGS) $(LIBS) -o $@ $^
	mv $(TARGET) $(CLUSTER).$(VENDOR).$(F90).$(TARGET)

cleanAll:
	rm -f *.exe *.o *.s

cleanThis:
	rm -f $(CLUSTER).$(VENDOR).$(F90).$(TARGET) $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s

leaveThis:
	rm -f $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s
