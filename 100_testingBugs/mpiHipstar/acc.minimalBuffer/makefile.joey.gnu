# --------------------
# reset this from the command line for easier making of other test code:
CLUSTER=joey
VENDOR=gnu
MPISURNAME=
SURNAME=swapBuffer
PRENAME=testing

# --------------------
CC=cc
F90=ftn
CFLAGS=-O3 -fopenacc -cpp
FFLAGS=-O3 -fopenacc -cpp
LIBS=
COMPILER_TAG=-D_GNU_
OBJ=$(PRENAME)_acc.$(SURNAME).o
TARGET=$(PRENAME)_acc.$(SURNAME).exe

%.o: %.f90
	$(F90) $(FFLAGS) $(COMPILER_TAG) -c -o $@ $<

$(TARGET): $(OBJ)
	$(F90) $(FFLAGS) $(LIBS) -o $@ $^
	mv $(TARGET) $(CLUSTER).$(VENDOR).$(F90).$(TARGET)

cleanAll:
	rm -f *.exe *.o *.s *.lst *.mod *.i

cleanThis:
	rm -f $(CLUSTER).$(VENDOR).$(F90).$(TARGET) $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s

leaveThis:
	rm -f $(TARGET) *$(SURNAME)*.o *$(SURNAME)*.s
