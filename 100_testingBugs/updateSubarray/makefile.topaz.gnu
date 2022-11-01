FC=gfortran
FFLAGS=-O3 -fopenmp
LIBS=
OBJ=updateSubarray.o
TARGET=updateSubarray

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

$(TARGET): $(OBJ)
	$(FC) $(FFLAGS) $(LIBS) -o $@ $^

clean:
	rm -f $(TARGET) *.o
