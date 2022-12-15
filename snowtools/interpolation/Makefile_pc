export VER_MPI=MPIAUTO
export ARCH=LXgfortran
.SUFFIXES:

ifeq "$(VER_MPI)" "NOMPI"
F90= gfortran -pg
CC = gcc
EXEC1 =interpol_NOMPI
else
F90 = mpif90
CC  = mpicc
EXEC1 =interpol_MPIAUTO
endif

OPT = -O2 -g 
CFLAGS = -c  
LINK = interpol


##############################################################################
#links interne libraries

NETCDF ?= /opt/netcdf4-parallel/netcdf-c-4.4.1.1/lib
NETCDF_FORTRAN_INC ?= /opt/netcdf4-parallel/netcdf-fortran-4.4.4/include
NETCDF_FORTRAN_LIB ?= /opt/netcdf4-parallel/netcdf-fortran-4.4.4/lib
HDF ?= /opt/netcdf4-parallel/hdf5-1.8.19/lib

LDFLAGS=-I${NETCDF_FORTRAN_INC} -Wl,-rpath,${NETCDF_FORTRAN_LIB}:${NETCDF}:${HDF} -L${NETCDF_FORTRAN_LIB} -lnetcdff -L${NETCDF} -lnetcdf -L${HDF} -lhdf5_hl -lhdf5 -fopenmp -Wl,-warn-once -ldl -lrt 


SOURCES = interpolate_safran.f90 modd_interpol_safran.f90 modn_interpol_safran.f90

all:  $(EXEC1)

#modd_interpol_safran.o : modd_interpol_safran.f90
#	$(F90) $(CFLAGS) $(OPT) $< -o $@ $(LDFLAGS)
#interpolate_safran.o : interpolate_safran.f90
#	$(F90) $(CFLAGS) $(OPT) $< -o $@ $(LDFLAGS)

$(EXEC1): $(subst .f90,.o,$(SOURCES)) #interpolate_safran.o
	$(F90) $(OPT) $^ -o $@ $(LDFLAGS)
	ln -fs $(EXEC1) $(LINK)

clean:
	rm -f *.o *.mod *.MOD
	rm -f *~

mrproper: clean
	rm -f $(EXEC1)

%.o : %.f90
	$(F90) $(CFLAGS) $(OPT) $< -o $@ $(LDFLAGS)
	@touch $@


interpolate_safran.o : modn_interpol_safran.o
modn_interpol_safran.o : modd_interpol_safran.o
