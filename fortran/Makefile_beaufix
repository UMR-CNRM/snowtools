F90 = mpiifort
CC  = mpiicc
OPT = -O2 -g -traceback 
CFLAGS = -c  
 
EXEC1 =main
##############################################################################
#links interne libraries

VINTEL        ?= /opt/softs/libraries/ICC16.1.150
CDF_PATH      ?= ${VINTEL}/netcdf-4.4.0#netcdf-4.4.0#netcdf-c-4.4.0_par_giec
HDF_PATH      ?= ${VINTEL}/hdf5-1.8.16_par_thrsaf
SZIP_PATH     ?= ${VINTEL}/szip-2.1
ZLIB_PATH     ?= ${VINTEL}/zlib-1.2.5
INC_NETCDF    ?= -I${CDF_PATH}/include -I${HDF_PATH}/include -I${SZIP_PATH}/include -I${ZLIB_PATH}/include
LIB_NETCDF    ?= -L${CDF_PATH}/lib -lnetcdff -lnetcdf -L${HDF_PATH}/lib -lhdf5_hl -lhdf5  -L${SZIP_PATH}/lib -lsz  -L${ZLIB_PATH}/lib -lz  -lcurl -Wl,-rpath,${CDF_PATH}/lib -Wl,-rpath,$(HDF_PATH)/lib -Wl,-rpath,$(SZIP_PATH)/lib -Wl,-rpath,$(ZLIB_PATH)/lib

LDFLAGS= $(INC_NETCDF) $(LIB_NETCDF)


all:  $(EXEC1)

interpolate_safran.o : interpolate_safran.f90
	$(F90) $(CFLAGS) $(OPT) $< -o $@ $(LDFLAGS)

$(EXEC1): interpolate_safran.o 
	$(F90) $(OPT) $^ -o $@ $(LDFLAGS)


clean:
	rm -f *.o *.mod *.MOD
	rm -f *~

mrproper: clean
	rm -f $(EXEC1)
