##################
# Les compilations
##################

cd $EXESURFEX
cd ../src
export VER_MPI=MPIAUTO
export OPTLEVEL=O2
export VER_XIOS=0
./configure
. ../conf/profile_surfex-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X0
make clean
make -j 4
make installmaster

export XYZ=""
export SRC_SURFEX=""
export VER_MPI=MPIAUTO
export OPTLEVEL=DEBUG
export VER_XIOS=0
./configure
. ../conf/profile_surfex-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X0
make clean
make -j 4
make installmaster

export XYZ=""
export SRC_SURFEX=""
export VER_MPI=NOMPI
export OPTLEVEL=O2
export VER_XIOS=0
./configure
. ../conf/profile_surfex-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0
make clean
make -j 4
make installmaster

export XYZ=""
export SRC_SURFEX=""
export VER_MPI=NOMPI
export OPTLEVEL=DEBUG
export VER_XIOS=0
./configure
. ../conf/profile_surfex-LXgfortran-SFX-V9-0-0-NOMPI-OMP-DEBUG-X0
make clean
make -j 4
make installmaster

export XYZ=""
export SRC_SURFEX=""
export VER_MPI=MPIAUTO
export OPTLEVEL=O2
export VER_XIOS=2
./configure
. ../conf/profile_surfex-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X2
make clean
make -j 4
make installmaster

export XYZ=""
export SRC_SURFEX=""
export VER_MPI=MPIAUTO
export OPTLEVEL=DEBUG
export VER_XIOS=2
./configure
. ../conf/profile_surfex-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X2
make clean
make -j 4
make installmaster


##################
# Les tests
##################
export NOFFLINE=2
cd $EXESURFEX
unlink OFFLINE
unlink PREP
unlink PGD
unlink SODA
ln -s OFFLINE-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X0 OFFLINE
ln -s PREP-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X0 PREP
ln -s PGD-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X0 PGD
ln -s SODA-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X0 SODA
cd $SNOWTOOLS_CEN
cd snowtools/tests/
python test_dev_surfex.py

cd $EXESURFEX
unlink OFFLINE
unlink PREP
unlink PGD
unlink SODA
ln -s OFFLINE-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X0 OFFLINE
ln -s PREP-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X0 PREP
ln -s PGD-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X0 PGD
ln -s SODA-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X0 SODA
cd $SNOWTOOLS_CEN
cd snowtools/tests/
python test_dev_surfex.py

cd $EXESURFEX
unlink OFFLINE
unlink PREP
unlink PGD
unlink SODA
ln -s OFFLINE-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 OFFLINE
ln -s PREP-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 PREP
ln -s PGD-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 PGD
ln -s SODA-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 SODA
cd $SNOWTOOLS_CEN
cd snowtools/tests/
python test_dev_surfex.py

cd $EXESURFEX
unlink OFFLINE
unlink PREP
unlink PGD
unlink SODA
ln -s OFFLINE-LXgfortran-SFX-V9-0-0-NOMPI-OMP-DEBUG-X0 OFFLINE
ln -s PREP-LXgfortran-SFX-V9-0-0-NOMPI-OMP-DEBUG-X0 PREP
ln -s PGD-LXgfortran-SFX-V9-0-0-NOMPI-OMP-DEBUG-X0 PGD
ln -s SODA-LXgfortran-SFX-V9-0-0-NOMPI-OMP-DEBUG-X0 SODA
cd $SNOWTOOLS_CEN
cd snowtools/tests/
python test_dev_surfex.py

cd $EXESURFEX
unlink OFFLINE
unlink PREP
unlink PGD
unlink SODA
ln -s OFFLINE-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X2 OFFLINE
ln -s PREP-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X2 PREP
ln -s PGD-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X2 PGD
ln -s SODA-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-O2-X2 SODA
cd $SNOWTOOLS_CEN
cd snowtools/tests/
python test_dev_surfex.py

cd $EXESURFEX
unlink OFFLINE
unlink PREP
unlink PGD
unlink SODA
ln -s OFFLINE-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X2 OFFLINE
ln -s PREP-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X2 PREP
ln -s PGD-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X2 PGD
ln -s SODA-LXgfortran-SFX-V9-0-0-MPIAUTO-OMP-DEBUG-X2 SODA
cd $SNOWTOOLS_CEN
cd snowtools/tests/
python test_dev_surfex.py

#######################################
# Se remettre en configuration standard
#######################################
cd $EXESURFEX
unlink OFFLINE
unlink PREP
unlink PGD
unlink SODA
ln -s OFFLINE-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 OFFLINE
ln -s PREP-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 PREP
ln -s PGD-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 PGD
ln -s SODA-LXgfortran-SFX-V9-0-0-NOMPI-OMP-O2-X0 SODA



































