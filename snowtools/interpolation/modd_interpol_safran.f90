!! Author:
!! Sabine Radanovics
!! Date:
!! 26/10/2020
MODULE MODD_INTERPOL_SAFRAN
    ! Declarations of namelist variables

    IMPLICIT NONE

    LOGICAL :: LMULTIINPUT = .FALSE. ! Set to .TRUE. in order to handle multiple input files
    LOGICAL :: LMULTIOUTPUT = .FALSE. ! Set to .TRUE. in order to handle multiple output files
    LOGICAL :: LTIMECHUNK = .FALSE. ! Set to .TRUE. to write one time step at a time to safe memory when working with larger files
    LOGICAL :: LSPATIALCHUNK = .FALSE. ! Set to .TRUE. to manualy fix chuncksizes for the first and second spatial dimension when writing the output netcdf file(s).
    LOGICAL :: LSELECTVAR = .FALSE. ! Set to .TRUE. to choose variables to interpolate from the input file
    INTEGER :: NNUMBER_INPUT_FILES = 1 ! Number of input files. Used if LMULTIINPUT=.TRUE..
    INTEGER :: NNUMBER_OUTPUT_GRIDS = 1 ! Number of output grid files. Used if LMULTIOUTPUT=.TRUE..
    INTEGER :: NNUMBER_OF_VARIABLES = 1 ! Number of variables to interpolate. Used if LSELECTVAR=.TRUE..
    INTEGER :: NLONCHUNKSIZE = 115 ! Chunksize for the first spatial dimension for writing the output netcdf file(s).
    INTEGER :: NLATCHUNKSIZE = 115 ! Chunksize for the second spatial dimension for writing the output netcdf file(s).
    CHARACTER(LEN=100) :: HFILEIN ! Filename of the input file. Used if LMULTIINPUT=.FALSE.
    CHARACTER(LEN=100) :: HFILEOUT ! Filename of the output file. Used if LMULTIOUTPUT=.FALSE.
    CHARACTER(LEN=100) :: HGRIDIN ! Filename of the grid definition file. Used if LMULTIOUTPUT=.FALSE.
    CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: HFILESIN ! Filenames of the input files. Used if LMULTIINPUT=.TRUE.. The number of filenames given must be equal to NNUMBER_INPUT_FILES.
    CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: HFILESOUT ! Filenames of the output files. Used if LMULTIOUTPUT=.TRUE.. The number of filenames given must be equal to NNUMBER_OUTPUT_GRIDS.
    CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: HGRIDSIN ! Filenames of the grid definition files. Used if LMULTIOUTPUT=.TRUE.. The number of filenames given must be equal to NNUMBER_OUTPUT_GRIDS.
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: HVAR_LIST ! List of variable names of variables to interpolate. Used if LSELECTVAR=.TRUE..

END MODULE MODD_INTERPOL_SAFRAN