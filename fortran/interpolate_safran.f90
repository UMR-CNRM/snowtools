PROGRAM INTERPOLATE_SAFRAN
! 
USE NETCDF
USE MODN_INTERPOL_SAFRAN
!
IMPLICIT NONE
include 'mpif.h'
!
!!!!! Files properties
CHARACTER(LEN=100):: HFILENAMEIN, HFILENAMEOUT ,HFILENAMEG     ! Name of the field file.
INTEGER::FILE_ID_IN, FILE_ID_OUT ,FILE_ID_GEO! id of input ,output and geometrie netcdf file
!
!!!!! Dim properties
CHARACTER(LEN=20),DIMENSION(:),ALLOCATABLE::DIM_NAME_IN, DIM_NAME_OUT! name of dimensions
INTEGER,DIMENSION(:),ALLOCATABLE:: DIM_ID_IN, DIM_ID_OUT ! id of dimensions
INTEGER,DIMENSION(:),ALLOCATABLE:: DIM_SIZE_IN, DIM_SIZE_OUT ! size of dimensions
!
!!!!! Var properties
INTEGER,DIMENSION(:),ALLOCATABLE::VAR_ID_IN, VAR_ID_OUT ! id of variables
CHARACTER(LEN=20),DIMENSION(:),ALLOCATABLE::VAR_NAME_IN ! name of variables
INTEGER,DIMENSION(:),ALLOCATABLE::VAR_TYPE_IN ! type of variables
INTEGER,DIMENSION(:),ALLOCATABLE::VAR_NDIMS_IN ! variables domensions
!
INTEGER,DIMENSION(:,:),ALLOCATABLE::VAR_ID_DIMS_IN, VAR_ID_DIMS_OUT,VAR_ID_DIMS_OUT_ADJ ! id of dimensions for each variable
!
INTEGER::INDIM, INVAR ! number of dimensions, variables and attributes of input nc file
!!!!!! ATTRIBUTE PROPERTIES
CHARACTER(LEN=20)::ATT_NAME ! name of attribute
INTEGER,DIMENSION(:),ALLOCATABLE:: INATT !number of attributes for each variables
!
!OUTPUT_GRID_VARIABLES
INTEGER,DIMENSION(:,:),ALLOCATABLE::IMASSIFOUT
DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::ZZSOUT,ZASPECTOUT,ZSLOPEOUT ! Elevation massif and aspect of output grid

DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::ZLATOUT,ZLONOUT,ZXOUT,ZYOUT, ZLATIN,ZLONIN
!
!Grid type
CHARACTER(LEN=2)::GRID_TYPE
INTEGER:: IRANK
INTEGER,DIMENSION(2)::GRID_DIM_REF! lenghts of dimensions
!
!INPUT_GRID_VARIABLES
INTEGER,DIMENSION(:),ALLOCATABLE::IZSIN,IMASSIFIN,IASPECTIN !Elevation massif and aspect of output grid
!
!INDICES parameter
INTEGER,DIMENSION(:,:),ALLOCATABLE::IINDICESBAS,IINDICESHAUT
!
!WORK VEC
INTEGER,DIMENSION(:),ALLOCATABLE:: VALUE_TIME
INTEGER,DIMENSION(:),ALLOCATABLE::  IVARIN
REAL,DIMENSION(:,:),ALLOCATABLE::  ZVARIN
REAL,DIMENSION(:,:,:),ALLOCATABLE::  ZVARINT
REAL ::  ZSCAIN
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZVAROUT2D
REAL,DIMENSION(:,:,:,:),ALLOCATABLE:: ZVAROUTXYT
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZVAROUTXYT1D
!
INTEGER :: NPATCH, NPATCHID
INTEGER :: NDECILE, IDECILEID
!
DOUBLE PRECISION ,DIMENSION(:,:),ALLOCATABLE:: ZVARLATLON
!ID of spatial and time dimension in input file 
INTEGER :: ILLOC_ID, ITIMEID
!
! MPI parameter: number of processors, rank, error code
INTEGER :: COMM, NPROC, PROC_ID, IERR
INTEGER :: NTIME
INTEGER :: NX, NY
INTEGER :: NX_PROC, IXSTART
INTEGER :: NY_PROC, IYSTART
INTEGER :: NPOINT_PROC, IPSTART
!
INTEGER::ID,IV,IA,IT,IDIN,IDOUT,INZ, JINFILE ! loop counter
INTEGER :: IOS ! status indicator
INTEGER :: INAM_UNIT
!
INTEGER :: IMASSIFTODELETE, ILAYERTODELETE, I
LOGICAL :: AFTERLOC
CHARACTER(LEN=10),DIMENSION(4) :: LL_VARNAME
!
REAL,    PARAMETER :: XUNDEF =  -9999999. ! Fill_Value for netcdf
!
COMM = MPI_COMM_WORLD
CALL MPI_INIT(IERR)
CALL MPI_COMM_RANK(COMM, PROC_ID, IERR)
CALL MPI_COMM_SIZE(COMM, NPROC, IERR)
!

!
LL_VARNAME(1)= "LON"
LL_VARNAME(2)= "LAT"
LL_VARNAME(3)= "latitude"
LL_VARNAME(4)= "longitude"
!
HFILENAMEIN ='input.nc'
HFILENAMEG =  'GRID.nc'
HFILENAMEOUT = 'output.nc'

! Open namelist
INAM_UNIT = 21
OPEN(UNIT=INAM_UNIT, ACTION='READ', STATUS='OLD', IOSTAT=IOS, FILE='interpolate_safran.nam')
IF (IOS .EQ. 0) THEN
  CALL READ_NML(INAM_UNIT)
!  PRINT*, 'namelist test ', LMULTIINPUT
!  PRINT*, NNUMBER_INPUT_GRIDS
!  PRINT*, 'HFILEIN ', HFILEIN, 'HGRIDIN ', HGRIDIN
!  PRINT*, 'HFILESIN ', HFILESIN, 'HGRIDSIN ', HGRIDSIN
ELSE
  PRINT*, 'WARNING: no namelist interpolate_safran.nam provided. Continue with default settings.'
  ALLOCATE(HFILESIN(1), HGRIDSIN(1), HFILESOUT(1))
  HFILESIN(1) = HFILENAMEIN
  HGRIDSIN(1) = HFILENAMEG
  HFILESOUT(1) = HFILENAMEOUT
  NNUMBER_INPUT_GRIDS = 1
END IF
CLOSE(INAM_UNIT)

! loop over input files
DO JINFILE = 1,NNUMBER_INPUT_GRIDS
  IMASSIFTODELETE = -1
  ILAYERTODELETE = -1
  NPATCHID = -1
  NPATCH = 1
  IDECILEID = -1
  NDECILE = 1
  ! PRINT*, IMASSIFTODELETE, ILAYERTODELETE, NPATCHID, NPATCH, IDECILEID, NDECILE
!
! Open input file
  CALL CHECK(NF90_OPEN(HFILESIN(JINFILE), IOR(NF90_NOWRITE, NF90_MPIIO), FILE_ID_IN, &
                    comm = COMM, info = MPI_INFO_NULL), &
                    "Cannot open file "//TRIM(HFILESIN(JINFILE)))
!
! Open output file 
  CALL CHECK(NF90_create(HFILESOUT(JINFILE),IOR(IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL),NF90_MPIIO),FILE_ID_OUT, &
                       comm = COMM, info = MPI_INFO_NULL),&
                       "Cannot open file "//TRIM(HFILESOUT(JINFILE)))
!
! Open grid file
  CALL CHECK(NF90_OPEN(HGRIDSIN(JINFILE), IOR(NF90_NOWRITE, NF90_MPIIO), FILE_ID_GEO, &
                     comm = COMM, info = MPI_INFO_NULL),&
                     "Cannot open file"//TRIM(HGRIDSIN(JINFILE)))
!
! Get number of dimensions and variables in the input file
  CALL CHECK(NF90_INQUIRE(FILE_ID_IN, INDIM, INVAR),"Cannot get file informations"//TRIM(HFILENAMEIN))
!
! Allocate descriptors of netcdf input file
!!DIM properties
  ALLOCATE(DIM_NAME_IN(INDIM))
  ALLOCATE(DIM_ID_IN(INDIM))
  ALLOCATE(DIM_SIZE_IN(INDIM))
!!VAR properties
  ALLOCATE(VAR_NAME_IN(INVAR))
  ALLOCATE(VAR_ID_IN(INVAR))
  ALLOCATE(VAR_TYPE_IN(INVAR))
  ALLOCATE(VAR_NDIMS_IN(INVAR))
  ALLOCATE(VAR_ID_DIMS_IN(INDIM, INVAR))
  ALLOCATE(INATT(INVAR))
!
! Allocate descriptors of netcdf output file
!!DIM properties
  ALLOCATE(DIM_NAME_OUT(INDIM+1))
  ALLOCATE(DIM_ID_OUT(INDIM+1))
  ALLOCATE(DIM_SIZE_OUT(INDIM+1))
!! VAR
  ALLOCATE(VAR_ID_OUT(INVAR+2))
  ALLOCATE(VAR_ID_DIMS_OUT(INDIM+1, INVAR+2))
!
  VAR_ID_IN = 0
  VAR_NDIMS_IN = 0
  VAR_ID_DIMS_IN =0
  DIM_ID_OUT = 0
  DIM_SIZE_OUT = 0
  VAR_ID_OUT = 0
  VAR_ID_DIMS_OUT=0

!
! Get dimension names, lengths
  DO ID=1,INDIM
    CALL CHECK(NF90_INQUIRE_DIMENSION(FILE_ID_IN,ID,DIM_NAME_IN(ID),DIM_SIZE_IN(ID)), &
            "Cannot get dim size "//TRIM(HFILENAMEIN))
    ! Get dimensions identifiers of netcdf input file
    CALL CHECK(NF90_INQ_DIMID(FILE_ID_IN,DIM_NAME_IN(ID), DIM_ID_IN(ID)), &
            "Cannot get variables dim ids "//TRIM(HFILENAMEIN))
    !
  ENDDO
!
!NF90_Inquire_Variable  get variable names, types, shapes
  DO IV=1,INVAR
    CALL CHECK(NF90_INQUIRE_VARIABLE(FILE_ID_IN, IV,VAR_NAME_IN(IV)), &
            "Cannot get variables name from ids "//TRIM(HFILENAMEIN))
    VAR_ID_IN(IV)=IV

    ! Get variable metadata
    CALL CHECK(NF90_INQUIRE_VARIABLE(FILE_ID_IN,VAR_ID_IN(IV),VAR_NAME_IN(IV),&
            VAR_TYPE_IN(IV),VAR_NDIMS_IN(IV),VAR_ID_DIMS_IN(:,IV),INATT(IV)) ,&
            "Cannot get metadata for a variable")
  ENDDO
!
! Get interpolation informations
!Read altitude massif numbers and aspects
  CALL READ_OUTPUT_GRID(FILE_ID_GEO,ZZSOUT,IMASSIFOUT,ZASPECTOUT,IRANK, &
          GRID_TYPE,GRID_DIM_REF,ZLATOUT,ZLONOUT,ZYOUT,ZXOUT,&
          ZSLOPEOUT)
!  IF (JINFILE == 2) THEN
!    PRINT*, ZSLOPEOUT
!  END IF
  ! open a file FORCING.nc by massif, altitude, aspect
  CALL READ_NC(FILE_ID_IN,IZSIN,IMASSIFIN,IASPECTIN)
  !
  CALL INDICES2D(ZZSOUT,IMASSIFOUT,ZASPECTOUT,IZSIN,IMASSIFIN,&
          IASPECTIN,IINDICESBAS,IINDICESHAUT)
  !
  IDOUT = 1
  DO ID=1,INDIM
    ! Define dimensions in output file
    IF (DIM_NAME_IN(ID) == 'Number_of_points') THEN
      ! Change spatial dimension
      IF (IRANK==1) THEN
        DIM_SIZE_OUT(IDOUT)=GRID_DIM_REF(1)
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),GRID_DIM_REF(1),DIM_ID_OUT(IDOUT)), &
                "Cannot def dim  "//TRIM(HFILENAMEOUT))
        ILLOC_ID=   DIM_ID_OUT(IDOUT)
        IDOUT =IDOUT+1
      ELSEIF (IRANK==2) THEN
        DIM_SIZE_OUT(IDOUT)=GRID_DIM_REF(2)
        DIM_SIZE_OUT(IDOUT+1)=GRID_DIM_REF(1)
        IF (GRID_TYPE == "LL") THEN
          !
          CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,'lat',DIM_SIZE_OUT(IDOUT),DIM_ID_OUT(IDOUT)), &
                  "Cannot def dim lat  "//TRIM(HFILENAMEOUT))
          CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,'lon',DIM_SIZE_OUT(IDOUT+1),DIM_ID_OUT(IDOUT+1)) , &
                  "Cannot def dim lon  "//TRIM(HFILENAMEOUT))
          !
        ELSEIF (GRID_TYPE == "XY") THEN
          !
          CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,'y',DIM_SIZE_OUT(IDOUT),DIM_ID_OUT(IDOUT)), &
                  "Cannot def dim y  "//TRIM(HFILENAMEOUT))
          CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,'x',DIM_SIZE_OUT(IDOUT+1),DIM_ID_OUT(IDOUT+1)) , &
                  "Cannot def dim x  "//TRIM(HFILENAMEOUT))
          !
        ELSE
          STOP "INCORRECT TYPE OF GRID"
        ENDIF
        ILLOC_ID=   DIM_ID_OUT(IDOUT)
        IDOUT = IDOUT + 2
      ELSE
        STOP 'INCORRECT RANK OF OUTPUT GRID'
      END IF
      !
    ELSEIF (DIM_NAME_IN(ID) == 'time') THEN
      ! Need to to distinguish because of the unlimited dimension
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),NF90_UNLIMITED, &
              DIM_ID_OUT(IDOUT)), "def time out")
      ITIMEID= DIM_ID_OUT(IDOUT)
      IDOUT = IDOUT + 1
      !
    ELSEIF(DIM_NAME_IN(ID) == 'massif') THEN
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
      IMASSIFTODELETE = IDOUT
      IDOUT = IDOUT+1
      !
    ELSEIF(DIM_NAME_IN(ID) == 'snow_layer') THEN
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
      ILAYERTODELETE = IDOUT
      IDOUT = IDOUT+1
    ELSEIF(DIM_NAME_IN(ID) == 'Number_of_Patches') THEN
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
      NPATCH = DIM_SIZE_IN(ID)
      NPATCHID =  IDOUT
      IDOUT = IDOUT+1
    ELSEIF(DIM_NAME_IN(ID) == 'decile') THEN
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
      NDECILE = DIM_SIZE_IN(ID)
      IDECILEID =  IDOUT
      IDOUT = IDOUT+1
    ELSE
      !
      IF (IRANK==1) THEN
        DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
                DIM_ID_OUT(IDOUT)) , "Cannot def dim "//TRIM(HFILENAMEOUT))
        IDOUT = IDOUT + 1
      ELSEIF (IRANK==2) THEN
        DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
                DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
        IDOUT = IDOUT +1
      ELSE
        STOP 'INCORRECT RANK OF OUTPUT GRID'
      ENDIF
    END IF
    !
  END DO
!
  IF (IRANK==1) THEN
    DO IV=1,INVAR
      DO I=1,COUNT(VAR_ID_DIMS_IN(:,IV)/=0)
        ! identify output dimension id and input dimension id
        CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT,DIM_NAME_IN(VAR_ID_DIMS_IN(I,IV)),ID) ,&
                "Cannot get metadata for a variable")
        VAR_ID_DIMS_OUT(I,IV) = ID
      ENDDO
    ENDDO
  ELSE IF (IRANK==2) THEN
    DO IV=1,INVAR
      AFTERLOC = .FALSE.
      DO I=1,COUNT(VAR_ID_DIMS_IN(:,IV)/=0)
        IF (VAR_ID_DIMS_IN(I,IV)/=0) THEN
          IF (DIM_NAME_IN(VAR_ID_DIMS_IN(I,IV)) == 'Number_of_points')THEN
            AFTERLOC= .TRUE.
            VAR_ID_DIMS_OUT(I,IV)= ILLOC_ID+1
            VAR_ID_DIMS_OUT(I+1,IV)= ILLOC_ID
          ELSE
            CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT,DIM_NAME_IN(VAR_ID_DIMS_IN(I,IV)),ID) ,&
                    "Cannot get metadata for a variable")
            IF (AFTERLOC)THEN
              VAR_ID_DIMS_OUT(I+1,IV) = ID
            ELSE
              VAR_ID_DIMS_OUT(I,IV) = ID
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDIF
! 
  IF(IRANK==2) THEN
    IF (GRID_TYPE == "LL") THEN
      CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,"LAT",NF90_DOUBLE,&
              DIM_ID_OUT(ILLOC_ID),VAR_ID_OUT(INVAR+1)),"Cannot def var latitude")
      !
      CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,"LON",NF90_DOUBLE,&
              DIM_ID_OUT(ILLOC_ID+1),VAR_ID_OUT(INVAR+2)),"Cannot def var longitude")
    ELSEIF (GRID_TYPE == "XY") THEN
      !
      CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,"y",NF90_DOUBLE,&
              DIM_ID_OUT(ILLOC_ID),VAR_ID_OUT(INVAR+1)),"Cannot def var y")
      !
      CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,"x",NF90_DOUBLE,&
              DIM_ID_OUT(ILLOC_ID+1),VAR_ID_OUT(INVAR+2)),"Cannot def var x")
      !
    ELSE
      STOP "INCORRECT TYPE OF GRID 2D"
    ENDIF
  ENDIF
!
! The dim ids array is used to pass the dim ids of the dimensions of
! the netCDF variables. In Fortran, the unlimited
! dimension must come last on the list of dimids. 
!And must not has an element equal to zero
  DO IV=1,INVAR
    IF (ANY( VAR_ID_DIMS_OUT(:,IV) .EQ. IMASSIFTODELETE ).OR.              &
            ANY( VAR_ID_DIMS_OUT(:,IV) .EQ. ILAYERTODELETE ).OR.               &
            (GRID_TYPE == "LL" .AND.  ANY(VAR_NAME_IN(IV).EQ. LL_VARNAME))) CYCLE
    ! Create variable in output file
    CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,VAR_NAME_IN(IV),VAR_TYPE_IN(IV), &
            PACK(VAR_ID_DIMS_OUT(:,IV),VAR_ID_DIMS_OUT(:,IV)/=0),VAR_ID_OUT(IV)),&
            "Cannot def var "//TRIM(VAR_NAME_IN(IV)))
    !copy attributes from infile to outfile
    DO IA=1,INATT(IV)
      CALL CHECK(NF90_INQ_ATTNAME(FILE_ID_IN,VAR_ID_IN(IV),IA,ATT_NAME)," Cannot get att name")
      !
      CALL CHECK(NF90_COPY_ATT(FILE_ID_IN, VAR_ID_IN(IV),ATT_NAME, FILE_ID_OUT, VAR_ID_OUT(IV)), &
              "Cannot copy att from infile to outfile")
      !
    ENDDO
  ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!End of definition of output file
  CALL CHECK(NF90_ENDDEF(FILE_ID_OUT),"Cannot end of definition of output file")
  IF(IRANK == 2 )THEN
    IF (GRID_TYPE == "LL") THEN
      !lat
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+1),ZLATOUT ,&
              start= (/1/) ,count =(/NY/)),"Cannot put lat")
      ! lon
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+2),ZLONOUT ,&
              start= (/1/) ,count =(/NX/)),"Cannot put lon")
    ELSEIF (GRID_TYPE == "XY") THEN
      !y
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+1),ZYOUT ,&
              start= (/1/) ,count =(/NY/)),"Cannot put y")
      !x
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+2),ZXOUT ,&
              start= (/1/) ,count =(/NX/)),"Cannot put x")

    ELSE
      STOP "INCORRECT TYPE OF GRID 2D"
    ENDIF
  ENDIF
!
  NTIME =  DIM_SIZE_OUT(ITIMEID)
  ALLOCATE(VALUE_TIME(NTIME))
  ntime = ntime
  !
  DO IV=1,INVAR
    IF (ANY( VAR_ID_DIMS_OUT(:,IV) .EQ. IMASSIFTODELETE ).OR.              &
            ANY( VAR_ID_DIMS_OUT(:,IV) .EQ. ILAYERTODELETE ).OR.               &
            (GRID_TYPE == "LL" .AND.  ANY(VAR_NAME_IN(IV).EQ. LL_VARNAME))) CYCLE
    !  !Unlimited dimensions require collective writes
    CALL CHECK(NF90_VAR_PAR_ACCESS(FILE_ID_OUT, VAR_ID_OUT(IV), nf90_collective),&
            "collective write")

    IF (VAR_NAME_IN(IV) == "time") THEN
      !
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),VALUE_TIME &
              , start =(/1/), count = (/NTIME/)) &
              ,"Cannot get var time")
      !
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),VALUE_TIME  &
              , start = (/1/), count =(/NTIME/) ),"Cannot put var time")
      !
    ELSEIF (VAR_NAME_IN(IV) == "ZS") THEN
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZZSOUT , &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var ZS")
    ELSEIF (VAR_NAME_IN(IV) == "aspect") THEN
      !ZASPECTOUT = Proc_id
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZASPECTOUT, &
              start =(/1,1/), count = (/NX,NY/)), &
              "Cannot put var aspect")
    ELSEIF (VAR_NAME_IN(IV) == "slope") THEN
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZSLOPEOUT, &
              start =(/1,1/), count = (/NX,NY/)), &
              "Cannot put var slope")
    ELSEIF (VAR_NAME_IN(IV) == "massif_number" .OR. VAR_NAME_IN(IV) == "massif_num") THEN
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),IMASSIFOUT, &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var massif_number")
    ELSEIF (VAR_NAME_IN(IV) == "LAT" .AND. GRID_TYPE == "XY") THEN
      !
      ALLOCATE(ZVARLATLON(NX,NY))
      CALL LATLON_IGN(ZXOUT,ZYOUT,PLAT=ZVARLATLON)
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARLATLON , &
              start =(/1,1/), count = (/NX,NY/)), &
              "Cannot put var LAT")
      DEALLOCATE(ZVARLATLON)
      !!    !
    ELSEIF (VAR_NAME_IN(IV) == "LON" .AND. GRID_TYPE == "XY") THEN
      !
      ALLOCATE(ZVARLATLON(NX,NY))
      CALL LATLON_IGN(ZXOUT,ZYOUT,PLON=ZVARLATLON)
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARLATLON , &
              start =(/1,1/), count = (/NX,NY/)), &
              "Cannot put var LON")
      DEALLOCATE(ZVARLATLON)
      !
    ELSEIF (ALL(VAR_ID_DIMS_OUT(:,IV) .NE. ILLOC_ID) .AND. &
            ANY(VAR_ID_DIMS_OUT(:,IV) .EQ.ITIMEID))THEN !TIME DIM and NOGRID DIM
      !
      ALLOCATE(ZVARINT(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV)),NPATCH,NTIME))
      !
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARINT),"Cannot get var"//TRIM(HFILENAMEIN))
      !
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARINT),"Q Cannot put var"//TRIM(HFILENAMEOUT))
      DEALLOCATE(ZVARINT)
      !
    ELSEIF (ALL(VAR_ID_DIMS_OUT(:,IV) == 0))THEN !SCALAR
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZSCAIN),"Cannot get var"//TRIM(HFILENAMEIN))
      !
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZSCAIN),"R Cannot put var"//TRIM(HFILENAMEOUT))
      !
    ELSEIF (ALL(VAR_ID_DIMS_OUT(:,IV) .NE. ILLOC_ID) .AND. &
            ALL(VAR_ID_DIMS_OUT(:,IV) .NE.ITIMEID))THEN !NO TIME DIM and NOGRID DIM
      !
      ALLOCATE(ZVARIN(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV)),NPATCH))
      !
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARIN),"Cannot get var"//TRIM(HFILENAMEIN))
      !
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARIN),"S Cannot put var"//TRIM(HFILENAMEOUT))
      !
      DEALLOCATE(ZVARIN)
    ELSEIF (ANY(VAR_ID_DIMS_OUT(:,IV) .EQ. ILLOC_ID) .AND. &
            ANY(VAR_ID_DIMS_OUT(:,IV) .EQ.ITIMEID))THEN !TIME and GRID DIM
      !
      IF (ANY(VAR_ID_DIMS_OUT(:,IV) .EQ. IDECILEID)) THEN
        ALLOCATE(ZVARINT(NDECILE,DIM_SIZE_IN(ILLOC_ID),NTIME))
        ALLOCATE(ZVAROUTXYT(NDECILE,NX_PROC,NY_PROC,NTIME))
      ELSE
        ALLOCATE(ZVARINT(DIM_SIZE_IN(ILLOC_ID),NPATCH,NTIME))
        ALLOCATE(ZVAROUTXYT(NX_PROC,NY_PROC,NPATCH,NTIME))
      ENDIF

      IF (ALL(VAR_ID_DIMS_OUT(:,IV) .NE. NPATCHID)) THEN
        !      !  Read variable
        IF (ANY(VAR_ID_DIMS_OUT(:,IV) .EQ. IDECILEID)) THEN
          ! case (time, Number_of_points, decile)
          CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARINT, &
                  start =(/1,1,1/), count = (/NDECILE,DIM_SIZE_IN(ILLOC_ID),NTIME/)) &
                  ,"Cannot get var"//TRIM(HFILENAMEIN))
        ELSE
          ! standard case (time, Number_of_points)
          CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARINT, &
                  start =(/1,1/), count = (/DIM_SIZE_IN(ILLOC_ID),NTIME/)) &
                  ,"Cannot get var"//TRIM(HFILENAMEIN))
        ENDIF
      ELSE
        ! case (time, Number_of_Patches, Number_of_points)
        CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARINT, &
                start =(/1,1,1/), count = (/DIM_SIZE_IN(ILLOC_ID),NPATCH,NTIME/)) &
                ,"Cannot get var"//TRIM(HFILENAMEIN))
      ENDIF
      !
      IF (ANY(VAR_ID_DIMS_OUT(:,IV) .EQ. IDECILEID)) THEN
        CALL INTERPOLZS2DDIMBEFORE(ZVAROUTXYT,ZVARINT,IINDICESBAS,IINDICESHAUT,&
                ZZSOUT,IZSIN)
      ELSE
        CALL INTERPOLZS2D(ZVAROUTXYT,ZVARINT,IINDICESBAS,IINDICESHAUT,&
                ZZSOUT,IZSIN)
      ENDIF
      !
      IF (IRANK ==1 )THEN
        ALLOCATE(ZVAROUTXYT1D(NX_PROC,NPATCH,NTIME))
        ZVAROUTXYT1D = XUNDEF
        ZVAROUTXYT1D = ZVAROUTXYT(:,1,:,:)
        ! Write variable
        IF (ALL(VAR_ID_DIMS_OUT(:,IV) .NE. NPATCHID)) THEN
          IF (ANY(VAR_ID_DIMS_OUT(:,IV) .EQ. IDECILEID)) THEN
            ! case (time, Number_of_points, decile)
            CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT1D,  &
                    start =(/1,IXSTART,1/) ,count = (/NDECILE,NX_PROC,NTIME/)),&
                    "T Cannot put var"//TRIM(HFILENAMEOUT))
          ELSE
            ! standard case (time, Number_of_points)
            CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT1D,  &
                    start =(/IXSTART,1/) ,count = (/NX_PROC,NTIME/)),&
                    "U Cannot put var"//TRIM(HFILENAMEOUT))
          ENDIF
        ELSE
          ! case (time, Number_of_Patches, Number_of_points)
          CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT1D,  &
                  start =(/IXSTART,1,1/) ,count = (/NX_PROC,NPATCH,NTIME/)),&
                  "V Cannot put var"//TRIM(HFILENAMEOUT))
        ENDIF
        DEALLOCATE(ZVAROUTXYT1D)
      ELSEIF( IRANK == 2)THEN
        IF (ALL(VAR_ID_DIMS_OUT(:,IV) .NE. NPATCHID)) THEN
          ! case (time, x, y, decile)
          IF (ANY(VAR_ID_DIMS_OUT(:,IV) .EQ. IDECILEID)) THEN
            CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT,  &
                    start =(/1,IXSTART,IYSTART,1/) ,count = (/NDECILE,NX_PROC,NY_PROC,NTIME/)),&
                    "W Cannot put var "//TRIM(VAR_NAME_IN(IV)))
          ELSE
            ! standard case (time, x, y)
            CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT,  &
                    start =(/IXSTART,IYSTART,1/) ,count = (/NX_PROC,NY_PROC,NTIME/)),&
                    "X Cannot put var "//TRIM(VAR_NAME_IN(IV)))
          ENDIF
        ELSE
          ! case (time, Number_of_Patches, x, y)
          CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT,  &
                  start =(/IXSTART,IYSTART,1,1/) ,count = (/NX_PROC,NY_PROC,NPATCH,NTIME/)),&
                  "Y Cannot put var "//TRIM(VAR_NAME_IN(IV)))
        ENDIF
      ENDIF
      !
      DEALLOCATE(ZVARINT)
      DEALLOCATE(ZVAROUTXYT)
      !
    ELSEIF (ANY(VAR_ID_DIMS_OUT(:,IV) .EQ. ILLOC_ID ) .AND. &
            ALL(VAR_ID_DIMS_OUT(:,IV) .NE. ITIMEID))THEN !GRID DIM
      !
      ALLOCATE(ZVARIN(DIM_SIZE_IN(ILLOC_ID),NPATCH))
      ALLOCATE(ZVAROUT2D(NX_PROC,NY_PROC,NPATCH))
      ZVAROUT2D = XUNDEF
      !  Read variable
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARIN, &
              start =(/1,1/), count = (/DIM_SIZE_IN(ILLOC_ID),NPATCH/)) &
              ,"Cannot get var"//TRIM(HFILENAMEIN))
      !
      CALL INTERPOLZS2DNOTIME(ZVAROUT2D,ZVARIN,IINDICESBAS,IINDICESHAUT,&
              ZZSOUT,IZSIN)
      !
      ! Write variable
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUT2D,  &
              start =(/IXSTART,IYSTART,1/) ,count = (/NX_PROC,NY_PROC,NPATCH/)),&
              "Z Cannot put var"//TRIM(HFILENAMEOUT))
      !
      DEALLOCATE(ZVARIN)
      DEALLOCATE(ZVAROUT2D)
      !
    ELSE
      !
      PRINT*, VAR_NAME_IN(IV),  VAR_ID_DIMS_OUT(:,IV), "DIMMENSION VAR NON TRAITEE"
      !
    ENDIF
    !
  ENDDO
  DEALLOCATE(VALUE_TIME)
  !
  DEALLOCATE(DIM_NAME_IN)
  DEALLOCATE(DIM_ID_IN)
  DEALLOCATE(DIM_SIZE_IN)
  !
  !!VAR properties
  DEALLOCATE(VAR_NAME_IN)
  DEALLOCATE(VAR_ID_IN)
  DEALLOCATE(VAR_TYPE_IN)
  DEALLOCATE(VAR_NDIMS_IN)
  DEALLOCATE(VAR_ID_DIMS_IN)
  DEALLOCATE(INATT)
  !
  DEALLOCATE(VAR_ID_OUT)
  DEALLOCATE(VAR_ID_DIMS_OUT)
  !
  DEALLOCATE(DIM_NAME_OUT)
  DEALLOCATE(DIM_ID_OUT)
  DEALLOCATE(DIM_SIZE_OUT)
  !
  DEALLOCATE(ZZSOUT)
  DEALLOCATE(IMASSIFOUT)
  DEALLOCATE(ZASPECTOUT)
  DEALLOCATE(ZSLOPEOUT)
  IF(IRANK == 1)THEN
    DEALLOCATE(ZLATOUT)
    DEALLOCATE(ZLONOUT)
  ELSEIF(IRANK == 2)THEN
    IF (GRID_TYPE == "LL") THEN
      DEALLOCATE(ZLATOUT)
      DEALLOCATE(ZLONOUT)
    ELSEIF (GRID_TYPE == "XY") THEN
      DEALLOCATE(ZYOUT)
      DEALLOCATE(ZXOUT)
    ENDIF
  ENDIF
  !
  DEALLOCATE(IZSIN)
  DEALLOCATE(IMASSIFIN)
  DEALLOCATE(IASPECTIN)
  !
  DEALLOCATE(IINDICESBAS)
  DEALLOCATE(IINDICESHAUT)
  !
  ! close output file
  CALL CHECK(NF90_CLOSE(FILE_ID_OUT),"Cannot close file "//TRIM(HFILENAMEOUT))
  ! close grid file
  CALL CHECK(NF90_CLOSE(FILE_ID_GEO),"Cannot close file"//TRIM(HFILENAMEG))
  ! close input file
  CALL CHECK(NF90_CLOSE(FILE_ID_IN),"Cannot close file"//TRIM(HFILENAMEIN))
  ! PRINT*, IMASSIFTODELETE, ILAYERTODELETE, NPATCHID, NPATCH, IDECILEID, NDECILE
END DO ! loop over input files (domains)
!
CALL MPI_FINALIZE(IERR)
!
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE XY_PROC_DISTRIBUTOR(IXYDIM,IPROC_ID, INPROC,IIXSTART, INX_PROC,IIYSTART, INY_PROC)
INTEGER ,INTENT(IN) :: IPROC_ID, INPROC
INTEGER,DIMENSION(2),INTENT(IN) ::  IXYDIM
INTEGER ,INTENT(OUT):: IIXSTART, INX_PROC,IIYSTART, INY_PROC
!
IF (INPROC > 1 ) THEN

  INX_PROC = IXYDIM(1)/2
  IF (MOD(IPROC_ID,2) .EQ. 0) THEN 
    IIXSTART=1
  ELSE
    IIXSTART = INX_PROC+1
    INX_PROC = INX_PROC +MOD(IXYDIM(1),2)
  ENDIF
  !
  INY_PROC = IXYDIM(2)/FLOOR(REAL(INPROC/2.))
  !
  IF (MOD(IPROC_ID,2) .EQ. 0) THEN 
       IIYSTART =(IPROC_ID/2)* INY_PROC+1
  ELSE
       IIYSTART =((IPROC_ID-1)/2)* INY_PROC+1
  ENDIF
  !
  IF ( MOD(INPROC,2) .NE. 0 .AND. IPROC_ID == INPROC-1) THEN
    INY_PROC = MOD(IXYDIM(2),(INPROC/2))
    INX_PROC = IXYDIM(1)
    IIXSTART = 1
  ELSEIF ( MOD(INPROC,2) .EQ. 0 .AND. (IPROC_ID == INPROC-1 .OR. IPROC_ID == INPROC-2))THEN 
    INY_PROC = INY_PROC +MOD(IXYDIM(2),(INPROC/2))
  ENDIF
  !
ELSE
  INY_PROC= IXYDIM(2)
  IIYSTART =1
  INX_PROC = IXYDIM(1)
  IIXSTART = 1
END IF 
!
END SUBROUTINE 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE NPOINT_PROC_DISTRIBUTOR(IPDIM,IPROC_ID, INPROC,IIPSTART,INP_PROC)
INTEGER ,INTENT(IN) :: IPROC_ID, INPROC
INTEGER,DIMENSION(2),INTENT(IN) ::  IPDIM
INTEGER ,INTENT(OUT):: IIPSTART,INP_PROC
!
IF (INPROC > 1 ) THEN
  !
  IF ( MOD(IPDIM(1),INPROC) .EQ. 0 ) THEN
    INP_PROC = IPDIM(1)/INPROC
    IIPSTART= (IPROC_ID*INP_PROC)+1
  ELSEIF (  MOD(IPDIM(1),INPROC) .NE. 0)THEN 
    INP_PROC = FLOOR(IPDIM(1)/REAL(INPROC))
    IIPSTART= (IPROC_ID*INP_PROC)+1
    IF (IPROC_ID == INPROC-1)THEN
      INP_PROC = INP_PROC + MOD(IPDIM(1),INPROC)
    ENDIF    
  ENDIF
  !
ELSE
  INP_PROC= IPDIM(1)
  IIPSTART =1
END IF
!
END SUBROUTINE 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE READ_OUTPUT_GRID(FILE_ID_GEO,PZSOUT,KMASSIFOUT,PASPECTOUT,IRANK,&
                          GT,ILENDIM,PLATOUT,PLONOUT,PYOUT,PXOUT,PSLOPEOUT)
INTEGER,INTENT(IN) :: FILE_ID_GEO
! OUTPUT VARIABLE 2D
INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: KMASSIFOUT
DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE,INTENT(OUT)::PZSOUT, PASPECTOUT,PSLOPEOUT ! Elevation massif and aspect of output grid
!
DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE,INTENT(OUT)::PLATOUT,PLONOUT,PYOUT,PXOUT
INTEGER,INTENT(OUT)::IRANK ! rank of output grid (1 for vector or 2 for matrix)
INTEGER,DIMENSION(1:2),INTENT(OUT)::ILENDIM ! lenghts of dimensions
CHARACTER(LEN=2)::GT !GRID TYPE
!
!INTEGER::IDVARG ! nc variable identifier
INTEGER::IDVARGZS,IDVARGMASSIF,IDVARGASPECT,IDVARSLOPE,IDVARLAT,IDVARLON,IDY,IDX ! nc variable identifier
INTEGER,DIMENSION(:),ALLOCATABLE::IDDIMSVARG ! dimension ids of nc variable
!
DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE :: PX2D,PY2D, PZSALL
!
INTEGER::ID ! dimension loop counter
!
CHARACTER(*),PARAMETER::HZS='ZS'
CHARACTER(*),PARAMETER::HMASSIF='massif_num'
CHARACTER(*),PARAMETER::HASPECT='aspect'
CHARACTER(*),PARAMETER::HSLOPE='slope'
CHARACTER(*),PARAMETER::HLAT='latitude'
CHARACTER(*),PARAMETER::HLON='longitude'
CHARACTER(*),PARAMETER::HX='x'
CHARACTER(*),PARAMETER::HY='y'
!
!Get for ZS :ID 
CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HZS,IDVARGZS),"Cannot find variable "//HZS)
!
!Get for ZS : number of dimension, vector of NDIMS dimension IDs
CALL CHECK(NF90_INQUIRE_VARIABLE(FILE_ID_GEO,IDVARGZS,ndims=IRANK),"Cannot find dim")
ALLOCATE(IDDIMSVARG(IRANK))
CALL CHECK(NF90_INQUIRE_VARIABLE(FILE_ID_GEO,IDVARGZS,dimids=IDDIMSVARG), &
                                 "Cannot find dim id")
!
ILENDIM(:)=1
DO ID=1,IRANK
  !Returned length of dimension
  CALL CHECK(NF90_INQUIRE_DIMENSION(FILE_ID_GEO,IDDIMSVARG(ID),len=ILENDIM(ID),&
                    name=DIM_NAME_OUT(ID)), "Cannot get dim "//TRIM(HFILENAMEG))
END DO
DEALLOCATE(IDDIMSVARG)
!
NX = ILENDIM(1)
NY = ILENDIM(2)
IF(IRANK == 2)THEN
  IF (ANY(DIM_NAME_OUT(:) .EQ. "x"))THEN
    GT="XY"
  ELSEIF (ANY(DIM_NAME_OUT .EQ. "lon"))THEN
    GT="LL"
  ELSE
    STOP "GRID 2D NON TRAITE"
  ENDIF
  CALL XY_PROC_DISTRIBUTOR(ILENDIM,PROC_ID, NPROC,IXSTART, NX_PROC,IYSTART, NY_PROC)
ELSEIF(IRANK == 1)THEN 
  CALL NPOINT_PROC_DISTRIBUTOR(ILENDIM,PROC_ID,NPROC,IPSTART,NPOINT_PROC)
  IXSTART= IPSTART
  IYSTART = 1
  NY_PROC = 1
  NX_PROC = NPOINT_PROC
ENDIF
!
ALLOCATE(PZSOUT(NX_PROC,NY_PROC))
ALLOCATE(KMASSIFOUT(NX_PROC,NY_PROC))
KMASSIFOUT = 0 ; PZSOUT= XUNDEF
!
CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGZS,PZSOUT, &
   start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)), "Cannot read "//HZS)
!
IF (NF90_INQ_VARID(FILE_ID_GEO,HMASSIF,IDVARGMASSIF) == NF90_NOERR) THEN

  CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGMASSIF,KMASSIFOUT, &
    start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)),"Cannot read "//HMASSIF)
ELSE
  KMASSIFOUT = 1
ENDIF
!
ALLOCATE(PASPECTOUT(NX,NY))
PASPECTOUT = -1.
ALLOCATE(PSLOPEOUT(NX,NY))
PSLOPEOUT = 0.
IF(IRANK == 1)THEN
  !
  ALLOCATE(PLATOUT(NPOINT_PROC))
  ALLOCATE(PLONOUT(NPOINT_PROC))
  IF(NF90_INQ_VARID(FILE_ID_GEO,HASPECT,IDVARGASPECT) .EQ. NF90_NOERR ) THEN
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGASPECT,PASPECTOUT , &
     start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)), &
                       "Cannot read "//HASPECT)
  ENDIF 
  !
  IF (NF90_INQ_VARID(FILE_ID_GEO,HSLOPE,IDVARSLOPE).EQ. NF90_NOERR)THEN
    !
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARSLOPE,PSLOPEOUT , &
       start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)), &
                         "Cannot read "//HSLOPE)
  ENDIF
  !
ELSEIF(IRANK == 2)THEN
  IF (GT == "XY") THEN  !X Y GRID
    ALLOCATE(PYOUT(NY))
    ALLOCATE(PXOUT(NX))
    !Y
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HY,IDY), "Cannot find "//HY)
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDY,PYOUT, &
               start =(/1/) , count = (/NY/)), "Cannot read "//HY)
    !X
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HX,IDX), "Cannot find "//HX)
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDX,PXOUT, &
               start =(/1/) , count = (/NX/)), "Cannot read "//HX)
    ALLOCATE(PZSALL(NX,NY))
    PZSALL= XUNDEF
    !maybe a gather from all procs is more efficient ?
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGZS,PZSALL, &
       start =(/1,1/) , count = (/NX,NY/)), "Cannot read all "//HZS)
    ! Calcul slope, aspect
    CALL EXPLICIT_SLOPE(PXOUT,PYOUT,PZSALL,PSLOPEOUT,PASPECTOUT)
    !   
    DEALLOCATE(PZSALL)
  ELSEIF (GT == "LL") THEN  !LAT LON GRID
    ALLOCATE(PLATOUT(NY))
    ALLOCATE(PLONOUT(NX))
    !LAT
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HLAT,IDVARLAT), "Cannot find "//HLAT)
    
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARLAT,PLATOUT, &
               start =(/1/) , count = (/NY/)), "Cannot read "//HLAT)
    !LON
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HLON,IDVARLON) ,"Cannot find "//HLON)
    
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARLON,PLONOUT, &
               start =(/1/) , count = (/NX/)), "Cannot read "//HLON)
    ! Calcul slope, aspect
    ALLOCATE(PX2D(NX,NY))
    ALLOCATE(PY2D(NX,NY))
    CALL XY_IGN(PLATOUT,PLONOUT,PX2D,PY2D)

    ALLOCATE(PZSALL(NX,NY))
    !maybe a gather from all procs is more efficient ?
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGZS,PZSALL, &
       start =(/1,1/) , count = (/NX,NY/)), "Cannot read all "//HZS)
    CALL EXPLICIT_SLOPE_LAT_LON(PX2D,PY2D,PZSALL,PSLOPEOUT,PASPECTOUT)

    DEALLOCATE(PZSALL)    
    DEALLOCATE(PX2D)
    DEALLOCATE(PY2D)
  ELSE
    STOP "GRID 2D NOT TRAITED"
  ENDIF
  !
ENDIF
!
END SUBROUTINE READ_OUTPUT_GRID
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE READ_NC(IDFICNC,KZSIN,KMASSIFIN,KASPECTIN)
INTEGER,INTENT(IN) :: IDFICNC
INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(OUT)::KZSIN, KMASSIFIN, KASPECTIN ! Elevation massif and aspect of output grid
REAL,DIMENSION(:),ALLOCATABLE::ZSLOPEIN
INTEGER::IDVARGZS,IDVARGMASSIF,IDVARGASPECT,IDVARGSLOPE ! nc variable identifier
INTEGER,DIMENSION(:),ALLOCATABLE::IDDIMSVARG ! dimension ids of nc variable
!
INTEGER::ID ,ISTATUS! dimension loop counter
INTEGER::IRANKNC ! rank of output grid (1 for vector or 2 for matrix)
INTEGER,DIMENSION(1:2)::ILENDIM ! lenghts of dimensions
CHARACTER(*),PARAMETER::HZS= 'ZS'
CHARACTER(*),PARAMETER::HMASSIF='massif_number'
CHARACTER(*),PARAMETER::HMASSIF2='massif_num'
CHARACTER(*),PARAMETER::HASPECT='aspect'
CHARACTER(*),PARAMETER::HSLOPE='slope'
!
! Read the data.
!Get for ZS :ID 
CALL CHECK(NF90_INQ_VARID(IDFICNC,HZS,IDVARGZS),"Cannot find "//HZS)
!
!Get for ZS : number of dimension, vector of NDIMS dimension IDs
CALL CHECK(NF90_INQUIRE_VARIABLE(IDFICNC,IDVARGZS,ndims=IRANKNC),"Cannot find var")
ALLOCATE(IDDIMSVARG(IRANKNC))
CALL CHECK(NF90_INQUIRE_VARIABLE(IDFICNC,IDVARGZS,dimids=IDDIMSVARG) ,&
                                 "Cannot find var id")
!                                 
ILENDIM(:)=1
!
DO ID=1,IRANKNC
!Returned length of dimension
  CALL CHECK(NF90_INQUIRE_DIMENSION(IDFICNC,IDDIMSVARG(ID),len=ILENDIM(ID)),&
                  "Cannot find dim")
END DO
DEALLOCATE(IDDIMSVARG)
!
ALLOCATE(KZSIN(ILENDIM(1)))
ALLOCATE(KMASSIFIN(ILENDIM(1)))
ALLOCATE(KASPECTIN(ILENDIM(1)))
ALLOCATE(ZSLOPEIN(ILENDIM(1)))
!
CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGZS,KZSIN), "Cannot read "//HZS)
!
!
ISTATUS = NF90_INQ_VARID(IDFICNC,HMASSIF2,IDVARGMASSIF)
IF (ISTATUS .NE. NF90_NOERR ) &
CALL CHECK(NF90_INQ_VARID(IDFICNC,HMASSIF,IDVARGMASSIF),"Cannot find "//HMASSIF)
!
CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGMASSIF,KMASSIFIN), "Cannot read "//HMASSIF)
!
!
CALL CHECK(NF90_INQ_VARID(IDFICNC,HASPECT,IDVARGASPECT),"Cannot find "//HASPECT)
!
CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGASPECT,KASPECTIN), "Cannot read "//HASPECT)
!
! A control of slope value is done because the input file must not have been already projected on slopes before interpolation
! If we interpolate the forcing, the projection will be done after either in snowtools for collection of points, either in SURFEX for 2d points
CALL CHECK(NF90_INQ_VARID(IDFICNC,HSLOPE,IDVARGSLOPE),"Cannot find "//HSLOPE)
!
CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGSLOPE,ZSLOPEIN), "Cannot read "//HSLOPE)
!
IF (MAXVAL(ZSLOPEIN) > 1.) THEN
 STOP "INPUT FORCING FILE MUST PROVIDE HORIZONTAL RADIATIONS AND MUST NEVER HAVE BEEN PROJECTED ON SLOPES."
END IF
!
END SUBROUTINE READ_NC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE INDICES2D(PZSOUT,KMASSIFOUT,PASPECTOUT,KZSIN,KMASSIFIN,&
                   KASPECTIN,KINDICESBAS,KINDICESHAUT)

INTEGER,DIMENSION(:,:),INTENT(IN)::KMASSIFOUT  ! Massif of output collection of points
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZSOUT,PASPECTOUT! Elevation and aspect of output collection of points
INTEGER,DIMENSION(:),INTENT(IN)::KZSIN,KMASSIFIN,KASPECTIN ! Elevation, massif and aspect of input collection of points
!
! Indexes of input collection of points to interpolate to obtain output grid
INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(OUT)::KINDICESBAS,KINDICESHAUT
!
INTEGER :: INX,INY ! dimensions of output grid
INTEGER :: ININ    ! dimension of input collection of points
!
INTEGER::JI,JX,JY ! loop counters
INTEGER::JDIFFZS ! elevation difference between output point and input point
!
LOGICAL :: GASPECT, GISFLAT
INTEGER, PARAMETER::JPRESOL_ELEV = 300 ! elevation resolution of input collection of points
!
INX=SIZE(PZSOUT, 1)
INY=SIZE(PZSOUT, 2)
ININ=SIZE(KZSIN)
!
ALLOCATE(KINDICESBAS(INX,INY))
ALLOCATE(KINDICESHAUT(INX,INY))
KINDICESBAS=0
KINDICESHAUT=0
!
GISFLAT = .FALSE.
IF (ALL(KASPECTIN .EQ. -1)) GISFLAT = .TRUE.
DO JX=1,INX
  DO JY=1,INY
    IF(PZSOUT(JX,JY) .EQ. XUNDEF .OR.  KMASSIFOUT(JX,JY) .EQ. 0) CYCLE
    DO JI=1,ININ
        IF (KMASSIFIN(JI) /= KMASSIFOUT(JX,JY)) CYCLE
        !Evaluate the aspet only if the input domain is not flat
        IF (.NOT. GISFLAT) THEN
        CALL EVALUATE_ASPECT (KASPECTIN(JI),PASPECTOUT(JX,JY),GASPECT)
        IF (.NOT. GASPECT) CYCLE
        ENDIF
        !
        JDIFFZS = KZSIN(JI) - PZSOUT(JX,JY)
        IF (JDIFFZS < -JPRESOL_ELEV) THEN
            CYCLE
        ELSEIF (JDIFFZS > JPRESOL_ELEV) THEN
            CYCLE
        ELSEIF (JDIFFZS <= 0) THEN
          ! Elevation of the level just below the point
          KINDICESBAS(JX,JY) = JI         
        ELSE
          ! Elevation of the level just above the point
          KINDICESHAUT(JX,JY) = JI 
        ENDIF
        
    END DO
  END DO
END DO
!
END SUBROUTINE INDICES2D
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE EVALUATE_ASPECT(KASPECTIN,PASPECTOUT,LASPECT)
DOUBLE PRECISION,INTENT(IN)::PASPECTOUT!Aspect of output points
INTEGER,INTENT(IN)::KASPECTIN ! Aspect of input points
LOGICAL, INTENT(OUT) :: LASPECT
!
LASPECT = .FALSE.
IF (KASPECTIN .EQ. 0.)THEN
  IF (PASPECTOUT .LT. 22.5 .OR. PASPECTOUT .GT. 337.5 ) LASPECT = .TRUE.
ELSEIF (KASPECTIN .LT. 0. .AND. PASPECTOUT .GE. 0. )THEN
  LASPECT = .FALSE.
ELSEIF ((PASPECTOUT .GE. KASPECTIN .AND. PASPECTOUT .LE. KASPECTIN+22.5) .OR. &
    (PASPECTOUT .LE. KASPECTIN .AND. PASPECTOUT .GE. KASPECTIN-22.5 ))THEN
  LASPECT = .TRUE.  
ENDIF
!
END SUBROUTINE EVALUATE_ASPECT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE INTERPOLZS2D(PVAROUT,PVARIN,KINDBAS,KINDHAUT,PZSGRID,KZSSAFRAN)
! Linear interpolation between SAFRAN elevation levels over 2D grids

REAL,DIMENSION(:,:,:,:),INTENT(OUT)::PVAROUT
REAL,DIMENSION(:,:,:),INTENT(IN)::PVARIN
INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS,KINDHAUT
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZSGRID
INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN
REAL ::ZA,ZB,ZC
INTEGER::JX,JY,JT,JP !Loop counters
INTEGER::KNX,KNY,KNSAFRAN,KNT,KNP
!
KNX=SIZE(PVAROUT,1)
KNY=SIZE(PVAROUT,2)
KNSAFRAN=SIZE(PVARIN,1)
KNP = SIZE(PVAROUT,3)
KNT=SIZE(PVARIN,3)
!
PVAROUT = XUNDEF
DO JX=1,KNX
    DO JY=1,KNY
      IF (KINDBAS(JX,JY).EQ. 0 .OR. KINDHAUT(JX,JY) .EQ. 0 ) cycle
      ! Interpolation parameters constant over time
      ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
      ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
      ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
      !
      DO JP=1,KNP

        DO JT=1,KNT      
          PVAROUT(JX,JY,JP,JT)= (ZA*PVARIN(KINDHAUT(JX,JY),JP,JT)+ZB*PVARIN(KINDBAS(JX,JY),JP,JT))/ZC 
        END DO
      ENDDO
    END DO
END DO
!
END SUBROUTINE INTERPOLZS2D
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE INTERPOLZS2DDIMBEFORE(PVAROUT,PVARIN,KINDBAS,KINDHAUT,PZSGRID,KZSSAFRAN)
! Linear interpolation between SAFRAN elevation levels over 2D grids

REAL,DIMENSION(:,:,:,:),INTENT(OUT)::PVAROUT
REAL,DIMENSION(:,:,:),INTENT(IN)::PVARIN
INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS,KINDHAUT
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZSGRID
INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN
REAL ::ZA,ZB,ZC
INTEGER::JX,JY,JT,JP !Loop counters
INTEGER::KNX,KNY,KNSAFRAN,KNT,KNP
!
KNX=SIZE(PVAROUT,2)
KNY=SIZE(PVAROUT,3)
KNSAFRAN=SIZE(PVARIN,2)
KNP = SIZE(PVAROUT,1)
KNT=SIZE(PVARIN,3)
!
PVAROUT = XUNDEF
DO JX=1,KNX
    DO JY=1,KNY
      IF (KINDBAS(JX,JY).EQ. 0 .OR. KINDHAUT(JX,JY) .EQ. 0 ) cycle
      ! Interpolation parameters constant over time
      ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
      ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
      ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
      !
      DO JP=1,KNP

        DO JT=1,KNT      
          PVAROUT(JP, JX,JY,JT)= (ZA*PVARIN(JP,KINDHAUT(JX,JY),JT)+ZB*PVARIN(JP,KINDBAS(JX,JY),JT))/ZC 
        END DO
      ENDDO
    END DO
END DO
!
END SUBROUTINE INTERPOLZS2DDIMBEFORE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE INTERPOLZS2DNOTIME(PVAROUT,PVARIN,KINDBAS,KINDHAUT,PZSGRID,KZSSAFRAN)
! Linear interpolation between SAFRAN elevation levels over 2D grids
REAL,DIMENSION(:,:,:),INTENT(OUT)::PVAROUT
REAL,DIMENSION(:,:),INTENT(IN)::PVARIN
INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS,KINDHAUT
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZSGRID
INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN
REAL ::ZA,ZB,ZC
INTEGER::JX,JY,JP !Loop counters
INTEGER::KNX,KNY,KNSAFRAN,KNP
!
KNX=SIZE(PVAROUT,1)
KNY=SIZE(PVAROUT,2)
KNSAFRAN=SIZE(PVARIN,1)
KNP = SIZE(PVAROUT,3)
!
PVAROUT = XUNDEF
DO JX=1,KNX
    DO JY=1,KNY
      IF (KINDBAS(JX,JY).EQ. 0 .OR. KINDHAUT(JX,JY) .EQ. 0 ) cycle
      ! Interpolation parameters constant over time
      ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
      ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
      ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
      !
      DO JP=1,KNP
        PVAROUT(JX,JY,JP)= (ZA*PVARIN(KINDHAUT(JX,JY),JP)+ZB*PVARIN(KINDBAS(JX,JY),JP))/ZC 
      ENDDO
    END DO
END DO
END SUBROUTINE INTERPOLZS2DNOTIME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 SUBROUTINE LATLON_IGN(PX,PY,PLAT,PLON)
!
!!****  *LATLON_IGN * - Routine to compute geographical coordinates
!    
DOUBLE PRECISION, DIMENSION(:),   INTENT(IN) :: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
DOUBLE PRECISION, DIMENSION(:,:), OPTIONAL,  INTENT(OUT):: PLAT,PLON    
                                           ! returned geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
!
DOUBLE PRECISION, DIMENSION(SIZE(PX),SIZE(PY)) :: ZGAMMA
DOUBLE PRECISION, DIMENSION(SIZE(PX),SIZE(PY)) :: ZR   ! length of arc meridian line projection
DOUBLE PRECISION, DIMENSION(SIZE(PX),SIZE(PY)) :: ZLATISO          ! Isometric latitude
DOUBLE PRECISION,DIMENSION(SIZE(PX),SIZE(PY)) :: ZLAT0            ! For iteration
! 
INTEGER                         :: J, JJ, IX,IY
DOUBLE PRECISION ,SAVE :: XPI   = 4.*ATAN(1.) 
DOUBLE PRECISION ,SAVE :: XN    = 0.7256077650  ! exposant de projection (n) 
DOUBLE PRECISION ,SAVE :: XC    = 11754255.426  ! constante de projection (c)(m)
DOUBLE PRECISION ,SAVE :: XXS   = 700000.000    ! X en projection du Pôle (Xs)(m)
DOUBLE PRECISION ,SAVE :: XYS   = 12655612.050  ! Y en projection du Pôle (Ys)(m)
DOUBLE PRECISION ,SAVE :: XLON0 = 3.            ! 3° Est Greenwitch pour L93
DOUBLE PRECISION ,SAVE :: XE    = 0.08181919112 ! premiere excentricité
!
!*       1.     LONGITUDE
!               -----------
!
IF (PRESENT(PLON)) THEN
  DO IY=1, SIZE(PY)
    DO IX=1, SIZE(PX)
      ZGAMMA(IX,IY)= ATAN ( (PX(IX)- XXS) / ( XYS - PY(IY)) )
      PLON(IX,IY)= ((XLON0 *(XPI/180.)) + (ZGAMMA(IX,IY)/XN))*180./XPI
    ENDDO
  ENDDO
ENDIF
!
!*       3.     LATITUDE
! 
IF (PRESENT(PLAT)) THEN
  
  DO IY=1, SIZE(PY)
    DO IX=1, SIZE(PX)
      ZR(IX,IY) = SQRT( (PX(IX) - XXS)**2. + (PY(IY) - XYS)**2. )   
      ZLATISO(IX,IY)=(-1./XN) * DLOG(ABS(ZR(IX,IY)/XC))
      !
      ZLAT0(IX,IY)  =2. * ATAN (EXP(ZLATISO(IX,IY))) - XPI/2.
      DO J=1, 10000
        PLAT(IX,IY) = 2. * ATAN(                                             &
        (((1.+XE *SIN(ZLAT0(IX,IY)))/(1.-XE *SIN(ZLAT0(IX,IY))))**(XE/2.))   &
        *EXP(ZLATISO(IX,IY)) ) -XPI/2.  
        !
        IF (ABS(PLAT(IX,IY) - ZLAT0(IX,IY)) < 1.E-16 ) EXIT
        ZLAT0(IX,IY)=PLAT(IX,IY)
      ENDDO
      !      
      !
    ENDDO
  ENDDO  
  PLAT(:,:)=PLAT(:,:) *180./XPI
ENDIF
!
END SUBROUTINE LATLON_IGN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE XY_IGN(PLAT,PLON,PX,PY)
!
!!****  *XY_IGN * - Routine to compute Lambert coordinates   
!    
DOUBLE PRECISION, DIMENSION(:), INTENT(IN):: PLON, PLAT
                                           ! returned geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
DOUBLE PRECISION, DIMENSION(:,:), INTENT(OUT):: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
!
REAL :: ZPI180, ZPI4, ZECC2
REAL :: ZWRK     ! working arrays
REAL :: ZLATRAD, ZLONRAD ! longitude and latitude in radians
REAL :: ZGAMMA
REAL :: ZLATFI           ! Isometric latitude
REAL :: ZR               ! length of arc meridian line projection
INTEGER :: JI,JJ
DOUBLE PRECISION ,SAVE :: XPI   = 4.*ATAN(1.) 
DOUBLE PRECISION ,SAVE :: XE    = 0.08181919112 ! premiere excentricité
DOUBLE PRECISION ,SAVE :: XN    = 0.7256077650  ! exposant de projection (n) 
DOUBLE PRECISION ,SAVE :: XC    = 11754255.426  ! constante de projection (c)(m)
DOUBLE PRECISION ,SAVE :: XXS   = 700000.000    ! X en projection du Pôle (Xs)(m)
DOUBLE PRECISION ,SAVE :: XYS   = 12655612.050  ! Y en projection du Pôle (Ys)(m)
DOUBLE PRECISION ,SAVE :: XLON0 = 3.            ! 3° Est Greenwitch pour L93
!
ZPI180 = XPI / 180.
ZPI4 = XPI / 4.
ZECC2 = XE / 2.
!
DO JI=1,SIZE(PLON)
  DO JJ=1,SIZE(PLAT)
    !
    IF (PLON(JI) > 180.) THEN
      ZLONRAD = (PLON(JI) - 360. - XLON0) * ZPI180
    ELSE
      ZLONRAD = (PLON(JI) - XLON0) * ZPI180
    ENDIF
    !
    ZLATRAD = PLAT(JJ) * ZPI180
    !
    !*       2.     Calcul of the isometric latitude :
    !               ----------------------------------
    !
    ZWRK   = SIN(ZLATRAD) * XE  
    !
    ZLATFI  = LOG(TAN(ZPI4 + ZLATRAD / 2.)) + ( (LOG(1-ZWRK)-LOG(1+ZWRK)) * ZECC2)
    !
    !*       3.     Calcul of the lambert II coordinates X and Y
    !               ---------------------------------------------
    !
    ZR      = EXP(- XN * ZLATFI) * XC
    !
    ZGAMMA  = XN * ZLONRAD
    !
    PX(JI,JJ) = XXS + SIN(ZGAMMA) * ZR
    PY(JI,JJ) = XYS- COS(ZGAMMA) * ZR   
    !    
  ENDDO
ENDDO
!
END SUBROUTINE XY_IGN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE EXPLICIT_SLOPE(PX,PY,PZS,PSSO_SLOPE,PSSO_DIR)
DOUBLE PRECISION, DIMENSION(:),   INTENT(IN) :: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZS ! resolved model orography
DOUBLE PRECISION,DIMENSION(:,:),INTENT(OUT)::PSSO_SLOPE ! resolved slope tangent
DOUBLE PRECISION,DIMENSION(:,:),INTENT(OUT)::PSSO_DIR ! resolved aspect
!
INTEGER :: IX       ! number of points in X direction
INTEGER :: IY       ! number of points in Y direction
!
INTEGER :: INNX !  number of points in X direction for large domain
INTEGER :: INNY !  number of points in Y direction for large domain
!
INTEGER :: JX       ! loop counter
INTEGER :: JY       ! loop counter
!
DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: ZZS_XY        ! orography at southwest corner of the mesh
DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE  :: ZZSL          ! orography in a 2D array
DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE    :: ZXHAT, ZYHAT  ! X Y coordinate
!
DOUBLE PRECISION,    PARAMETER :: XPI=4.*ATAN(1.)  ! Pi
INTEGER, PARAMETER :: JPHEXT = 1 ! number of points around the physical domain
!
DOUBLE PRECISION    :: ZDZSDX   ! slope in X and Y direction
DOUBLE PRECISION    :: ZDZSDY   ! of a triangle surface
DOUBLE PRECISION    :: ZSURF    ! surface of 4 triangles
REAL    :: ZDIRX , ZDIRY   ! slope in X and Y direction of a grid point for aspect calculus
!
INTEGER :: IIB, IIE, IJB, IJE
INTEGER :: JI, JJ, JB
INTEGER :: JT
!-------------------------------------------------------------------------------
!
!*          Gets the geometry of the grid
!          -----------------------------
!
IX = SIZE(PX)
IY = SIZE(PY)
!
INNX=IX+2
INNY=IY+2
!
ALLOCATE(ZZSL (INNX,INNY))
!
ZZSL(2:INNX-1,2:INNY-1) = PZS(:,:)
ZZSL(1,:) = ZZSL(2,:)
ZZSL(INNX,:) = ZZSL(INNX-1,:)
ZZSL(:,1) = ZZSL(:,2)
ZZSL(:,INNY) = ZZSL(:,INNY-1)

!------------------------------------------------------------------------------------------
!
!*        Orography of SW corner of grid meshes
!         -------------------------------------
!
ALLOCATE(ZZS_XY (INNX,INNY))
!
ZZS_XY(2:INNX,2:INNY) = 0.25*(  ZZSL(2:INNX,2:INNY)   + ZZSL(1:INNX-1,2:INNY)   &
                          + ZZSL(2:INNX,1:INNY-1) + ZZSL(1:INNX-1,1:INNY-1) )
!
ZZS_XY(1,:) = ZZS_XY(2,:)
ZZS_XY(:,1) = ZZS_XY(:,2)
!
!*      Initialize Grid meshes
!       -----------
!
ALLOCATE(ZXHAT (INNX))
ALLOCATE(ZYHAT (INNY))
!
DO JX=1,INNX
  ZXHAT(JX) = (PX(2)-PX(1))*JX
END DO
DO JY=1,INNY
  ZYHAT(JY) = (PY(2)-PY(1))*JY
END DO
!-------------------------------------------------------------------------------
!
IIB= 1+JPHEXT
IIE=INNX-JPHEXT
IJB=1+JPHEXT
IJE=INNY-JPHEXT
!
!*       1.    LOOP ON GRID MESHES
!              -------------------
!
!* discretization of the grid mesh in four triangles
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    ZSURF=0.
    DO JT=1,4
!
!* slopes in x and y
!
      SELECT CASE (JT)
        CASE (1)
          ZDZSDX=(    2.* ZZSL   (JI,JJ)                   &
                   - (ZZS_XY(JI,JJ)+ZZS_XY(JI,JJ+1)) )    &
                 / (ZXHAT(JI+1)-ZXHAT(JI))
          ZDZSDY=(  ZZS_XY(JI,JJ+1) - ZZS_XY(JI,JJ) )     &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))
        CASE (2)
           ZDZSDX=(  ZZS_XY(JI+1,JJ+1) -ZZS_XY(JI,JJ+1))  &
                 / (ZXHAT(JI+1)-ZXHAT(JI))
           ZDZSDY=(  (ZZS_XY(JI+1,JJ+1)+ZZS_XY(JI,JJ+1))  &
                     - 2.* ZZSL (JI,JJ) )                  &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))
        CASE (3)
          ZDZSDX=(  (ZZS_XY(JI+1,JJ)+ZZS_XY(JI+1,JJ+1))   &
                   - 2.* ZZSL(JI,JJ)                    )  &
                 / (ZXHAT(JI+1)-ZXHAT(JI))
          ZDZSDY=(  ZZS_XY(JI+1,JJ+1) - ZZS_XY(JI+1,JJ) ) &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))
        CASE (4)
           ZDZSDX=(  ZZS_XY(JI+1,JJ) - ZZS_XY(JI,JJ) )    &
                 / (ZXHAT(JI+1)-ZXHAT(JI))
           ZDZSDY=(  2.* ZZSL(JI,JJ)                       &
                   - (ZZS_XY(JI+1,JJ)+ZZS_XY(JI,JJ)) )    &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))
      END SELECT
      !
      ! If slope is higher than 60 degrees : numerical problems
      ZDZSDX=MIN(2.0,MAX(-2.0,ZDZSDX))
      ZDZSDY=MIN(2.0,MAX(-2.0,ZDZSDY))
     ! total surface of 4 triangles
      ZSURF=ZSURF+0.25*SQRT(1. + ZDZSDX**2. + ZDZSDY**2.)
    END DO
    !
    !equivalent tangent slope of a homogeneous surface with the same area
    PSSO_SLOPE(JI-JPHEXT,JJ-JPHEXT)=ATAN(SQRT(ZSURF**2.-1.))*(180./XPI)
    !
    !Calcul de SSO_DIR via formules(9),(10)et (15) de Zevenbergen and Thorne,1987
    ! SSO_DIR en degré from north between 0 and 360 (North = 0°, East = 90 °, South = 180 °, West = 270 °)
    ZDIRX=(  ZZSL(JI+1,JJ) -ZZSL(JI-1,JJ))  &
                 / (ZXHAT(JI+1)-ZXHAT(JI))
    ZDIRY=(  ZZSL(JI,JJ+1) - ZZSL(JI,JJ-1) ) &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))
    PSSO_DIR(JI-JPHEXT,JJ-JPHEXT)=( 1.5*XPI - ATAN2( ZDIRY, ZDIRX + SIGN(1.E-30,ZDIRX) ) )*(180.0/XPI)
    IF (PSSO_DIR(JI-JPHEXT,JJ-JPHEXT) > 360.0) THEN
        PSSO_DIR(JI-JPHEXT,JJ-JPHEXT) = PSSO_DIR(JI-JPHEXT,JJ-JPHEXT) - 360.0
    ENDIF
  END DO
END DO

DEALLOCATE(ZZSL)
DEALLOCATE(ZZS_XY)
DEALLOCATE(ZXHAT)
DEALLOCATE(ZYHAT)
!
END SUBROUTINE EXPLICIT_SLOPE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE EXPLICIT_SLOPE_LAT_LON(PX,PY,PZS,PSSO_SLOPE,PSSO_DIR)
DOUBLE PRECISION, DIMENSION(:,:),   INTENT(IN) :: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZS ! resolved model orography
DOUBLE PRECISION,DIMENSION(:,:),INTENT(OUT)::PSSO_SLOPE ! resolved slope tangent
DOUBLE PRECISION,DIMENSION(:,:),INTENT(OUT)::PSSO_DIR ! resolved aspect
!
INTEGER :: IX       ! number of points in X direction
INTEGER :: IY       ! number of points in Y direction
!
INTEGER :: INNX !  number of points in X direction for large domain
INTEGER :: INNY !  number of points in Y direction for large domain
!
INTEGER :: JX       ! loop counter
INTEGER :: JY       ! loop counter
!
DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: ZZS_XY        ! orography at southwest corner of the mesh
DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE  :: ZZSL          ! orography in a 2D array
DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE    :: ZXHAT, ZYHAT  ! X Y coordinate
!
DOUBLE PRECISION,    PARAMETER :: XPI=4.*ATAN(1.)  ! Pi
INTEGER, PARAMETER :: JPHEXT = 1 ! number of points around the physical domain
!
DOUBLE PRECISION    :: ZDZSDX   ! slope in X and Y direction
DOUBLE PRECISION    :: ZDZSDY   ! of a triangle surface
DOUBLE PRECISION    :: ZSURF    ! surface of 4 triangles
REAL    :: ZDIRX , ZDIRY   ! slope in X and Y direction of a grid point for aspect calculus
!
INTEGER :: IIB, IIE, IJB, IJE
INTEGER :: JI, JJ, JB
INTEGER :: JT
!-------------------------------------------------------------------------------
!
!*          Gets the geometry of the grid
!          -----------------------------
!
IX = SIZE(PX,1)
IY = SIZE(PY,2)
!
INNX=IX+2
INNY=IY+2
!
ALLOCATE(ZZSL (INNX,INNY))
!
ZZSL(2:INNX-1,2:INNY-1) = PZS(:,:)
ZZSL(1,:) = ZZSL(2,:)
ZZSL(INNX,:) = ZZSL(INNX-1,:)
ZZSL(:,1) = ZZSL(:,2)
ZZSL(:,INNY) = ZZSL(:,INNY-1)

!------------------------------------------------------------------------------------------
!
!*        Orography of SW corner of grid meshes
!         -------------------------------------
!
ALLOCATE(ZZS_XY (INNX,INNY))
!
ZZS_XY(2:INNX,2:INNY) = 0.25*(  ZZSL(2:INNX,2:INNY)   + ZZSL(1:INNX-1,2:INNY)   &
                          + ZZSL(2:INNX,1:INNY-1) + ZZSL(1:INNX-1,1:INNY-1) )
!
ZZS_XY(1,:) = ZZS_XY(2,:)
ZZS_XY(:,1) = ZZS_XY(:,2)
!
!*      Initialize Grid meshes
!       -----------
!
ALLOCATE(ZXHAT (INNX, INNY))
ALLOCATE(ZYHAT (INNX, INNY))
ZXHAT = 0.
ZYHAT = 0.
!
DO JX=2,INNX-1
  DO JY=2,INNY-1
    ZXHAT(JX,JY) = ABS(PX(JX-1,JY-1)-PX(1,JY-1))
    ZYHAT(JX,JY) = ABS(PY(JX-1,JY-1)-PY(JX-1,1))
  END DO
END DO
!
!-------------------------------------------------------------------------------
!
IIB= 1+JPHEXT
IIE=INNX-JPHEXT
IJB=1+JPHEXT
IJE=INNY-JPHEXT
!
!*       1.    LOOP ON GRID MESHES
!              -------------------
!
!* discretization of the grid mesh in four triangles
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    ZSURF=0.
    DO JT=1,4
!
!* slopes in x and y
!
      SELECT CASE (JT)
        CASE (1)
          ZDZSDX=(    2.* ZZSL   (JI,JJ)                   &
                   - (ZZS_XY(JI,JJ)+ZZS_XY(JI,JJ+1)) )    &
                 / (ZXHAT(JI+1,JJ)-ZXHAT(JI,JJ))
          ZDZSDY=(  ZZS_XY(JI,JJ+1) - ZZS_XY(JI,JJ) )     &
                 / (ZYHAT(JI,JJ+1)-ZYHAT(JI,JJ))
        CASE (2)
           ZDZSDX=(  ZZS_XY(JI+1,JJ+1) -ZZS_XY(JI,JJ+1))  &
                 / (ZXHAT(JI+1,JJ)-ZXHAT(JI,JJ))
           ZDZSDY=(  (ZZS_XY(JI+1,JJ+1)+ZZS_XY(JI,JJ+1))  &
                     - 2.* ZZSL (JI,JJ) )                  &
                 / (ZYHAT(JI,JJ+1)-ZYHAT(JI,JJ))
        CASE (3)
          ZDZSDX=(  (ZZS_XY(JI+1,JJ)+ZZS_XY(JI+1,JJ+1))   &
                   - 2.* ZZSL(JI,JJ)                    )  &
                 / (ZXHAT(JI+1,JJ)-ZXHAT(JI,JJ))
          ZDZSDY=(  ZZS_XY(JI+1,JJ+1) - ZZS_XY(JI+1,JJ) ) &
                 / (ZYHAT(JI,JJ+1)-ZYHAT(JI,JJ))
        CASE (4)
           ZDZSDX=(  ZZS_XY(JI+1,JJ) - ZZS_XY(JI,JJ) )    &
                 / (ZXHAT(JI+1,JJ)-ZXHAT(JI,JJ))
           ZDZSDY=(  2.* ZZSL(JI,JJ)                       &
                   - (ZZS_XY(JI+1,JJ)+ZZS_XY(JI,JJ)) )    &
                 / (ZYHAT(JI,JJ+1)-ZYHAT(JI,JJ))
      END SELECT
      !
      ! If slope is higher than 60 degrees : numerical problems
      ZDZSDX=MIN(2.0,MAX(-2.0,ZDZSDX))
      ZDZSDY=MIN(2.0,MAX(-2.0,ZDZSDY))
     ! total surface of 4 triangles
      ZSURF=ZSURF+0.25*SQRT(1. + ZDZSDX**2. + ZDZSDY**2.)
    END DO
    !

    !equivalent tangent slope of a homogeneous surface with the same area
    PSSO_SLOPE(JI-JPHEXT,JJ-JPHEXT)=ATAN(SQRT(ZSURF**2.-1.))*(180./XPI)
    !
    !Calcul de SSO_DIR via formules(9),(10)et (15) de Zevenbergen and Thorne,1987
    ! SSO_DIR en degré from north between 0 and 360 (North = 0°, East = 90 °, South = 180 °, West = 270 °)
    ZDIRX=(  ZZSL(JI+1,JJ) -ZZSL(JI-1,JJ))  &
                 / (ZXHAT(JI+1,JJ)-ZXHAT(JI,JJ))
    ZDIRY=(  ZZSL(JI,JJ+1) - ZZSL(JI,JJ-1) ) &
                 / (ZYHAT(JI,JJ+1)-ZYHAT(JI,JJ))
    PSSO_DIR(JI-JPHEXT,JJ-JPHEXT)=( 1.5*XPI - ATAN2( ZDIRY, ZDIRX + SIGN(1.E-30,ZDIRX) ) )*(180.0/XPI)
    IF (PSSO_DIR(JI-JPHEXT,JJ-JPHEXT) > 360.0) THEN
        PSSO_DIR(JI-JPHEXT,JJ-JPHEXT) = PSSO_DIR(JI-JPHEXT,JJ-JPHEXT) - 360.0
    ENDIF
  END DO
END DO

!
DEALLOCATE(ZZSL)
DEALLOCATE(ZZS_XY)
DEALLOCATE(ZXHAT)
DEALLOCATE(ZYHAT)
!
END SUBROUTINE EXPLICIT_SLOPE_LAT_LON
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE CHECK(STATUS,LINE)
  INTEGER, INTENT ( IN) :: STATUS
    CHARACTER(*), INTENT(IN) :: LINE
  
  IF(STATUS /= NF90_NOERR) THEN 
    PRINT *, TRIM(NF90_STRERROR(STATUS)),":", LINE
    STOP "STOPPED"
  END IF
END SUBROUTINE CHECK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE READ_NML(KNAMUNIT)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: KNAMUNIT

    INTEGER :: IOS

    READ(UNIT=KNAMUNIT,NML=NAM_SWITCHES_INT, IOSTAT=IOS)
    IF (IOS .NE. 0) THEN
      PRINT*, IOS
      STOP 'ERROR reading namelist NAM_SWITCHES_INT'
    END IF
    ! If multiinput is wanted, read the number of inputs wanted, allocate the filename arrays and
    ! read the input filenames and the associated grid filenames
    IF (LMULTIINPUT) THEN
      READ(UNIT=KNAMUNIT, NML=NAM_MULTIIN_SETTING, IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
        STOP 'ERROR reading namelist NAM_MULTIIN_SETTING'
      END IF
      ALLOCATE(HFILESIN(NNUMBER_INPUT_GRIDS), HGRIDSIN(NNUMBER_INPUT_GRIDS))
      READ(UNIT=KNAMUNIT, NML=NAM_FILENAMES_MULTI_IN, IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
        PRINT*, IOS, HFILESIN, HGRIDSIN, SHAPE(HGRIDSIN)
        STOP 'ERROR reading namelist NAM_FILENAMES_MULTI_IN'
      END IF
      IF (LMULTIOUTPUT) THEN
        ALLOCATE(HFILESOUT(NNUMBER_INPUT_GRIDS))
        READ(KNAMUNIT,NML=NAM_FILENAMES_MULTI_OUT, IOSTAT=IOS)
        IF (IOS .NE. 0) THEN
          STOP 'ERROR reading namelist NAM_FILENAMES_MULTI_OUT'
        END IF
      ELSE
        STOP 'ERROR Single output not yet implemented with multiple inputs. Check namelist settings'
        ALLOCATE(HFILESOUT(1))
      END IF
    ! otherwise read the filenames in a scalar variable
    ELSE
      READ(UNIT=KNAMUNIT, NML=NAM_FILENAMES_SINGLE_IN, IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
        STOP 'ERROR: problem reading namelist NAM_FILENAMES_SINGLE_IN'
      END IF
      ALLOCATE(HFILESIN(1), HGRIDSIN(1))
      HFILESIN(1) = HFILEIN
      HGRIDSIN(1) = HGRIDIN
      IF (LMULTIOUTPUT) THEN
        STOP 'ERROR: Output of multiple files not supported for a single input. Check namelist settings'
      ELSE
        READ(UNIT=KNAMUNIT, NML=NAM_FILENAMES_SINGLE_OUT, IOSTAT=IOS)
        IF (IOS .NE. 0) THEN
          STOP 'ERROR: problem reading namelist NAM_FILENAMES_SINGLE_OUT'
        END IF
        ALLOCATE(HFILESOUT(1))
        HFILESOUT(1) = HFILEOUT
      END IF

    END IF

  END SUBROUTINE READ_NML
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM INTERPOLATE_SAFRAN

