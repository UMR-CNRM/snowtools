PROGRAM INTERPOLATE_SAFRAN
! 
USE NETCDF
!
IMPLICIT NONE
include 'mpif.h'
!
!!!!! Files properties
CHARACTER(LEN=28):: HFILENAMEIN, HFILENAMEOUT ,HFILENAMEG     ! Name of the field file.
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
INTEGER,ALLOCATABLE,DIMENSION(:) :: REAL_DIM
INTEGER,DIMENSION(:),ALLOCATABLE::  IVARIN
REAL,DIMENSION(:),ALLOCATABLE::  ZVARIN
REAL,DIMENSION(:,:),ALLOCATABLE::  ZVARINT
REAL ::  ZSCAIN
REAL,DIMENSION(:,:),ALLOCATABLE:: ZVAROUT2D
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZVAROUTXYT
REAL,DIMENSION(:,:),ALLOCATABLE:: ZVAROUTXYT1D
!
DOUBLE PRECISION ,DIMENSION(:,:),ALLOCATABLE:: ZVARLATLON
!ID of spatial and time dimension in input file 
INTEGER :: ILLOC_ID, ITIMEID
!
! MPI parameter: number of processors, rank, error code
INTEGER :: COMM, NPROC, PROC_ID, IERR
INTEGER :: NTIME_PROC, ITSTART, NTIME
INTEGER :: NX_PROC, IXSTART
INTEGER :: NY_PROC, IYSTART
INTEGER :: NPOINT_PROC, IPSTART
!
INTEGER::ID,IV,IA,IT,IDIN,IDOUT, COUNTER,INZ ! loop counter
!
INTEGER :: ITODELETE
!
COMM = MPI_COMM_WORLD
CALL MPI_INIT(IERR)
CALL MPI_COMM_RANK(COMM, PROC_ID, IERR)
CALL MPI_COMM_SIZE(COMM, NPROC, IERR)
!
HFILENAMEIN ='input.nc'
HFILENAMEG =  'GRID.nc'
HFILENAMEOUT = 'output.nc'
!
! Open input file
CALL CHECK(NF90_OPEN(HFILENAMEIN,  IOR(NF90_NOWRITE, NF90_MPIIO),FILE_ID_IN, &
                    comm = COMM, info = MPI_INFO_NULL),&
                    "Cannot open file "//TRIM(HFILENAMEIN))
!
! Open output file 
CALL CHECK(NF90_create(HFILENAMEOUT,IOR(IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL),NF90_MPIIO),FILE_ID_OUT, &
                       comm = COMM, info = MPI_INFO_NULL),&
                       "Cannot open file "//TRIM(HFILENAMEOUT))
!
! Open grid file
CALL CHECK(NF90_OPEN(HFILENAMEG, IOR(NF90_NOWRITE, NF90_MPIIO), FILE_ID_GEO, & 
                     comm = COMM, info = MPI_INFO_NULL),&
                     "Cannot open file"//TRIM(HFILENAMEG))
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
VAR_ID_DIMS_OUT = 0
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
!!!! Get interpolation informations
!Read altitude massif numbers and aspects
CALL READ_OUTPUT_GRID(FILE_ID_GEO,ZZSOUT,IMASSIFOUT,ZASPECTOUT,IRANK, &
                    GRID_TYPE,GRID_DIM_REF,ZLATOUT,ZLONOUT,ZYOUT,ZXOUT,&
                    ZSLOPEOUT)
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
    ITODELETE = IDOUT
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
    DO IDIN=1,INDIM
      DO ID=1,INDIM+1      
        ! identify output dimension id and input dimension id
        IF (VAR_ID_DIMS_IN(IDIN,IV)==DIM_ID_OUT(ID)) &
            VAR_ID_DIMS_OUT(ID,IV) = DIM_ID_OUT(ID)
      ENDDO
    ENDDO
  ENDDO
ELSE IF (IRANK==2) THEN
  DO IV=1,INVAR
    DO IDIN=1,INDIM
      DO ID=1,INDIM+1        
        IF (VAR_ID_DIMS_IN(IDIN,IV)==DIM_ID_OUT(ID)) THEN
          IF (DIM_NAME_IN(ID) == 'time') THEN
            VAR_ID_DIMS_OUT(ITIMEID,IV) = DIM_ID_OUT(ITIMEID)
          ELSEIF (DIM_NAME_IN(ID) == 'Number_of_points')THEN
            VAR_ID_DIMS_OUT(ILLOC_ID,IV) = DIM_ID_OUT(ILLOC_ID)
            ! if 2d output grid add second coordinate in variable dimensions
            VAR_ID_DIMS_OUT(ILLOC_ID+1,IV)= DIM_ID_OUT(ILLOC_ID+1)
          ELSE
            IF (DIM_ID_OUT(ID) .GE. ILLOC_ID+1 ) THEN
              VAR_ID_DIMS_OUT(ID+1,IV) = DIM_ID_OUT(ID+1)          
            ELSE
              VAR_ID_DIMS_OUT(ID,IV) = DIM_ID_OUT(ID)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
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
  IF (ANY( VAR_ID_DIMS_OUT(:,IV) .EQ. ITODELETE) .OR. &
  (GRID_TYPE == "LL" .AND. (VAR_NAME_IN(IV) == "LAT" .OR. VAR_NAME_IN(IV) == "LON")))CYCLE
  COUNTER=0
  DO ID = 1,INDIM+1
    IF (VAR_ID_DIMS_OUT(ID,IV) .NE. 0) &
      COUNTER = COUNTER +1
  ENDDO
  ALLOCATE(REAL_DIM(COUNTER))
  REAL_DIM =0
  INZ =0
  DO ID = 1,INDIM+1
    IF (VAR_ID_DIMS_OUT(ID,IV) .NE. 0) THEN
        INZ = INZ +1
        REAL_DIM(INZ) =VAR_ID_DIMS_OUT(ID,IV)
    ENDIF
  ENDDO
  ! Create variable in output file
  CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,VAR_NAME_IN(IV),VAR_TYPE_IN(IV), & 
          REAL_DIM(counter:1:-1),VAR_ID_OUT(IV)),& 
          "Cannot def var "//TRIM(VAR_NAME_IN(IV)))      
  DEALLOCATE(REAL_DIM)
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
                start= (/IYSTART/) ,count =(/NY_PROC/)),"Cannot put lat")
    ! lon     
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+2),ZLONOUT ,& 
                start= (/IXSTART/) ,count =(/NX_PROC/)),"Cannot put lon") 
  ELSEIF (GRID_TYPE == "XY") THEN
    !y
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+1),ZYOUT ,& 
                start= (/IYSTART/) ,count =(/NY_PROC/)),"Cannot put y")
    !x     
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+2),ZXOUT ,& 
                start= (/IXSTART/) ,count =(/NX_PROC/)),"Cannot put x") 
                
  ELSE
    STOP "INCORRECT TYPE OF GRID 2D"
  ENDIF
ENDIF
!
NTIME =  DIM_SIZE_IN(1)
CALL TIME_PROC_DISTRIBUTOR(NTIME,PROC_ID,NPROC,ITSTART,NTIME_PROC)
ALLOCATE(VALUE_TIME(NTIME_PROC))  
DO IV=1,INVAR
  IF (ANY( VAR_ID_DIMS_OUT(:,IV) .EQ. ITODELETE))CYCLE
  IF (GRID_TYPE == "LL" .AND. (VAR_NAME_IN(IV) == "LAT" .OR. VAR_NAME_IN(IV) == "LON"))  CYCLE
!  !Unlimited dimensions require collective writes
  CALL CHECK(NF90_VAR_PAR_ACCESS(FILE_ID_OUT, VAR_ID_OUT(IV), nf90_collective),&
                    "collective write")
                
  IF (VAR_NAME_IN(IV) == "time") THEN
    !
    CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),VALUE_TIME &
     , start =(/ITSTART/), count = (/NTIME_PROC/)) &
     ,"Cannot get var time")
    !    
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),VALUE_TIME  &
    , start = (/ITSTART/), count =(/NTIME_PROC/) ),"Cannot put var time")
    !
  ELSEIF (VAR_NAME_IN(IV) == "ZS") THEN
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZZSOUT , &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var ZS")
  ELSEIF (VAR_NAME_IN(IV) == "aspect") THEN
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZASPECTOUT, &
    start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
    "Cannot put var aspect")
  ELSEIF (VAR_NAME_IN(IV) == "slope") THEN
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZSLOPEOUT, &
    start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
    "Cannot put var slope")
  ELSEIF (VAR_NAME_IN(IV) == "massif_number") THEN
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),IMASSIFOUT, &
    start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
    "Cannot put var massif_number")
  ELSEIF (VAR_NAME_IN(IV) == "LAT" .AND. GRID_TYPE == "XY") THEN
    !    
    ALLOCATE(ZVARLATLON(NX_PROC,NY_PROC))
    CALL LATLON_IGN(ZXOUT,ZYOUT,PLAT=ZVARLATLON)
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARLATLON , &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var LAT")
    DEALLOCATE(ZVARLATLON)
!!    !
  ELSEIF (VAR_NAME_IN(IV) == "LON" .AND. GRID_TYPE == "XY") THEN
    !
    ALLOCATE(ZVARLATLON(NX_PROC,NY_PROC))
    CALL LATLON_IGN(ZXOUT,ZYOUT,PLON=ZVARLATLON)       
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARLATLON , &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var LON")
    DEALLOCATE(ZVARLATLON)
    ! 
  ELSEIF (ALL(VAR_ID_DIMS_IN(:,IV) .NE. ILLOC_ID) .AND. &
          ANY(VAR_ID_DIMS_IN(:,IV) .EQ.ITIMEID))THEN !TIME DIM and NOGRID DIM
    !
    ALLOCATE(ZVARINT(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV)),NTIME))   
    !
    CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARINT),"Cannot get var"//TRIM(HFILENAMEIN))
    !
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARINT),"Cannot put var"//TRIM(HFILENAMEOUT))
    DEALLOCATE(ZVARINT)
    !
  ELSEIF (ALL(VAR_ID_DIMS_IN(:,IV) == 0))THEN !SCALAR
    CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZSCAIN),"Cannot get var"//TRIM(HFILENAMEIN))
    !
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZSCAIN),"Cannot put var"//TRIM(HFILENAMEOUT))
    !
  ELSEIF (ALL(VAR_ID_DIMS_IN(:,IV) .NE. ILLOC_ID) .AND. &
          ALL(VAR_ID_DIMS_IN(:,IV) .NE.ITIMEID))THEN !NO TIME DIM and NOGRID DIM
    !
    ALLOCATE(ZVARIN(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV))))
    !
    CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARIN),"Cannot get var"//TRIM(HFILENAMEIN))
    !
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARIN),"Cannot put var"//TRIM(HFILENAMEOUT))
    !
    DEALLOCATE(ZVARIN)
  ELSEIF (ANY(VAR_ID_DIMS_IN(:,IV) .EQ. ILLOC_ID) .AND. &
          ANY(VAR_ID_DIMS_IN(:,IV) .EQ.ITIMEID))THEN !TIME and GRID DIM
    !  
    ALLOCATE(ZVARINT(DIM_SIZE_IN(ILLOC_ID),NTIME))
    ALLOCATE(ZVAROUTXYT(NX_PROC,NY_PROC,NTIME))
    !  Read variable
    CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARINT, &
    start =(/1,1/), count = (/DIM_SIZE_IN(ILLOC_ID),NTIME/)) &
    ,"Cannot get var"//TRIM(HFILENAMEIN))
    ! 
    CALL INTERPOLZS2D(ZVAROUTXYT,ZVARINT,IINDICESBAS,IINDICESHAUT,&
    ZZSOUT,IZSIN)
    !
    IF (IRANK ==1 )THEN 
      ALLOCATE(ZVAROUTXYT1D(NX_PROC,NTIME))
      ZVAROUTXYT1D = ZVAROUTXYT(:,1,:)
      ! Write variable
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT1D,  &
      start =(/IXSTART,1/) ,count = (/NX_PROC,NTIME/)),&
      "Cannot put var"//TRIM(HFILENAMEOUT))
      DEALLOCATE(ZVAROUTXYT1D)
    ELSEIF( IRANK == 2)THEN 
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT,  &
      start =(/IXSTART,IYSTART,1/) ,count = (/NX_PROC,NY_PROC,NTIME/)),&
      "Cannot put var"//TRIM(HFILENAMEOUT))
    ENDIF
    !
    DEALLOCATE(ZVARINT)
    DEALLOCATE(ZVAROUTXYT)
    !
  ELSEIF (ANY(VAR_ID_DIMS_IN(:,IV) .EQ. ILLOC_ID ) .AND. &
          ALL(VAR_ID_DIMS_IN(:,IV) .NE. ITIMEID))THEN !GRID DIM
    !      
    ALLOCATE(ZVARIN(DIM_SIZE_IN(ILLOC_ID)))
    ALLOCATE(ZVAROUT2D(NX_PROC,NY_PROC))
    !  Read variable
    CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARIN, &
    start =(/1/), count = (/DIM_SIZE_IN(ILLOC_ID)/)) &
    ,"Cannot get var"//TRIM(HFILENAMEIN))
    ! 
    CALL INTERPOLZS2DNOTIME(ZVAROUT2D,ZVARIN,IINDICESBAS,IINDICESHAUT,&
    ZZSOUT,IZSIN)
    !
    ! Write variable
    CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUT2D,  &
    start =(/IXSTART,IYSTART/) ,count = (/NX_PROC,NY_PROC/)),&
    "Cannot put var"//TRIM(HFILENAMEOUT))
    !
    DEALLOCATE(ZVARIN)
    DEALLOCATE(ZVAROUT2D)
    !
  ELSE
    !
    PRINT*, VAR_NAME_IN(IV),  VAR_ID_DIMS_IN(:,IV), "DIMMENSION VAR NON TRAITER"
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
!
CALL MPI_FINALIZE(IERR)
!
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE TIME_PROC_DISTRIBUTOR(ITIMEDIM,IPROC_ID, INPROC,ISTART, ITIMEDIMPROC)
INTEGER,INTENT(IN) :: IPROC_ID, INPROC, ITIMEDIM
INTEGER ,INTENT(OUT):: ISTART, ITIMEDIMPROC
!
ITIMEDIMPROC = ITIMEDIM/INPROC
IF(IPROC_ID == 0)  THEN
  ISTART=1
ELSE
  ISTART = (IPROC_ID)*ITIMEDIMPROC+1
ENDIF
IF(IPROC_ID == INPROC-1) THEN
  ITIMEDIMPROC = ITIMEDIMPROC +MOD(ITIMEDIM,INPROC)
ENDIF
END SUBROUTINE 
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
DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE :: PX2D,PY2D
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
  ixstart= IPSTART
  IYSTART = 1
  NY_PROC = 1
  NX_PROC = NPOINT_PROC
ENDIF
!
ALLOCATE(PZSOUT(NX_PROC,NY_PROC))
ALLOCATE(KMASSIFOUT(NX_PROC,NY_PROC))
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
ALLOCATE(PASPECTOUT(NX_PROC,NY_PROC))
ALLOCATE(PSLOPEOUT(NX_PROC,NY_PROC))
IF(IRANK == 1)THEN
  !
  ALLOCATE(PLATOUT(NPOINT_PROC))
  ALLOCATE(PLONOUT(NPOINT_PROC))
  IF(NF90_INQ_VARID(FILE_ID_GEO,HASPECT,IDVARGASPECT) .EQ. NF90_NOERR ) THEN
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGASPECT,PASPECTOUT , &
     start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)), &
                       "Cannot read "//HASPECT)
  ELSE
    PASPECTOUT = -1.
  ENDIF 
  !
  IF (NF90_INQ_VARID(FILE_ID_GEO,HSLOPE,IDVARSLOPE).EQ. NF90_NOERR)THEN
    !
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARSLOPE,PSLOPEOUT , &
       start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)), &
                         "Cannot read "//HSLOPE)
  ELSE
    PSLOPEOUT = 0.
  ENDIF
  !
ELSEIF(IRANK == 2)THEN
  IF (GT == "XY") THEN  !X Y GRID
    ALLOCATE(PYOUT(NY_PROC))
    ALLOCATE(PXOUT(NX_PROC))
    !Y
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HY,IDY), "Cannot find "//HY)
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDY,PYOUT, &
               start =(/IYSTART/) , count = (/NY_PROC/)), "Cannot read "//HY)
    !X
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HX,IDX), "Cannot find "//HX)
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDX,PXOUT, &
               start =(/IXSTART/) , count = (/NX_PROC/)), "Cannot read "//HX)
    ! Calcul slope, aspect
    CALL EXPLICIT_SLOPE(PXOUT,PYOUT,PZSOUT,PSLOPEOUT,PASPECTOUT)
    !   
  ELSEIF (GT == "LL") THEN  !LAT LON GRID
    ALLOCATE(PLATOUT(NY_PROC))
    ALLOCATE(PLONOUT(NX_PROC))
    !LAT
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HLAT,IDVARLAT), "Cannot find "//HLAT)
    
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARLAT,PLATOUT, &
               start =(/IYSTART/) , count = (/NY_PROC/)), "Cannot read "//HLAT)
    !LON
    CALL CHECK(NF90_INQ_VARID(FILE_ID_GEO,HLON,IDVARLON) ,"Cannot find "//HLON)
    
    CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARLON,PLONOUT, &
               start =(/IXSTART/) , count = (/NX_PROC/)), "Cannot read "//HLON)
    ! Calcul slope, aspect
    ALLOCATE(PX2D(NX_PROC,NY_PROC))
    ALLOCATE(PY2D(NX_PROC,NY_PROC))
    CALL XY_IGN(PLATOUT,PLONOUT,PX2D,PY2D)
    CALL EXPLICIT_SLOPE_LAT_LON(PX2D,PY2D,PZSOUT,PSLOPEOUT,PASPECTOUT)
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
INTEGER::IDVARGZS,IDVARGMASSIF,IDVARGASPECT ! nc variable identifier
INTEGER,DIMENSION(:),ALLOCATABLE::IDDIMSVARG ! dimension ids of nc variable
!
INTEGER::ID ! dimension loop counter
INTEGER::IRANKNC ! rank of output grid (1 for vector or 2 for matrix)
INTEGER,DIMENSION(1:2)::ILENDIM ! lenghts of dimensions
CHARACTER(*),PARAMETER::HZS= 'ZS'
CHARACTER(*),PARAMETER::HMASSIF='massif_number'
CHARACTER(*),PARAMETER::HASPECT='aspect'
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
!
CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGZS,KZSIN), "Cannot read "//HZS)
!
!
CALL CHECK(NF90_INQ_VARID(IDFICNC,HMASSIF,IDVARGMASSIF),"Cannot find "//HMASSIF)
!
CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGMASSIF,KMASSIFIN), "Cannot read "//HMASSIF)
!
!
CALL CHECK(NF90_INQ_VARID(IDFICNC,HASPECT,IDVARGASPECT),"Cannot find "//HASPECT)
!
CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGASPECT,KASPECTIN), "Cannot read "//HASPECT)
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

REAL,DIMENSION(:,:,:),INTENT(OUT)::PVAROUT
REAL,DIMENSION(:,:),INTENT(IN)::PVARIN
INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS,KINDHAUT
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZSGRID
INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN
REAL ::ZA,ZB,ZC
INTEGER::JX,JY,JT !Loop counters
INTEGER::KNX,KNY,KNSAFRAN,KNT
!
KNX=SIZE(PVAROUT,1)
KNY=SIZE(PVAROUT,2)
KNSAFRAN=SIZE(PVARIN,1)
KNT=SIZE(PVARIN,2)
!
PVAROUT = 0.
DO JX=1,KNX
    DO JY=1,KNY
      ! Interpolation parameters constant over time
      ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
      ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
      ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
      !
      DO JT=1,KNT      
        PVAROUT(JX,JY,JT)= (ZA*PVARIN(KINDHAUT(JX,JY),JT)+ZB*PVARIN(KINDBAS(JX,JY),JT))/ZC 
      END DO
    END DO
END DO
!
END SUBROUTINE INTERPOLZS2D
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE INTERPOLZS2DNOTIME(PVAROUT,PVARIN,KINDBAS,KINDHAUT,PZSGRID,KZSSAFRAN)
! Linear interpolation between SAFRAN elevation levels over 2D grids
REAL,DIMENSION(:,:),INTENT(OUT)::PVAROUT
REAL,DIMENSION(:),INTENT(IN)::PVARIN
INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS,KINDHAUT
DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::PZSGRID
INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN
REAL ::ZA,ZB,ZC
INTEGER::JX,JY !Loop counters
INTEGER::KNX,KNY,KNSAFRAN
!
KNX=SIZE(PVAROUT,1)
KNY=SIZE(PVAROUT,2)
KNSAFRAN=SIZE(PVARIN,1)
!
PVAROUT = 0.
DO JX=1,KNX
    DO JY=1,KNY
      ! Interpolation parameters constant over time
      ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
      ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
      ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
      !
      PVAROUT(JX,JY)= (ZA*PVARIN(KINDHAUT(JX,JY))+ZB*PVARIN(KINDBAS(JX,JY)))/ZC 
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
END PROGRAM INTERPOLATE_SAFRAN

