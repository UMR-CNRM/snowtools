MODULE SUBS
  USE NETCDF
  USE MODN_INTERPOL_SAFRAN
  USE MPI
  IMPLICIT NONE
  !!!!! Files properties
  CHARACTER(LEN=100):: HFILENAMEG ! Filename of the grid definition file
  !!!!! Dim properties
  CHARACTER(LEN=20),DIMENSION(:),ALLOCATABLE:: DIM_NAME_OUT ! name of output dimensions
  !
  REAL,    PARAMETER :: XUNDEF =  -9999999. ! Fill_Value for netcdf

  INTEGER :: COMM ! MPI parameter
  INTEGER :: NPROC ! Number of MPI processors
  INTEGER :: PROC_ID ! MPI rank
  INTEGER :: IERR ! MPI error code
  INTEGER :: NTIME ! length of time dimension
  INTEGER :: NX ! length of first spatial dimension
  INTEGER :: NY ! length of second spatial dimension
  INTEGER :: NX_PROC ! number of indices in the x-dimension to be treated by the processor
  INTEGER :: IXSTART ! start index in the x-dimension for the given processor
  INTEGER :: NY_PROC ! number of indices in the y-dimension to be treated by the processor
  INTEGER :: IYSTART ! start index in the y-dimension for the given processor
  INTEGER :: NPOINT_PROC ! number of indices in the spatial dimension to be treated by the processor
  INTEGER :: IPSTART ! start index in the spatial dimension (points) for the given processor

CONTAINS

  SUBROUTINE ABORT_INTERPOLATE(YTEXT)
  ! Crash the program
  CHARACTER(LEN=*),  INTENT(IN)  :: YTEXT
  !
  write(0,*) "aborted with text:",TRIM(ytext),"|"
   
  CALL ABORT

  !MPI_ABORT(COMM, IERR)
  CALL MPI_FINALIZE(IERR)

  STOP  
  !
  END SUBROUTINE ABORT_INTERPOLATE
  !
  SUBROUTINE XY_PROC_DISTRIBUTOR(IXYDIM,IPROC_ID, INPROC,IIXSTART, INX_PROC,IIYSTART, INY_PROC)
    ! Routine used for parallelisation with MPI.
    !
    ! Defines indices of the two spatial dimensions to be treated by each of the processors.
    INTEGER, INTENT(IN) :: IPROC_ID
    INTEGER, INTENT(IN) :: INPROC
    INTEGER,DIMENSION(2),INTENT(IN) ::  IXYDIM ! Dimension lengths of the spatial dimensions
    INTEGER, INTENT(OUT):: IIXSTART ! start index in the x-dimension for the given processor
    INTEGER, INTENT(OUT):: INX_PROC ! number of indices in the x-dimension to be treated by the processor
    INTEGER, INTENT(OUT):: IIYSTART ! start index in the y-dimension for the given processor
    INTEGER, INTENT(OUT):: INY_PROC ! number of indices in the y-dimension to be treated by the processor
    !
    IF (INPROC > 1 ) THEN

      INX_PROC = IXYDIM(1)/2
      IF (MOD(IPROC_ID,2) == 0) THEN
        IIXSTART=1
      ELSE
        IIXSTART = INX_PROC+1
        INX_PROC = INX_PROC +MOD(IXYDIM(1),2)
      ENDIF
      !
      INY_PROC = IXYDIM(2)/FLOOR(REAL(INPROC/2.))
      !
      IF (MOD(IPROC_ID,2) == 0) THEN
        IIYSTART =(IPROC_ID/2)* INY_PROC+1
      ELSE
        IIYSTART =((IPROC_ID-1)/2)* INY_PROC+1
      ENDIF
      !
      IF ( MOD(INPROC,2) /= 0 .AND. IPROC_ID == INPROC-1) THEN
        INY_PROC = MOD(IXYDIM(2),(INPROC/2))
        INX_PROC = IXYDIM(1)
        IIXSTART = 1
      ELSEIF ( MOD(INPROC,2) == 0 .AND. (IPROC_ID == INPROC-1 .OR. IPROC_ID == INPROC-2))THEN
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
    ! Routine used for parallelisation with MPI.
    !
    ! Defines indices of the unique spatial dimension in the 1D case to be treated by each of the processors.
    INTEGER, INTENT(IN):: IPROC_ID
    INTEGER, INTENT(IN):: INPROC
    INTEGER,DIMENSION(2),INTENT(IN) ::  IPDIM ! Dimension lengths of the spatial dimensions
    INTEGER, INTENT(OUT):: IIPSTART ! start index in the spatial dimension (points) for the given processor
    INTEGER, INTENT(OUT):: INP_PROC ! number of indices in the spatial dimension to be treated by the processor
    !
    IF (INPROC > 1 ) THEN
      !
      IF ( MOD(IPDIM(1),INPROC) == 0 ) THEN
        INP_PROC = IPDIM(1)/INPROC
        IIPSTART= (IPROC_ID*INP_PROC)+1
      ELSEIF (  MOD(IPDIM(1),INPROC) /= 0)THEN
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
          GT,ILENDIM,PLATOUT,PLONOUT,PYOUT,PXOUT,PSLOPEOUT,KSTATIONOUT, KMASSIFGATHER)
    ! Read the netcdf file containing the output grid definition (default: GRID.nc)
    INTEGER,INTENT(IN) :: FILE_ID_GEO ! Netcdf id of the grid file
    ! OUTPUT VARIABLE 2D
    INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: KMASSIFOUT ! Massif number of the output points
    INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: KMASSIFGATHER ! Massif number of the output points gathered from all procs
    INTEGER,DIMENSION(:), ALLOCATABLE, INTENT(OUT):: KSTATIONOUT ! Station number of the output points (used in the 1D case only)
    REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: PZSOUT  ! Elevation of output grid
    REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: PASPECTOUT ! aspect of output grid
    REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: PSLOPEOUT ! slope of output grid
    !
    REAL(KIND=8),DIMENSION(:),ALLOCATABLE,INTENT(OUT):: PLATOUT ! output latitudes of 2D grid or 1D stations
    REAL(KIND=8),DIMENSION(:),ALLOCATABLE,INTENT(OUT):: PLONOUT ! output longitudes of 2D grid or 1D Stations
    REAL(KIND=8),DIMENSION(:),ALLOCATABLE,INTENT(OUT):: PYOUT ! output y coordinates in the case of 2D grid with a XY grid type
    REAL(KIND=8),DIMENSION(:),ALLOCATABLE,INTENT(OUT):: PXOUT ! output x coordinates in the case of 2D grid with a XY grid type
    INTEGER,INTENT(OUT)::IRANK ! rank of output grid (1 for vector or 2 for matrix)
    INTEGER,DIMENSION(1:2),INTENT(OUT)::ILENDIM ! lenghts of dimensions
    CHARACTER(LEN=2), INTENT(OUT):: GT ! GRID TYPE

    !INTEGER::IDVARG ! nc variable identifier
    INTEGER::IDVARGZS,IDVARGMASSIF,IDVARGASPECT,IDVARSLOPE,IDVARLAT,IDVARLON,IDY,IDX, IDVARSTATION ! nc variable identifier
    INTEGER,DIMENSION(:),ALLOCATABLE::IDDIMSVARG ! dimension ids of nc variable
    !
    REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE :: ZX2D,ZY2D, ZZSALL, ZASPECTALL, ZSLOPEALL ! variables covering the whole domain
    INTEGER,DIMENSION(NPROC) :: PROC_XCOUNT, PROC_YCOUNT, PROC_STRIDE
    !
    INTEGER::ID, IPROC ! dimension loop counter
    !
    CHARACTER(*),PARAMETER::HZS='ZS'
    CHARACTER(*),PARAMETER::HMASSIF='massif_num'
    CHARACTER(*),PARAMETER::HASPECT='aspect'
    CHARACTER(*),PARAMETER::HSLOPE='slope'
    CHARACTER(*),PARAMETER::HLAT='latitude'
    CHARACTER(*),PARAMETER::HLAT1D='LAT'
    CHARACTER(*),PARAMETER::HLON='longitude'
    CHARACTER(*),PARAMETER::HLON1D='LON'
    CHARACTER(*),PARAMETER::HX='x'
    CHARACTER(*),PARAMETER::HY='y'
    CHARACTER(*),PARAMETER::HSTATION='station'
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
      IF (ANY(DIM_NAME_OUT(:) == "x"))THEN
        GT="XY"
      ELSEIF (ANY(DIM_NAME_OUT == "lon"))THEN
        GT="LL"
      ELSE
        CALL ABORT_INTERPOLATE("GRID 2D NON TRAITE")
      ENDIF
      CALL XY_PROC_DISTRIBUTOR(ILENDIM,PROC_ID, NPROC,IXSTART, NX_PROC,IYSTART, NY_PROC)
    ELSEIF(IRANK == 1)THEN
      GT="LL"
      CALL NPOINT_PROC_DISTRIBUTOR(ILENDIM,PROC_ID,NPROC,IPSTART,NPOINT_PROC)
      IXSTART= IPSTART
      IYSTART = 1
      NY_PROC = 1
      NX_PROC = NPOINT_PROC
      ALLOCATE(KSTATIONOUT(NX))
    ENDIF
    !
    ALLOCATE(PZSOUT(NX_PROC,NY_PROC))
    PZSOUT= XUNDEF
    ALLOCATE(PASPECTOUT(NX_PROC,NY_PROC))
    PASPECTOUT = -1.
    ALLOCATE(PSLOPEOUT(NX_PROC,NY_PROC))
    PSLOPEOUT = 0.
    ALLOCATE(KMASSIFOUT(NX_PROC,NY_PROC), KMASSIFGATHER(NX,NY))
    KMASSIFOUT = 0
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
    ! PRINT*,NX_PROC,NY_PROC
    CALL MPI_ALLGATHER(NX_PROC, 1, MPI_INT, PROC_XCOUNT, 1, MPI_INT, COMM, IERR)
    CALL MPI_ALLGATHER(NY_PROC, 1, MPI_INT, PROC_YCOUNT, 1, MPI_INT, COMM, IERR)
    ! PRINT*,PROC_XCOUNT, PROC_YCOUNT, PROC_ID
    PROC_STRIDE(1) = 0
    IF (NPROC > 1) THEN
      DO IPROC=2,NPROC
        PROC_STRIDE(IPROC) = PROC_STRIDE(IPROC-1) + PROC_XCOUNT(IPROC-1)*PROC_YCOUNT(IPROC-1)
      END DO
    END IF
    ! PRINT*, PROC_STRIDE
    CALL MPI_ALLGATHERV(KMASSIFOUT, NX_PROC*NY_PROC, MPI_INT, KMASSIFGATHER, PROC_XCOUNT*PROC_YCOUNT, PROC_STRIDE, &
            MPI_INT, COMM, IERR)
    ! 
    IF(IRANK == 1)THEN
      !
      ALLOCATE(PLATOUT(NX))
      ALLOCATE(PLONOUT(NX))
      IF(NF90_INQ_VARID(FILE_ID_GEO,HASPECT,IDVARGASPECT) == NF90_NOERR ) THEN
        CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGASPECT,PASPECTOUT , &
                start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)), &
                "Cannot read "//HASPECT)
      ENDIF
      !
      IF (NF90_INQ_VARID(FILE_ID_GEO,HSLOPE,IDVARSLOPE) == NF90_NOERR) THEN
        !
        CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARSLOPE,PSLOPEOUT , &
                start =(/IXSTART,IYSTART/) , count = (/NX_PROC,NY_PROC/)), &
                "Cannot read "//HSLOPE)
      ENDIF
      IF (NF90_INQ_VARID(FILE_ID_GEO, HLAT1D,IDVARLAT) == NF90_NOERR) THEN
        CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARLAT,PLATOUT , &
                start =(/1/) , count = (/NX/)), &
                "Cannot read "//HLAT1D)
      END IF
      IF (NF90_INQ_VARID(FILE_ID_GEO, HLON1D,IDVARLON) == NF90_NOERR) THEN
        CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARLON,PLONOUT , &
                start =(/1/) , count = (/NX/)), &
                "Cannot read "//HLON1D)
      END IF
      IF (NF90_INQ_VARID(FILE_ID_GEO, HSTATION,IDVARSTATION) == NF90_NOERR) THEN
        CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARSTATION,KSTATIONOUT , &
                start =(/1/) , count = (/NX/)), &
                "Cannot read "//HSTATION)
      END IF
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
        ALLOCATE(ZZSALL(NX,NY))
        ALLOCATE(ZASPECTALL(NX,NY))
        ALLOCATE(ZSLOPEALL(NX,NY))
        ZZSALL= XUNDEF
        !maybe a gather from all procs is more efficient ?
        CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGZS,ZZSALL, &
                start =(/1,1/) , count = (/NX,NY/)), "Cannot read all "//HZS)
        ! Calcul slope, aspect
        CALL EXPLICIT_SLOPE(PXOUT,PYOUT,ZZSALL,ZSLOPEALL,ZASPECTALL)
        !
        ! Local values of aspect and slope are extracted from the arrays covering the whole domain
        PASPECTOUT(:,:) = ZASPECTALL(IXSTART:IXSTART+NX_PROC-1, IYSTART:IYSTART+NY_PROC-1)
        PSLOPEOUT(:,:) = ZSLOPEALL(IXSTART:IXSTART+NX_PROC-1, IYSTART:IYSTART+NY_PROC-1)
        !
        DEALLOCATE(ZZSALL)
        DEALLOCATE(ZASPECTALL)
        DEALLOCATE(ZSLOPEALL)
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
        ALLOCATE(ZX2D(NX,NY))
        ALLOCATE(ZY2D(NX,NY))
        CALL XY_IGN(PLATOUT,PLONOUT,ZX2D,ZY2D)

        ALLOCATE(ZZSALL(NX,NY))
        ALLOCATE(ZASPECTALL(NX,NY))
        ALLOCATE(ZSLOPEALL(NX,NY))
        !maybe a gather from all procs is more efficient ?
        CALL CHECK(NF90_GET_VAR(FILE_ID_GEO,IDVARGZS,ZZSALL, &
                start =(/1,1/) , count = (/NX,NY/)), "Cannot read all "//HZS)
        CALL EXPLICIT_SLOPE_LAT_LON(ZX2D,ZY2D,ZZSALL,ZSLOPEALL,ZASPECTALL)
        ! Local values of aspect and slope are extracted from the arrays covering the whole domain
        PASPECTOUT(:,:) = ZASPECTALL(IXSTART:IXSTART+NX_PROC-1, IYSTART:IYSTART+NY_PROC-1)
        PSLOPEOUT(:,:) = ZSLOPEALL(IXSTART:IXSTART+NX_PROC-1, IYSTART:IYSTART+NY_PROC-1)

        DEALLOCATE(ZZSALL)
        DEALLOCATE(ZASPECTALL)
        DEALLOCATE(ZSLOPEALL)
        DEALLOCATE(ZX2D)
        DEALLOCATE(ZY2D)
      ELSE
        CALL ABORT_INTERPOLATE("GRID 2D NOT TRAITED")
      ENDIF
      !
    ENDIF
    !
  END SUBROUTINE READ_OUTPUT_GRID
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE READ_NC(IDFICNC,KZSIN,KMASSIFIN,KASPECTIN, PSLOPEIN)
    ! Read NetCDF file with forcing data to be interpolated.
    INTEGER,INTENT(IN) :: IDFICNC ! NetCDF id of the forcing file
    INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(OUT):: KZSIN  ! Elevation of the input data points
    INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(OUT):: KMASSIFIN ! Massif of the input data points
    INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(OUT):: KASPECTIN ! Aspect of the input data points
    REAL,DIMENSION(:),ALLOCATABLE, INTENT(OUT):: PSLOPEIN ! Slope of the input data points
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
    ALLOCATE(PSLOPEIN(ILENDIM(1)))
    !
    CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGZS,KZSIN), "Cannot read "//HZS)
    !
    !
    ISTATUS = NF90_INQ_VARID(IDFICNC,HMASSIF2,IDVARGMASSIF)
    IF (ISTATUS /= NF90_NOERR ) &
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
    CALL CHECK(NF90_GET_VAR(IDFICNC,IDVARGSLOPE,PSLOPEIN), "Cannot read "//HSLOPE)
    !
    IF (MAXVAL(PSLOPEIN) > 1.) THEN
      WRITE(*,*) "WARNING: Input file contains non-zero slopes. Only zero slopes will be interpolated"
      ! STOP "INPUT FORCING FILE MUST PROVIDE HORIZONTAL RADIATIONS AND MUST NEVER HAVE BEEN PROJECTED ON SLOPES."
    END IF
    !
  END SUBROUTINE READ_NC
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE INDICES2D(PZSOUT,KMASSIFOUT,PASPECTOUT,PSLOPEOUT,KZSIN,KMASSIFIN,&
          KASPECTIN, PSLOPEIN, KINDICESBAS,KINDICESHAUT)
   ! Determines the  indexes of input collection of points to interpolate between in order to obtain the output grid.
    !
    ! Selects the hight level above and below the hight of the output grid point, at the corresponding massif number and
    ! aspect
    INTEGER,DIMENSION(:,:),INTENT(IN):: KMASSIFOUT  ! Massif of output collection of points
    ! DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN):: PZSOUT
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN):: PZSOUT ! Elevation of output collection of points
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN):: PASPECTOUT ! Aspect of output collection of points
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN):: PSLOPEOUT ! Slope of output collection of points
    INTEGER,DIMENSION(:),INTENT(IN):: KZSIN ! Elevation of input collection of points
    INTEGER,DIMENSION(:),INTENT(IN):: KMASSIFIN ! Massif of input collection of points
    INTEGER,DIMENSION(:),INTENT(IN):: KASPECTIN ! Aspect of input collection of points
    REAL, DIMENSION(:), INTENT(IN):: PSLOPEIN ! slope of input collection of points
    !
    ! Indexes of input collection of points to interpolate to obtain output grid
    INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: KINDICESBAS ! Indices of the input collection of points corresponding to the points below the target point
    INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(OUT):: KINDICESHAUT ! Indices of the input collection of points corresponding to the points above the target point
    !
    INTEGER :: INX,INY ! dimensions of output grid
    INTEGER :: ININ    ! dimension of input collection of points
    !
    INTEGER::JI,JX,JY ! loop counters
    REAL::ZDIFFZS ! elevation difference between output point and input point TAKE CARE MUST BE REAL !
    !
    LOGICAL :: GASPECT, GSLOPE, GISFLAT
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
    IF (ALL(KASPECTIN == -1)) GISFLAT = .TRUE.
    DO JX=1,INX
      DO JY=1,INY
        IF(PZSOUT(JX,JY) == XUNDEF .OR.  KMASSIFOUT(JX,JY) == 0) CYCLE
        DO JI=1,ININ
          IF (KMASSIFIN(JI) /= KMASSIFOUT(JX,JY)) CYCLE
          ! Take only zero slopes
          !IF (PSLOPEIN(JI) /= 0.) THEN
          !  CYCLE
          !END IF
          !Evaluate the aspect only if the input domain is not flat
          IF (.NOT. GISFLAT) THEN
            CALL EVALUATE_SLOPE(PSLOPEIN(JI),PSLOPEOUT(JX,JY),GSLOPE)
            IF (.NOT. GSLOPE) CYCLE
            IF (KASPECTIN(JI) /= -1) THEN
              CALL EVALUATE_ASPECT (KASPECTIN(JI),PASPECTOUT(JX,JY),GASPECT)
              IF (.NOT. GASPECT) CYCLE
            END IF
          ENDIF
          !
          ZDIFFZS = KZSIN(JI) - PZSOUT(JX,JY)
          IF (ZDIFFZS < -JPRESOL_ELEV) THEN
            CYCLE
          ELSEIF (ZDIFFZS > JPRESOL_ELEV) THEN
            CYCLE
          ELSEIF (ZDIFFZS <= 0) THEN
            ! Elevation of the level just below the point
            KINDICESBAS(JX,JY) = JI
          ELSE
            ! Elevation of the level just above the point
            KINDICESHAUT(JX,JY) = JI
          ENDIF

        END DO
        ! PRINT*, KINDICESHAUT(JX,JY), KINDICESBAS(JX,JY),KZSIN(KINDICESHAUT(JX,JY)), KZSIN(KINDICESBAS(JX,JY))
      END DO
    END DO
    !
  END SUBROUTINE INDICES2D
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE EVALUATE_ASPECT(KASPECTIN,PASPECTOUT,LASPECT)
    ! Evaluate if the aspect of the current input point correspond to the aspect of the current output point
    REAL(KIND=8),INTENT(IN):: PASPECTOUT !Aspect of output points
    INTEGER,INTENT(IN):: KASPECTIN ! Aspect of input points
    LOGICAL, INTENT(OUT) :: LASPECT ! Aspect correspondence indicator
    !
    LASPECT = .FALSE.
    IF (KASPECTIN == 0.)THEN
      IF (PASPECTOUT < 22.5 .OR. PASPECTOUT > 337.5 ) LASPECT = .TRUE.
    ELSEIF (KASPECTIN < 0. .AND. PASPECTOUT >= 0. )THEN
      LASPECT = .FALSE.
    ELSEIF ((PASPECTOUT >= KASPECTIN .AND. PASPECTOUT <= KASPECTIN+22.5) .OR. &
            (PASPECTOUT <= KASPECTIN .AND. PASPECTOUT >= KASPECTIN-22.5 ))THEN
      LASPECT = .TRUE.
    ENDIF
    !
  END SUBROUTINE EVALUATE_ASPECT
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE EVALUATE_SLOPE(PSLOPEIN,PSLOPEOUT,LSLOPE)
    ! Evaluate if the slope of the current input point correspond to the slope of the current output point
    REAL(KIND=8),INTENT(IN):: PSLOPEOUT !Aspect of output points
    REAL,INTENT(IN):: PSLOPEIN ! Aspect of input points
    LOGICAL, INTENT(OUT) :: LSLOPE ! Aspect correspondence indicator
    !
    LSLOPE = (((PSLOPEOUT - PSLOPEIN) <= 10) .AND. ((PSLOPEOUT - PSLOPEIN) > -10)) &
             .OR. ((PSLOPEOUT >= 40.) .AND. (INT(PSLOPEIN) == 40))
    !
  END SUBROUTINE EVALUATE_SLOPE
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE INTERPOLZS2D(PVAROUT,PVARIN,KINDBAS,KINDHAUT,PZSGRID,KZSSAFRAN)
    ! Linear interpolation between SAFRAN elevation levels over 2D grids

    REAL,DIMENSION(:,:,:,:),INTENT(OUT)::PVAROUT ! Interpolated variable on 2D grid. The first 2 dimensions are the spatial dimensions, the last dimension is the time dimension.
    REAL,DIMENSION(:,:,:),INTENT(IN)::PVARIN ! Variable on SAFRAN elevation levels
    INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS ! Index of **pvarin** containing the values at the SAFRAN elevation level below the grid point elevation at the corresponding massif.
    INTEGER,DIMENSION(:,:),INTENT(IN)::KINDHAUT ! Index of **pvarin** containing the values at the SAFRAN elevation level above the grid point elevation at the corresponding massif.
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN)::PZSGRID ! Gridpoint elevation data
    INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN ! SAFRAN elevation data
    REAL ::ZA,ZB,ZC
    INTEGER::JX,JY,JT,JP !Loop counters
    INTEGER::KNX,KNY,KNSAFRAN,KNT,KNP

    KNX=SIZE(PVAROUT,1)
    KNY=SIZE(PVAROUT,2)
    KNSAFRAN=SIZE(PVARIN,1)
    KNP = SIZE(PVAROUT,3)
    KNT=SIZE(PVARIN,3)

    PVAROUT = XUNDEF
    DO JX=1,KNX
      DO JY=1,KNY
        IF (KINDBAS(JX,JY) == 0 .OR. KINDHAUT(JX,JY) == 0 ) cycle
        ! Interpolation parameters constant over time
        ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
        ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
        ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
        !
        DO JP=1,KNP
          DO JT=1,KNT
            IF (PVARIN(KINDHAUT(JX,JY),JP,JT) /= XUNDEF .AND. PVARIN(KINDBAS(JX,JY),JP,JT) /= XUNDEF) THEN
              PVAROUT(JX,JY,JP,JT)= (ZA*PVARIN(KINDHAUT(JX,JY),JP,JT)+ZB*PVARIN(KINDBAS(JX,JY),JP,JT))/ZC
            END IF
          END DO
        ENDDO
      END DO
    END DO
    !
  END SUBROUTINE INTERPOLZS2D
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE INTERPOLZS2DDIMBEFORE(PVAROUT,PVARIN,KINDBAS,KINDHAUT,PZSGRID,KZSSAFRAN)
    ! Linear interpolation between SAFRAN elevation levels over 2D grids

    REAL,DIMENSION(:,:,:,:),INTENT(OUT)::PVAROUT ! Interpolated variable on 2D grid. The second and third dimension are the spatial dimensions, the last dimension is the time dimension.
    REAL,DIMENSION(:,:,:),INTENT(IN)::PVARIN ! Variable on SAFRAN elevation levels
    INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS ! Index of **pvarin** containing the values at the SAFRAN elevation level below the grid point elevation at the corresponding massif.
    INTEGER,DIMENSION(:,:),INTENT(IN)::KINDHAUT ! Index of **pvarin** containing the values at the SAFRAN elevation level above the grid point elevation at the corresponding massif.
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN)::PZSGRID  ! Gridpoint elevation data
    INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN ! SAFRAN elevation data
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
        IF (KINDBAS(JX,JY) == 0 .OR. KINDHAUT(JX,JY) == 0 ) cycle
        ! Interpolation parameters constant over time
        ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
        ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
        ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
        !
        DO JP=1,KNP

          DO JT=1,KNT
            IF (PVARIN(JP,KINDHAUT(JX,JY),JT) /= XUNDEF .AND. PVARIN(JP,KINDBAS(JX,JY),JT) /= XUNDEF) THEN
              PVAROUT(JP, JX,JY,JT)= (ZA*PVARIN(JP,KINDHAUT(JX,JY),JT)+ZB*PVARIN(JP,KINDBAS(JX,JY),JT))/ZC
            END IF
          END DO
        ENDDO
      END DO
    END DO
    !
  END SUBROUTINE INTERPOLZS2DDIMBEFORE
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE INTERPOLZS2DNOTIME(PVAROUT,PVARIN,KINDBAS,KINDHAUT,PZSGRID,KZSSAFRAN)
    ! Linear interpolation between SAFRAN elevation levels over 2D grids
    REAL,DIMENSION(:,:,:),INTENT(OUT)::PVAROUT ! Interpolated variable on 2D grid. The first two dimensions are the spatial dimensions. There is no time dimension.
    REAL,DIMENSION(:,:),INTENT(IN)::PVARIN ! Variable on SAFRAN elevation levels without time dimension.
    INTEGER,DIMENSION(:,:),INTENT(IN)::KINDBAS ! Index of **pvarin** containing the values at the SAFRAN elevation level below the grid point elevation at the corresponding massif.
    INTEGER,DIMENSION(:,:),INTENT(IN)::KINDHAUT ! Index of **pvarin** containing the values at the SAFRAN elevation level above the grid point elevation at the corresponding massif.
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN)::PZSGRID ! Gridpoint elevation data
    INTEGER,DIMENSION(:),INTENT(IN)::KZSSAFRAN ! SAFRAN elevation data
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
        IF (KINDBAS(JX,JY) == 0 .OR. KINDHAUT(JX,JY) == 0 ) cycle
        ! Interpolation parameters constant over time
        ZA = PZSGRID(JX,JY) - KZSSAFRAN(KINDBAS(JX,JY))
        ZB = KZSSAFRAN(KINDHAUT(JX,JY)) - PZSGRID(JX,JY)
        ZC = 1.*(KZSSAFRAN(KINDHAUT(JX,JY))-KZSSAFRAN(KINDBAS(JX,JY)))
        !
        DO JP=1,KNP
          IF (PVARIN(KINDHAUT(JX,JY),JP) /= XUNDEF .AND. PVARIN(KINDBAS(JX,JY),JP) /= XUNDEF) THEN
            PVAROUT(JX,JY,JP)= (ZA*PVARIN(KINDHAUT(JX,JY),JP)+ZB*PVARIN(KINDBAS(JX,JY),JP))/ZC
          END IF
        ENDDO
      END DO
    END DO
  END SUBROUTINE INTERPOLZS2DNOTIME
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE FUSE_FLD_3D(PVAROUT_OLD, PVAROUT, XUNDEF)
    ! Merge two 3D fields.
    !
    ! :actions: Put values of PVAROUT into PVAROUT_OLD where PVAROUT_OLD == XUNDEF
    REAL,DIMENSION(:,:,:), INTENT(INOUT) :: PVAROUT_OLD ! Variable on 2D grid with existing values to be completed.
    REAL,DIMENSION(:,:,:), INTENT(IN) :: PVAROUT ! Variable on 2D grid with new values to be added.
    REAL, INTENT(IN) :: XUNDEF ! Missing value indicator.

    INTEGER::JX,JY,JT !Loop counters
    INTEGER::KNX,KNY, KNT !KNSAFRAN,KNP
    !
    KNX=SIZE(PVAROUT,1)
    KNY=SIZE(PVAROUT,2)
    KNT = SIZE(PVAROUT,3)

    DO JX = 1,KNX
      DO JY = 1, KNY
        DO JT = 1,KNT
          IF (PVAROUT_OLD(JX,JY,JT) == XUNDEF) THEN
            PVAROUT_OLD(JX,JY,JT) = PVAROUT(JX,JY,JT)
          END IF
        END DO
      END DO
    END DO
  END SUBROUTINE FUSE_FLD_3D

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE LATLON_IGN(PX,PY,PLAT,PLON)
    ! Routine to compute geographical coordinates from lambert conformal coordinates
    !
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: PX ! given conformal x-coordinates of the processed points (meters).
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: PY ! given conformal y-coordinates of the processed points (meters).
    REAL(KIND=8), DIMENSION(:,:), OPTIONAL,  INTENT(OUT):: PLAT ! returned geographic latitudes of the processed point (degrees).
    REAL(KIND=8), DIMENSION(:,:), OPTIONAL,  INTENT(OUT):: PLON ! returned geographic longitudes of the processed point (degrees).
    !
    REAL(KIND=8), DIMENSION(SIZE(PX),SIZE(PY)) :: ZGAMMA
    REAL(KIND=8), DIMENSION(SIZE(PX),SIZE(PY)) :: ZR   ! length of arc meridian line projection
    REAL(KIND=8), DIMENSION(SIZE(PX),SIZE(PY)) :: ZLATISO          ! Isometric latitude
    REAL(KIND=8),DIMENSION(SIZE(PX),SIZE(PY)) :: ZLAT0            ! For iteration
    !
    INTEGER                         :: J, JJ, IX,IY
    REAL(KIND=8) ,SAVE :: XPI   = 4.*ATAN(1.)
    REAL(KIND=8) ,SAVE :: XN    = 0.7256077650  ! exposant de projection (n)
    REAL(KIND=8) ,SAVE :: XC    = 11754255.426  ! constante de projection (c)(m)
    REAL(KIND=8) ,SAVE :: XXS   = 700000.000    ! X en projection du Pole (Xs)(m)
    REAL(KIND=8) ,SAVE :: XYS   = 12655612.050  ! Y en projection du Pole (Ys)(m)
    REAL(KIND=8) ,SAVE :: XLON0 = 3.            ! 3 degrees Est Greenwitch pour L93
    REAL(KIND=8) ,SAVE :: XE    = 0.08181919112 ! first excentricity
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
    ! Routine to compute Lambert coordinates from logitude and latitude
    !
    REAL(KIND=8), DIMENSION(:), INTENT(IN):: PLON ! given geographic longitudes of the processed points (degrees).
    REAL(KIND=8), DIMENSION(:), INTENT(IN):: PLAT ! given geographic latitudes of the processed points (degrees).
    REAL(KIND=8), DIMENSION(:,:), INTENT(OUT):: PX ! returned lambert conformal x-coordinates of the processed points (meters).
    REAL(KIND=8), DIMENSION(:,:), INTENT(OUT):: PY ! returned lambert conformal y-coordinates of the processed points (meters).

    !
    REAL :: ZPI180, ZPI4, ZECC2
    REAL :: ZWRK     ! working arrays
    REAL :: ZLATRAD, ZLONRAD ! longitude and latitude in radians
    REAL :: ZGAMMA
    REAL :: ZLATFI           ! Isometric latitude
    REAL :: ZR               ! length of arc meridian line projection
    INTEGER :: JI,JJ
    REAL(KIND=8) ,SAVE :: XPI   = 4.*ATAN(1.)
    REAL(KIND=8) ,SAVE :: XE    = 0.08181919112 ! first excentricity
    REAL(KIND=8) ,SAVE :: XN    = 0.7256077650  ! exposant de projection (n)
    REAL(KIND=8) ,SAVE :: XC    = 11754255.426  ! constante de projection (c)(m)
    REAL(KIND=8) ,SAVE :: XXS   = 700000.000    ! X en projection du Pole (Xs)(m)
    REAL(KIND=8) ,SAVE :: XYS   = 12655612.050  ! Y en projection du Pole (Ys)(m)
    REAL(KIND=8) ,SAVE :: XLON0 = 3.            ! 3 degrees Est Greenwitch pour L93
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
    ! calculate resolved slope and aspect on a Lambert conformal grid.
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: PX  ! given Lambert conformal x-coordinates of the processed points (meters).
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: PY ! given Lambert conformal y-coordinates of the processed points (meters).
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN):: PZS ! resolved model orography
    REAL(KIND=8),DIMENSION(:,:),INTENT(OUT)::PSSO_SLOPE ! resolved slope tangent
    REAL(KIND=8),DIMENSION(:,:),INTENT(OUT)::PSSO_DIR ! resolved aspect
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
    REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: ZZS_XY        ! orography at southwest corner of the mesh
    REAL(KIND=8), DIMENSION(:,:),ALLOCATABLE  :: ZZSL          ! orography in a 2D array
    REAL(KIND=8), DIMENSION(:),ALLOCATABLE    :: ZXHAT, ZYHAT  ! X Y coordinate
    !
    REAL(KIND=8),    PARAMETER :: XPI=4.*ATAN(1.)  ! Pi
    INTEGER, PARAMETER :: JPHEXT = 1 ! number of points around the physical domain
    !
    REAL(KIND=8)    :: ZDZSDX   ! slope in X and Y direction
    REAL(KIND=8)    :: ZDZSDY   ! of a triangle surface
    REAL(KIND=8)    :: ZSURF    ! surface of 4 triangles
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
        ! SSO_DIR, degrees from north between 0 and 360 (North = 0 deg, East = 90 deg, South = 180 deg, West = 270 deg)
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
    ! calculate resolved slope and aspect on a lat/lon grid.
    !
    REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: PX  ! given longitude coordinates of the processed points (degrees).
    REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: PY ! given latitude coordinates of the processed points (degrees).
    REAL(KIND=8),DIMENSION(:,:),INTENT(IN)::PZS ! resolved model orography
    REAL(KIND=8),DIMENSION(:,:),INTENT(OUT)::PSSO_SLOPE ! resolved slope tangent
    REAL(KIND=8),DIMENSION(:,:),INTENT(OUT)::PSSO_DIR ! resolved aspect
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
    REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: ZZS_XY        ! orography at southwest corner of the mesh
    REAL(KIND=8), DIMENSION(:,:),ALLOCATABLE  :: ZZSL          ! orography in a 2D array
    REAL(KIND=8), DIMENSION(:,:),ALLOCATABLE    :: ZXHAT, ZYHAT  ! X Y coordinate
    !
    REAL(KIND=8),    PARAMETER :: XPI=4.*ATAN(1.)  ! Pi
    INTEGER, PARAMETER :: JPHEXT = 1 ! number of points around the physical domain
    !
    REAL(KIND=8)    :: ZDZSDX   ! slope in X and Y direction
    REAL(KIND=8)    :: ZDZSDY   ! of a triangle surface
    REAL(KIND=8)    :: ZSURF    ! surface of 4 triangles
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
        ! SSO_DIR, degrees from north between 0 and 360 (North = 0 deg, East = 90 deg, South = 180 deg, West = 270 deg)
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
    ! Handles error messages from the netcdf API
    INTEGER, INTENT ( IN) :: STATUS ! error number
    CHARACTER(*), INTENT(IN) :: LINE ! description line

    IF(STATUS /= NF90_NOERR) THEN    
      CALL ABORT_INTERPOLATE(TRIM(NF90_STRERROR(STATUS)) // ":" // LINE)
    END IF
  END SUBROUTINE CHECK
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE READ_NML(KNAMUNIT)
    ! reads namelist interpolate_safran.nam
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: KNAMUNIT ! unit number

    INTEGER :: IOS

    READ(UNIT=KNAMUNIT,NML=NAM_SWITCHES_INT, IOSTAT=IOS)
    IF (IOS /= 0) THEN
      PRINT*, IOS
      CALL ABORT_INTERPOLATE('ERROR reading namelist NAM_SWITCHES_INT')
    END IF
    ! If multiinput is wanted, read the number of inputs wanted, allocate the filename arrays and
    ! read the input filenames
    IF (LMULTIINPUT) THEN
      READ(UNIT=KNAMUNIT, NML=NAM_MULTIIN_SETTING, IOSTAT=IOS)
      IF (IOS /= 0) THEN
        CALL ABORT_INTERPOLATE('ERROR reading namelist NAM_MULTIIN_SETTING')
      END IF
      ALLOCATE(HFILESIN(NNUMBER_INPUT_FILES))
      READ(UNIT=KNAMUNIT, NML=NAM_FILENAMES_MULTI_IN, IOSTAT=IOS)
      IF (IOS /= 0) THEN
        PRINT*, IOS, HFILESIN
        CALL ABORT_INTERPOLATE('ERROR reading namelist NAM_FILENAMES_MULTI_IN')
      END IF
      IF (LMULTIOUTPUT) THEN
        READ(UNIT=KNAMUNIT, NML=NAM_MULTIOUT_SETTING, IOSTAT=IOS)
        IF (IOS /= 0) THEN
          PRINT*, IOS
          CALL ABORT_INTERPOLATE('ERROR reading namelist NAM_MULTIOUT_SETTING')
        END IF
        ! check if number of input files equals number of output grids
        IF (NNUMBER_OUTPUT_GRIDS /= NNUMBER_INPUT_FILES) THEN
          CALL ABORT_INTERPOLATE('ERROR for multiinput and multioutput the number of output grids must &
                                  equal the number of input files. Check namelist settings')
        END IF
        ALLOCATE(HFILESOUT(NNUMBER_OUTPUT_GRIDS), HGRIDSIN(NNUMBER_OUTPUT_GRIDS))
        ! read the output file names and the associated grid filenames
        READ(KNAMUNIT,NML=NAM_FILENAMES_MULTI_OUT, IOSTAT=IOS)
        IF (IOS /= 0) THEN
          PRINT*, IOS, HFILESOUT, HGRIDSIN, SHAPE(HGRIDSIN)
          CALL ABORT_INTERPOLATE('ERROR reading namelist NAM_FILENAMES_MULTI_OUT')
        END IF
      ELSE
        ALLOCATE(HFILESOUT(1), HGRIDSIN(1))
        READ(KNAMUNIT, NML=NAM_FILENAMES_SINGLE_OUT, IOSTAT=IOS)
        IF (IOS /= 0) THEN
          PRINT*, IOS, HFILEOUT, HGRIDIN
          CALL ABORT_INTERPOLATE('ERROR reading namelist NAM_FILENAMES_SINGLE_OUT')
        END IF
        HGRIDSIN(1) = HGRIDIN
        HFILESOUT(1) = HFILEOUT
      END IF
      ! otherwise read the filenames in a scalar variable
    ELSE
      READ(UNIT=KNAMUNIT, NML=NAM_FILENAMES_SINGLE_IN, IOSTAT=IOS)
      IF (IOS /= 0) THEN
        CALL ABORT_INTERPOLATE('ERROR: problem reading namelist NAM_FILENAMES_SINGLE_IN')
      END IF
      ALLOCATE(HFILESIN(1))
      HFILESIN(1) = HFILEIN
      IF (LMULTIOUTPUT) THEN
        CALL ABORT_INTERPOLATE('ERROR: Output of multiple files not supported for a single input.&
                                Check namelist settings')
      ELSE
        READ(UNIT=KNAMUNIT, NML=NAM_FILENAMES_SINGLE_OUT, IOSTAT=IOS)
        IF (IOS /= 0) THEN
          CALL ABORT_INTERPOLATE('ERROR: problem reading namelist NAM_FILENAMES_SINGLE_OUT')
        END IF
        ALLOCATE(HFILESOUT(1), HGRIDSIN(1))
        HFILESOUT(1) = HFILEOUT
        HGRIDSIN(1) = HGRIDIN
      END IF

    END IF
    ! If variable selection is activated, read the number of variables and the variable names.
    IF (LSELECTVAR) THEN
      READ(UNIT=KNAMUNIT, NML=NAM_SELECT_VARS_SETTING, IOSTAT=IOS)
      IF (IOS /= 0) THEN
        CALL ABORT_INTERPOLATE('ERROR: problem reading namelist NAM_SELECT_VARS_SETTING')
      END IF
      ALLOCATE(HVAR_LIST(NNUMBER_OF_VARIABLES))
      READ(UNIT=KNAMUNIT, NML=NAM_SELECT_VARS_NAMES, IOSTAT=IOS)
      IF (IOS /= 0) THEN
        CALL ABORT_INTERPOLATE('ERROR: problem reading namelist NAM_SELECT_VARS_NAMES')
      END IF
    END IF
    !
    READ(UNIT=KNAMUNIT,NML=NAM_OTHER_STUFF, IOSTAT=IOS)
    IF (IOS /= 0) THEN
      PRINT*, IOS, LTIMECHUNK, LSPATIALCHUNK, NLONCHUNKSIZE, NLATCHUNKSIZE
      CALL ABORT_INTERPOLATE('ERROR reading namelist NAM_OTHER_STUFF')
    END IF
    ! PRINT*, HFILESOUT
    ! PRINT*, HFILESIN
    ! PRINT*, HGRIDSIN
  END SUBROUTINE READ_NML
  !  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE SUBS

PROGRAM INTERPOLATE_SAFRAN
  !
  ! This program interpolates SAFRAN meteorological data.
  !
  ! If a namelist called interpolate_safran.nam is present, the configuration information from the namelist is used.
  ! Otherwise the default configuration is used. An example namelist can be found in sonwtools/DATA.
  !
  ! :Default configuration:
  !   #) A file named "GRID.nc" is read, containing the output grid (2D case) or station (1D case) information.
  !   #) A file named "input.nc" is supposed to contain the data in massif geometry to be interpolated on some grid or station.
  !   #) A file named "ouput.nc" is written containing the interpolated version of the data.
  !
  ! :Possible configurations via the namelist:
  !   #) Possibility to specify custom filenames (mandatory for multiinput and multioutput options).
  !   #) Iterating over several input files
  !
  !      - Providing a grid definition file for each of them (set :f:var:`lmultiinput` =.TRUE. and :f:var:`lmultioutput` =.TRUE.) and producing a separate output file for each of them
  !      - Combining the data from them on a single grid in a single output file (set :f:var:`lmultiinput` =.TRUE. and :f:var:`lmultioutput` =.FALSE.)
  !   #) Treat time steps sepatately in order to save memory in the case of very large fields by setting :f:var:`ltimechunk` = .TRUE.
  !   #) customize the NetCDF chunksize of the spatial output dimensions in order to optimise write performance for large fields. (:f:var:`lspatialchunk` = .TRUE. and :f:var:`nlonchunksize`,   :f:var:`nlatchunksize`)
  !   #) Select variables to be interpolated. (set :f:var:`lselectvar` = .TRUE., assign a list of variables to :f:var:`hvar_list` and give the number of selected variables to :f:var:`nnumber_of_variables`)
  !
  ! Input variables can be of any type in input.nc.
  ! In output variables, the type of variable of floating points is always single precision, integers types are not modified.
USE SUBS
!
IMPLICIT NONE
! include 'mpif.h'
!
!!!!! Files properties
CHARACTER(LEN=100):: HFILENAMEIN, HFILENAMEOUT   ! Name of the field file.
INTEGER::FILE_ID_IN, FILE_ID_OUT, FILE_ID_GEO ! id of input ,output and geometrie netcdf file
!
!!!!! Dim properties
CHARACTER(LEN=20),DIMENSION(:),ALLOCATABLE::DIM_NAME_IN ! name of input dimensions
INTEGER,DIMENSION(:),ALLOCATABLE:: DIM_ID_IN, DIM_ID_OUT ! id of dimensions
INTEGER,DIMENSION(:),ALLOCATABLE:: DIM_SIZE_IN, DIM_SIZE_OUT, DIM_CHUNK_OUT ! size of dimensions
INTEGER :: IMASSIVE_DIM_SIZE_OUT ! dimension length of massif dimension is determ
!
!!!!! Var properties
INTEGER,DIMENSION(:),ALLOCATABLE::VAR_ID_IN, VAR_ID_OUT ! id of variables
CHARACTER(LEN=20),DIMENSION(:),ALLOCATABLE::VAR_NAME_IN ! name of variables
CHARACTER(LEN=20),DIMENSION(7):: STANDARD_VARS ! variables to select anyway
CHARACTER(LEN=20) :: VARNAME_LOC ! temporary variable for variable name
INTEGER,DIMENSION(:),ALLOCATABLE::VAR_TYPE_IN ! type of input variables
INTEGER::VAR_TYPE_OUT ! type of output variable
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
INTEGER,DIMENSION(:,:),ALLOCATABLE:: IMASSIFOUT
INTEGER,DIMENSION(:,:),ALLOCATABLE:: IMASSIFGATHER
INTEGER,DIMENSION(:),ALLOCATABLE:: ISTATIONOUT
REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE::ZZSOUT,ZASPECTOUT,ZSLOPEOUT ! Elevation massif and aspect of output grid

REAL(KIND=8),DIMENSION(:),ALLOCATABLE::ZLATOUT,ZLONOUT,ZXOUT,ZYOUT, ZLATIN,ZLONIN
!
!Grid type
CHARACTER(LEN=2)::GRID_TYPE
INTEGER:: IRANK
INTEGER,DIMENSION(2)::GRID_DIM_REF! lenghts of dimensions
!
!INPUT_GRID_VARIABLES
INTEGER,DIMENSION(:),ALLOCATABLE::IZSIN,IMASSIFIN,IASPECTIN !Elevation massif and aspect of output grid
REAL, DIMENSION(:), ALLOCATABLE :: ZSLOPEIN ! slope of input grid
!
!INDICES parameter
INTEGER,DIMENSION(:,:),ALLOCATABLE::IINDICESBAS,IINDICESHAUT
!
!WORK VEC
INTEGER,DIMENSION(:),ALLOCATABLE:: VALUE_TIME, VALUE_TIME_OLD
INTEGER,DIMENSION(:),ALLOCATABLE::  IVARIN
REAL,DIMENSION(:,:),ALLOCATABLE::  ZVARIN, ZVARIN_TMP
REAL,DIMENSION(:,:,:),ALLOCATABLE::  ZVARINT, ZVARINT_TMP
REAL ::  ZSCAIN, ZSCAIN_TMP
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZVAROUT2D, ZVAROUT2D_OLD
REAL,DIMENSION(:,:,:,:),ALLOCATABLE:: ZVAROUTXYT, ZVAROUTXYT_OLD
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZVAROUTXYT1D, ZVAROUTXYT1D_OLD
!
INTEGER :: NPATCH, NPATCHID
INTEGER :: NDECILE, IDECILEID
!
REAL(KIND=8) ,DIMENSION(:,:),ALLOCATABLE:: ZVARLATLON
!ID of spatial and time dimension in input file 
INTEGER :: ILLOC_ID, ITIMEID
!

!
INTEGER::ID,IV,IA,IT,IDIN,IDOUT,INZ, JINFILE, IM ! loop counter
INTEGER :: IV_TMP
INTEGER :: IOS ! status indicator
INTEGER :: INAM_UNIT
!
INTEGER :: IMASSIFTODELETE, ILAYERTODELETE, I, status
LOGICAL :: AFTERLOC
LOGICAL :: LPRINT
CHARACTER(LEN=10),DIMENSION(4) :: LL_VARNAME
!
! LOGICAL :: LSPATIALCHUNK! temporary, to be defined in namelist
!
!
COMM = MPI_COMM_WORLD
CALL MPI_INIT(IERR)
CALL MPI_COMM_RANK(COMM, PROC_ID, IERR)
CALL MPI_COMM_SIZE(COMM, NPROC, IERR)
!
LPRINT = .FALSE.
!
LL_VARNAME(1)= "LON"
LL_VARNAME(2)= "LAT"
LL_VARNAME(3)= "latitude"
LL_VARNAME(4)= "longitude"
!
STANDARD_VARS(1) = "time"
STANDARD_VARS(2) = "ZS"
STANDARD_VARS(3) = "aspect"
STANDARD_VARS(4) = "slope"
STANDARD_VARS(5) = "massif_number"
STANDARD_VARS(6) = "massif_num"
STANDARD_VARS(7) = "decile"
!
HFILENAMEIN ='input.nc'
HFILENAMEG =  'GRID.nc'
HFILENAMEOUT = 'output.nc'

! Open namelist
INAM_UNIT = 21
OPEN(UNIT=INAM_UNIT, ACTION='READ', STATUS='OLD', IOSTAT=IOS, FILE='interpolate_safran.nam')
IF (IOS == 0) THEN
  CALL READ_NML(INAM_UNIT)
!  PRINT*, 'namelist test ', LMULTIINPUT
!  PRINT*, NNUMBER_INPUT_GRIDS
!  PRINT*, 'HFILEIN ', HFILEIN, 'HGRIDIN ', HGRIDIN
!  PRINT*, 'HFILESIN ', HFILESIN, 'HGRIDSIN ', HGRIDSIN
!  LSPATIALCHUNK = .TRUE. ! temporary --> should be defined in namelist
!
ELSE
  PRINT*, 'WARNING: no namelist interpolate_safran.nam provided. Continue with default settings.'
  ALLOCATE(HFILESIN(1), HGRIDSIN(1), HFILESOUT(1))
  HFILESIN(1) = HFILENAMEIN
  HGRIDSIN(1) = HFILENAMEG
  HFILESOUT(1) = HFILENAMEOUT
  NNUMBER_INPUT_FILES = 1
  NNUMBER_OUTPUT_GRIDS = 1
  LTIMECHUNK = .FALSE.
  LSPATIALCHUNK = .FALSE.
  NLONCHUNKSIZE = 100
  NLATCHUNKSIZE = 100
END IF
CLOSE(INAM_UNIT)

! loop over input files
DO JINFILE = 1,NNUMBER_INPUT_FILES
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
  IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
! Open new output file
    CALL CHECK(NF90_create(HFILESOUT(JINFILE),IOR(IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL),NF90_MPIIO),FILE_ID_OUT, &
                       comm = COMM, info = MPI_INFO_NULL),&
                       "Cannot open file "//TRIM(HFILESOUT(JINFILE)))
!
! Open grid file
    CALL CHECK(NF90_OPEN(HGRIDSIN(JINFILE), IOR(NF90_NOWRITE, NF90_MPIIO), FILE_ID_GEO, &
                     comm = COMM, info = MPI_INFO_NULL),&
                     "Cannot open file"//TRIM(HGRIDSIN(JINFILE)))

  END IF
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
  VAR_ID_IN = 0
  VAR_NDIMS_IN = 0
  VAR_ID_DIMS_IN =0

  IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
! Allocate descriptors of netcdf output file
!!DIM properties
    ALLOCATE(DIM_NAME_OUT(INDIM+1))
    ALLOCATE(DIM_ID_OUT(INDIM+1))
    ALLOCATE(DIM_SIZE_OUT(INDIM+1))
    ALLOCATE(DIM_CHUNK_OUT(INDIM+1))
!! VAR
    ALLOCATE(VAR_ID_OUT(INVAR+2))
    ALLOCATE(VAR_ID_DIMS_OUT(INDIM+1, INVAR+2))


    VAR_ID_OUT = 0
    VAR_ID_DIMS_OUT=0
  END IF
  DIM_ID_OUT = 0
  DIM_SIZE_OUT = 0
!
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
  IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
! Get interpolation informations
! Read altitude massif numbers and aspects
    CALL READ_OUTPUT_GRID(FILE_ID_GEO,ZZSOUT,IMASSIFOUT,ZASPECTOUT,IRANK, &
          GRID_TYPE,GRID_DIM_REF,ZLATOUT,ZLONOUT,ZYOUT,ZXOUT,&
          ZSLOPEOUT,ISTATIONOUT, IMASSIFGATHER)
    ! PRINT*, MINVAL(IMASSIFGATHER,MASK=IMASSIFGATHER/=0), MAXVAL(IMASSIFGATHER)
    ! PRINT*, IMASSIFGATHER
    IMASSIVE_DIM_SIZE_OUT = 2
    DO IM = MINVAL(IMASSIFGATHER,MASK=IMASSIFGATHER/=0)+1, MAXVAL(IMASSIFGATHER)-1
      IF (ANY(IMASSIFGATHER == IM)) THEN
        IMASSIVE_DIM_SIZE_OUT = IMASSIVE_DIM_SIZE_OUT + 1
      ELSE
       ! PRINT*, IM
      END IF
    END DO
    ! IMASSIVE_DIM_SIZE_OUT = (MAXVAL(IMASSIFOUT) - MINVAL(IMASSIFOUT,MASK=IMASSIFOUT/=0)) + 1 ! to define the "massif" dimension in the output file
!  IF (JINFILE == 2) THEN
!    PRINT*, ZSLOPEOUT
!  END IF
  END IF
!  PRINT*, IMASSIVE_DIM_SIZE_OUT

  ! open a file FORCING.nc by massif, altitude, aspect
  CALL READ_NC(FILE_ID_IN,IZSIN,IMASSIFIN,IASPECTIN,ZSLOPEIN)
  !
  CALL INDICES2D(ZZSOUT,IMASSIFOUT,ZASPECTOUT,ZSLOPEOUT,IZSIN,IMASSIFIN,&
          IASPECTIN,ZSLOPEIN, IINDICESBAS,IINDICESHAUT)
  !
  IDOUT = 1
  DO ID=1,INDIM
    ! Define dimensions in output file
    IF (DIM_NAME_IN(ID) == 'Number_of_points') THEN
      ! Change spatial dimension
      IF (IRANK==1) THEN
        DIM_SIZE_OUT(IDOUT)=GRID_DIM_REF(1)
        DIM_CHUNK_OUT(IDOUT)=MIN(GRID_DIM_REF(1), NLONCHUNKSIZE*NLATCHUNKSIZE)
        IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
          CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),GRID_DIM_REF(1),DIM_ID_OUT(IDOUT)), &
                "Cannot def dim  "//TRIM(HFILENAMEOUT))
        ELSE
          CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, DIM_NAME_IN(ID), DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid "//DIM_NAME_IN(ID)//" in "//TRIM(HFILENAMEOUT))
        END IF
        ILLOC_ID=   DIM_ID_OUT(IDOUT)
        IDOUT =IDOUT+1
      ELSEIF (IRANK==2) THEN
        DIM_SIZE_OUT(IDOUT)=GRID_DIM_REF(2)
        DIM_SIZE_OUT(IDOUT+1)=GRID_DIM_REF(1)
        ! PRINT*, NLATCHUNKSIZE, NLONCHUNKSIZE
        DIM_CHUNK_OUT(IDOUT)=MIN(GRID_DIM_REF(2), NLATCHUNKSIZE)
        DIM_CHUNK_OUT(IDOUT+1)=MIN(GRID_DIM_REF(1), NLONCHUNKSIZE)
        IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
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
            CALL ABORT_INTERPOLATE("INCORRECT TYPE OF GRID")
          ENDIF
        ELSE
          IF (GRID_TYPE == "LL") THEN
            CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'lat', DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for lat in "//TRIM(HFILENAMEOUT))
            CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'lon', DIM_ID_OUT(IDOUT+1)), &
                  "Cannot inquire dimid for lon in "//TRIM(HFILENAMEOUT))
          ELSEIF (GRID_TYPE == "XY") THEN
            CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'y', DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for y in "//TRIM(HFILENAMEOUT))
            CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'x', DIM_ID_OUT(IDOUT+1)), &
                  "Cannot inquire dimid for x in "//TRIM(HFILENAMEOUT))
          END IF
        END IF
          ILLOC_ID = DIM_ID_OUT(IDOUT)
          IDOUT = IDOUT + 2
      ELSE
        CALL ABORT_INTERPOLATE('INCORRECT RANK OF OUTPUT GRID')
      END IF

      !
    ELSEIF (DIM_NAME_IN(ID) == 'time') THEN
      ! Need to to distinguish because of the unlimited dimension
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      DIM_CHUNK_OUT(IDOUT) = 1
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),NF90_UNLIMITED, &
              DIM_ID_OUT(IDOUT)), "def time out")
!        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_OUT(IDOUT), &
!                DIM_ID_OUT(IDOUT)), "def time out")
      ELSE
        CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'time', DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for time in "//TRIM(HFILENAMEOUT))
      END IF
      ITIMEID= DIM_ID_OUT(IDOUT)
      IDOUT = IDOUT + 1
      !
    ELSEIF(DIM_NAME_IN(ID) == 'massif') THEN
      DIM_SIZE_OUT(IDOUT) = IMASSIVE_DIM_SIZE_OUT !DIM_SIZE_IN(ID)
      DIM_CHUNK_OUT(IDOUT) = IMASSIVE_DIM_SIZE_OUT
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_OUT(IDOUT),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
      ELSE
        CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'massif', DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for massif in "//TRIM(HFILENAMEOUT))
      END IF
      IMASSIFTODELETE = DIM_ID_OUT(IDOUT)
      IDOUT = IDOUT+1
      !
    ELSEIF(DIM_NAME_IN(ID) == 'snow_layer') THEN
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      DIM_CHUNK_OUT(IDOUT) = DIM_SIZE_OUT(IDOUT)
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim snow_layer in "//TRIM(HFILENAMEOUT))
      ELSE
        CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'snow_layer', DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for snow_layer in "//TRIM(HFILENAMEOUT))
      END IF
      ILAYERTODELETE = DIM_ID_OUT(IDOUT)
      IDOUT = IDOUT+1
    ELSEIF(DIM_NAME_IN(ID) == 'Number_of_Patches') THEN
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      DIM_CHUNK_OUT(IDOUT) = DIM_SIZE_OUT(IDOUT)
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim "//DIM_NAME_IN(ID)//' in '//TRIM(HFILENAMEOUT))
      ELSE
        CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'Number_of_Patches', DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for Number_of_Patches in "//TRIM(HFILENAMEOUT))
      END IF
      NPATCH = DIM_SIZE_IN(ID)
      NPATCHID =  DIM_ID_OUT(IDOUT)
      IDOUT = IDOUT+1
    ELSEIF(DIM_NAME_IN(ID) == 'decile') THEN
      DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
      DIM_CHUNK_OUT(IDOUT) = DIM_SIZE_OUT(IDOUT)
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
              DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
      ELSE
        CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, 'decile', DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for decile in "//TRIM(HFILENAMEOUT))
      END IF
      NDECILE = DIM_SIZE_IN(ID)
      IDECILEID =  DIM_ID_OUT(IDOUT)
      IDOUT = IDOUT+1
    ELSE
      !
      IF (IRANK==1) THEN
        DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
        DIM_CHUNK_OUT(IDOUT) = DIM_SIZE_OUT(IDOUT)
        IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
          CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
                DIM_ID_OUT(IDOUT)) , "Cannot def dim "//DIM_NAME_IN(ID)//' in '//TRIM(HFILENAMEOUT))
        ELSE
          CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, DIM_NAME_IN(ID), DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for "//DIM_NAME_IN(ID)//" in "//TRIM(HFILENAMEOUT))
        END IF
        IDOUT = IDOUT + 1
      ELSEIF (IRANK==2) THEN
        DIM_SIZE_OUT(IDOUT)=DIM_SIZE_IN(ID)
        DIM_CHUNK_OUT(IDOUT) = DIM_SIZE_OUT(IDOUT)
        IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
          CALL CHECK(NF90_DEF_DIM(FILE_ID_OUT,DIM_NAME_IN(ID),DIM_SIZE_IN(ID),&
                DIM_ID_OUT(IDOUT)),"Cannot def dim "//TRIM(HFILENAMEOUT))
        ELSE
          CALL CHECK(NF90_INQ_DIMID(FILE_ID_OUT, DIM_NAME_IN(ID), DIM_ID_OUT(IDOUT)), &
                  "Cannot inquire dimid for "//DIM_NAME_IN(ID)//" in "//TRIM(HFILENAMEOUT))
        END IF
        IDOUT = IDOUT +1
      ELSE
        CALL ABORT_INTERPOLATE('INCORRECT RANK OF OUTPUT GRID')
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
          IF (DIM_NAME_IN(VAR_ID_DIMS_IN(I,IV)) == 'Number_of_points') THEN
            AFTERLOC= .TRUE.
            VAR_ID_DIMS_OUT(I,IV) = ILLOC_ID+1
            VAR_ID_DIMS_OUT(I+1,IV) = ILLOC_ID
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
  IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
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
        CALL ABORT_INTERPOLATE("INCORRECT TYPE OF GRID 2D")
      ENDIF
    ELSEIF(IRANK==1) THEN
      CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,"LAT",NF90_DOUBLE,&
                DIM_ID_OUT(ILLOC_ID),VAR_ID_OUT(INVAR+1)),"Cannot def var latitude")
        !
      CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,"LON",NF90_DOUBLE,&
                DIM_ID_OUT(ILLOC_ID),VAR_ID_OUT(INVAR+2)),"Cannot def var longitude")
      CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,"station",NF90_INT,&
              DIM_ID_OUT(ILLOC_ID),VAR_ID_OUT(INVAR+3)),"Cannot def var station")
      !
    ENDIF

!
! The dim ids array is used to pass the dim ids of the dimensions of
! the netCDF variables. In Fortran, the unlimited
! dimension must come last on the list of dimids. 
! And must not have an element equal to zero
    ! PRINT*, IMASSIFTODELETE
    DO IV=1,INVAR
      IF (LSELECTVAR) THEN
        IF ((.NOT. ANY(VAR_NAME_IN(IV) == STANDARD_VARS)) .AND. (.NOT. ANY(VAR_NAME_IN(IV) == LL_VARNAME))&
            .AND. (.NOT. ANY(VAR_NAME_IN(IV) == HVAR_LIST))) CYCLE
      END IF
      IF (ANY( VAR_ID_DIMS_OUT(:,IV) == IMASSIFTODELETE ).OR.              &
              ANY( VAR_ID_DIMS_OUT(:,IV) == ILAYERTODELETE ).OR.               &
              (GRID_TYPE == "LL" .AND.  ANY(VAR_NAME_IN(IV) == LL_VARNAME))) CYCLE
      ! Create variable in output file
      ! To reduce file volume, we force data type to be real instead of double in output files
      IF (VAR_TYPE_IN(IV) == NF90_DOUBLE) THEN
          VAR_TYPE_OUT = NF90_REAL
      ELSE
          VAR_TYPE_OUT = VAR_TYPE_IN(IV)
      ENDIF
      
      ! ML : on large domains (e.g. Gdes Rousses at 30 m resolution from Ange), hdf5 random crashes
      ! are obtained with NF90_PUT_VAR when defining the chunk size.
      ! Furthermore, running time is lower with standard definition (without chunk size).
      ! Therefore, default is changed to standard variable definition,
      ! as chunk size must ALWAYS be tested and adjusted for each application. 
      IF ((.NOT. LSPATIALCHUNK) .OR. (SUM(VAR_ID_DIMS_OUT(:,IV)) == 0)) THEN
        PRINT*,"variable " // VAR_NAME_IN(IV) //" defined normally"      
        ! PRINT*, DIM_CHUNK_OUT, VAR_NAME_IN(IV), VAR_ID_DIMS_OUT(:,IV)
        ! PRINT*, DIM_CHUNK_OUT(PACK(VAR_ID_DIMS_OUT(:,IV),VAR_ID_DIMS_OUT(:,IV)/=0))
        CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,VAR_NAME_IN(IV),VAR_TYPE_OUT, &
                PACK(VAR_ID_DIMS_OUT(:,IV),VAR_ID_DIMS_OUT(:,IV)/=0),VAR_ID_OUT(IV)),&
                "Cannot def var "//TRIM(VAR_NAME_IN(IV)))
      ELSE
        PRINT*,"variable " // VAR_NAME_IN(IV) //" defined with explicit chunksize :"
        ! PRINT*, DIM_CHUNK_OUT, VAR_NAME_IN(IV), VAR_ID_DIMS_OUT(:,IV)
        PRINT*, DIM_CHUNK_OUT(PACK(VAR_ID_DIMS_OUT(:,IV),VAR_ID_DIMS_OUT(:,IV)/=0))
        CALL CHECK(NF90_DEF_VAR(FILE_ID_OUT,VAR_NAME_IN(IV),VAR_TYPE_OUT, &
              PACK(VAR_ID_DIMS_OUT(:,IV),VAR_ID_DIMS_OUT(:,IV)/=0),VAR_ID_OUT(IV), &
              chunksizes=DIM_CHUNK_OUT(PACK(VAR_ID_DIMS_OUT(:,IV),VAR_ID_DIMS_OUT(:,IV)/=0))),&
              "Cannot def var "//TRIM(VAR_NAME_IN(IV)))
      END IF
      !copy attributes from infile to outfile
      DO IA=1,INATT(IV)
        CALL CHECK(NF90_INQ_ATTNAME(FILE_ID_IN,VAR_ID_IN(IV),IA,ATT_NAME)," Cannot get att name")
        !
        IF (TRIM(ATT_NAME) /= '_FillValue') THEN
            CALL CHECK(NF90_COPY_ATT(FILE_ID_IN, VAR_ID_IN(IV),ATT_NAME, FILE_ID_OUT, VAR_ID_OUT(IV)), &
                "Cannot copy att from infile to outfile")
        ENDIF
        !
      ENDDO
      !
      SELECT CASE (VAR_TYPE_OUT)
        CASE (NF90_REAL)
          CALL CHECK(NF90_DEF_VAR_FILL(FILE_ID_OUT, VAR_ID_OUT(IV), 0, XUNDEF), &
                 "Cannot set _FillValue of variable " // VAR_NAME_IN(IV))
        CASE (NF90_DOUBLE)
          CALL CHECK(NF90_DEF_VAR_FILL(FILE_ID_OUT, VAR_ID_OUT(IV), 0, DBLE(XUNDEF)), &
                 "Cannot set _FillValue of variable " // VAR_NAME_IN(IV))
        CASE DEFAULT
          ! do not update fill value
      END SELECT
    ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!End of definition of output file
    CALL CHECK(NF90_ENDDEF(FILE_ID_OUT),"Cannot end of definition of output file")

    IF(IRANK == 2 )THEN
      ! Set independent access for coordinate variables (full writing by 1 thread)
      DO IV = INVAR+1, INVAR+2
        CALL CHECK(NF90_VAR_PAR_ACCESS(FILE_ID_OUT, VAR_ID_OUT(IV), nf90_independent),&
            "can not set independent write for 2d coordinates")
      ENDDO
      ! Write coordinates variables
      IF (PROC_ID == 0) THEN
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
          CALL ABORT_INTERPOLATE("INCORRECT TYPE OF GRID 2D")        
        ENDIF
      ENDIF
    ELSEIF(IRANK==1) THEN
      ! Set independent access for coordinate variables (full writing by 1 thread)
      DO IV = INVAR+1, INVAR+3
        CALL CHECK(NF90_VAR_PAR_ACCESS(FILE_ID_OUT, VAR_ID_OUT(IV), nf90_independent),&
            "can not set independent write for 1d metadata")
      ENDDO
      ! Write coordinates variables
      IF (PROC_ID == 0) THEN    
        !lat
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+1),ZLATOUT ,&
                start= (/1/) ,count =(/NX/)),"Cannot put lat")
        ! lon
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+2),ZLONOUT ,&
                start= (/1/) ,count =(/NX/)),"Cannot put lon")
        ! station
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(INVAR+3),ISTATIONOUT ,&
                start= (/1/) ,count =(/NX/)),"Cannot put station")
      END IF
    END IF
!
    NTIME =  DIM_SIZE_OUT(ITIMEID)
    ALLOCATE(VALUE_TIME_OLD(NTIME))
    ntime = ntime
  ENDIF

  ALLOCATE(VALUE_TIME(NTIME))
  !
  DO IV=1,INVAR
    PRINT*, VAR_NAME_IN(IV)
    IF (LSELECTVAR) THEN
      IF ((.NOT. ANY(VAR_NAME_IN(IV) == STANDARD_VARS)) .AND. (.NOT. ANY(VAR_NAME_IN(IV) == HVAR_LIST)) .AND. &
              VAR_NAME_IN(IV) /= "LAT" .AND. VAR_NAME_IN(IV) /= "LON" .AND.&
              VAR_NAME_IN(IV) /= "latitude" .AND. VAR_NAME_IN(IV) /= "longitude") THEN
        CYCLE
      END IF
    END IF
    IF (ANY( VAR_ID_DIMS_OUT(:,IV) == IMASSIFTODELETE ).OR.              &
            ANY( VAR_ID_DIMS_OUT(:,IV) == ILAYERTODELETE ).OR.               &
            (GRID_TYPE == "LL" .AND.  ANY(VAR_NAME_IN(IV) == LL_VARNAME))) CYCLE
    
    ! Set independent access for coordinate variables (full writing by 1 thread)
    IF ((VAR_NAME_IN(IV) == "aspect") .OR. (VAR_NAME_IN(IV) == "slope")  .OR. &
       ((VAR_NAME_IN(IV) == "LAT" .OR. VAR_NAME_IN(IV) == "LON" &
       .OR. VAR_NAME_IN(IV) == "latitude" .OR. VAR_NAME_IN(IV) == "longitude") &
       .AND. GRID_TYPE == "XY")) THEN
      ! Special case: lat / lon computed for the full domain and written by a single thread
      IF (PROC_ID == 0) THEN
        PRINT*, "This thread writes the LAT LON coordinates of the full XY grid."
      ELSE
        PRINT*, "This thread does not write any LAT LON coordinates."      
      END IF
      CALL CHECK(NF90_VAR_PAR_ACCESS(FILE_ID_OUT, VAR_ID_OUT(IV), nf90_independent),&
            "collective write"//VAR_NAME_IN(IV)//HFILESIN(JINFILE))
    ELSE
      ! Set collective writing access for other variables
      CALL CHECK(NF90_VAR_PAR_ACCESS(FILE_ID_OUT, VAR_ID_OUT(IV), nf90_collective),&
            "collective write"//VAR_NAME_IN(IV)//HFILESIN(JINFILE))
    ENDIF
    !
    IF (VAR_NAME_IN(IV) == "time") THEN
      ! time variable
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),VALUE_TIME &
              , start =(/1/), count = (/NTIME/)) &
              ,"Cannot get var time")
      !
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),VALUE_TIME  &
              , start = (/1/), count =(/NTIME/) ),"Cannot put var time")
        !PRINT*, VALUE_TIME
      ELSE
        CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),VALUE_TIME &
              , start =(/1/), count = (/NTIME/)) &
              ,"Cannot get var time")
        IF (ANY((VALUE_TIME - VALUE_TIME_OLD) /= 0)) THEN
          CALL ABORT_INTERPOLATE("ERROR: Multiple inputs on a single output grid&
                                  must be valid for the same time steps")
        END IF
      END IF
      !
    ELSEIF (VAR_NAME_IN(IV) == "ZS") THEN
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZZSOUT , &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var ZS")
      END IF
    ELSEIF (VAR_NAME_IN(IV) == "aspect") THEN
      !ZASPECTOUT = Proc_id
      IF (LMULTIOUTPUT .OR. (JINFILE == 1))  THEN
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZASPECTOUT, &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var aspect")
      END IF
    ELSEIF (VAR_NAME_IN(IV) == "slope") THEN
      IF (LMULTIOUTPUT .OR. (JINFILE == 1))  THEN
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZSLOPEOUT, &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var slope")
      END IF
    ELSEIF (VAR_NAME_IN(IV) == "massif_number" .OR. VAR_NAME_IN(IV) == "massif_num") THEN
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),IMASSIFOUT, &
              start =(/IXSTART,IYSTART/), count = (/NX_PROC,NY_PROC/)), &
              "Cannot put var massif_number")
      END IF
    ELSEIF ((VAR_NAME_IN(IV) == "LAT" .OR. VAR_NAME_IN(IV) == "latitude") .AND. GRID_TYPE == "XY") THEN
      !
      IF ((LMULTIOUTPUT .OR. (JINFILE == 1)) .AND. (PROC_ID == 0)) THEN
        ! This could be parallelized, but it is not expensive.
        ! For now as for coordinates, a single thread deals with the full domain
        ALLOCATE(ZVARLATLON(NX,NY))
        CALL LATLON_IGN(ZXOUT,ZYOUT,PLAT=ZVARLATLON)
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARLATLON , &
                start =(/1,1/), count = (/NX,NY/)), &
                "Cannot put var LAT")
        DEALLOCATE(ZVARLATLON)
      END IF
      !!    !
    ELSEIF ((VAR_NAME_IN(IV) == "LON" .OR. VAR_NAME_IN(IV) == "longitude") .AND. GRID_TYPE == "XY") THEN
      !
      IF ((LMULTIOUTPUT .OR. (JINFILE == 1)) .AND. (PROC_ID == 0)) THEN
        ! This could be parallelized, but it is not expensive.
        ! For now as for coordinates, a single thread deals with the full domain
        ! NB : It is not efficient to call twice LATLON_IGN ! Should be done once and saved
        ALLOCATE(ZVARLATLON(NX,NY))
        CALL LATLON_IGN(ZXOUT,ZYOUT,PLON=ZVARLATLON)
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARLATLON , &
                start =(/1,1/), count = (/NX,NY/)), &
                "Cannot put var LON")
        DEALLOCATE(ZVARLATLON)
      END IF
      !
    ELSEIF (ALL(VAR_ID_DIMS_OUT(:,IV) /= ILLOC_ID) .AND. &
            ANY(VAR_ID_DIMS_OUT(:,IV) == ITIMEID)) THEN !TIME DIM and NOGRID DIM
      !
      ALLOCATE(ZVARINT(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV)),NPATCH,NTIME))
      !
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARINT),"Cannot get var"//TRIM(HFILENAMEIN))
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
      !
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARINT),"Q Cannot put var"//TRIM(HFILENAMEOUT))
        DEALLOCATE(ZVARINT)
      ELSE
        ALLOCATE(ZVARINT_TMP(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV)),NPATCH,NTIME))
        CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
        CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVARINT_TMP), &
                "Q cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
        IF (ANY((ZVARINT - ZVARINT_TMP) /= 0)) THEN
          PRINT*, "Problem with variable ", VAR_NAME_IN(IV)
          CALL ABORT_INTERPOLATE("ERROR: Multiple inputs on a single output grid must have equal values for variable ")
        END IF
        DEALLOCATE(ZVARINT, ZVARINT_TMP)
      END IF
      !
    ELSEIF (ALL(VAR_ID_DIMS_OUT(:,IV) == 0)) THEN !SCALAR
      PRINT*, "scalar variable found", VAR_NAME_IN(IV)
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZSCAIN),"Cannot get var"//TRIM(HFILENAMEIN))
      !
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZSCAIN),"R Cannot put var"//TRIM(HFILENAMEOUT))
      ELSE
        CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
        CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZSCAIN_TMP), &
                "R cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
        IF ((ZSCAIN - ZSCAIN_TMP) /= 0) THEN
          PRINT*, "Problem with variable ", VAR_NAME_IN(IV)
          CALL ABORT_INTERPOLATE("ERROR: Multiple inputs on a single output grid must have equal values for variable ")
        END IF
      END IF
      !
    ELSEIF (ALL(VAR_ID_DIMS_OUT(:,IV) /= ILLOC_ID) .AND. &
            ALL(VAR_ID_DIMS_OUT(:,IV) /= ITIMEID)) THEN !NO TIME DIM and NOGRID DIM
      ! PRINT*, "variable with no time dimension and no grid dimension found"
      ALLOCATE(ZVARIN(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV)),NPATCH))
      !
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARIN),"Cannot get var"//TRIM(HFILENAMEIN))
      IF (LMULTIOUTPUT .OR. (JINFILE == 1)) THEN
        CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVARIN),"S Cannot put var"//TRIM(HFILENAMEOUT))
        DEALLOCATE(ZVARIN)
      ELSE
        ALLOCATE(ZVARIN_TMP(DIM_SIZE_IN(VAR_ID_DIMS_IN(1,IV)),NPATCH))
        CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
        CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVARIN_TMP), &
                "S cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
        IF (ANY((ZVARIN - ZVARIN_TMP) /= 0)) THEN
          PRINT*, "Problem with variable ", VAR_NAME_IN(IV)
          CALL ABORT_INTERPOLATE("ERROR: Multiple inputs on a single output grid must have equal values for variable ")
        END IF
        DEALLOCATE(ZVARIN, ZVARIN_TMP)
      END IF

    ELSEIF (ANY(VAR_ID_DIMS_OUT(:,IV) == ILLOC_ID) .AND. &
            ANY(VAR_ID_DIMS_OUT(:,IV) == ITIMEID)) THEN !TIME and GRID DIM
      !
      IF (ANY(VAR_ID_DIMS_OUT(:,IV) == IDECILEID)) THEN
        ALLOCATE(ZVARINT(NDECILE,DIM_SIZE_IN(ILLOC_ID),NTIME))
        ALLOCATE(ZVAROUTXYT(NDECILE,NX_PROC,NY_PROC,NTIME))
        IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
          IF (LTIMECHUNK) THEN
            ALLOCATE(ZVAROUTXYT_OLD(NDECILE,NX_PROC,NY_PROC,1))
          ELSE
            ALLOCATE(ZVAROUTXYT_OLD(NDECILE,NX_PROC,NY_PROC,NTIME))
          END IF

        END IF
      ELSE
        ALLOCATE(ZVARINT(DIM_SIZE_IN(ILLOC_ID),NPATCH,NTIME))
        ALLOCATE(ZVAROUTXYT(NX_PROC,NY_PROC,NPATCH,NTIME))
        IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
          IF (LTIMECHUNK) THEN
            ALLOCATE(ZVAROUTXYT_OLD(NX_PROC,NY_PROC,NPATCH,1))
          ELSE
            ALLOCATE(ZVAROUTXYT_OLD(NX_PROC,NY_PROC,NPATCH,NTIME))
          END IF
        END IF
      ENDIF

      IF (ALL(VAR_ID_DIMS_OUT(:,IV) /= NPATCHID)) THEN
        !      !  Read variable
        IF (ANY(VAR_ID_DIMS_OUT(:,IV) == IDECILEID)) THEN
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
      IF (ANY(VAR_ID_DIMS_OUT(:,IV) == IDECILEID)) THEN
        CALL INTERPOLZS2DDIMBEFORE(ZVAROUTXYT,ZVARINT,IINDICESBAS,IINDICESHAUT,&
                ZZSOUT,IZSIN)
      ELSE
!        IF (VAR_NAME_IN(IV) == "SD_1DY_ISBA") THEN
!          LPRINT = .TRUE.
!        ELSE
!          LPRINT = .FALSE.
!        END IF
        CALL INTERPOLZS2D(ZVAROUTXYT,ZVARINT,IINDICESBAS,IINDICESHAUT,&
                ZZSOUT,IZSIN)
      ENDIF
      !
      IF (IRANK == 1) THEN
        ALLOCATE(ZVAROUTXYT1D(NX_PROC,NPATCH,NTIME))
         ! allocate old variable
        IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
          ALLOCATE(ZVAROUTXYT1D_OLD(NX_PROC,NPATCH,NTIME))
        END IF
        ZVAROUTXYT1D = XUNDEF
        ZVAROUTXYT1D = ZVAROUTXYT(:,1,:,:)
        ! Write variable
        IF (ALL(VAR_ID_DIMS_OUT(:,IV) /= NPATCHID)) THEN
          IF (ANY(VAR_ID_DIMS_OUT(:,IV) == IDECILEID)) THEN
            ! case (time, Number_of_points, decile)
            IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
              ! read old variable
              CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT1D_OLD), &
                "T cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              ! fusion of old and new variable
              WHERE (ZVAROUTXYT1D == XUNDEF) ZVAROUTXYT1D = ZVAROUTXYT1D_OLD
              ! write fusioned variable
            END IF
            CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT1D,  &
                    start = (/1,IXSTART,1/), count = (/NDECILE,NX_PROC,NTIME/)), &
                    "T Cannot put var"//TRIM(HFILENAMEOUT))
          ELSE
            ! 07/01/2021 don't forget to test 1D case
            ! standard case (time, Number_of_points)
            IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
              ! read old variable
              CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT1D_OLD(:,1,:)), &
                "U cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              ! fusion of old and new variable
              WHERE (ZVAROUTXYT1D == XUNDEF) ZVAROUTXYT1D = ZVAROUTXYT1D_OLD
              ! write fusioned variable
            END IF
            CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT1D,  &
                    start =(/IXSTART,1/) ,count = (/NX_PROC,NTIME/)),&
                    "U Cannot put var"//TRIM(HFILENAMEOUT))
          ENDIF
        ELSE
          ! case (time, Number_of_Patches, Number_of_points)
          IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
              ! read old variable
              CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT1D_OLD), &
                "V cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              ! fusion of old and new variable
              WHERE (ZVAROUTXYT1D == XUNDEF) ZVAROUTXYT1D = ZVAROUTXYT1D_OLD
              ! write fusioned variable
            END IF
          CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT1D,  &
                  start =(/IXSTART,1,1/) ,count = (/NX_PROC,NPATCH,NTIME/)),&
                  "V Cannot put var"//TRIM(HFILENAMEOUT))
        ENDIF
        DEALLOCATE(ZVAROUTXYT1D)
        IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
          DEALLOCATE(ZVAROUTXYT1D_OLD)
        END IF
      ELSEIF( IRANK == 2)THEN
        IF (ALL(VAR_ID_DIMS_OUT(:,IV) /= NPATCHID)) THEN
          ! case (time, x, y, decile)
          IF (ANY(VAR_ID_DIMS_OUT(:,IV) == IDECILEID)) THEN
            IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
              ! read old variable
              CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              IF (LTIMECHUNK) THEN
                DO I=1, NTIME
                  CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT_OLD, start =(/1,IXSTART,IYSTART,I/) ,&
                          count = (/NDECILE,NX_PROC,NY_PROC,1/)), &
                  "W cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
                ! fusion of old and new variable
                  WHERE (ZVAROUTXYT(:,:,:,I) == XUNDEF) ZVAROUTXYT(:,:,:,I) = ZVAROUTXYT_OLD(:,:,:,1)

                END DO
              ELSE
                CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT_OLD), &
                        "W cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
                ! fusion of old and new variable
                WHERE (ZVAROUTXYT == XUNDEF) ZVAROUTXYT = ZVAROUTXYT_OLD
              END IF
              ! write fusioned variable
            END IF
            ! PRINT*, "before put", SHAPE(ZVAROUTXYT)
            IF (LTIMECHUNK) THEN
              DO I=1, NTIME
                !PRINT*, I, MAXVAL(ZVAROUTXYT(:,:,:,I))
                CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT(:,:,:,I),  &
                        start =(/1,IXSTART,IYSTART,I/) ,count = (/NDECILE,NX_PROC,NY_PROC,1/)),&
                        "W Cannot put var "//TRIM(VAR_NAME_IN(IV)))
              END DO
            ELSE
              CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT,  &
                    start =(/1,IXSTART,IYSTART,1/) ,count = (/NDECILE,NX_PROC,NY_PROC,NTIME/)),&
                    "W Cannot put var "//TRIM(VAR_NAME_IN(IV)))
            END IF
            ! PRINT*, "after put"
          ELSE
            ! standard case (time, x, y)
            IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
              ! read old variable
              CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              IF (LTIMECHUNK) THEN
                DO I=1, NTIME
                  CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT_OLD(:,:,1,:), start =(/IXSTART,IYSTART,I/),&
                          count = (/NX_PROC,NY_PROC,1/)), &
                          "X cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
                  ! fusion of old and new variable
                  WHERE (ZVAROUTXYT(:,:,:,I) == XUNDEF) ZVAROUTXYT(:,:,:,I) = ZVAROUTXYT_OLD(:,:,:,1)
                END DO
              ELSE
                CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT_OLD(:,:,1,:)), &
                  "X cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
                ! fusion of old and new variable
                WHERE (ZVAROUTXYT == XUNDEF) ZVAROUTXYT = ZVAROUTXYT_OLD
              END IF
              ! write fusioned variable
            END IF
            IF (LTIMECHUNK) THEN
              DO I=1, NTIME
                CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT(:,:,:,I),  &
                        start =(/IXSTART,IYSTART,I/) ,count = (/NX_PROC,NY_PROC,1/)),&
                        "X Cannot put var "//TRIM(VAR_NAME_IN(IV)))
              END DO
            ELSE
              CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT,  &
                      start =(/IXSTART,IYSTART,1/) ,count = (/NX_PROC,NY_PROC,NTIME/)),&
                      "X Cannot put var "//TRIM(VAR_NAME_IN(IV)))
            END IF

          ENDIF
        ELSE
          ! case (time, Number_of_Patches, x, y)
          IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
              ! read old variable
              CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              IF (LTIMECHUNK) THEN
                DO I=1, NTIME
                  CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT_OLD, start =(/IXSTART,IYSTART,1,I/), &
                          count = (/NX_PROC,NY_PROC,NPATCH,1/)), &
                          "Y cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
                  ! fusion of old and new variable
                  WHERE (ZVAROUTXYT(:,:,:,I) == XUNDEF) ZVAROUTXYT(:,:,:,I) = ZVAROUTXYT_OLD(:,:,:,1)
                END DO
              ELSE
                CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUTXYT_OLD), &
                        "Y cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
                ! fusion of old and new variable
                WHERE (ZVAROUTXYT == XUNDEF) ZVAROUTXYT = ZVAROUTXYT_OLD
              END IF
              ! write fusioned variable
          END IF
          IF (LTIMECHUNK) THEN
            DO I=1, NTIME
              CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT(:,:,:,I),  &
                      start =(/IXSTART,IYSTART,1,I/) ,count = (/NX_PROC,NY_PROC,NPATCH,1/)),&
                      "Y Cannot put var "//TRIM(VAR_NAME_IN(IV)))
            END DO
          ELSE
            CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUTXYT,  &
                    start =(/IXSTART,IYSTART,1,1/) ,count = (/NX_PROC,NY_PROC,NPATCH,NTIME/)),&
                    "Y Cannot put var "//TRIM(VAR_NAME_IN(IV)))
          END IF

        ENDIF
      ENDIF
      !
      DEALLOCATE(ZVARINT)
      DEALLOCATE(ZVAROUTXYT)
      IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
        DEALLOCATE(ZVAROUTXYT_OLD)
      END IF
      !
    ELSEIF (ANY(VAR_ID_DIMS_OUT(:,IV) == ILLOC_ID ) .AND. &
            ALL(VAR_ID_DIMS_OUT(:,IV) /= ITIMEID))THEN !GRID DIM
      !
      ALLOCATE(ZVARIN(DIM_SIZE_IN(ILLOC_ID),NPATCH))
      ALLOCATE(ZVAROUT2D(NX_PROC,NY_PROC,NPATCH))
      IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
        ALLOCATE(ZVAROUT2D_OLD(NX_PROC,NY_PROC,NPATCH))
      END IF
      ZVAROUT2D = XUNDEF
      !  Read variable
      CALL CHECK(NF90_GET_VAR(FILE_ID_IN,VAR_ID_IN(IV),ZVARIN, &
              start =(/1,1/), count = (/DIM_SIZE_IN(ILLOC_ID),NPATCH/)) &
              ,"Cannot get var"//TRIM(HFILENAMEIN))
      !
      CALL INTERPOLZS2DNOTIME(ZVAROUT2D,ZVARIN,IINDICESBAS,IINDICESHAUT,&
              ZZSOUT,IZSIN)
      !
      IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
        ! read old variable
        CALL CHECK(NF90_INQ_VARID(FILE_ID_OUT, VAR_NAME_IN(IV), IV_TMP), &
                "Cannot inquire variable id for "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
        CALL CHECK(NF90_GET_VAR(FILE_ID_OUT, IV_TMP, ZVAROUT2D_OLD), &
                "Z cannot get variable "//VAR_NAME_IN(IV)//"from "//TRIM(HFILENAMEOUT))
              ! fusion of old and new variable
        WHERE (ZVAROUT2D == XUNDEF) ZVAROUT2D = ZVAROUT2D_OLD
              ! write fusioned variable
      END IF
      ! Write variable
      CALL CHECK(NF90_PUT_VAR(FILE_ID_OUT,VAR_ID_OUT(IV),ZVAROUT2D,  &
              start =(/IXSTART,IYSTART,1/) ,count = (/NX_PROC,NY_PROC,NPATCH/)),&
              "Z Cannot put var"//TRIM(HFILENAMEOUT))
      !
      DEALLOCATE(ZVARIN)
      DEALLOCATE(ZVAROUT2D)
      IF (.NOT. LMULTIOUTPUT .AND. (JINFILE /= 1)) THEN
        DEALLOCATE(ZVAROUT2D_OLD)
      END IF
      !
    ELSE
      !
      PRINT*, VAR_NAME_IN(IV),  VAR_ID_DIMS_OUT(:,IV), "DIMENSION VAR NON TRAITEE"
      !
    ENDIF
    !
  ENDDO
  ! 07/01/2021 check proper allocation/deallocation for multi-input single-output case
  IF (JINFILE == 1) THEN
    VALUE_TIME_OLD = VALUE_TIME
  END IF
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
  !
  IF (LMULTIOUTPUT .OR. (JINFILE == NNUMBER_INPUT_FILES)) THEN
    DEALLOCATE(VALUE_TIME_OLD)
    DEALLOCATE(VAR_ID_OUT)
    DEALLOCATE(VAR_ID_DIMS_OUT)
  !
    DEALLOCATE(DIM_NAME_OUT)
    DEALLOCATE(DIM_ID_OUT)
    DEALLOCATE(DIM_SIZE_OUT)
    DEALLOCATE(ZZSOUT)
    DEALLOCATE(IMASSIFOUT)
    DEALLOCATE(ZASPECTOUT)
    DEALLOCATE(ZSLOPEOUT)
    IF(IRANK == 1)THEN
      DEALLOCATE(ZLATOUT)
      DEALLOCATE(ZLONOUT)
      DEALLOCATE(ISTATIONOUT)
    ELSEIF(IRANK == 2)THEN
      IF (GRID_TYPE == "LL") THEN
        DEALLOCATE(ZLATOUT)
        DEALLOCATE(ZLONOUT)
      ELSEIF (GRID_TYPE == "XY") THEN
        DEALLOCATE(ZYOUT)
        DEALLOCATE(ZXOUT)
      ENDIF
    ENDIF
  END IF
  !
  DEALLOCATE(IZSIN)
  DEALLOCATE(IMASSIFIN)
  DEALLOCATE(IASPECTIN)
  !
  DEALLOCATE(IINDICESBAS)
  DEALLOCATE(IINDICESHAUT)
  !
  IF (LMULTIOUTPUT .OR. (JINFILE == NNUMBER_INPUT_FILES)) THEN
  ! close output file
    CALL CHECK(NF90_CLOSE(FILE_ID_OUT),"Cannot close file "//TRIM(HFILENAMEOUT))
    ! close grid file
    CALL CHECK(NF90_CLOSE(FILE_ID_GEO),"Cannot close file"//TRIM(HFILENAMEG))
  END IF
  ! close input file
  CALL CHECK(NF90_CLOSE(FILE_ID_IN),"Cannot close file"//TRIM(HFILENAMEIN))
  ! PRINT*, IMASSIFTODELETE, ILAYERTODELETE, NPATCHID, NPATCH, IDECILEID, NDECILE
END DO ! loop over input files (domains)
!
CALL MPI_FINALIZE(IERR)
!
END PROGRAM INTERPOLATE_SAFRAN
!END PROGRAM INTERPOLATE_SAFRAN
!CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

