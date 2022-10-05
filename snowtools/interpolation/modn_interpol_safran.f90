! Created by  on 26/10/2020.
! Author:
! Sabine Radanovics
MODULE MODN_INTERPOL_SAFRAN
! Module with namelist declarations.
!
    !.. f:variable:: NAM_SWITCHES_INT
    !    :type: namelist
    !
    !    :f:var:`lmultiinput`
    !    :f:var:`lmultioutput`
    !    :f:var:`lselectvar`
    !.. f:variable:: NAM_FILENAMES_SINGLE_IN
    !    :type: namelist
    !
    !    :f:var:`hfilein`
    !.. f:variable:: NAM_FILENAMES_SINGLE_OUT
    !    :type: namelist
    !
    !    :f:var:`hfileout`
    !    :f:var:`hgridin`
    !.. f:variable:: NAM_MULTIIN_SETTING
    !    :type: namelist
    !
    !    :f:var:`nnumber_input_files`
    !.. f:variable:: NAM_MULTIOUT_SETTING
    !    :type: namelist
    !
    !    :f:var:`nnumber_output_grids`
    !.. f:variable:: NAM_FILENAMES_MULTI_IN
    !    :type: namelist
    !
    !    :f:var:`hfilesin`
    !.. f:variable:: NAM_FILENAMES_MULTI_OUT
    !    :type: namelist
    !
    !    :f:var:`hfilesout`
    !    :f:var:`hgridsin`
    !.. f:variable:: NAM_SELECT_VARS_SETTING
    !    :type: namelist
    !
    !    :f:var:`nnumber_of_variables`
    !.. f:variable:: NAM_SELECT_VARS_NAMES
    !    :type: namelist
    !
    !    :f:var:`hvar_list`
    !.. f:variable:: NAM_OTHER_STUFF
    !    :type: namelist
    !
    !    :f:var:`ltimechunk`
    !    :f:var:`lspatialchunk`
    !    :f:var:`nlonchunksize`
    !    :f:var:`nlatchunksize`
USE MODD_INTERPOL_SAFRAN
IMPLICIT NONE

    NAMELIST /NAM_SWITCHES_INT/ LMULTIINPUT, LMULTIOUTPUT, LSELECTVAR
    NAMELIST /NAM_FILENAMES_SINGLE_IN/ HFILEIN
    NAMELIST /NAM_FILENAMES_SINGLE_OUT/ HFILEOUT, HGRIDIN
    NAMELIST /NAM_MULTIIN_SETTING/ NNUMBER_INPUT_FILES
    NAMELIST /NAM_MULTIOUT_SETTING/ NNUMBER_OUTPUT_GRIDS
    NAMELIST /NAM_FILENAMES_MULTI_IN/ HFILESIN
    NAMELIST /NAM_FILENAMES_MULTI_OUT/ HFILESOUT, HGRIDSIN
    NAMELIST /NAM_SELECT_VARS_SETTING/ NNUMBER_OF_VARIABLES
    NAMELIST /NAM_SELECT_VARS_NAMES/ HVAR_LIST
    NAMELIST /NAM_OTHER_STUFF/ LTIMECHUNK, LSPATIALCHUNK, NLONCHUNKSIZE, NLATCHUNKSIZE

END MODULE MODN_INTERPOL_SAFRAN