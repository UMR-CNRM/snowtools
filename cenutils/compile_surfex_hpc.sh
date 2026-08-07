#!/usr/bin/env bash

#####################################################
# script : compile_surfex_hpc.sh
# Compilation of SURFEX on MeteoFrance supercomputer
#####################################################

set -euo pipefail

usage()
{
echo
echo "Usage:"
echo "./compile_surfex_hpc.sh --surfex_dir <path_to_SURFEX_dir> [options]"
echo
echo "Options:"
echo "    --surfex_dir     : Absolute path to the local SURFEX repository"
echo "    --ver_mpi        : Set MPI compilation option. Possible values : 'MPI' or 'NOMPI'. Default : 'MPI'"
echo "    --optlevel       : Set optimisation level. Possible values : 'O2' or 'DEBUG'. Default : 'O2' (Long runs must never be run in DEBUG mode)"
echo "    -h|--help        : Print help message"
echo
echo Minimal example for a compilation with MPI parallelisation and an O2 optimisation:
echo ./compile_surfex_hpc.sh --surfex_dir $HOME/SURFEX
echo
}

export SRC_SURFEX=""
export XYZ=""

# Default values
VER_MPI=MPIAUTO
MPI_ID=MPI
OPTLEVEL=O2

if [[ $# -eq 0 ]]; then
  usage
  exit 0
fi

while [[ $# -gt 0 ]]; do
    case "$1" in
        --surfex_dir)
            [[ -n "${2-}" ]] || { echo "ERROR: option --surfex_dir requires an argument"; exit 1; }
            SURFEX_DIR="$2"
            shift 2
            ;;
        --ver_mpi)
            [[ -n "${2-}" ]] || { echo "ERROR: option --ver_mpi requires an argument"; exit 1; }
            case $2 in
                MPI)
                    VER_MPI=MPIAUTO
                    MPI_ID=MPI
                ;;
                NOMPI)
                    export VER_MPI=NOMPI
                    MPI_ID=NOMPI
                    export VER_CDF=CDF2020
                ;;
                *)
                    echo "Invalid option for --ver_mpi: $2"
                    exit 1
            esac
            shift 2
            ;;
        --optlevel)
            [[ -n "${2-}" ]] || { echo "ERROR: option --optlevel requires an argument"; exit 1; }
            case $2 in
                O2)
                    OPTLEVEL=O2
                ;;
                DEBUG)
                    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                    echo "WARNING : Long runs must never be run in DEBUG mode."
                    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                    OPTLEVEL=DEBUG
                ;;
                *)
                    echo "Invalid option for --optlevel: $2"
                    exit 1
            esac
            shift 2
            ;;
        -h|--help)  usage ; exit 0 ;;
        *)          echo "Unknown option: $1" ; exit 1 ;;
    esac
done

export $VER_MPI
export OPTLEVEL=$OPTLEVEL

module load intel
module load intelmpi
module load curl

SRC_DIR="${SURFEX_DIR}/src"
if [[ ! -d "$SRC_DIR" ]]; then
    echo "ERROR: directory $SRC_DIR does not exist."
    exit 1
fi
cd $SRC_DIR

./configure

# source the last profile created
# TODO : ajouter un control sur la date pour plus de sécurité ?
PROFILE=$(ls -lrt ../conf/profile_* 2>/dev/null | tail -n 1 | grep -o '[^ ]*$' || true)
if [[ -z "$PROFILE" ]]; then
    echo "Error : no profile found in ${SURFEX_DIR}/conf"
    exit 1
fi
echo "Sourcing profile $PROFILE"
set +e
source $PROFILE
set -e

make
make installmaster

EXESURFEX="${SURFEX_DIR}/exe"
if [[ ! -d "$EXESURFEX" ]]; then
    echo
    echo "ERROR: directory $EXESURFEX does not exist."
    echo
    exit 1
fi

if git rev-parse --is-inside-work-tree ; then
    # $SURFEX_DIR is a git repository
    if ! git diff HEAD --quiet ; then
        # Un-tracked local changes : the commit may not ensure reproducibility
        # TODO : définir le comportement à adopter dans ce cas
        surfex_commit="Unknown"
        outstr="WARNING : there are untracked local changes since last commit.
                The compiled executables can not be associated to any git commit."
    else
        # Code up to date with last commit
        surfex_commit=$(git log -n 1 --pretty=format:"%h")
        if [[ $(git branch -r --contains $surfex_commit) = "" ]]; then
            # Commit on local branch only --> must be pushed to ensure reproducibililty
            outstr="WARNING : The executables have successfully been linked to a git commit
                    but the commit has not been pushed to a remote repository yet."
        else
            # Commit on remote --> we can guarantee reproducibililty
            outstr="The executables have successfully been linked to a git commit already pushed on a remote repository"
        fi
    fi
elif [[ -f ${SURFEX_DIR}/.git_info ]]; then
    # $SURFEX_DIR is a copy of a git repository with the "put" command
    # --> The code may differ from the commit in the .git_info file du to un-commited local changes
    outstr="WARNING : the SURFEX commit associated to the executables is uncertain.
            You need to ensure iyourself that the code is up-to date with the latest commit"
    surfex_commit="$(cat ${SURFEX_DIR}/.git_info | awk '{print $1}')_uncertain"
else
    # No information on the git commit
    # TODO : définir le comportement à adopter dans ce cas
    outstr="WARNING : no git commit found"
    surfex_commit="Unknown"
fi

uenv=$HOME/.vortexrc/hack/uget/$USER/env/surfex_executables_${VER_MPI,,}_${surfex_commit}

if [[ -f $uenv ]]; then
    echo
    echo "ERROR : Uenv ${uenv} already exists"
    echo
    exit 1
fi

data_dir=$HOME/.vortexrc/hack/uget/$USER/data

# Look for the produced binaries' directory
# TODO : ajouter un control sur la date pour plus de sécurité ?
find_binary() {
    local pattern=$1   # ex : "OFFLINE-*MPIAUTO*O2*"
    bin_dir=$(ls -lrt $EXESURFEX/$pattern 2>/dev/null | tail -n 1 | grep -o '[^ ]*$' || true)
    if [[ -z "$bin_dir" ]]; then
        echo
        echo "ERROR : No binary matching $pattern in $EXESURFEX"
        echo
        exit 1
    fi
    echo "$bin_dir"
}

link() {
    local src=$1
    local dst=$2

    if [[ ! -f "$src" ]]; then
        echo
        echo "ERROR : File $src does not exist"
        echo
        exit 1
    fi
    if [[ -L "$dst" ]]; then
        echo "Removing existing link $dst"
        rm $dst
    fi
    echo "Creation of symbolic link $dst"
    ln -s "$src" "$dst"
}

save_executable()
{
    local EXE_NAME=$1
    EXE_PATH=$(find_binary "${EXE_NAME}-*${VER_MPI}*${OPTLEVEL}*")
    link "${EXE_PATH}" "${EXESURFEX}/${EXE_NAME}"
    cp ${EXE_PATH} ${data_dir}/${EXE_NAME}_${VER_MPI}_${surfex_commit}
    echo "MASTER_${EXE_NAME}_${MPI_ID}=\"uenv:${EXE_NAME}_${VER_MPI}_${surfex_commit}@${USER}\"" >> $uenv
}

echo "Creation of symbolic links toward SURFEX executables"

for executable in OFFLINE PREP PGD SODA ;
    do save_executable ${executable}
done

export EXESURFEX="${EXESURFEX}"

echo
echo "====================================================================="
echo "The SURFEX compilation was successful"
echo
echo $outstr
echo
echo The executables are available in the following uenv :
echo $(basename ${uenv})
echo
echo To use this uenv in your simulations, add the following variable
echo in your configuration file:
echo surfex_uenv=uenv:$(basename ${uenv})@${USER}
echo "====================================================================="
echo
exit 0
