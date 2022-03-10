#!/bin/bash

if [ "$#" -ne 4 ]; then
    echo "clone_simulation [CONFIG_REF] [CASE_REF] [CONFIG] [CASE]"
    exit 42
fi
echo ''

CONFIG_REF=$1
CASE_REF=$2
CONFIG=$3
CASE=$4

echo ''
echo '[1.0] create architecture'
echo '========================='
echo ''

if [ -d "$CONFIG-$CASE" ]; then
    echo "SIMULATION ALREADY EXISTING: " $CONFIG-$CASE
    echo "ABORT"
    exit 42
fi

if [ ! -d "../${CONFIG_REF}/SIMU/${CONFIG_REF}-${CASE_REF}" ]; then
    echo "ERROR: reference simulation do not exist"
    exit 42
fi

if [ $CONFIG_REF != $CONFIG ]; then
    echo "WARNING: CONFIG_REF and CONFIG are different"
    echo "         - It is highly recommended to check carefully all the files"
    echo "         - Solver are not copied across, only simulation parameter files and running script"
    echo "              especially: elmer.incf"
fi

# load param arch file
. REF/param_arch.bash

echo ''
echo '[1.0] create architecture'
echo '========================='
echo ''

if [ -d "$CONFIG-$CASE" ]; then
    echo "SIMULATION ALREADY EXISTING: " $CONFIG-$CASE
    echo "ABORT"
    exit 42
fi

# init error count
nerr=0

# Create run dir
RUND=./SIMU/$CONFIG-$CASE/
mkdir -p $RUND || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RUND"; echo ''; exit 42; fi

# Create src dir
mkdir $RUND/MY_SRC || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RUND/MY_SRC"; echo ''; exit 42; fi

# Create bld dir
mkdir $RUND/MY_BLD || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RUND/MY_SRC"; echo ''; exit 42; fi

# Create log dir
mkdir $RUND/LOG || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RUND/LOG"; echo ''; exit 42; fi

# Create workdir
mkdir -p $WELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $WELMER"; echo ''; exit 42; fi

# create dtadir
mkdir -p $SELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $SELMER"; echo ''; exit 42; fi

# create mshdir
mkdir -p $RELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RELMER"; echo ''; exit 42; fi

# Create input dir
mkdir -p $IELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $IELMER"; echo ''; exit 42; fi

echo ''
echo '[1.0] copy files'
echo '================'
echo ''

nerr=0
RUND_REF=../${CONFIG_REF}/SIMU/${CONFIG_REF}-${CASE_REF}/
# copy reference sif
cp ${RUND_REF}/${CONFIG_REF}-${CASE_REF}_elmer.sif $RUND/${CONFIG}-${CASE}_elmer.sif || nerr=$((nerr+1))
# copy reference parameter
cp ${RUND_REF}/${CONFIG_REF}-${CASE_REF}_elmer.param $RUND/${CONFIG}-${CASE}_elmer.param || nerr=$((nerr+1))
# copy lsol parameter
cp ${RUND_REF}/${CONFIG_REF}-${CASE_REF}_elmer.lsol $RUND/${CONFIG}-${CASE}_elmer.lsol || nerr=$((nerr+1))
# copy reference script to submit elmer
cp ${RUND_REF}/run_elmer_skel.bash $RUND/run_elmer_skel.bash || nerr=$((nerr+1))
# copy reference script to submit elmer
cp ${RUND_REF}/run_param.bash $RUND/run_param.bash || nerr=$((nerr+1))
# copy slurm header
cp ${RUND_REF}/run_arch.slurm $RUND/run_arch.slurm || nerr=$((nerr+1))
# copy arch param
cp ${RUND_REF}/param_arch.bash $RUND/param_arch.bash || nerr=$((nerr+1))


# copy and update reference include file
sed -e "s/\$name=\"${CONFIG_REF}-${CASE_REF}\"/\$name=\"${CONFIG}-${CASE}\"/g;s!\$out_dir.*!\$out_dir = \"$SELMER\"!g" \
    ${RUND_REF}/${CONFIG_REF}-${CASE_REF}_elmer.incf > $RUND/${CONFIG}-${CASE}_elmer.incf || nerr=$((nerr+1))

# copy prepare script
sed -e "s/CONFIG=${CONFIG_REF}/CONFIG=${CONFIG}/g;s/CASE=${CASE_REF}/CASE=${CASE}/g" ${RUND_REF}/prepare_elmer.bash > $RUND/prepare_elmer.bash || nerr=$((nerr+1))
chmod u+x $RUND/prepare_elmer.bash
if (( $nerr > 0 )); then echo ''; 'E R R O R: one of the files failed to be copied'; echo ''; exit 42; fi

echo ''
echo '[1.1] copy sources and solver'
echo '============================='
echo ''
echo '   no re-compilation as it is a clone'
echo ''

nerr=0
# copy sources
cp -r ${RUND_REF}/MY_SRC/* $RUND/MY_SRC/. || nerr=$((nerr+1))
# copy bld
cp ${RUND_REF}/MY_BLD/* $RUND/MY_BLD/. || nerr=$((nerr+1))

if (( $nerr > 0 )); then echo ''; echo 'E R R O R: fail to copy extra sources and bin'; echo ''; exit 42; fi

echo 'DONE'
