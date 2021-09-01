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

if [ ! -d "../${CONFIG_REF}/${CONFIG_REF}-${CASE_REF}" ]; then
    echo "ERROR: reference simulation do not exist"
    exit 42
fi

if [ $CONFIG_REF != $CONFIG ]; then
    echo "WARNING: CONFIG_REF and CONFIG are different"
    echo "         - It is highly recommended to check carefully all the files"
    echo "         - Solver are not copied across, only simulation parameter files and running script"
    echo "              especially: elmer.incf"
fi

# init error count
nerr=0

# Create run dir
RELMER=./$CONFIG-$CASE/
mkdir $RELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RELMER"; echo ''; exit 42; fi

# Create log dir
mkdir $RELMER/LOG || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RELMER/LOG"; echo ''; exit 42; fi

# Create workdir
WELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_WORK
mkdir -p $WELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $WELMER"; echo ''; exit 42; fi

# create dtadir
SELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_S
mkdir -p $SELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $SELMER"; echo ''; exit 42; fi

# create mshdir
RSTELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_R
mkdir -p $RSTELMER || nerr=1
if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $RSTELMER"; echo ''; exit 42; fi

# Create input dir
IELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-I
if [ ! -d $IELMER ]; then
   mkdir -p $IELMER || nerr=1
   if (( $nerr > 0 )); then echo ''; "E R R O R: cannot create $IELMER"; echo ''; exit 42; fi
fi

echo ''
echo '[1.0] copy files'
echo '================'
echo ''

nerr=0
RELMER_REF=../${CONFIG_REF}/${CONFIG_REF}-${CASE_REF}/
# copy reference sif
cp ${RELMER_REF}/${CONFIG_REF}-${CASE_REF}_elmer.sif $RELMER/${CONFIG}-${CASE}_elmer.sif || nerr=$((nerr+1))
# copy reference parameter
cp ${RELMER_REF}/${CONFIG_REF}-${CASE_REF}_elmer.param $RELMER/${CONFIG}-${CASE}_elmer.param || nerr=$((nerr+1))
# copy lsol parameter
cp ${RELMER_REF}/${CONFIG_REF}-${CASE_REF}_elmer.lsol $RELMER/${CONFIG}-${CASE}_elmer.lsol || nerr=$((nerr+1))
# copy reference script to submit elmer
cp ${RELMER_REF}/run_elmer_skel.bash $RELMER/run_elmer_skel.bash || nerr=$((nerr+1))
# copy reference script to submit elmer
cp ${RELMER_REF}/run_param.bash $RELMER/run_param.bash || nerr=$((nerr+1))

# copy and update reference include file
sed -e "s/\$name=\"${CONFIG_REF}-${CASE_REF}\"/\$name=\"${CONFIG}-${CASE}\"/g;s!\$out_dir.*!\$out_dir = \"$SELMER\"!g" \
    ${RELMER_REF}/${CONFIG_REF}-${CASE_REF}_elmer.incf > $RELMER/${CONFIG}-${CASE}_elmer.incf || nerr=$((nerr+1))

# copy prepare script
sed -e "s/CONFIG=${CONFIG_REF}/CONFIG=${CONFIG}/g;s/CASE=${CASE_REF}/CASE=${CASE}/g" ${RELMER_REF}/prepare_elmer.bash > $RELMER/prepare_elmer.bash || nerr=$((nerr+1))
chmod u+x $RELMER/prepare_elmer.bash
if (( $nerr > 0 )); then echo ''; 'E R R O R: one of the files failed to be copied'; echo ''; exit 42; fi

echo ''
echo '[1.1] compile extra sources'
echo '==========================='
echo ''

nerr=0
make all || nerr=$((nerr+1))
if (( $nerr > 0 )); then echo ''; echo 'E R R O R: fail to compile extra sources'; echo ''; exit 42; fi
