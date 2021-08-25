#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "setup_simulation [CONFIG] [CASE]"
    exit 42
fi
echo ''

CONFIG=$1
CASE=$2

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
# copy reference sif
cp REF/elmer.sif $RELMER/${CONFIG}-${CASE}_elmer.sif || nerr=$((nerr+1))
# copy reference parameter
cp REF/elmer.param $RELMER/${CONFIG}-${CASE}_elmer.param || nerr=$((nerr+1))
# copy lsol parameter
cp REF/elmer.lsol $RELMER/${CONFIG}-${CASE}_elmer.lsol || nerr=$((nerr+1))
# copy reference script to submit elmer
cp REF/run_elmer_skel.bash $RELMER/run_elmer_skel.bash || nerr=$((nerr+1))
# copy reference script to submit elmer
cp REF/run_param_skel.bash $RELMER/run_param.bash || nerr=$((nerr+1))
# copy and update reference include file
sed -e "s/<CONFIG>/${CONFIG}/g;s/<CASE>/${CASE}/g" REF/elmer.incf > $RELMER/${CONFIG}-${CASE}_elmer.incf || nerr=$((nerr+1))
# copy prepare script
sed -e "s/<CONFIG>/${CONFIG}/g;s/<CASE>/${CASE}/g" REF/prepare_elmer.bash > $RELMER/prepare_elmer.bash || nerr=$((nerr+1))
chmod u+x $RELMER/prepare_elmer.bash
if (( $nerr > 0 )); then echo ''; 'E R R O R: one of the files failed to be copied'; echo ''; exit 42; fi

echo ''
echo '[1.1] compile extra sources'
echo '==========================='
echo ''

nerr=0
make all || nerr=$((nerr+1))
if (( $nerr > 0 )); then echo ''; echo 'E R R O R: fail to compile extra sources'; echo ''; exit 42; fi
