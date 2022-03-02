#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "setup_simulation [CONFIG] [CASE]"
    exit 42
fi
echo ''

CONFIG=$1
CASE=$2

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
RUND=./$CONFIG-$CASE/
mkdir $RUND || nerr=1
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
cp REF/elmer.sif $RUND/${CONFIG}-${CASE}_elmer.sif || nerr=$((nerr+1))
# copy reference parameter
cp REF/elmer.param $RUND/${CONFIG}-${CASE}_elmer.param || nerr=$((nerr+1))
# copy lsol parameter
cp REF/elmer.lsol $RUND/${CONFIG}-${CASE}_elmer.lsol || nerr=$((nerr+1))
# copy slurm header
cp REF/run_arch.slurm $RUND/run_arch.slurm || nerr=$((nerr+1))
# copy arch param
cp REF/param_arch.bash $RUND/param_arch.bash || nerr=$((nerr+1))
# copy reference script to submit elmer
cp REF/run_elmer_skel.bash $RUND/run_elmer_skel.bash || nerr=$((nerr+1))
# copy reference script to submit elmer
cp REF/run_param_skel.bash $RUND/run_param.bash || nerr=$((nerr+1))
# copy and update reference include file
sed -e "s/<CONFIG>/${CONFIG}/g;s/<CASE>/${CASE}/g" REF/elmer.incf > $RUND/${CONFIG}-${CASE}_elmer.incf || nerr=$((nerr+1))
# copy prepare script
sed -e "s/<CONFIG>/${CONFIG}/g;s/<CASE>/${CASE}/g" REF/prepare_elmer.bash > $RUND/prepare_elmer.bash || nerr=$((nerr+1))
chmod u+x $RUND/prepare_elmer.bash
if (( $nerr > 0 )); then echo 'E R R O R: one of the files failed to be copied'; echo ''; exit 42; fi

echo ''
echo '[1.1] compile extra sources'
echo '==========================='
echo ''
echo ' - Copy source to config directory'
cp -r SRC/* $RUND/MY_SRC/. || nerr=$((nerr+1))

echo ' - load modules'
load_elmer_modules || nerr=$((nerr+1))

echo ' - build MY_BLD directory'
if [ ! -d $RUND/MY_BLD ]; then mkdir $RUND/MY_BLD || nerr=$((nerr+1)); fi
cd $RUND/MY_SRC/. || nerr=$((nerr+1))
if (( $nerr > 0 )); then echo 'E R R O R: one of the compilation preparation step failed'; echo ''; exit 42; fi

echo ' - start compilation'
make all || nerr=$((nerr+1))
cd ../..
if (( $nerr > 0 )); then echo ''; echo 'E R R O R: fail to compile extra sources'; echo ''; exit 42; fi

echo ''
echo '           Compilation successful'
echo ''
