#!/bin/bash
ulimit -s unlimited

# INPUTS
# segment number
i=<ID>
# restart name
RSTFILEb=<RSTFILEb>
# conf case
CONFIG=<ECONFIG>
CASE=<ECASE>

# load arch parameter
. ./param_arch.bash

# load modules
load_elmer_modules

# 
echo ''
echo "run Elmer/Ice in $WELMER"
echo ''
cd $WELMER

# manage sif info
echo "sif use is : elmer_t${i}.sif"
if [ ! -f elmer_t${i}.sif ] ; then echo "E R R O R: sif file missing"; exit 42; fi
echo elmer_t${i}.sif > ELMERSOLVER_STARTINFO

# manage restart
if [[ $i -gt 1 ]] && [[ ! -f $WELMER/${RSTFILEb}.0 ]]; then
   echo '$WELMER/${RSTFILEb}.0 is missing, we pick it up from $RELMER'
   cp -f $RELMER/${RSTFILEb}.* $WELMER/MSH/. || nerr=$((nerr+1))

   if [[ $nerr -ne 0 ]] ; then
   echo 'ERROR during copying restart file; please check'
   touch zERROR_pp_${i}
   exit 42
   fi
 
fi

# run elmer (see function in param_hpc.bash)
run_elmer

# post processing
RUNSTATUS=$?
echo $RUNSTATUS
if [[ $RUNSTATUS == 0 ]]; then

   # error count
   nerr=0

   # mv data to S dir
   echo ''
   echo "mv vtu and dat to $SELMER"
   mv MSH/$CONFIG-${CASE}_${i}_??np??_t????.vtu        $SELMER/. || nerr=$((nerr+1))
   mv MSH/$CONFIG-${CASE}_${i}_t????.pvtu              $SELMER/. || nerr=$((nerr+1))
   mv $CONFIG-${CASE}_${i}_scalars.dat*                $SELMER/. || nerr=$((nerr+1))
   mv *INITMIP_Scalar_OUTPUT_$CONFIG-${CASE}_${i}.dat* $SELMER/. || nerr=$((nerr+1))

   # cp restart to RST dir
   echo "cp result to $RELMER"
   cp -f MSH/$CONFIG-${CASE}_${i}.result.* $RELMER/.   || nerr=$((nerr+1))

else

   echo 'ELMER failed, exit 42'
   touch zERROR_elmer_${i}
   exit 42

fi

if [[ $nerr -ne 0 ]] ; then
   echo 'ERROR during copying output file/results; please check'
   touch zERROR_pp_${i}
   exit 42
fi
