#!/bin/bash
CONFIG=ANT50.GL1
CASE=RST2x5BDF2

WELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_WORK
RELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_R
IELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-I

. run_param.bash

echo ''
echo "Copy executable to $WELMER"
echo "=========================="
echo ''
# to clean this we could find a way to check only what is needed in the sif file
# like this workdir only contains what is needed
cp ../BLD/* $WELMER/.

# copy sif and param
echo ''
echo "Copy incf and param to: $WELMER"
echo "======================="
echo
\cp $CONFIG-${CASE}_elmer.param $WELMER/elmer.param
\cp $CONFIG-${CASE}_elmer.incf  $WELMER/elmer.incf
\cp $CONFIG-${CASE}_elmer.lsol  $WELMER/elmer.lsol

# link data
# elmer point directly to the data dir => no need to do anything
# except for the mesh
if [ ! -d $WELMER/MSH/partitioning.$NP ] ; then mkdir -p $WELMER/MSH/partitioning.$NP ; fi
ln -sf $IELMER/MSH/partitioning.$NP/part.* $WELMER/MSH/partitioning.$NP/.

echo ''
echo "$(($ENDITER - $STARTITER + 1)) segment to run"
echo '===================='

for ((i=$STARTITER ; i<=$ENDITER ; i++))
do

    # name
    NAME=$CONFIG-$CASE

    # get restart
    # no need to do anything, elmer point directly to the directory
    # except for the first one
    if [[ $((i-1)) -eq 0 ]]; then
      if [[ $RSTINITfile != NONE ]]; then
         RSTFILEb=$RSTINITfile
         ln -sf $RSTINITpath/${RSTINITfile}.* $WELMER/MSH/.
      fi
    else
         RSTFILEb="${NAME}_$((i-1)).result"
    fi   
    RSTFILEa="${NAME}_${i}.result"

    echo ''
    echo "start: ${NAME}_elmer_$i from $RSTFILEb"
    echo '======' 
    echo ''

    # prepare sif
    sed -e "s/<ID-1>/$(($i-1))/g"     \
        -e "s/<ID>/$(($i))/g"         \
        -e "s/<RSTFILEb>/$RSTFILEb/g" \
        -e "s/<RSTFILEa>/$RSTFILEa/g" $CONFIG-${CASE}_elmer.sif  > $WELMER/elmer_t$i.sif 

    # prepare run script
    sed -e "s!<NAME>!${NAME}_$i!g"     \
        -e "s!<NNODES>!${NN}!g"        \
        -e "s!<NTASKS>!${NP}!g"        \
        -e "s!<WORKDIR>!$WELMER!g"     \
        -e "s!<SIF>!elmer_t$i.sif!g"   \
        -e "s!<ECONFIG>!$CONFIG!g"     \
        -e "s!<ECASE>!$CASE!g"         \
        -e "s!<RSTFILEb>!$RSTFILEb!g"  \
        -e "s!<RUNID>!${i}!"           run_elmer_skel.bash > run_elmer_${i}.slurm

    # submit job
    if [ ! -z "$jobid0" ];then
        jobid=$(sbatch --parsable --dependency=afterok:$jobid0  run_elmer_$i.slurm)
        echo "        id        : $jobid"
        echo "        dependency: $jobid0"
    else
        jobid=$(sbatch --parsable run_elmer_$i.slurm)
        echo "        id        : $jobid"
    fi
    jobid0=$jobid
 
done
echo ''
