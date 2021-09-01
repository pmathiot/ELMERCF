#!/bin/bash
#SBATCH -J <NAME>

#SBATCH --nodes=<NNODES>
#SBATCH --constraint=HSW24

#SBATCH --ntasks=<NTASKS>
#SBATCH --ntasks-per-node=24
#SBATCH --threads-per-core=1
#SBATCH --cpus-per-task=1

#SBATCH --time=4:00:00

#SBATCH --output LOG/<NAME>.o%j
#SBATCH --error  LOG/<NAME>.e%j

#multi-threads
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export KMP_AFFINITY=granularity=fine,compact,1,0,verbose

module load intel/19.4
module load cmake/3.9.0

module load netcdf/4.6.3-intel-19.0.4-intelmpi-2019.4.243
module load netcdf-fortran/4.4.4-intel-19.0.4-intelmpi-2019.4.243

module load mumps/5.2.1-intel-19.0.4-intelmpi-2019.4.243
module load mkl/19.4

module load /scratch/cnt0021/egi6035/pmathiot/ELMER/Elmer_Benoit_20210401/elmerfem-64255215

ulimit -s unlimited

cd <WORKDIR>

echo <SIF> > ELMERSOLVER_STARTINFO

i=<RUNID>
CONFIG=<ECONFIG>
CASE=<ECASE>
RSTFILEb=<RSTFILEb>

# path
RELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_R
SELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_S
WELMER=$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_WORK

if [[ $i -gt 1 ]] && [[ ! -f $WELMER/${RSTFILEb}.0 ]]; then
   echo '$WELMER/${RSTFILEb}.0 is missing, we pick it up from $RELMER'
   cp -f $RELMER/${RSTFILEb}.* $WELMER/MSH/.
fi

srun --mpi=pmi2 -K1 --resv-ports -n $SLURM_NTASKS ElmerSolver_mpi

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
   exit 42

fi

if [[ $nerr -ne 0 ]] ; then
   echo 'ERROR during copying output file/results; please check'
   exit 42
fi