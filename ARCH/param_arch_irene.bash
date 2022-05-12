#!/bin/bash

# define keywords
ELMER_SCRATCHDIR=${CCCSCRATCHDIR}
ELMER_WORKDIR=${CCCWORKDIR}

# define workdir, restart dir, input dir and output dir
HELMER=${ELMER_HOMEDIR}/ELMER/$CONFIG/$CONFIG-$CASE
WELMER=${ELMER_SCRATCHDIR}/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_WORK
RELMER=${ELMER_SCRATCHDIR}/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_R
IELMER=${ELMER_SCRATCHDIR}/ELMER/$CONFIG/$CONFIG-I
SELMER=${ELMER_SCRATCHDIR}/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_S

## function to submit elmer script (output must be job id)
function submit_elmer() {
    ccc_msub run_elmer_$i.slurm | awk '{print $4}'
    }

function submit_elmer_dependency() {
    ccc_msub -E "--dependency=afterok:$1" run_elmer_$i.slurm | awk '{print $4}'
    }

## function to run elmer:
function run_elmer() {
    ccc_mprun ElmerSolver_mpi
                     }

## function to load modules:
function load_elmer_modules() {
     module purge
     module load c/intel/19.0.5.281 c++/intel/19.0.5.281 fortran/intel/19.0.5.281 intel/19.0.5.281 mpi/openmpi/4.0.2
     module load flavor/buildcompiler/intel/19 flavor/buildmpi/openmpi/4.0 flavor/hdf5/parallel
     module load netcdf-c/4.6.0 netcdf-fortran/4.4.4
     module load cmake/3.16.5
     module load ELMER/Elmer_v9.0_r21ddff3a

     # add check on elmerf90 et elmersolver_mpi here

           }
