# ELMER CONFIGURATION MANAGER

## Requierement
The following modules are needed to compile and run Elmer:
- elmerfem
- netcdf-fortran
- mumps

On Occigen, here are the modules I used:
```
  1) c/intel/.19.4
  2) c++/intel/.19.4
  3) fortran/intel/.19.4
  4) mkl/19.4
  5) idb/19.4
  6) intel/19.4
  7) cmake/3.9.0
  8) netcdf/4.6.3-intel-19.0.4-intelmpi-2019.4.243
  9) netcdf-fortran/4.4.4-intel-19.0.4-intelmpi-2019.4.243
 10) mumps/5.2.1-intel-19.0.4-intelmpi-2019.4.243
 11) cdo/1.7.2-gcc-4.8.5-hdf5-1.8.18-openmpi-2.0.4-smp
 12) /scratch/cnt0021/egi6035/pmathiot/ELMER/Elmer_Benoit_20210401/elmerfem-64255215
```

## How to compile solver
- Step 0: go in your configuration directory
- Step 1: add your solver in the *Makefile* (solver.F90 in SRC/.)
- Step 2: `make`

## How to setup a new configuration

## How to clone an existing configuration

## How to setup a new simulation
- Step 0: go in your configuration directory
- Step 1: run `./setup_simulation.bash`
```
setup_simulation [CONFIG] [CASE]
```
- Step 2: go in *CONFIG-CASE* directory
- Step 3: update the following files (.incf, .sif, .param., run_param.bash ...)

## How to clone an existing simulation

## How to run a simulation
- Step 0: update your *run_param.bash*. This file define on what segment to start and end. Length of each segment is defined in the .sif file.
- Step 1: `./prepare_elmer.bash`

In case you want to extend or re-run some part of an experiemnt, adjust *run_param.bash* and rerun *prepare_elmer.bash*

## How to manage elmer output
All the outputs (.vtu and .dat) are store in : `$SCRATCHDIR/ELMER/$CONFIG/$CONFIG-$CASE/$CONFIG-${CASE}_S`

You can easily setup a monitoring of your simulations following these [instructions](ANT50.GL1/VAL/README.md).

# configuration list

## ANT50.GL1
see details [here](ANT50.GL1/README.md)
