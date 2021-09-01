# ELMER CONFIGURATION MANAGER

## Requierement
The following modules are needed to compile and run Elmer:
- elmerfem
- netcdf-fortran
- mumps

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

# configuration list

## ANT50.GL1
