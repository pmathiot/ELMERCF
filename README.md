# ELMER CONFIGURATION MANAGER

## Organisation
Configurations directory are:
- ANT50.Gl1: Antarctic configuration

In each configuration directory, you could find:
- ./: the script to setup new simulation or clone old simulation.
- REF: contains all the script, sif (...) needed to run the simulation.
- SRC: the source code on top of the elmer/ice layer needed.
- 

## Requirement
The following modules are needed to compile and run Elmer:
- elmer module
- netcdf-fortran

## How to setup a new simulation
- Step 0: go in your configuration directory
- Step 1: run `./setup_arch.bash your_computer` (it is likely you will have to edit: `REF/param_arch.bash`)
```
usage: setup_arch.bash [COMPUTER (irene)]
```
- Step 2: run `./setup_simulation.bash`
```
setup_simulation [CONFIG] [CASE]
```
- Step 3: go in *CONFIG-CASE* directory
- Step 4: update the following files (.incf, .sif, .param., run_param.bash ...)

### If arch not available
To be able to run ELMER_CM with a new architecture, you need to:
- Step 1: create `../ARCH/run_arch_yourcomputer.slurm` which defines the script header
- Step 2: create `../ARCH/param_arch_yourcomputer.bash` which defines the arch dependent function (submit script, run elmer and load module)

## How to clone an existing simulation
- Step 0: go in your target configuration directory
- Step 1: run `./clone_simulation.bash`
```
clone_simulation [CONFIG_REF] [CASE_REF] [CONFIG] [CASE]
```
- Step 2: go in *CONFIG-CASE* directory
- Step 3: update the following files (.incf, .sif, .param., run_param.bash ...)

## How to compile solver
- Step 0: go in your configuration directory
- Step 1: add your solver in the *Makefile* (solver.F90 in SRC/.)
- Step 2: `make`

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
