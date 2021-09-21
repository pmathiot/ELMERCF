#!/bin/bash

# number of elmer partition
NP=48

# number of HPC nodes
NN=2

# first iteration number (if more than 1, means restart i-1 are already in place)
# end iteration
STARTITER=2
ENDITER=2

# restart path and rst file (assume all in $IELMER)
RSTINITpath=${IELMER}/RST/OPTIM_VEL_R15_48part
RSTINITfile=OPTIM_R15_24to48.result
