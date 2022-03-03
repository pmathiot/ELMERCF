#!/bin/bash

# number of elmer partition
NP=48

# number of HPC nodes
NN=2

# first iteration number (if more than 1, means restart i-1 are already in place)
# end iteration
STARTITER=1
ENDITER=2

# restart path and rst file (assume all in $IELMER)
RSTINITpath=${IELMER}/RST_simplified2/NEWMESH_48part/
RSTINITfile=NEWMESH.result

# MSH path and file
MSHINITpath=${IELMER}/MSH_simplified2/
