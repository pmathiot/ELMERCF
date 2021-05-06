#!/bin/bash

# number of elmer partition
NP=24

# first iteration number (if more than 1, means restart i-1 are already in place)
# end iteration
STARTITER=1
ENDITER=1

# restart path and rst file (assume all in $IELMER)
RSTINITpath=${IELMER}/RST/RELAX_R4_24part/
RSTINITfile=RELAX_R4.result
