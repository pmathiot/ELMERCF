#!/bin/bash

# here is defined irene slurm header

######################
## IRENE   TGCC/CEA ##
######################

#MSUB -r <NAME>
#MSUB -o LOG/<NAME>.e%j
#MSUB -e LOG/<NAME>.o%j
#MSUB -n <NTASKS>
#MSUB -x
#MSUB -T 1200
#MSUB -A gen6035
#MSUB -q rome
#MSUB -m store,work,scratch
#MSUB -E "--parsable"

# here are define option specific to irene
. ~/.bashrc

# below we instert the generic script to run elmer based on run_elmer_skel.bash
# done in prepare_elmer.bash
