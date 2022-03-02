#!/bin/bash

if [ $# -ne 1 ]; then echo "usage: setup_arch.bash [COMPUTER (irene)]"; exit 42; fi

PARAM_FILE=../ARCH/param_arch_${1}.bash
if [ -f $PARAM_FILE ]; then 
   ln -sf ../$PARAM_FILE REF/param_arch.bash; 
else
   echo "E R R O R: computer $1 unknown" 
   exit 42
fi

RUN_FILE=../ARCH/run_arch_${1}.slurm
if [ -f $RUN_FILE ]; then 
   ln -sf ../$RUN_FILE REF/run_arch.slurm; 
else
   echo "E R R O R: computer $1 unknown" 
   exit 42
fi
