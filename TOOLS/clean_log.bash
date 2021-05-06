#!/bin/bash

if [ ! -d LOG ] ; then
    echo 'LOG directory is missing; please run it in a run directory (~/ELMER/CONFIG/CONFIG-CASE)'
    exit 42
else
    rm -f LOG/*_elmer_t[0-9]*.[e-o][0-9]*[0-9]
fi

