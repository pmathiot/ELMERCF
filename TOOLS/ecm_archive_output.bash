#!/bin/bash

nerr=0

if [ $# == 0 ]; then echo "ecm_archive_output [CONFIG] [CASE] [lst of segment numbers]"; fi

CONFIG=$1
CASE=$2
SEGLST=${@:3}

cd $EHDIR/${CONFIG}/SIMU/${CONFIG}-${CASE}/ || exit 42

############# directory setting #####################
if [ ! -d $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S ]; then mkdir -p $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S || nerr=$((nerr+1)); fi
if [ ! -d $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R ]; then mkdir -p $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R || nerr=$((nerr+1)); fi

if [ $nerr != 0 ]; then
   echo ""
   echo " E R R O R in setting archive architecture : $nerr " 
   echo ""
   exit 42
fi

############# check old tar     #####################
echo ""
for SEG in $SEGLST; do

  # check if segment already in the archive
   if [ -f $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S/${CONFIG}-${CASE}_${SEG}.tar ] ; then 
      echo "   Output  tar segment $SEG of ${CONFIG}-${CASE} already in $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S/ : E R R O R" 
      nerr=$((nerr+1))
   fi

  # check if segment already in the archive
   if [ -f $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R/${CONFIG}-${CASE}_${SEG}.tar ] ; then 
      echo "   Restart tar segment $SEG of ${CONFIG}-${CASE} already in $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R/ : E R R O R" 
      nerr=$((nerr+1))
   fi
done

if [ $nerr != 0 ]; then
   echo ""
   echo " E R R O R : $nerr " 
   echo ""
   exit 42
fi

############# tar data          #####################
for SEG in $SEGLST; do
   nsuberr=0 

   # if not present in the archive tar data
   echo ""
   echo "Tar segment $SEG of ${CONFIG}-${CASE} ..."
 
   cd $EDDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S/
   NFILE=`ls *${CONFIG}-${CASE}_$SEG* 2> /dev/null | wc -l`
   echo "    tar output ($NFILE files) ..."
   if [[ $NFILE > 0 ]]; then
      tar -cf $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S/${CONFIG}-${CASE}_${SEG}_output.tar *${CONFIG}-${CASE}_$SEG* || nsuberr=$((nsuberr+1))
   else
      echo "      Ouput   segment $SEG does not seems to exist !!!!"
      nsuberr=$((nsuberr+1))
   fi

   cd $EDDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R/
   NFILE=`ls *${CONFIG}-${CASE}_$SEG* 2> /dev/null | wc -l`
   echo "    tar restart ($NFILE files) ..."
   if [[ $NFILE > 0 ]]; then
      tar -cf $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R/${CONFIG}-${CASE}_${SEG}_restart.tar *${CONFIG}-${CASE}_$SEG* || nsuberr=$((nsuberr+1))
   else
      echo "      Restart segment $SEG does not seems to exist !!!!"
      nsuberr=$((nsuberr+1))
   fi

   if [[ $nsuberr == 0 ]]; then
      echo ""
      echo " Segment $SEG : SUCCESSFUL "
      echo ""
   else
      echo ""
      echo " Segment $SEG : E R R O R " 
      if [ -f $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R/${CONFIG}-${CASE}_${SEG}_restart.tar ]; then
         echo "    rm $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_R/${CONFIG}-${CASE}_${SEG}_restart.tar by hand before retrying"
      fi
      if [ -f $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S/${CONFIG}-${CASE}_${SEG}_output.tar  ]; then
         echo "    rm $ESDIR/${CONFIG}/${CONFIG}-${CASE}/${CONFIG}-${CASE}_S/${CONFIG}-${CASE}_${SEG}_output.tar  by hand before retrying"
      fi
      echo ""
   fi
   # 
   nerr=$((nerr+nsuberr))
done

############# end script        #####################

if [[ $nerr == 0 ]]; then
   echo ""
   echo " SUCCESSFUL "
   echo ""
else
   echo ""
   echo " E R R O R : $nerr " 
   echo ""
   exit 42
fi
