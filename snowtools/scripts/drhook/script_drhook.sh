#!/usr/bin/sh
# If you run this script in a directory with drhook outputs from a SURFEX simulation, 
# it will summarize the results among MPI threads and sort the routines by decreasing numerical cost.
# This script was used to produce the table of numerical costs in Crocus GMD paper.
# Author : Matthieu Lafaysse, August 2025.

for diag in `ls drhookprof.*.n????`
do
# Remove header
tail -n +14 $diag > $diag.noheader
done

awk '{print $9}' drhookprof.surfex-surfex_vortex_task.n0001.noheader > list_routines

for routine in `cat list_routines`
do
echo $routine
rm -f $routine
for diag in `ls drhookprof.*.noheader`
do
grep $routine $diag >> $routine
done
done

rm -f TOTAL
for routine in `cat list_routines`
do
awk '{CUMUL += $5} END {print CUMUL}' $routine > TOTAL_$routine
done

for total in `ls TOTAL_*`
do
value=`cat $total`
echo $value $total >> TOTAL
done

sort -n -r TOTAL >> SORT_TOTAL
