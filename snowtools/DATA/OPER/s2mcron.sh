#!/usr/bin/bash
reseau=$1
task=$2
datepivot=`date '+%Y%m%d'`$reseau
. ~/.bash_profile
export SNOWTOOLS_CEN=$HOME/PycharmProjects/snowtools_git
export WORKDIR=/scratch/work/`whoami`
module load python/3.7.6nomkl

for domain in alp pyr cor vog jur mac postes
do
python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py oper -b $datepivot -r $domain --task=$task --dev > cron_${domain}_${task}${reseau}.out 2>&1
done

