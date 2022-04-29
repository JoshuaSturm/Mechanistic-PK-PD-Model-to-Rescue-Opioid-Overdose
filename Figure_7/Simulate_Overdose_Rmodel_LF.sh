#!/bin/sh
#$ -cwd
#$ -pe thread 1
#$ -j y
#$ -P CDERID0047
#$ -N Simulate
#$ -l s_rt=24:00:00
#$ -R y
#$ -l h_vmem=2G
#$ -l h_rt=24:00:00
#$ -t 1-4002
#$ -o NULL

FORMULATION=(Generic)
NFormulation=${#FORMULATION[@]}
IDX=$(((SGE_TASK_ID-1)/NFormulation))
PATIENTIDX=$((IDX+1))
IDX2=$((SGE_TASK_ID-1-IDX*NFormulation))
ANTAGONIST=${FORMULATION[IDX2]}




#Rscript simulateToGetOD_IM.R -i "$PATIENTIDX" -p "chronic" -m "yes" -q "yes" -c "1.625">& logfiles/"$JOB_NAME".o4"$JOB_ID"."$SGE_TASK_ID".txt
Rscript simulateToGetOD_IM.R -a "$ANTAGONIST" -i "$PATIENTIDX" -p "chronic" -m "yes" -q "yes" -c "1.625" #>& logfiles/"$JOB_NAME".o1"$JOB_ID"."$SGE_TASK_ID".txt
