#!/bin/bash
#$ -S /bin/bash
#$ -q all.q
#$ -N BatchJobs
#$ -l h_vmem=1G
#$ -cwd
#$ -j Y
#$ -V
#$ -m be

R CMD BATCH summarize.R








