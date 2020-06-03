#!/bin/bash -l
#$ -l memory=0.1G
#$ -l h_rt=00:05:00
#$ -N trial
#$ -pe smp 1
#$ -cwd

make

./trial
