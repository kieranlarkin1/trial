#!/bin/bash -l

#SBATCH -A snic2020-5-178
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 00:05:00
#SBATCH -J trial

make clean
make

./trial
