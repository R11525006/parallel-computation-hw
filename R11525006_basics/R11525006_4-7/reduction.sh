#!/bin/bash 
#PBS -P ACD111143 
#PBS -N reduction 
#PBS -q ctest 
#PBS -l select=1:ncpus=1:mpiprocs=4 
#PBS -l place=scatter 
#PBS -l walltime=00:02:00 
#PBS -j n 
module purge 
module load intel/2018_u1 
cd $PBS_O_WORKDIR 
echo $PBS_O_WORKDIR 

date
mpirun ./reduction
