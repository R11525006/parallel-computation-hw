#!/bin/bash
#PBS -P ACD111143
#PBS -N 5.11
#PBS -q ctest
#PBS -l select=1:ncpus=2:mpiprocs=4
#PBS -l place=scatter
#PBS -l walltime=00:02:00
#PBS -j n
module purge
module load mpi/openmpi-3.0.0/gcc485
cd $PBS_O_WORKDIR
echo $PBS_O_WORKDIR

date
mpirun ./5.11