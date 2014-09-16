#!/bin/bash --login
# This is an example batch script for running with SLURM on a Cray XC30

# Generic SLURM options
#SBATCH --job-name=vh1
#SBATCH --ntasks=24
#SBATCH --ntasks-per-node=24
#SBATCH --time=0:05:00
#SBATCH --partition=debugq

#SBATCH --export=NONE

export OMP_NUM_THREADS=1

EXE=vh1-mpi-cray+pat
#EXE=vh1-mpi-cray
# Turn off the stop message from the Cray compiler
export NO_STOP_MESSAGE=1

# Change to the same directory that sbatch was run in
#ROOTDIR=${SLURM_SUBMIT_DIR}
ROOTDIR=`pwd`
echo ${ROOTDIR}
RUNDIR=${ROOTDIR}
BINDIR=${ROOTDIR}/../bin

# Set the number of ranks to use
NPROC=24
NTASK=24

# Create the output directory
mkdir -p ${RUNDIR}/output
cd ${RUNDIR}
#cp ${ROOTDIR}/indat ${RUNDIR}
cp ${BINDIR}/${EXE} ${RUNDIR}

# Launch the application in parallel with NPROCS ranks 
aprun -n ${NPROC} -N ${NTASK} ./${EXE}

# Cat the output file to screen
if [[ -e output/NCState.hst ]]; then
  cat output/NCState.hst
fi
