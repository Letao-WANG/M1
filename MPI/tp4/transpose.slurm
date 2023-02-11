#!/bin/bash
#SBATCH --job-name=transpose
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --time=00:01:00
#SBATCH --output=transpose-%j.out
#SBATCH --error=transpose-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
set -x
 
srun ./transpose
