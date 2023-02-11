#!/bin/bash
#SBATCH --job-name=pi
#SBATCH --nodes=1
#SBATCH --ntasks=6
#SBATCH --time=00:01:00
#SBATCH --output=pi-%j.out
#SBATCH --error=pi-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
set -x
 
srun ./pi
