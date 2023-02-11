#!/bin/bash
#SBATCH --job-name=commsplit
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --time=00:01:00
#SBATCH --output=commsplit-%j.out
#SBATCH --error=commsplit-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
set -x
 
srun ./commsplit
