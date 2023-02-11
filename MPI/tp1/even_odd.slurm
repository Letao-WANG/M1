#!/bin/bash
#SBATCH --job-name=even_odd
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --time=00:01:00
#SBATCH --output=even_odd-%j.out
#SBATCH --error=even_odd-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
 
set -x
 
srun ./even_odd
