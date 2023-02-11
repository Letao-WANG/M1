#!/bin/bash
#SBATCH --job-name=matrix_products
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --time=00:01:00
#SBATCH --output=matrix_products-%j.out
#SBATCH --error=matrix_products-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
set -x
 
srun ./matrix_products
