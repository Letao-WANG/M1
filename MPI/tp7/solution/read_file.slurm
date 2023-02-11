#!/bin/bash
#SBATCH --job-name=read_file
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --time=00:01:00
#SBATCH --output=read_file-%j.out
#SBATCH --error=read_file-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
set -x
 
srun ./read_file
