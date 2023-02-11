#!/bin/bash
#SBATCH --job-name=ping_pong_1
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --time=00:01:00
#SBATCH --output=ping_pong_1-%j.out
#SBATCH --error=ping_pong_1-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
 
set -x
 
srun ./ping_pong_1
