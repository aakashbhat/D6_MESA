#!/bin/bash
#SBATCH --job-name try
#SBATCH --time 08:00:00
#SBATCH --output test-job_%A_%a.out
#SBATCH --error test-job_%A_%a.err
#SBATCH --partition=remeis
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=3
#SBATCH --ntasks-per-core=1	
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aakashbhat7@gmail.com          
#SBATCH --export=all

export OMP_NUM_THREADS=3
srun -l ./rn
