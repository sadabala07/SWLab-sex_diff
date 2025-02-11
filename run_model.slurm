#!/bin/bash

#SBATCH -J sex_diff-inital-run    # Job name
#SBATCH -p general                # Partition
#SBATCH -o output_%j.txt          # Output file (%j is job ID)
#SBATCH -e error_%j.err           # Error file
#SBATCH --mail-type=ALL           # Notifications (ALL, BEGIN, END, FAIL)
#SBATCH --mail-user=sadabala@iu.edu # Your email for notifications
#SBATCH --nodes=1                 # Number of nodes
#SBATCH --ntasks-per-node=1       # Tasks per node
#SBATCH --time=02:00:00           # Time limit (1 hour)
#SBATCH --mem=8G                  # Memory request
#SBATCH -A r01010                 # Slurm account name (replace with yours)

module load R/4.4.1         

Rscript run_model.R
