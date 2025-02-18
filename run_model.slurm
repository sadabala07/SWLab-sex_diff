#!/bin/bash

#SBATCH --job-name=SexDiffModel     # Job name
#SBATCH --output=/N/u/sadabala/BigRed200/SWLab-sex_diff/Logs/SexDiffModel_%j.out  # Path for the standard output file
#SBATCH --error=/N/u/sadabala/BigRed200/SWLab-sex_diff/Logs/SexDiffModel_%j.err   # Path for the error file
#SBATCH --mail-type=ALL               # Email notification for all states
#SBATCH --mail-user=sadabala@iu.edu     # Email address for notifications
#SBATCH -A r01010
#SBATCH -p general                 # Partition: General compute nodes
#SBATCH --time=02:00:00           # Job run time (1 hour)
#SBATCH --mem=8G                   # Total memory per node (8 GB)
#SBATCH --nodes=2
#SBATCH --cpus-per-task=4
#SBATCH --ntasks-per-node=24

module load r/4.2.1

job_id=26

# Run the R script
Rscript /N/u/sadabala/BigRed200/SWLab-sex_diff/run_model.R "$job_id"
