#!/bin/bash
#SBATCH --job-name=merge_med
#SBATCH --account=project_ID
#SBATCH --partition=small
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=00:10:00
#SBATCH --output=logs/merge_%j.out
#SBATCH --error=logs/merge_%j.err

export R_ROOT=/scratch/project_ID/softwares/R-4.4.0/R-4.4.0
export PATH="$R_ROOT/bin:$PATH"

export R_LIBS_USER=/scratch/project_ID/user_ID/Rlibs/4.4.0

Rscript /scratch/project_ID/user_ID/path/to/code/merge_mediation_results.R