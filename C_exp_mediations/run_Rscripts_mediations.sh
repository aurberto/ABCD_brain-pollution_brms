#!/bin/bash
#SBATCH --job-name=Rbrms_med_array
#SBATCH --account=project_ID
#SBATCH --partition=small
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=04:00:00
#SBATCH --array=1-130
#SBATCH --output=logs/med_%A_%a.out
#SBATCH --error=logs/med_%A_%a.err
#SBATCH --mail-type=END
#SBATCH --mail-user=user@mail.com

# Run on terminal like:
# JOBID=$(sbatch run_Rscripts_mediations.sh | awk '{print $4}')
# sbatch --dependency=afterok:$JOBID run_Rscripts_merge.sh

export R_ROOT=/scratch/project_ID/softwares/R-4.4.0/R-4.4.0
export PATH="$R_ROOT/bin:$PATH"

export R_LIBS_USER=/scratch/project_ID/user_ID/Rlibs/4.4.0

export TMPDIR=/scratch/project_ID/user_ID/ABCD-brms_pollution/tmp
mkdir -p $TMPDIR

# avoid oversubscription
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

mkdir -p logs
mkdir -p EXEC

R CMD BATCH --no-save --no-restore \
  /scratch/project_ID/user_ID/path/to/code/script_R_mediation.R \
  EXEC/R_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.Rout
