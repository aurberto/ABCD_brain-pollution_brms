#!/bin/bash
#SBATCH --job-name=R_brms
#SBATCH --account=project_ID
#SBATCH --partition=small
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=08:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=user@email.com

# --- set R custom in PATH ---
export R_ROOT=/scratch/project_ID/softwares/R-4.4.0/R-4.4.0
export PATH="$R_ROOT/bin:$PATH"

# --- set user library ---
export R_LIBS_USER=/scratch/project_ID/user_ID/Rlibs/4.4.0

# --- Scratch tmp dir ---
export TMPDIR=/scratch/project_ID/user_ID/ABCD-brms_pollution/tmp
mkdir -p $TMPDIR

# --- threading for brms ---
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export OPENBLAS_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK

# --- run analysis ---
# PM2.5
R CMD BATCH --no-save --no-restore \
  /scratch/project_ID/user_ID/path/to/your_R_script.R \
  EXEC/R_${SLURM_JOB_ID}.Rout