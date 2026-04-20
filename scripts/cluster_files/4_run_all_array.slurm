#!/bin/bash

#SBATCH --job-name=4_run_all
#SBATCH --output=4_run_all_%A_%a.out
#SBATCH --error=4_run_all_%A_%a.err
#SBATCH --mail-type=ALL

#SBATCH --partition=bigmem
#SBATCH --qos=normal
#SBATCH --time=30-00:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=32000

#SBATCH --array=0-600%25  

export LD_LIBRARY_PATH=/home/stefanin/miniforge3/lib:$LD_LIBRARY_PATH
source /home/stefanin/miniforge3/bin/activate nemo_environment

NEMO="/home/stefanin/nemo/nemo_files/nemoage0.32.6b/nemoage"

# automatically collect ini files 
INI_FILES=(/home/stefanin/nemo/nemo_files/nemoage0.32.6b/ini_files/run/*.ini)

INI=${INI_FILES[$SLURM_ARRAY_TASK_ID]}

echo "Running task $SLURM_ARRAY_TASK_ID with $INI"

$NEMO "$INI" 