#!/bin/bash

#SBATCH --job-name=3_create_sim_files
#SBATCH --output=3_create_sim_files.out
#SBATCH --error=3_create_sim_files.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=camilla.stefanini@wsl.ch

#SBATCH --qos=normal
#SBATCH --time=2:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16000


module load r-4.3.0-gcc-10.2.0-abccnaw
SCRIPT="/home/stefanin/nemo/nemo_files/nemoage0.32.6b/3_create_sim_files.R"
R --vanilla -f $SCRIPT
