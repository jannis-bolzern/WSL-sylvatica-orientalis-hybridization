#!/bin/bash

#SBATCH --job-name=5c_build
#SBATCH --output=5c_build.out
#SBATCH --error=5c_build.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=camilla.stefanini@wsl.ch

#SBATCH --partition=bigmem
#SBATCH --qos=normal
#SBATCH --time=30-00:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=86000


module load r-4.3.0-gcc-10.2.0-abccnaw
SCRIPT="/home/stefanin/nemo/nemo_files/nemoage0.32.6b/5c_build_fit_dataset.R"
R --vanilla -f $SCRIPT
