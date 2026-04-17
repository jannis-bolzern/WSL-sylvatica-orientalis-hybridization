#!/bin/bash

#SBATCH --job-name=5a_build
#SBATCH --output=5a_build.out
#SBATCH --error=5a_build.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=camilla.stefanini@wsl.ch

#SBATCH --partition=bigmem
#SBATCH --qos=normal
#SBATCH --time=30-00:00:00
#SBATCH --ntasks=1
#SBATCH --mem=100G


module load r-4.3.0-gcc-10.2.0-abccnaw
SCRIPT="/home/stefanin/nemo/nemo_files/nemoage0.32.6b/5a_build_quanti_dataset.R"
R --vanilla -f $SCRIPT
