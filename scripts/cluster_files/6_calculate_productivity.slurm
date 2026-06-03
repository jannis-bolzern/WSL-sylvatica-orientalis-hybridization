#!/bin/bash

#SBATCH --job-name=6_calc
#SBATCH --output=6_calc.out
#SBATCH --error=6_calc.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=camilla.stefanini@wsl.ch

#SBATCH --partition=bigmem
#SBATCH --qos=normal
#SBATCH --time=30-00:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=150000


module load r-4.3.0-gcc-10.2.0-abccnaw
SCRIPT="/home/stefanin/nemo/nemo_files/nemoage0.32.6b/6_calculate_productivity.R"
R --vanilla -f $SCRIPT
