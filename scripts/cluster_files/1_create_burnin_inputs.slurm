#!/bin/bash

#SBATCH --job-name=1_create_burnin_inputs
#SBATCH --output=1_create_burnin_inputs.out
#SBATCH --error=1_create_burnin_inputs.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=camilla.stefanini@wsl.ch

#SBATCH --qos=normal
#SBATCH --time=2:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16000


module load r-4.3.0-gcc-10.2.0-abccnaw
SCRIPT="/home/stefanin/nemo/nemo_files/nemoage0.32.6b/1_create_burnin_inputs.R"
R --vanilla -f $SCRIPT
