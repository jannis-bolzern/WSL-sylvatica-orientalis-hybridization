#!/bin/bash

#SBATCH --job-name=4b_run_baseline
#SBATCH --output=4b_run_baseline.out
#SBATCH --error=4b_run_baseline.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=camilla.stefanini@wsl.ch

#SBATCH --qos=normal
#SBATCH --time=100:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16000


export LD_LIBRARY_PATH=/home/stefanin/miniforge3/lib:$LD_LIBRARY_PATH
source /home/stefanin/miniforge3/bin/activate nemo_environment

## print libraries required
ldd /home/stefanin/nemo/nemo_files/nemoage0.32.6b/nemoage

## run nemo ini file
ls -l /home/stefanin/nemo/nemo_files/nemoage0.32.6b/input_files/disperse/Pollen_connectivity_matrix_d30.txt

cd /home/stefanin/nemo/nemo_files/nemoage0.32.6b
/home/stefanin/nemo/nemo_files/nemoage0.32.6b/nemoage ini_files/no_introduction_p0_r01_sel_E_high.ini
/home/stefanin/nemo/nemo_files/nemoage0.32.6b/nemoage ini_files/no_introduction_p0_r01_sel_E_mid.ini
/home/stefanin/nemo/nemo_files/nemoage0.32.6b/nemoage ini_files/no_introduction_p0_r01_sel_E_low.ini