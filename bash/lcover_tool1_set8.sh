#!/bin/bash
#SBATCH --account=def-mfortin   # replace this with your own account
#SBATCH --mem-per-cpu=100G      # memory; default unit is megabytes
#SBATCH --time=0-18:00           # time (DD-HH:MM)
#SBATCH --cpus-per-task=1
#SBATCH --job-name=lcover1_set8
#SBATCH --output=%x-%j.out
#SBATCH --mail-user=peter.rodriguez@mail.utoronto.ca
#SBATCH --mail-type=ALL

module load  StdEnv/2023  gcc/12.3  udunits/2.2.28  hdf/4.2.16  gdal/3.7.2  r/4.3.1 # adjust package versions

Rscript /home/georod/projects/def-mfortin/georod/scripts/github/amazon-mortality/scripts/lcover_metrics_v8_lyr_61_v2.R

