#!/bin/bash

# Job name and who to send updates to
#SBATCH --job-name=<plantsads>
#SBATCH --mail-user=<diaz.renata@ufl.edu>
#SBATCH --mail-type=ALL

# Where to put the outputs: %j expands into the job number (a unique identifier for this job)
#SBATCH --output plantsads%j.out
#SBATCH --error plantsads%j.err

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=2gb  # Per processor memory
#SBATCH --cpus-per-task=16
#SBATCH --time=48:00:00   # Walltime

#Record the time and compute node the job ran on
date;hostname; pwd

#Use modules to load the environment for R
module load R

Rscript analysis/report/portal_plants_notebook_justmete.R
