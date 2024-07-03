#!/bin/sh
#SBATCH --account=def-cseiler-ab 
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=10G
#SBATCH --time=00:15:00
#SBATCH --job-name=DAISYtest
#SBATCH --output=/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/daisy.out
#SBATCH --mail-user=christian.seiler@queensu.ca
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

module load  StdEnv/2023  gcc/12.3  udunits/2.2.28  hdf/4.2.16  gdal/3.7.2  r/4.3.1
export R_LIBS=/home/cseiler/daisy/renv
Rscript run_daisy.R

# To run this script:
# sbatch submit_daisy.sh
