#!/bin/sh
####SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=5
#SBATCH --nodes=1
####SBATCH --nodelist=atlas10
#SBATCH --account=snap
#SBATCH -p main

#### $1 should be passed as i=10 or i=11 for CRU, i=1 through i=9 for GCM, required;
#### $2 spatial domain for PRISM passed as domain=\'akcan2km\' or as domain=\'ak771m\', required;
Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Downscaling/code/ds_prism.R $1 $2
