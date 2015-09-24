#!/bin/sh
####SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=12
#SBATCH --nodes=1
####SBATCH --nodelist=atlas03
#SBATCH --account=snap
#SBATCH -p main

#### $1 should be passed as i=10, 11 or 12 for CRU, required;
#### $2 spatial domain for PRISM passed as domain=\'akcan2km\' or as domain=\'ak771m\', required;
#### recommended optional $3 as month.index=1:12 to parallelize by months
Rscript /workspace/UA/mfleonawicz/projects/Downscaling/code/ds_prism.R $1 $2 $3
