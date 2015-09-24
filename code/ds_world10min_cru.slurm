#!/bin/sh
####SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=12
#SBATCH --nodes=1
####SBATCH --nodelist=atlas10
#SBATCH --account=snap
#SBATCH -p main

#### $1 should be passed as i=10, 11 or 12 for CRU, recommended optional $2 as month.index=1:12 to parallelize by months
Rscript /workspace/UA/mfleonawicz/projects/Downscaling/code/ds_world10min.R $1 $2
