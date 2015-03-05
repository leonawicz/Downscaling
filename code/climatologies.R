#########################################################################################################################################################################################
#### This R script creates monthly climatologies for a given range of years from ar4/ar5 gcm tif files and CRU 3.0/3.1/3.1.01/3.22 nc files, with optional resample to another grid. ####
#########################################################################################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/05/2015        ####

# @knitr clim
library(raster)
library(parallel)

# Source functions
path.fun <- file.path("/workspace/UA/mfleonawicz/leonawicz/projects/Downscaling/code")
source(file.path(path.fun,"climPrep.R"))
source(file.path(path.fun,"climCalc.R"))

#### Choose datasets (current options: "cru30" "cru31" "cru3101" "cru322" "ERA40" "AR4gcm" "AR5gcm") #### Note cru3101 will still place precipitation outputs into in a "CRU31" folder.
dataset <- "cru322"

#### Set year ranges for reference climatology period ####
yrs.clim <- c(1961, 1990)

#### Top level climatology directory ####
dir.create(newDir <- file.path("/Data/Base_Data/Climate/World/Climatologies"), showWarnings=F)

#### Set to TRUE is resampling outputs to a new grid and specify any raster layer as the new grid ####
resample <- F; if(resample) r <- raster(file.path(drive[3],"/Base_Data/Climate/World/ERA40/ERA40_historical_monthly_195709-200208.nc"),varname="t2m",layer=1)

dp <- climPrep(dataset, yrs.clim, include.psl=F) # add include.psl=T if psl is to be included. Only available for ERA40 and historical GCMs.
for(i in 1:length(dp)) assign(names(dp)[i], dp[[i]])
for(k in 1:length(mainDir))	mclapply(1:length(files[[k]]), climCalc, yr1=yrs.clim[1], yr2=yrs.clim[2], var=varid[k], scen=scen[k], datID=datID, arID=arID, pq=c(0.01,0.99), tq=c(0.01,0.99), setNAs=0.5, interpNAs=T, mc.cores=length(files[[k]]))
