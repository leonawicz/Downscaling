##########################################################################################################################################################################
#### This R script creates monthly anomalies for a given range of years from ar4/ar5 gcm or CRU 3.0/3.1/3.1.01/3.22 geotiffs, with optional resample to another grid. ####
##########################################################################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   09/24/2015        ####

# @knitr anom
library(raster)
library(parallel)

# Source functions
path.fun <- file.path("/workspace/UA/mfleonawicz/projects/Downscaling/code")
source(file.path(path.fun,"anomPrep.R"))
source(file.path(path.fun,"anomCalc.R"))
source(file.path(path.fun,"anomParSubFunc.R"))
anom.script.file <- file.path(path.fun,"anomCalcAsScript.R") # source below

#### Choose datasets (current options: "cru322" "cru3101" "cru31" "cru30" "AR4gcm" "AR5gcm") ####
dataset <- "cru322" 

#### Set year ranges for reference climatology period and anomalies ####
yrs.clim <- c(1961,1990)
yrs.anom <- c(1901,2013)

#### Top level climatology and anomaly directories ####
dir.create(newDir <- paste("/Data/Base_Data/Climate/World/Anomalies/",yrs.clim[1],"_",yrs.clim[2],"_Base_Climatology",sep=""),showWarnings=F,recur=T)
climDir1 <- paste("/Data/Base_Data/Climate/World/Climatologies/",yrs.clim[1],"_",yrs.clim[2],sep="")

#### Set to TRUE is resampling outputs to a new grid and specify any raster layer as the new grid ####
resample <- F; if(resample) r <- raster(file.path("/Data/Base_Data/Climate/World/ERA40/ERA40_historical_monthly_195709-200208.nc"),varname="t2m",layer=1)

#### Prepare workspace. Specify senario name (options above) only if using a projected climatology period ####
dp <- anomPrep(dataset,yrs.anom,yrs.clim,scenario=NULL) 
for(i in 1:length(dp)) assign(names(dp)[i], dp[[i]])

#### Loop serially over variables, parallel processing anomalies by models/datasets. Ideal for a collection of GCMs of similar file size. ####
for(k in 1:length(mainDir))	mclapply(1:length(files[[k]]), anomCalc, yr1=yrs.anom[1], yr2=yrs.anom[2] ,var=varid[k], scen=scen[k], datID=datID, arID=arID, pq=c(0.01,0.99), tq=c(0.01,0.99), setNAs=0.5, interpNAs=T, pcdf=0, pr0maps=T)

#### Use below only for faster parallel processing by month of a single model/dataset at a time. Ideal for CRU or a collection of GCMs one at a time which differ substantially in file size. ####
yr1=yrs.anom[1]; yr2=yrs.anom[2]; scens <- scen; pq=c(0.01,0.99); tq=c(0.01,0.99); setNAs=0.5; interpNAs=T; pcdf=0; pr0maps=T; parallelizeMonths <- T
for(k in 1:length(mainDir)){
	var=varid[k]
	scen=scens[k]
	for(i in 1:length(files[[k]])) {
		source(anom.script.file)
		print(paste("Completed model ",i," of ",length(files[[k]])," for variable-scenario combination ",k," of ",length(mainDir),sep=""),quote=F)
		gc()
	}
}
