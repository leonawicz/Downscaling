



##
##
## ds_prism.R

This script downscales monthly climate anomalies geotiffs to either of two sets of monthly PRISM climatologies,
the 2-km resolution 1961-1990 climatologies or the 771-m 1971-2000 climatologies.

Unlike the `climatologies.R` and `amomalies.R` which create their respective geotiffs for any of a number of different data sets,
this downscaling script strictly performs downscaling to these PRISM climatologies over their respective spatial extents at the respective resolutions.

Although the PRISM downscaling code below includes a `method` argument, which bifurcates the processing function into two methods of downscaling,
only one method is functional when attempting to downscale such a large amount of data to such a fine spatial resolution at once using parallelized code.

Using `method='gdal'` will tend to throw segmentation fault errors related to GDAL dataset copy failures on the Atlas cluster.
As a result, the tried and true `method='akima'` must be used.
The former approach is successfully used in the 10-minute resolution CRU 2.0 climatology downscaling, where GDAL does not choke on the smaller amount of data.
The upside to this method, although slower even when functional, is that it allows for more familiar-looking code,
in the sense that more people working with spatial data are familiar with the `raster` package, which handles the transformations, as opposed to the `akima` package.

At this scale, however, `method='akima'` must be used, unless one wants to wait forever for serial processing of so many files using the `raster` package.
Therefore, the methods used in this version of downscaling are not the same as those employed when downscaling some coarser climatologies.

`ds_prism.R` is called with command line arguments by one of two SLURM scripts, `ds_prism_cru.slurm` or `ds_prism_gcm.slurm`.

### R code

#### Setup


```r
comargs <- (commandArgs(TRUE))
if (!length(comargs)) q("no") else for (z in 1:length(comargs)) eval(parse(text = comargs[[z]]))

if (!exists("i")) stop("Index variable 'i' not passed at command line.")
if (!exists("domain")) stop("Spatial domain variable 'domain' not passed at command line.")
if (!(domain %in% c("akcan2km", "ak771m"))) stop("Improper spatial domain specified.")

library(raster)
library(parallel)

# Part of GDAL test rasterOptions(maxmemory=1e+10, chunksize=2e+08) # 12
# simultaneous processes on one Atlas node
# rasterOptions(tmpdir='/big_scratch/mfleonawicz/tmp/')
# print(rasterOptions())

if (domain == "akcan2km") climDir <- paste0("/Data/Base_Data/Climate/AK_CAN_2km/historical/singleBand/prism/AK_CAN_2km_PRISM/AK_CAN_geotiffs/", 
    c("pr", "tas"), "/ak83albers")
if (domain == "ak771m") climDir <- paste0("/Data/Base_Data/Climate/AK_800m/historical/singleBand/prism/AK_800m_PRISM/geotiffs/", 
    c("pr", "tas"))
b.clim.p <- readAll(stack(list.files(climDir[1], full = TRUE, pattern = ".tif$")))
b.clim.t <- readAll(stack(list.files(climDir[2], full = TRUE, pattern = ".tif$")))

anomDir <- list.files("/Data/Base_Data/Climate/World/Anomalies/1961_1990_Base_Climatology", 
    full = T)
allowed.models <- c("cccma-cgcm3-1-t47", "gfdl-cm2-1", "miroc3-2-medres", "mpi-echam5", 
    "ukmo-hadcm3", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", 
    "CRU_TS30", "CRU_TS31", "CRU_TS32")

outDir <- if (domain == "akcan2km") "/Data/Base_Data/Climate/AK_CAN_2km" else "/Data/Base_Data/Climate/AK_800m"

if (i > length(anomDir)) stop("Index variable 'i' exceeds maximum value.")

mos <- c(paste0(0, 1:9), 10:12)
meta <- unlist(strsplit(basename(anomDir[i]), "_"))
grp <- meta[1]
grp2 <- if (grp == "AR4") "AR4_CMIP3_models" else if (grp == "AR5") "AR5_CMIP5_models" else grp
scenario <- meta[4]
period.scenario <- if (scenario == "historical") c(scenario, "") else c("projected", 
    scenario)
# period.scenario <- if(scenario=='historical') c(file.path(scenario,
# 'singleBand'), '') else c('projected', scenario)
models <- list.files(anomDir[i])
models <- models[models %in% allowed.models]
```

#### Processing function



#### Run downscaling


```r
# Run
if (exists("month.index") && all(1:12 %in% month.index)) {
    mclapply(month.index, f, i = i, par.by.month = TRUE, anomDir = anomDir, 
        outDir = outDir, b.clim.t = b.clim.t, b.clim.p = b.clim.p, method = "akima", 
        mc.cores = length(month.index))
} else mclapply(1:length(models), f, i = i, anomDir = anomDir, outDir = outDir, 
    b.clim.t = b.clim.t, b.clim.p = b.clim.p, method = "akima", mc.cores = min(length(models), 
        32))
```
