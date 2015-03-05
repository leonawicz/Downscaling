



##
##
## ds_world10min.R

This script downscales monthly climate anomalies geotiffs to CRU 2.0 10-minute resolution monthly climatologies, which cover the world grid less some extreme latitudes.
The monthly climatologies used are technically derived climatologies which incorporate some methods that help make downscaling more robust to extreme outliers and downscaling artifacts.
For this reason they are not identical to the original CRU 2.0 10-minute monthly climatologies.

Unlike the `climatologies.R` and `amomalies.R` which create their respective geotiffs for any of a number of different data sets,
this downscaling script strictly performs downscaling to 10-minute resolution using CRU climatologies.
Furthermore, though similar, the methods used in this version of downscaling are not the same as those employed when downscaling to the higher-resolution PRISM climatologies for example.

`ds_world10min.R` is called with command line arguments by one of two SLURM scripts, `ds_world10min_cru.slurm` or `ds_world10min_gcm.slurm`.

### R code

#### Setup


```r
comargs <- (commandArgs(TRUE))
if (!length(comargs)) q("no") else for (z in 1:length(comargs)) eval(parse(text = comargs[[z]]))

if (!exists("i")) stop("Index variable 'i' not passed at command line.")

library(raster)
library(parallel)

climDir <- "/Data/Base_Data/Climate/World/Climatologies/1961_1990/CRU_CL20_climatologies_historical_1961_1990"
b.clim.p <- readAll(brick(file.path(climDir, "pr_CRU_CL20_historical_climatology_01_12_1961_1990.tif")))
b.clim.t <- readAll(brick(file.path(climDir, "tas_CRU_CL20_historical_climatology_01_12_1961_1990.tif")))
projection(b.clim.t) <- projection(b.clim.p) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

anomDir <- list.files("/Data/Base_Data/Climate/World/Anomalies/1961_1990_Base_Climatology", 
    full = T)
allowed.models <- c("cccma-cgcm3-1-t47", "gfdl-cm2-1", "miroc3-2-medres", "mpi-echam5", 
    "ukmo-hadcm3", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3", 
    "CRU_TS30", "CRU_TS31", "CRU_TS32")

outDir <- "/Data/Base_Data/Climate/World/World_10min"

if (i > length(anomDir)) stop("Index variable 'i' exceeds maximum value.")

mos <- c(paste0(0, 1:9), 10:12)
meta <- unlist(strsplit(basename(anomDir[i]), "_"))
grp <- meta[1]
grp2 <- if (grp == "AR4") "AR4_CMIP3_models" else if (grp == "AR5") "AR5_CMIP5_models" else grp
scenario <- meta[4]
period.scenario <- if (scenario == "historical") c(scenario, "") else c("projected", 
    scenario)
models <- list.files(anomDir[i])
models <- models[models %in% allowed.models]
```

#### Processing function


```r
# Processing function
f <- function(j, i, par.by.month = FALSE, anomDir, outDir, b.clim.t, b.clim.p) {
    if (par.by.month) {
        mo <- j
        jj <- j <- 1
    } else {
        mo <- 1:12
        jj <- length(models)
    }
    for (k in mo) {
        r.clim.t <- subset(b.clim.t, k)
        r.clim.p <- subset(b.clim.p, k)
        # Read temp and precip (two) annual time series multiband tif files for
        # given anomalies directory, model, and month
        pat.t <- paste0("^tas.*._", mos[k], "_.*.tif$")
        pat.p <- paste0("^pr.*._", mos[k], "_.*.tif$")
        file.t <- list.files(file.path(anomDir[i], models[j]), pattern = pat.t, 
            full = T)
        file.p <- list.files(file.path(anomDir[i], models[j]), pattern = pat.p, 
            full = T)
        b.anom.t <- brick(file.t)
        b.anom.p <- brick(file.p)
        if (xmin(b.anom.t) < 0 & xmax(b.anom.t) < 360) 
            b.anom.t <- extend(b.anom.t, c(xmin(b.anom.t), xmax(b.anom.t) + 
                res(b.anom.t)[1], ymin(b.anom.t), ymax(b.anom.t)))  # slight adjustments for some files to avoid erroneous regions of NAs following interpolation
        if (xmin(b.anom.t) > 0 | xmax(b.anom.t) < 360) 
            stop("Source raster brick files require direct investigation of extent values.")
        if (xmin(b.anom.p) < 0 & xmax(b.anom.p) < 360) 
            b.anom.p <- extend(b.anom.p, c(xmin(b.anom.p), xmax(b.anom.p) + 
                res(b.anom.p)[1], ymin(b.anom.p), ymax(b.anom.p)))  # slight adjustments for some files to avoid erroneous regions of NAs following interpolation
        if (xmin(b.anom.p) > 0 | xmax(b.anom.p) < 360) 
            stop("Source raster brick files require direct investigation of extent values.")
        isCRU <- substr(models[j], 1, 3) == "CRU"
        file.year.ind <- if (isCRU) 
            7:8 else 9:10
        yrs <- as.numeric(substr(unlist(strsplit(basename(file.t), "_"))[file.year.ind], 
            1, 4))
        yrs <- seq(yrs[1], yrs[2])
        dir.create(outDir.t <- gsub("//", "/", file.path(outDir, period.scenario[1], 
            grp2, period.scenario[2], models[j], "tas")), recur = T, showWarnings = F)
        dir.create(outDir.p <- gsub("//", "/", file.path(outDir, period.scenario[1], 
            grp2, period.scenario[2], models[j], "pr")), recur = T, showWarnings = F)
        # Downscale anomalies and combine with CRU 2.0 climatology, write anual .tif
        # files
        nc <- ncol(b.anom.t)
        for (h in 1:nlayers(b.anom.t)) {
            r.anom.t <- subset(b.anom.t, h)
            r.anom.p <- subset(b.anom.p, h)
            if (h == 1 & xmin(r.anom.t) < 0 & all(is.na(r.anom.t[nc]))) 
                fill.last.col <- TRUE else fill.last.col <- FALSE  # If TRUE, then TRUE for all h and both tas and pr
            if (fill.last.col) {
                # Last row is the same as first row
                r.anom.t[, nc] <- r.anom.t[, 1]
                r.anom.p[, nc] <- r.anom.p[, 1]
            }
            r.new.t <- round(mask(resample(r.anom.t, r.clim.t), r.clim.t) + 
                r.clim.t, 1)
            r.new.p <- round(mask(resample(r.anom.p, r.clim.p), r.clim.p) * 
                r.clim.p)
            projection(r.new.t) <- projection(r.new.p) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
            names(r.new.t) <- paste("tas_mean_C", grp, models[j], scenario, 
                mos[k], yrs[h], sep = "_")
            names(r.new.p) <- paste("pr_total_mm", grp, models[j], scenario, 
                mos[k], yrs[h], sep = "_")
            if (isCRU) {
                names(r.new.t) <- gsub("CRU_CRU_", "CRU_", names(r.new.t))
                names(r.new.p) <- gsub("CRU_CRU_", "CRU_", names(r.new.p))
            }
            print(paste0(outDir.t, "/", names(r.new.t), ".tif"))
            writeRaster(r.new.t, paste0(outDir.t, "/", names(r.new.t), ".tif"), 
                datatype = "FLT4S", options = "COMPRESS=LZW", overwrite = T)
            writeRaster(r.new.p, paste0(outDir.p, "/", names(r.new.p), ".tif"), 
                datatype = "FLT4S", options = "COMPRESS=LZW", overwrite = T)
            print(paste("######## Model", j, "of", jj, "| Month", k, "of 12 | Year", 
                h, "of", nlayers(b.anom.t), "########"))
        }
    }
    return()
}
```

#### Run downscaling


```r
# Run
if (exists("month.index") && all(1:12 %in% month.index)) {
    mclapply(month.index, f, i = i, par.by.month = TRUE, anomDir = anomDir, 
        outDir = outDir, b.clim.t = b.clim.t, b.clim.p = b.clim.p, mc.cores = 12)
} else mclapply(1:length(models), f, i = i, anomDir = anomDir, outDir = outDir, 
    b.clim.t = b.clim.t, b.clim.p = b.clim.p, mc.cores = min(length(models), 
        32))
```
