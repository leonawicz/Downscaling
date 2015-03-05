


##
##
## anomCalcAsScript.R

`anomCalcAsScript.R` is similar to `anomCalc.R` except that it is, under certain conditions, sourced later in `anomalies.R` in script form rather than as calling a function.
The result is that same as with `anomCalc.R`, which is the calculation of climate anomalies geotiffs.

The methods differ in that calling this script from `anomalies.R` rather than calling the `anomCalc` function, offers a different point of parallelization.
Instead of parallelizing across data sets, `anomCalcAsScript.R` is often used to process a single data set while parallelizing across twelve months.

This is ideal for CRU data sets, for reprocessing a single GCM, or for processing multiple GCMs one at a time where the GCMs may differ substantially in respective file sizes.
However, this is strictly available for CRU at this time, which is always processed one data set at a time (hence there is no reason not to use this method).
This option remains currently unavailable for GCM anomalies geotiff generation until some code updates can be made.

### R code


```r
resamp <- F
pacifCentCRU <- T
gridName <- NULL
clim.mod.dirs <- F
if (interpNAs == T & !is.null(setNAs)) require(akima)
if (!datID == "GCM") arID <- ""
if (clim.mod.dirs) climModDir <- modnames[[k]][i] else climModDir <- ""

if (datID == "GCM") {
    yr.base <- as.numeric(substr(files[[k]][i], nchar(files[[k]][i]) - 16, nchar(files[[k]][i]) - 
        13))
    yr.tail <- as.numeric(substr(files[[k]][i], nchar(files[[k]][i]) - 9, nchar(files[[k]][i]) - 
        6))
} else if (datID == "CRU") {
    yr.base <- as.numeric(substr(files[[k]][i], nchar(files[[k]][i]) - 15, nchar(files[[k]][i]) - 
        12))
    yr.tail <- as.numeric(substr(files[[k]][i], nchar(files[[k]][i]) - 10, nchar(files[[k]][i]) - 
        7))
}

dirName <- gsub("_CRU", modnames[[k]][i], paste(arID, datID, "anomalies", scen, 
    yr1, yr2, sep = "_"))
if (yr1 < yr.base) yr1 <- yr.base

if (resamp == T & !is.null(gridName)) {
    dir.create(outDir <- file.path(newDir, paste(gridName, "_", dirName, sep = ""), 
        modnames[[k]][i]), showWarnings = F, recursive = T)
} else {
    dir.create(outDir <- file.path(newDir, dirName, modnames[[k]][i]), showWarnings = F, 
        recursive = T)
}

dir.create(qaqcDir <- file.path(outDir, "QAQC"), showWarnings = F)
mo <- c(paste("0", 1:9, sep = ""), 10:12)
b <- brick(file.path(mainDir[[k]], files[[k]][i]))
print(paste("brick ", i, " loaded", sep = ""), quote = F)

if (datID == "GCM") {
    if (var == "pr" & interpNAs) {
        b1 <- (ncell(b) - ncol(b) + 1):ncell(b)
        b2 <- seq(1, ncell(b), ncol(b))
        b3 <- 1:ncol(b)
        b4 <- seq(ncol(b), ncell(b), ncol(b))
        bound <- sort(unique(c(b1, b2, b3, b4)))
    }
} else if (datID == "CRU") {
    b <- stack(b)
    r.cru <- raster(b, 1)
    if (modnames[[k]][i] == "CRU_TS30") {
        true.na <- Which(r.cru == -99.9, cells = T)
        r.cru[true.na] <- NA
    } else true.na <- Which(is.na(r.cru), cells = T)  # CRU 3.0 uses -99.9 as NA
    if (var == "pr" & interpNAs) 
        bound <- Which(boundaries(r.cru, type = "inner") == 1, cells = T)
}
if (arID == "AR4" & ((yr2 == 2000 & yr.tail == 1999) | (yr2 == 2100 & yr.tail == 
    2099))) {
    # this line deals with HAD GCM ar4-type issues
    year.n <- subset(b, (nlayers(b) - 11):nlayers(b))
    # year.n.names <-
    # paste(substr(names(year.n),1,1),as.numeric(substr(names(year.n),2,5))+1,substr(names(year.n),6,11),sep='')
    # ## Still need to get Raster package to write specified layer names!
    b <- addLayer(b, year.n)
    # names(b)[(nlayers(b)-11):nlayers(b)] <- year.n.names
}

len <- 12 * length(yr1:yr2)
if (yr1 == yr.base) ind1 <- 1:len else ind1 <- (12 * length(yr.base:(yr1 - 1)) + 
    1):(12 * length(yr.base:(yr1 - 1)) + len)
climFile <- gsub("//", "/", list.files(file.path(climDir, climModDir), pattern = gsub("expression", 
    "", paste(bquote(expression("^", .(var), ".*.", .(modnames[[k]][i]), "_.*.climatology_01_12")), 
        collapse = "")), full = T))
b.clim <- brick(climFile)

qfun <- function(x, p) {
    a <- quantile(x, p, na.rm = T)
    x[x < a[1]] <- a[1]
    x[x > a[2]] <- a[2]
    x
}
pfun <- function(x, p) ecdf(x)(p)

pr0.hold <- mclapply(1:12, anomCalcSubFun, mc.cores = 12)

if (var == "pr" & !is.null(pcdf)) {
    pr0.hold <- do.call(cbind, pr0.hold)
    pr0.hold <- data.frame(cbind(Year = yr1:yr2, pr0.hold))
    names(pr0.hold) <- c("Year", paste("pr0.frac.", c(paste(0, 1:9, sep = ""), 
        10:12), sep = ""))
    rownames(pr0.hold) <- NULL
    write.table(pr0.hold, file.path(qaqcDir, "PrPropZero.txt"), row.names = F, 
        col.names = T, quote = F)
}
print(paste("#### ", modnames[[k]][i], " completed ####", sep = ""), quote = F)
```
