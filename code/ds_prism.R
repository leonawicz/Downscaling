###############################################################################################################################
#### This R script downscales GCM and CRU climate anomaly geotiffs to PRISM 1961-1990 2-km or 1971-2000 771-m climatology. ####
###############################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/10/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("i")) stop("Index variable 'i' not passed at command line.")
if(!exists("domain")) stop("Spatial domain variable 'domain' not passed at command line.")
if(!(domain %in% c("akcan2km", "ak771m"))) stop("Improper spatial domain specified.")

library(raster)
library(parallel)

# Part of GDAL test
#rasterOptions(maxmemory=1e+10, chunksize=2e+08) # 12 simultaneous processes on one Atlas node
#rasterOptions(tmpdir="/big_scratch/mfleonawicz/tmp/")
#print(rasterOptions())

if(domain=="akcan2km") climDir <- paste0("/Data/Base_Data/Climate/AK_CAN_2km/historical/singleBand/prism/AK_CAN_2km_PRISM/AK_CAN_geotiffs/", c("pr", "tas"), "/ak83albers")
if(domain=="ak771m") climDir <- paste0("/Data/Base_Data/Climate/AK_800m/historical/singleBand/prism/AK_800m_PRISM/geotiffs/", c("pr", "tas"))
b.clim.p <- readAll(stack(list.files(climDir[1], full=TRUE, pattern=".tif$")))
b.clim.t <- readAll(stack(list.files(climDir[2], full=TRUE, pattern=".tif$")))

anomDir <- list.files("/Data/Base_Data/Climate/World/Anomalies/1961_1990_Base_Climatology", full=T)
allowed.models <- c("cccma-cgcm3-1-t47", "gfdl-cm2-1", "miroc3-2-medres", "mpi-echam5", "ukmo-hadcm3",
	"CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3",
	"CRU_TS30", "CRU_TS31", "CRU_TS32")

outDir <- if(domain=="akcan2km") "/Data/Base_Data/Climate/AK_CAN_2km" else"/Data/Base_Data/Climate/AK_800m"

if(i > length(anomDir)) stop("Index variable 'i' exceeds maximum value.")

mos <- c(paste0(0, 1:9), 10:12)
meta <- unlist(strsplit(basename(anomDir[i]), "_"))
grp <- meta[1]
grp2 <- if(grp=="AR4") "AR4_CMIP3_models" else if(grp=="AR5") "AR5_CMIP5_models" else grp
scenario <- meta[4]
period.scenario <- if(scenario=="historical") c(scenario, "") else c("projected", scenario)
#period.scenario <- if(scenario=="historical") c(file.path(scenario, "singleBand"), "") else c("projected", scenario)
models <- list.files(anomDir[i])
models <- models[models %in% allowed.models]

# @knitr func_trim2
# Support function for method='akima'
trim2 <- function(x,y=NULL,out="matrix",rc.ind=F,xy.ext=F){
    if(!any(out==c("matrix","raster"))) stop("output must be a matrix or raster")
	if(class(x)=="matrix") {
		if(class(y)!="RasterLayer" & (out=="raster" | xy.ext==T)) stop("must input spatial information for raster or extent outputs")
		if(out=="raster" | xy.ext==T) { cres <- 0.5*res(y); crs <- projection(y) }
	}
	if(class(x)=="RasterLayer") {
		if(out=="raster" | xy.ext==T) { cres <- 0.5*res(x); crs <- projection(x);	y <- x }
		x <- matrix(as.array(x),nrow=nrow(x),ncol=ncol(x))
	}
	if(class(x)!="matrix") { stop("x must be of class raster or matrix")
	} else {
		r.na <- c.na <- c()
		for(i in 1:nrow(x)) r.na <- c(r.na, all(is.na(x[i,])))
		for(i in 1:ncol(x)) c.na <- c(c.na, all(is.na(x[,i])))
		r1 <- 1 + which(diff(which(r.na))>1)[1]; r2 <- nrow(x) -  which(diff(which(rev(r.na)))>1)[1]
		c1 <- 1 + which(diff(which(c.na))>1)[1]; c2 <- ncol(x) - which(diff(which(rev(c.na)))>1)[1]
		x <- x[r1:r2,c1:c2]
		if(out=="raster" | xy.ext==T) {
			xs <- xFromCol(y,col=c(c1,c2)) + c(-1,1)*cres[1]
			ys <- yFromRow(y,row=c(r2,r1)) + c(-1,1)*cres[2]
		}
		if(out=="raster") x <- raster(x,xmn=xs[1],xmx=xs[2],ymn=ys[1],ymx=ys[2],crs=crs)
		if(rc.ind==T & xy.ext==T) { x <- list(x=x,rc=c(r1,r2,c1,c2),xy=c(xs,ys))
			} else if(rc.ind) { x <- list(x=x,rc=c(r1,r2,c1,c2))
			} else if(xy.ext) { x <- list(x=x,xy=c(xs,ys))
		}
	}
	return(x)
}

# @knitr func_proc
# Processing function
f <- function(j, i, par.by.month=FALSE, anomDir, outDir, b.clim.t, b.clim.p, method="akima"){
	if(par.by.month) { mo <- j; jj <- j <- 1} else { mo <- 1:12; jj <- length(models) }
	for(k in mo){
		r.clim.t <- subset(b.clim.t, k)
		r.clim.p <- subset(b.clim.p, k)
		# Read temp and precip (two) annual time series multiband tif files for given anomalies directory, model, and month
		pat.t <- paste0("^tas.*._", mos[k], "_.*.tif$")
		pat.p <- paste0("^pr.*._", mos[k], "_.*.tif$")
		file.t <- list.files(file.path(anomDir[i], models[j]), pattern=pat.t, full=T)
		file.p <- list.files(file.path(anomDir[i], models[j]), pattern=pat.p, full=T)
		isCRU <- substr(models[j], 1, 3)=="CRU"
		file.year.ind <- if(isCRU) 7:8 else 9:10
		yrs <- as.numeric(substr(unlist(strsplit(basename(file.t), "_"))[file.year.ind], 1, 4))
		yrs <- seq(yrs[1], yrs[2])
		dir.create(outDir.t <- gsub("//", "/", file.path(outDir, period.scenario[1], grp2, period.scenario[2], models[j], "tas")), recur=T, showWarnings=F)
		dir.create(outDir.p <- gsub("//", "/", file.path(outDir, period.scenario[1], grp2, period.scenario[2], models[j], "pr")), recur=T, showWarnings=F)
		
		b.anom.t <- brick(file.t)
		b.anom.p <- brick(file.p)
		if(xmin(b.anom.t) < 0 & xmax(b.anom.t) < 360) b.anom.t <- extend(b.anom.t, c(xmin(b.anom.t), xmax(b.anom.t) + res(b.anom.t)[1], ymin(b.anom.t), ymax(b.anom.t))) # slight adjustments for some files to avoid erroneous regions of NAs following interpolation
		if(xmin(b.anom.t) > 0 | xmax(b.anom.t) < 360) stop("Source raster brick files require direct investigation of extent values.")
		if(xmin(b.anom.p) < 0 & xmax(b.anom.p) < 360) b.anom.p <- extend(b.anom.p, c(xmin(b.anom.p), xmax(b.anom.p) + res(b.anom.p)[1], ymin(b.anom.p), ymax(b.anom.p))) # slight adjustments for some files to avoid erroneous regions of NAs following interpolation
		if(xmin(b.anom.p) > 0 | xmax(b.anom.p) < 360) stop("Source raster brick files require direct investigation of extent values.")
		ext <- extent(c(xmin(b.anom.t), xmax(b.anom.t), 45, 76)) # Bigger than PRISM WGS84 bounding box by at least half a degree on all sides
		nc <- ncol(b.anom.t)
		if(xmin(b.anom.t) < 0 & all(is.na(b.anom.t[nc]))){
			for(h in 1:nlayers(b.anom.t)){
				r.anom.t <- subset(b.anom.t, h)
				r.anom.p <- subset(b.anom.p, h)
				r.anom.t[,nc] <- r.anom.t[,1] # Last row is the same as first row
				r.anom.p[,nc] <- r.anom.p[,1]
				b.anom.t <- setValues(b.anom.t, r.anom.t[], layer=h)
				b.anom.p <- setValues(b.anom.p, r.anom.p[], layer=h)
				print(h)
			}
		}
		b.anom.t <- rotate(crop(b.anom.t, ext))
		b.anom.p <- rotate(crop(b.anom.p, ext))
		projection(b.anom.t) <- projection(b.anom.p) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
		gc()
		print("Bricks loaded. Commence downscaling.")
		
		if(method=="akima"){
			require("akima")
			proj4.out <- projection(r.clim.p)
			nr <- nrow(r.clim.t)
			nc <- ncol(r.clim.t)
			cres <- 0.5*res(r.clim.t)
			xmn <- xmin(r.clim.t) + cres[1]; xmx <- xmax(r.clim.t) - cres[1]; ymn <- ymin(r.clim.t) + cres[2]; ymx <- ymax(r.clim.t) - cres[2]
			x.out <- seq(xmn, xmx, l=nc)
			y.out <- seq(ymn, ymx, l=nr)
			m.clim.p <- trim2(r.clim.p, rc.ind=T, xy.ext=T)
			rc.ind <- m.clim.p[[2]]
			xy.ext <- m.clim.p[[3]]
			xmn <- xy.ext[1]; xmx <- xy.ext[2]; ymn <- xy.ext[3]; ymx <- xy.ext[4]
			m.clim.p <- m.clim.p[[1]]
			m.clim.t <- trim2(r.clim.t)

			for(h in 1:nlayers(b.anom.t)){
				r.anom.t <- subset(b.anom.t, h); r.anom.p <- subset(b.anom.p, h)
				v.t <- getValues(r.anom.t);	v.p <- getValues(r.anom.p)
				if(h==1){
					xy <-data.frame(xyFromCell(r.anom.p, 1:ncell(r.anom.p)))
					coordinates(xy) <- c("x","y")
					proj4string(xy)<- projection(r.anom.p)
					xy <- coordinates(spTransform(xy, CRS=CRS(proj4.out)))
					bb <- as.numeric(bbox(r.clim.t)*matrix(c(1.1,0.9,1.1,1.1),2,2))
					e <- 120000 # extend all sides by 120 km
					ind1 <- which(point.in.polygon(xy[,1], xy[,2], c(xmn - e, xmx + e, xmx + e, xmn - e), c(ymn - e, ymn + e, ymx + e, ymx - e)) > 0)
				}
				ind <- ind1
				z.t <- v.t[ind]; z.p <- v.p[ind]
				if(any(is.na(z.p))) ind <- ind[!is.na(z.p)]
				x <- xy[ind,1]; y <- xy[ind,2]
				z.t <- v.t[ind]; z.p <- v.p[ind]
				spl.t <- interp(x, y, z.t, xo=x.out, yo=y.out, linear=T); spl.p <- interp(x, y, z.p, xo=x.out, yo=y.out, linear=T)
				mat.t <- t(spl.t$z)[nr:1,]; mat.p <- t(spl.p$z)[nr:1,]
				if(exists("rc.ind")) { mat.t <- mat.t[rc.ind[1]:rc.ind[2], rc.ind[3]:rc.ind[4]]; mat.p <- mat.p[rc.ind[1]:rc.ind[2], rc.ind[3]:rc.ind[4]] }
				r.new.t <- raster(round(mat.t + m.clim.t, 1), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=proj4.out)
				r.new.p <- raster(round(mat.p * m.clim.p), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=proj4.out)
				names(r.new.t) <- paste("tas_mean_C", grp, models[j], scenario, mos[k], yrs[h], sep="_")
				names(r.new.p) <- paste("pr_total_mm", grp, models[j], scenario, mos[k], yrs[h], sep="_")
				if(isCRU) { names(r.new.t) <- gsub("CRU_CRU_", "CRU_", names(r.new.t)); names(r.new.p) <- gsub("CRU_CRU_", "CRU_", names(r.new.p)) }
				writeRaster(r.new.t, paste0(outDir.t, "/", names(r.new.t), ".tif"), datatype="FLT4S", options="COMPRESS=LZW", overwrite=T)
				writeRaster(r.new.p, paste0(outDir.p, "/", names(r.new.p), ".tif"), datatype="FLT4S", options="COMPRESS=LZW", overwrite=T)
				print(paste("######## Model", j, "of", jj, "| Month", k, "of 12 | Year", h, "of", nlayers(b.anom.t), "########"))
			}
			
		} else if(method=="gdal"){ # GDAL cannot process such a large chunk of data at once, use akima method for PRISM 2-km (or 771-m) downscaling
			t.on <- Sys.time()
			b.new.t <- projectRaster(b.anom.t, r.clim.t)
			rm(b.anom.t)
			gc()
			print("Brick projected")
			b.new.t <- mask(b.new.t, r.clim.t)
			gc()
			print("Brick masked")
			b.new.t <- round(b.new.t + r.clim.t, 1)
			#assign(paste0("b.t.", k), b.new.t, pos=1)
			#rm(b.new.t)
			gc()
			print("Done with temperature downscaling")
			t.off <- Sys.time()
			print(difftime(t.off, t.on))
			t.on <- Sys.time()
			
			#b.new.t <- get(paste0("b.t.", k), envir=.GlobalEnv)
			nc <- ncol(b.new.t)
			for(h in 1:nlayers(b.new.t)){
				print(paste("h =", h))
				r.new.t <- subset(b.new.t, h)
				print(r.new.t)
				names(r.new.t) <- paste("tas_mean_C", grp, models[j], scenario, mos[k], yrs[h], sep="_")
				if(isCRU) names(r.new.t) <- gsub("CRU_CRU_", "CRU_", names(r.new.t))
				writeRaster(r.new.t, paste0(outDir.t, "/", names(r.new.t), ".tif"), datatype="FLT4S", options="COMPRESS=LZW", overwrite=T)
				print(paste("######## Model", j, "of", jj, "| Month", k, "of 12 | Year", h, "of", nlayers(b.new.t), "########"))
			}
			rm(b.new.t)
			gc()
			
			b.new.p <- projectRaster(b.anom.p, r.clim.p)
			rm(b.anom.p)
			gc()
			b.new.p <- mask(b.new.p, r.clim.p)
			gc()
			b.new.p <- round(b.new.p * r.clim.p)
			#assign(paste0("b.p.", k), b.new.p, pos=1)
			#rm(b.new.p)
			gc()
			print("Done with precipitation downscaling")
			t.off <- Sys.time()
			print(difftime(t.off, t.on))
			
			#b.new.p <- get(paste0("b.p.", k), envir=.GlobalEnv)
			for(h in 1:nlayers(b.new.p)){
				r.new.p <- subset(b.new.p, h)
				names(r.new.p) <- paste("pr_total_mm", grp, models[j], scenario, mos[k], yrs[h], sep="_")
				if(isCRU) names(r.new.p) <- gsub("CRU_CRU_", "CRU_", names(r.new.p))
				writeRaster(r.new.p, paste0(outDir.p, "/", names(r.new.p), ".tif"), datatype="FLT4S", options="COMPRESS=LZW", overwrite=T)
				print(paste("######## Model", j, "of", jj, "| Month", k, "of 12 | Year", h, "of", nlayers(b.new.p), "########"))
			}
			rm(b.new.p)
			gc()
		}
		
	}
return()
}

# @knitr run
# Run
if(exists("month.index") && all(1:12 %in% month.index)){
	mclapply(month.index, f, i=i, par.by.month=TRUE, anomDir=anomDir, outDir=outDir, b.clim.t=b.clim.t, b.clim.p=b.clim.p, method="akima", mc.cores=length(month.index))
} else mclapply(1:length(models), f, i=i, anomDir=anomDir, outDir=outDir, b.clim.t=b.clim.t, b.clim.p=b.clim.p, method="akima", mc.cores=min(length(models), 32))
