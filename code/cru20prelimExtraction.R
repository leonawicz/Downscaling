#### Preliminary extraction of CRU 2.0 climatology data ####
#### 12/18/12

#### Update 01/03/13 ####
#### The CRU 2.0 world grid 10-minute climatologies for temperature and precipitation diverge from all other climatology datasets including CRU 3.0 and 3.1,
#### in that it uses 0.001 and 0.999 percentile cuts rather than 0.01 and 0.99
#### Also, it does NOT use an interpolation step for precipitation. It simply replaces ALL cell values below the 0.5 mm threshold with 0.5 mm (as opposed to only doing this for boundary cells).
#### Non-boundary cells are not replaced with NAs and interpolated over, since it does not perform well in this case.

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/05/2015        ####

# @knitr setup
path <- "/Data/Base_Data/Climate/World/CRU_grids/CRU_TS20"
path.out <- "/Data/Base_Data/Climate/World/Climatologies/1961_1990/CRU_CL20_climatologies_historical_1961_1990"
library(raster)
library(akima)
library(parallel)

# Load and rasterize data
d <- read.table(list.files(path,pattern="tmp.dat$",full=T))
for(i in 1:12){
	r <- rasterFromXYZ(d[c(2,1,i+2)], res=rep(1/6,2), crs="+proj=longlat +datum=WGS84", digits=0)
	if(i==1) s.t <- r else s.t <- stack(s.t,r)
	print(i)
}
names(s.t) <- paste0("tas_CRU_TS20_historical_climatology_",c(paste(0,1:9,sep=""),10:12))

d <- read.table(list.files(path,pattern="pre.dat$",full=T))
for(i in 1:12){
	r <- rasterFromXYZ(d[c(2,1,i+2)], res=rep(1/6,2), crs="+proj=longlat +datum=WGS84", digits=0)
	if(i==1) s.p <- r else s.p <- stack(s.p,r)
	print(i)
}
names(s.p) <- paste0("pr_CRU_TS20_historical_climatology_",c(paste(0,1:9,sep=""),10:12))

# Set variable: tas or pr
var <- "tas"
if(var=="pr") s <- s.p else if(var=="tas") s <- s.t
setNAs <- 0.5
interpNAs <- T
true.na <- Which(is.na(r),cells=T)
pq <- tq <- c(0.001,0.999) # for CRU 2.0 only!
pacifCentCRU <- T
if(var=="pr" & interpNAs) bound <- Which(edge(r, type='inner')==1,cells=T)
qfun <- function(x,p) { a <- quantile(x,p,na.rm=T); x[x<a[1]] <- a[1]; x[x>a[2]] <- a[2]; x }

# @kntir proc_func
f <- function(j){
	s.clim <- subset(s,j)
	if(var=="pr" & !is.null(pq)){ m <- qfun(getValues(s.clim),p=pq); s.clim <- setValues(s.clim,m) }
	if( (var=="tas") & !is.null(tq) ){ m <- qfun(getValues(s.clim),p=tq); s.clim <- setValues(s.clim,m) }
	if(var=="pr" & !is.null(setNAs)){
		if(!interpNAs) s.clim[s.clim<=setNAs] <- NA
		if(interpNAs){
			cres <- 0.5*res(s.clim)
			xmn <- xmin(s.clim)+cres[1]; xmx <- xmax(s.clim)-cres[1]; ymn <- ymin(s.clim)+cres[2]; ymx <- ymax(s.clim)-cres[2]
			x.out <- seq(xmn, xmx, l=ncol(s.clim))
			y.out <- seq(ymn, ymx, l=nrow(s.clim))
			xy <- data.frame(xyFromCell(s.clim,1:ncell(s.clim)))
			v <- getValues(s.clim)
			v[-true.na][v[-true.na]<setNAs | is.na(v[-true.na])] <- setNAs # for CRU 2.0 only!!!
			s.clim <- setValues(s.clim,v) # for CRU 2.0 only!!!
			#v[-bound][v[-bound]<setNAs] <- NA
			#if(any(is.na(v[-true.na]))) doInterp <- T else doInterp <- F
			#if(doInterp){
			#	v[bound][v[bound]<=setNAs] <- setNAs
			#	ind1 <- which(is.na(v))
			#	ind2 <- which(v==setNAs)
			#	ind <- sort(c(ind1,ind2))
			#	spl <- interp(xy[-ind1,1], xy[-ind1,2], v[-ind1], xo=x.out, yo=y.out, linear=T, extrap=F) ## Bilinear spline interpolation
			#	m <- t(spl$z)[nrow(s.clim):1,]
			#	s.clim[ind] <- t(m)[ind]
			#}
			s.clim <- mask(s.clim,r)
		}
	}
	if(pacifCentCRU){
		extent(s.clim) <- c(0,360,ymin(s.clim),ymax(s.clim))
		s.clim <- rotate(s.clim)
		extent(s.clim) <- c(0,360,ymin(s.clim),ymax(s.clim))
		projection(s.clim) <- NA
	}
	print(paste("brick subset climatology ",j," calculated",sep=""),quote=F)
	#if(j==1) s.clim.hold <- s.clim else s.clim.hold <- stack(s.clim.hold,s.clim)
	return(s.clim)
}

# @knitr run
# run
out <- mclapply(1:12, f, mc.cores=12)
for(i in 1:12){	if(i==1) s <- out[[i]] else s <- stack(s,out[[i]]) }
if(var=="tas") writeRaster(s, file.path(path.out,"tas_CRU_TS20_historical_climatology_01_12_1961_1990.tif"), datatype="FLT4S", options="COMPRESS=LZW", overwrite=T) else
if(var=="pr") writeRaster(s, file.path(path.out,"pr_CRU_TS20_historical_climatology_01_12_1961_1990.tif"), datatype="FLT4S", options="COMPRESS=LZW", overwrite=T)
