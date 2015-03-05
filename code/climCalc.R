# @knitr func_climCalc
climCalc <- function(i,yr1,yr2,arID,datID,var,scen,resamp=F,pq=NULL,tq=NULL,setNAs=NULL,interpNAs=F,pacifCentCRU=T,gridName=NULL,monthly.files=F,model.dirs=F){
	if(interpNAs==T & !is.null(setNAs)) require(akima)
	if(!datID=="GCM") arID <- ""
	if(model.dirs) modDir <- modnames[[k]][i] else modDir <- ""
	if(datID=="GCM"|datID=="ERA40"){
		yr.base <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-16,nchar(files[[k]][i])-13))
		yr.tail <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-9,nchar(files[[k]][i])-6))
	} else if(datID=="CRU") {
		yr.base <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-15,nchar(files[[k]][i])-12))
		yr.tail <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-10,nchar(files[[k]][i])-7))
	}
	if(yr1<yr.base) stop("First year of climatology precedes dataset.") 
	dirName <- gsub("_ERA40",modnames[[k]][i],gsub("_CRU",modnames[[k]][i],paste(arID,datID,"climatologies",scen,yr1,yr2,sep="_")))
	if(resamp==T & !is.null(gridName)){
		dir.create(outDir <- file.path(newDir,paste(yr1,"_",yr2,"/",gridName,"_",dirName,sep=""),modDir),showWarnings=F,recursive=T)
	} else {
		dir.create(outDir <- file.path(newDir,paste(yr1,"_",yr2,"/",dirName,sep=""),modDir),showWarnings=F,recursive=T) }
	mo <- c(paste("0",1:9,sep=""),10:12)
	b <- brick(file.path(mainDir[[k]],files[[k]][i]))
	print(paste("brick ",i," loaded",sep=""),quote=F)
	if(datID=="GCM"|datID=="ERA40"){
		if(var=="pr" & interpNAs){
			b1 <- (ncell(b)-ncol(b)+1):ncell(b)
			b2 <- seq(1,ncell(b),ncol(b))
			b3 <- 1:ncol(b)
			b4 <- seq(ncol(b),ncell(b),ncol(b))
			bound <- sort(unique(c(b1,b2,b3,b4)))
		}
	} else if(datID=="CRU") {
		b <- stack(b)
		r.cru <- raster(b,1)
		if(modnames[[k]][i]=="CRU_TS30") { true.na <- Which(r.cru==-99.9,cells=T); r.cru[true.na] <- NA } else true.na <- Which(is.na(r.cru),cells=T) # CRU 3.0 uses -99.9 as NA
		if(var=="pr" & interpNAs) bound <- Which(boundaries(r.cru, type='inner')==1,cells=T)
	}
	if(arID=="AR4" & yr2==2000 & yr.tail==1999) len <- 12*length(yr1:(yr2-1)) else len <- 12*length(yr1:yr2) # this line deals with HAD GCM ar4-type issues
	if(yr1==yr.base) ind <- 1:len else ind <- (12*length(yr.base:(yr1-1))+1):(12*length(yr.base:(yr1-1))+len)
	s <- subset(b,ind)
	qfun <- function(x,p) { a <- quantile(x,p,na.rm=T); x[x<a[1]] <- a[1]; x[x>a[2]] <- a[2]; x }
	names.tmp.hold <- c()
	for(j in 1:12){
		if(datID=="GCM") names.out <- paste(modinfo[[k]][i,],collapse="_") else if(datID=="CRU"|datID=="ERA40") names.out <- paste(var,modnames[[k]][i],scen,sep="_")
		names.tmp <- gsub("__","_",paste(names.out,"_",arID,"_climatology_",mo[j],"_",yr1,"_",yr2,sep=""))
		names.tmp.hold <- c(names.tmp.hold,names.tmp)
		ind <- which(rep(1:12,nlayers(s)/12)==j)
		s.clim <- subset(s,ind)
		s.clim <- calc(s.clim,mean)
		if(modnames[[k]][i]=="CRU_TS30") s.clim[true.na] <- NA
		if(var=="pr" & !is.null(pq)){ m <- qfun(getValues(s.clim),p=pq); s.clim <- setValues(s.clim,m) }
		if( (var=="tas"|var=="psl") & !is.null(tq) ){ m <- qfun(getValues(s.clim),p=tq); s.clim <- setValues(s.clim,m) }
		if(var=="pr" & !is.null(setNAs)){
			if(!interpNAs) s.clim[s.clim<=setNAs] <- NA
			if(interpNAs){
				cres <- 0.5*res(s.clim)
				xmn <- xmin(s.clim)+cres[1]; xmx <- xmax(s.clim)-cres[1]; ymn <- ymin(s.clim)+cres[2]; ymx <- ymax(s.clim)-cres[2]
				x.out <- seq(xmn, xmx, l=ncol(s.clim))
				y.out <- seq(ymn, ymx, l=nrow(s.clim))
				xy <- data.frame(xyFromCell(s.clim,1:ncell(s.clim)))
				v <- getValues(s.clim)
				v[-bound][v[-bound]<setNAs] <- NA
				if( datID=="GCM" & any(is.na(v))) doInterp <- T else doInterp <- F
				if(datID=="CRU") { if(any(is.na(v[-true.na]))) doInterp <- T else doInterp <- F }
				if(doInterp){
					v[bound][v[bound]<=setNAs] <- setNAs
					ind1 <- which(is.na(v))
					ind2 <- which(v==setNAs)
					ind <- sort(c(ind1,ind2))
					spl <- interp(xy[-ind1,1], xy[-ind1,2], v[-ind1], xo=x.out, yo=y.out, linear=T, extrap=F) ## Bilinear spline interpolation
					m <- t(spl$z)[nrow(s.clim):1,]
					s.clim[ind] <- t(m)[ind]
				}
				if(datID=="CRU") s.clim <- mask(s.clim,r.cru)
			}
		}
		if(datID=="CRU" & pacifCentCRU){
			extent(s.clim) <- c(0,360,-90,90) # fake extent to ease rotation
			s.clim <- rotate(s.clim)
			extent(s.clim) <- c(0,360,-90,90) # set proper
			projection(s.clim) <- NA
		}
		print(paste("brick ",i," subset climatology ",j," calculated",sep=""),quote=F)
		if(resamp) s.clim <- resample(s.clim,r,"ngb") # nearest neighbor or bilinear
		if(resamp) print(paste("brick ",i," subset climatology ",j," resampled",sep=""),quote=F)
		out <- gsub("//","/",paste(outDir,"/",names.tmp,".tif",sep=""))
		if(monthly.files) writeRaster(s.clim,out,options="COMPRESS=LZW",datatype="FLT4S",overwrite=T)
		if(j==1) s.clim.hold <- s.clim else s.clim.hold <- stack(s.clim.hold,s.clim)
		print(paste(modnames[[k]][i]," : ",j," completed",sep=""),quote=F)
	}
	names(s.clim.hold) <- names.tmp.hold
	out <- gsub("__","_",gsub("//","/",paste(outDir,"/",names.out,"_",arID,"_climatology_01_12_",yr1,"_",yr2,".tif",sep="")))
	writeRaster(s.clim.hold,out,options="COMPRESS=LZW",datatype="FLT4S",overwrite=T)
	print(paste("#### ",modnames[[k]][i]," completed ####",sep=""),quote=F)
}
