# @knitr func_anomCalcSubFun
anomCalcSubFun <- function(j){
	if(datID=="GCM") names.out <- paste(modinfo[[k]][i,],collapse="_") else if(datID=="CRU") names.out <- paste(var,modnames[[k]][i],scen,sep="_")
	names.tmp <- gsub("__","_",paste(names.out,"_",arID,"_anomaly_",mo[j],"_",yr1,"_",yr2,sep=""))
	ind <- which(rep(1:12,length(ind1)/12)==j)
	s.mo <- subset(b,ind1[ind])
	#### if(datID=="CRU" & pacifCentCRU) { s.mo.pacifCRU <- stack(s.mo,values=F); extent(s.mo) <- c(0,360,-90,90); projection(s.mo) <- NA }
	r.clim <- raster(b.clim,j)
	if(var=="pr") {
		m <- getValues(s.mo)
		if(modnames[[k]][i]=="CRU_TS30") m[true.na,] <- NA
		s.mo <- setValues(s.mo,m)
		if(!is.null(pcdf)) pr0.hold <- round(as.numeric(apply(m,2,FUN=pfun,p=pcdf)),6)
		if(pr0maps){
			zp <- s.mo
			zp[!is.na(zp) & zp>0] <- NA
			zp[zp==0] <- 1
			zp <- calc(zp,sum,na.rm=T)
			zp.out <- paste(qaqcDir,"/PrFreqZeroOverTime",substr(names.tmp,nchar(names.tmp)-12,nchar(names.tmp)),".tif",sep="")
			writeRaster(zp,zp.out,options="COMPRESS=LZW",datatype="FLT4S",overwrite=T)
		}
		if(!is.null(setNAs)){
			na.yrs <- c()
			if(interpNAs){
				for(zz in 1:nlayers(s.mo)){
					v <- getValues(subset(s.mo,zz))
					v[-bound][v[-bound]<=setNAs] <- NA
					if(datID=="GCM") na.yrs <- c(na.yrs,any(is.na(v))) else if(datID=="CRU") na.yrs <- c(na.yrs,any(is.na(v[-true.na])))
					if(!exists("cres") & any(na.yrs)){
						cres <- 0.5*res(s.mo)
						xmn <- xmin(s.mo)+cres[1]; xmx <- xmax(s.mo)-cres[1]; ymn <- ymin(s.mo)+cres[2]; ymx <- ymax(s.mo)-cres[2]
						x.out <- seq(xmn, xmx, l=ncol(s.mo))
						y.out <- seq(ymn, ymx, l=nrow(s.mo))
						xy <- data.frame(xyFromCell(s.mo,1:ncell(s.mo)))
					}
					if(na.yrs[zz]){
						if( datID=="GCM" & any(is.na(v))) doInterp <- T else doInterp <- F
						if(datID=="CRU") { if(any(is.na(v[-true.na]))) doInterp <- T else doInterp <- F }
						if(doInterp){
							v[bound][v[bound]<=setNAs] <- setNAs
							ind <- which(is.na(v))
							spl <- interp(xy[-ind,1], xy[-ind,2], v[-ind], xo=x.out, yo=y.out, linear=T, extrap=F) ## Bilinear spline interpolation
							####spl <- interp(jitter(xy[-ind,1]), jitter(xy[-ind,2]), v[-ind], xo=x.out, yo=y.out, linear=T, extrap=F) ## Bilinear spline interpolation
							m <- t(spl$z)[nrow(s.mo):1,]
							v <- as.numeric(t(m))
						}
					}
					s.mo <- setValues(s.mo,v,layer=zz)
					print(zz)
				}
			} else {
				 s.mo[s.mo<=setNAs] <- NA
			}
		}
		if(datID=="CRU"){
			s.mo <- mask(s.mo,r.cru)
			if(pacifCentCRU){
				extent(s.mo) <- c(0,360,-90,90)
				s.mo <- rotate(s.mo)
				extent(s.mo) <- c(0,360,-90,90)
				projection(s.mo) <- NA
			}
		}
		s.mo <- s.mo/r.clim
		if(!is.null(pq)){ m <- getValues(s.mo); m <- apply(m,2,FUN=qfun,p=pq); s.mo <- setValues(s.mo,m) }
		####if(!is.null(pcdf)) { pr0b <- as.numeric(apply(m,2,FUN=pfun,p=pcdf)); pr0.hold <- cbind(pr0.hold,pr0,pr0b) }
	} else if(var=="tas") {
		if(datID=="CRU"){
			s.mo <- mask(s.mo,r.cru)
			if(pacifCentCRU){
				extent(s.mo) <- c(0,360,-90,90)
				s.mo <- rotate(s.mo)
				extent(s.mo) <- c(0,360,-90,90)
				projection(s.mo) <- NA
			}
		}
		s.mo <- s.mo-r.clim
		if(!is.null(tq)){
			m <- getValues(s.mo)
			m <- apply(m,2,FUN=qfun,p=tq)
			s.mo <- setValues(s.mo,m)
		}
	}
	print(paste("brick ",i," subset anomalies ",j," calculated",sep=""),quote=F)
	if(resamp) s.mo <- resample(s.mo,r,"ngb") # nearest neighbor or bilinear
	if(resamp) print(paste("brick ",i," subset anomalies ",j," resampled",sep=""),quote=F)
	names(s.mo) <- paste(substr(names.tmp,1,nchar(names.tmp)-9),yr1:yr2,sep="")
	out <- paste(outDir,"/",names.tmp,".tif",sep="")
	writeRaster(s.mo,out,options="COMPRESS=LZW",datatype="FLT4S",overwrite=T)
	print(paste(modnames[[k]][i]," : ",j," completed",sep=""),quote=F)
	if(exists("pr0.hold")) return(pr0.hold=pr0.hold) else return(NULL)
}
