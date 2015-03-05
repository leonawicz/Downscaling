# @knitr func_anomCalc
anomCalc <- function(i,yr1,yr2,arID,datID,var,scen="historical",resamp=F,pq=NULL,tq=NULL,setNAs=NULL,interpNAs=F,pcdf=NULL,pr0maps=F,pacifCentCRU=T,gridName=NULL,clim.mod.dirs=F){
	if(interpNAs==T & !is.null(setNAs)) require(akima)
	if(!datID=="GCM") arID <- ""
	if(clim.mod.dirs) climModDir <- modnames[[k]][i] else climModDir <- ""
	if(datID=="GCM"){
		yr.base <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-16,nchar(files[[k]][i])-13))
		yr.tail <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-9,nchar(files[[k]][i])-6))
	} else if(datID=="CRU") {
		yr.base <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-15,nchar(files[[k]][i])-12))
		yr.tail <- as.numeric(substr(files[[k]][i],nchar(files[[k]][i])-10,nchar(files[[k]][i])-7))
	}
	dirName <- gsub("_CRU",modnames[[k]][i],paste(arID,datID,"anomalies",scen,yr1,yr2,sep="_"))
	if(yr1<yr.base) yr1 <- yr.base
	if(resamp==T & !is.null(gridName)){
		dir.create(outDir <- file.path(newDir,paste(gridName,"_",dirName,sep=""),modnames[[k]][i]),showWarnings=F,recursive=T)
	} else {
		dir.create(outDir <- file.path(newDir,dirName,modnames[[k]][i]),showWarnings=F,recursive=T) }
	dir.create(qaqcDir <- file.path(outDir,"QAQC"),showWarnings=F)
	mo <- c(paste("0",1:9,sep=""),10:12)
	b <- brick(file.path(mainDir[[k]],files[[k]][i]))
	print(paste("brick ",i," loaded",sep=""),quote=F)
	if(datID=="GCM"){
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
	if(arID=="AR4" & ((yr2==2000 & yr.tail==1999)|(yr2==2100 & yr.tail==2099)) ){ # this line deals with HAD GCM ar4-type issues
		year.n <- subset(b,(nlayers(b)-11):nlayers(b))
		#year.n.names <- paste(substr(names(year.n),1,1),as.numeric(substr(names(year.n),2,5))+1,substr(names(year.n),6,11),sep="") ## Still need to get Raster package to write specified layer names!
		b <- addLayer(b,year.n)
		#names(b)[(nlayers(b)-11):nlayers(b)] <- year.n.names
	}
	len <- 12*length(yr1:yr2)
	if(yr1==yr.base) ind1 <- 1:len else ind1 <- (12*length(yr.base:(yr1-1))+1):(12*length(yr.base:(yr1-1))+len)
	climFile <- gsub("//","/",list.files(file.path(climDir,climModDir),pattern=gsub("expression","",paste(bquote(expression("^",.(var),".*.",.(modnames[[k]][i]),"_.*.climatology_01_12")),collapse="")),full=T))
	b.clim <- brick(climFile)
	qfun <- function(x,p) { a <- quantile(x,p,na.rm=T); x[x<a[1]] <- a[1]; x[x>a[2]] <- a[2]; x }
	pfun <- function(x,p) ecdf(x)(p)
	if(!is.null(pcdf)) pr0.hold <- c()
	for(j in 1:12){
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
			if(!is.null(pcdf)) pr0.hold <- cbind(pr0.hold, round(as.numeric(apply(m,2,FUN=pfun,p=pcdf)),6)) 
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
					extent(s.mo) <- c(0,360,-90,90) # fake extent to ease rotation
					s.mo <- rotate(s.mo)
					extent(s.mo) <- c(0,360,-90,90) # set proper
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
					extent(s.mo) <- c(0,360,-90,90) # fafe extent to ease rotation
					s.mo <- rotate(s.mo)
					extent(s.mo) <- c(0,360,-90,90) # set proper
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
	}
	if(var=="pr" & !is.null(pcdf)){
		pr0.hold <- data.frame(cbind(Year=yr1:yr2,pr0.hold))
		#### names(pr0.hold) <- c("Year",paste("pr0.",rep(c("q1.","q2."),12),rep(c(paste(0,1:9,sep=""),10:12),each=2),sep=""))
		names(pr0.hold) <- c("Year",paste("pr0.frac.",c(paste(0,1:9,sep=""),10:12),sep=""))
		rownames(pr0.hold) <- NULL
		write.table(pr0.hold, file.path(qaqcDir,"PrPropZero.txt"),row.names=F,col.names=T,quote=F)
	}
	print(paste("#### ",modnames[[k]][i]," completed ####",sep=""),quote=F)
}
