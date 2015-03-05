# @knitr func_climPrep
climPrep <- function(dataset,years,include.psl=F){
	if(dataset=="AR4gcm"|dataset=="AR5gcm"){
		datID <- "GCM"
		arID <- substr(dataset,1,3)
		varid <- c("tas","pr")
		if(any(years>2005)){
			if( arID=="AR4" & (years[1]<2001|years[2]>2100) ) stop("Year range for AR4 projected GCMs restricted to 2001:2100")
			if( arID=="AR5" & (years[1]<2006|years[2]>2100) ) stop("Year range for AR5 projected GCMs restricted to 2006:2100")
			if(arID=="AR4") scen <- c("sresb1","sresa1b","sresa2") else if(arID=="AR5") scen <- c("rcp26","rcp45","rcp60","rcp85")
			varid <- rep(varid,each=length(scen))
			scen <- rep(scen,length(unique(varid)))
			mainDir <- paste("/Data/Base_Data/Climate/World/GCM_raw/IPCC_",arID,"_monthly/projected_tifs/",scen,"/",varid,sep="")
		} else {
			if( arID=="AR4" & (years[1]<1901|years[2]>2000) ) stop("Year range for most AR4 historical GCMs restricted to 1901:2000")
			if( arID=="AR5" & (years[1]<1860|years[2]>2005) ) stop("Year range for AR5 historical GCMs restricted to 1860:2005")
			if(include.psl) varid <- c("tas","pr","psl")
			scen <- rep("historical",length(varid))
			mainDir <- paste("/Data/Base_Data/Climate/World/GCM_raw/IPCC_",arID,"_monthly/historical_tifs/",varid,sep="")
		}
		files <- modinfo <- modnames <- list()
		for(i in 1:length(mainDir)){
			files[[i]] <- list.files(mainDir[i],pattern=".tif$")
			modinfo[[i]] <- do.call("rbind",strsplit(files[[i]],"_"))[,1:5]
			modnames[[i]] <- modinfo[[i]][,3]
		}
	} else if(dataset=="cru322"){
		datID <- "CRU"
		arID <- ""
		if(years[1]<1901|years[2]>2013) stop("Year range for CRU 3.22 restricted to 1901:2013")
		scen <- rep(c("historical"),2)
		varid <- c("tas","pr")
		mainDir <- rep(file.path("/Data/Base_Data/Climate/World/CRU_grids/CRU_TS322"),2)
		files <- modnames <- list()
		files[[1]] <- list.files(mainDir[1],pattern="*.tmp.*.nc$")
		files[[2]] <- list.files(mainDir[1],pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames[[2]] <-  modnames[[1]] <- "CRU_TS32"
	} else if(dataset=="cru3101"){
		datID <- "CRU"
		arID <- ""
		if(years[1]<1901|years[2]>2009) stop("Year range for CRU 3.1.01 restricted to 1901:2009")
		scen <- "historical"
		varid <- "pr"
		mainDir <- file.path("/Data/Base_Data/Climate/World/CRU_grids/CRU_TS31_01")
		files <- list.files(mainDir,pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames <- "CRU_TS31"
	} else if(dataset=="cru31"){
		datID <- "CRU"
		arID <- ""
		if(years[1]<1901|years[2]>2009) stop("Year range for CRU 3.1 restricted to 1901:2009")
		scen <- rep(c("historical"),2)
		varid <- c("tas","pr")
		mainDir <- rep(file.path("/Data/Base_Data/Climate/World/CRU_grids/CRU_TS31"),2)
		files <- modnames <- list()
		files[[1]] <- list.files(mainDir[1],pattern="*.tmp.*.nc$")
		files[[2]] <- list.files(mainDir[2],pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames[[2]] <-  modnames[[1]] <- "CRU_TS31"
	} else if(dataset=="cru30"){
		datID <- "CRU"
		arID <- ""
		if(years[1]<1901|years[2]>2006) stop("Year range for CRU 3.0 restricted to 1901:2006")
		scen <- rep(c("historical"),2)
		varid <- c("tas","pr")
		mainDir <- rep(file.path("/Data/Base_Data/Climate/World/CRU_grids/CRU_TS30"),2)
		files <- modnames <- list()
		files[[1]] <- list.files(mainDir[1],pattern="*.tmp.*.nc$")
		files[[2]] <- list.files(mainDir[2],pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames[[2]] <-  modnames[[1]] <- "CRU_TS30"
	} else if(dataset=="ERA40"){
		datID <- "ERA40"
		arID <- ""
		if(years[1]<1958|years[2]>2001) stop("Year range for ERA-40 restricted to 1958:2001")
		if(include.psl) varid <- c("tas","pr","psl") else varid <- c("tas","pr")
		scen <- rep("historical",length(varid))
		mainDir <- rep(paste("/Data/Base_Data/Climate/World/ERA40/historical_tifs",sep=""),length(varid))
		files <- modnames <- list()
		for(i in 1:length(mainDir)){
			files[[i]] <- list.files(mainDir[i],pattern=gsub("expression","",paste(bquote(expression("^",.(varid[i]),".*.tif$")),collapse="")))
			modnames[[i]] <- "ERA40"
		}
		modinfo <- NULL
	}
	return(list(datID=datID,arID=arID,scen=scen,varid=varid,mainDir=mainDir,files=files,modinfo=modinfo,modnames=modnames))
}
