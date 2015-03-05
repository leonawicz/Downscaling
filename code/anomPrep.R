# @knitr func_anomPrep
anomPrep <- function(dataset,years,yrs.clim,scenario=NULL){
	if(max(yrs.clim)>2005 & is.null(scenario)) stop("Must provide scenario name of base climatology if using a projected climatology period")
	if(max(yrs.clim)<2001) climID <- "historical" else climID <- "projected"
	if(dataset=="AR4gcm"|dataset=="AR5gcm"){
		datID <- "GCM"
		arID <- substr(dataset,1,3)
		varid <- c("tas","pr")
		if(any(years>2005)){
			if( arID=="AR4" & (years[1]<2001|years[2]>2100) ) stop("Year range for AR4 projected GCMs restricted to 2001:2100")
			if( arID=="AR5" & (years[1]<2006|years[2]>2100) ) stop("Year range for AR5 projected GCMs restricted to 2006:2100")
			if(arID=="AR4") scen <- c("sresb1","sresa1b","sresa2") else if(arID=="AR5") scen <- c("rcp26","rcp45","rcp60","rcp85")
			if(!is.null(scenario)) scen <- scenario else scenario <- "historical"
			varid <- rep(varid,each=length(scen))
			scen <- rep(scen,length(unique(varid)))
			mainDir <- paste("/Data/Base_Data/Climate/World/GCM_raw/IPCC_",arID,"_monthly/projected_tifs/",scen,"/",varid,sep="")
			pat=gsub("expression","",paste(bquote(expression("^",.(arID),"_GCM.*.",.(scenario),".*.")),collapse=""))
			climDir <- file.path(climDir1,list.files(climDir1,pattern=pat))
		} else {
			if( arID=="AR4" & (years[1]<1850|years[2]>2000) ) stop("Year range for AR4 historical GCMs restricted to 1850:2000")
			if( arID=="AR5" & (years[1]<1850|years[2]>2005) ) stop("Year range for AR5 historical GCMs restricted to 1850:2005")
			scen <- rep("historical",length(varid))
			mainDir <- paste("/Data/Base_Data/Climate/World/GCM_raw/IPCC_",arID,"_monthly/historical_tifs/",varid,sep="")
			pat=gsub("expression","",paste(bquote(expression("^",.(arID),"_GCM.*.historical.*.")),collapse=""))
			climDir <- file.path(climDir1,list.files(climDir1,pattern=pat))
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
		pat=gsub("expression","",paste(bquote(expression("^CRU_TS32.*.historical.*.")),collapse=""))
		climDir <- file.path(climDir1,list.files(climDir1,pattern=pat))
		files <- modnames <- list()
		files[[1]] <- list.files(mainDir[1],pattern="*.tmp.*.nc$")
		files[[2]] <- list.files(mainDir[2],pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames[[2]] <- modnames[[1]] <- "CRU_TS32"
	} else if(dataset=="cru3101"){
		datID <- "CRU"
		arID <- ""
		if(years[1]<1901|years[2]>2009) stop("Year range for CRU 3.1.01 restricted to 1901:2009")
		scen <- "historical"
		varid <- "pr"
		mainDir <- file.path("/DataBase_Data/Climate/World/CRU_grids/CRU_TS31_01")
		pat=gsub("expression","",paste(bquote(expression("^CRU_TS31.*.historical.*.")),collapse=""))
		climDir <- file.path(climDir1,list.files(climDir1,pattern=pat))
		files <- list.files(mainDir,pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames <- "CRU_TS31"
	} else if(dataset=="cru31"){
		datID <- "CRU"
		arID <- ""
		if(years[1]<1901|years[2]>2009) stop("Year range for CRU 3.1 restricted to 1901:2009")
		scen <- rep(c("historical"),2)
		varid <- c("tas","pr")
		mainDir <- rep(file.path("/DataBase_Data/Climate/World/CRU_grids/CRU_TS31"),2)
		pat=gsub("expression","",paste(bquote(expression("^CRU_TS31.*.historical.*.")),collapse=""))
		climDir <- file.path(climDir1,list.files(climDir1,pattern=pat))
		files <- modnames <- list()
		files[[1]] <- list.files(mainDir[1],pattern="*.tmp.*.nc$")
		files[[2]] <- list.files(mainDir[2],pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames[[2]] <- modnames[[1]] <- "CRU_TS31"
	} else if(dataset=="cru30"){
		datID <- "CRU"
		arID <- ""
		if(years[1]<1901|years[2]>2006) stop("Year range for CRU 3.0 restricted to 1901:2006")
		scen <- rep(c("historical"),2)
		varid <- c("tas","pr")
		mainDir <- rep(file.path("/DataBase_Data/Climate/World/CRU_grids/CRU_TS30"),2)
		pat=gsub("expression","",paste(bquote(expression("^CRU_TS30.*.historical.*.")),collapse=""))
		climDir <- file.path(climDir1,list.files(climDir1,pattern=pat))
		files <- modnames <- list()
		files[[1]] <- list.files(mainDir[1],pattern="*.tmp.*.nc$")
		files[[2]] <- list.files(mainDir[2],pattern="*.pre.*.nc$")
		modinfo <- NULL
		modnames[[2]] <- modnames[[1]] <- "CRU_TS30"
	}
	return(list(datID=datID,arID=arID,scen=scen,varid=varid,mainDir=mainDir,climDir=climDir,files=files,modinfo=modinfo,modnames=modnames))
}
