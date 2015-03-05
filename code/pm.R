# @knitr create_project
source("C:/github/ProjectManagement/code/rpm.R") # eventually load a package instead of source script
proj.name <- "Downscaling" # Project name
proj.location <- matt.proj.path # Use default file location

docDir <- c("Rmd/include", "md", "html", "Rnw", "pdf", "timeline")
newProject(proj.name, proj.location, docs.dirs=docDir, overwrite=T) # create a new project

rfile.path <- file.path(proj.location, proj.name, "code") # path to R scripts
docs.path <- file.path(proj.location, proj.name, "docs")
rmd.path <- file.path(docs.path, "Rmd")

# generate Rmd files from existing R scripts using default yaml front-matter
genRmd(path=rfile.path) # specify header.args list argument if necessary

# @knitr update_project
# update yaml front-matter only
genRmd(path=rfile.path, update.header=TRUE)

# obtain knitr code chunk names in existing R scripts
chunkNames(path=file.path(proj.location, proj.name, "code"))

# append new knitr code chunk names found in existing R scripts to any Rmd files which are outdated
chunkNames(path=file.path(proj.location, proj.name, "code"), append.new=TRUE)

# @knitr website
# Setup for generating a project website
user <- "leonawicz"
proj.github <- file.path("https://github.com", user, proj.name)
index.url <- "index.html"
#file.copy(index.url, "index.html")

proj.title <- "Downscaling"
proj.menu <- c("Overview", "Climatologies", "Anomalies", "Downscaling", "All Projects")

proj.submenu <- list(
	c("empty"),
	c("Main script", "climatologies.R", "divider", "Functions", "climPrep.R", "climCalc.R", "divider", "CRU 2.0", "cru20prelimExtraction"),
	c("Main script", "anomalies.R", "divider", "Functions", "anomPrep.R", "anomCalc.R", "anomCalcAsScript.R", "anomParSubFunc.R"),
	c("10-minute CRU 2.0", "ds_world10min.R"),
	c("empty")
)

proj.files <- list(
	c("index.html"),
	c("header", "climatologies.html", "divider", "header", "climPrep.html", "climCalc.html", "divider", "header", "cru20prelimExtraction"),
	c("header", "anomalies.html", "divider", "header", "anomPrep.html", "anomCalc.html", "anomCalcAsScript.html", "anomParSubFunc.html"),
	c("header", "ds_world10min.html"),
	c("http://leonawicz.github.io")
)

# generate navigation bar html file common to all pages
genNavbar(htmlfile=file.path(rmd.path, "include/navbar.html"), title=proj.title, menu=proj.menu, submenus=proj.submenu, files=proj.files, site.url=proj.github, include.home=FALSE)

# generate _output.yaml file
# Note that external libraries are expected, stored in the "libs" below
yaml.out <- file.path(proj.location, proj.name, "docs/Rmd/_output.yaml")
libs <- "libs"
common.header <- "include/in_header.html"
genOutyaml(file=yaml.out, lib=libs, header=common.header, before_body="include/navbar.html", after_body="include/after_body.html")

# @knitr knit_setup
library(rmarkdown)
library(knitr)
setwd(rmd.path)

# R scripts
#files.r <- list.files("../../code", pattern=".R$", full=T)

# Rmd files
files.Rmd <- list.files(pattern=".Rmd$", full=T)

# @knitr save
# write all yaml front-matter-specified outputs to Rmd directory for all Rmd files
lapply(files.Rmd, render, output_format="all")
insert_gatc(list.files(pattern=".html$"))
moveDocs(path.docs=docs.path)

# if also making PDFs for a project, speed up the Rmd to Rnw file conversion/duplication
rnw.path <- file.path(docs.path, "Rnw")
setwd(rnw.path)
#themes <- knit_theme$get()
highlight <- "solarized-dark"
convertDocs(path=rmd.path, emphasis="replace", overwrite=TRUE, highlight=highlight) # Be careful
lapply(list.files(pattern=".Rnw$"), knit2pdf)
moveDocs(path.docs=docs.path, type="pdf", remove.latex=FALSE)
