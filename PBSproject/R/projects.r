## PBSproject R code (RH: 221104)
## ==============================

## createPkg----------------------------2022-11-01
##  Create a new R package
## ---------------------------------------------RH
createPkg = function(new.pkg="newPkg", projpath=".")
{
	#setwd("projpath")
	target = file.path(projpath,new.pkg)
#browser();return()
	if (file.exists(target)) {
		stop (paste0("Package '", target, "' already exists."))
	} else {
		create(target)
	}
}
.win.createPkg =function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	createPkg(new.pkg=pkgname, projpath=projpath)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createPkg

## addRcode----------------------------2022-11-04
##  Add R files to package from current working directory.
## ---------------------------------------------RH
addRcode = function(rfiles, wfiles, vfiles, frompath=".", topath="./myNewPkg")
{
	if (missing(rfiles)) ## R code
		rfiles=c("projects.r", "optFuns.r", "utilFuns.r")
	if (missing(wfiles)) ## Windows GUI
		wfiles=c("projectWin.txt")
	if (missing(vfiles)) ## Vignettes
		vfiles=c("PBSproj.Rmd")

	if (!dir.exists(topath))
		createPkg(new.pkg=basename(topath), projpath=dirname(topath))

	## Copy R code file for PBSproject
	rpath = paste0(topath,"/R")
	if (!dir.exists(rpath))
		dir.create(rpath)
	zr = sapply(rfiles, function(x){ file.exists(paste0(frompath,"/",x)) })
	rgood = names(zr)[zr]
	if (!.is.empty(rgood)) {
		file.copy(from=paste0(frompath,"/",rgood), to=paste0(rpath,"/",rgood), overwrite=TRUE, copy.date=TRUE)
	}
	## Copy window GUI files for PBSproject
	wpath = paste0(topath,"/inst/win")
	if (!dir.exists(wpath))
		dir.create(wpath, recursive=TRUE)
	zw = sapply(wfiles, function(x){ file.exists(paste0(frompath,"/",x)) })
	wgood = names(zw)[zw]
	if (!.is.empty(wgood)) {
		file.copy(from=paste0(frompath,"/",wgood), to=paste0(wpath,"/",wgood), overwrite=TRUE, copy.date=TRUE)
	}
	## Copy vignettes for PBSproject
	vpath = paste0(topath,"/vignettes")
	if (!dir.exists(vpath))
		dir.create(vpath)
	zv = sapply(vfiles, function(x){ file.exists(paste0(frompath,"/",x)) })
	vgood = names(zv)[zv]
	if (!.is.empty(vgood)) {
		file.copy(from=paste0(frompath,"/",vgood), to=paste0(vpath,"/",vgood), overwrite=TRUE, copy.date=TRUE)
	}
}
.win.addRcode = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	## Change R files here, not in main function:
	rfiles=c("projects.r", "optFuns.r", "utilFuns.r")
	wfiles=c("projectWin.txt")
	vfiles=c("PBSproj.Rmd")
	addRcode(rfiles=rfiles, wfiles=wfiles, vfiles=vfiles, frompath=".", topath=paste0(projpath,"/",pkgname))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~addRcode

.win.check.release = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	openFile(paste0(currentdir,"/Check_Release.txt"))
}


##=============================
require(PBSmodelling)
require(devtools)
require(roxygen2)

# Taking cue from Roger Bivand's maptools:
.PBSprojEnv <- new.env(FALSE, parent=globalenv())  # be sure to exportPattern("^\\.PBS") in NAMESPACE

source("optFuns.r")
source("utilFuns.r")

createWin("projectWin.txt")
.initOptions()

