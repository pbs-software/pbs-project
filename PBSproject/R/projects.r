## PBSproject R code (RH: 221114)
## ==============================

## createPkg----------------------------2022-11-01
##  Create a new R package
## ---------------------------------------------RH
createPkg = function(pkgname="myNewPkg", projpath=".")
{
	#setwd("projpath")
	target = file.path(projpath,pkgname)
#browser();return()
	if (file.exists(target)) {
		mess = paste0("Package '", target, "' already exists.")
		showAlert(mess); stop (mess)
	} else {
		create(target)
	}
}
.win.createPkg =function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	createPkg(pkgname=pkgname, projpath=projpath)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createPkg


## addTemplates-------------------------2022-11-14
##  Add code templates to R package
## ---------------------------------------------RH
addTemplates = function(pkgname="myNewPkg", projpath=".")
{
	#setwd("projpath")
	target = file.path(projpath,pkgname)
	rpath = paste0(target,"/R")
	if (!dir.exists(rpath))
		dir.create(rpath)
	rtargs = paste0(rpath,"/",pkgname,c(".r","Win.txt"))
	for (i in rtargs) {
		if (file.exists(i)) next  ## don't overwrite existing files
		ii = basename(i)
		if (!file.exists(rtargs[i])) {
			if (grepl("\\.r$", ii)) {
				## Create R code
				rcode = c(
					paste0("## R code for '", pkgname, "', created ", Sys.time()), 
					"## ===============================================",
					paste0("## calcMean ----------------------------", substring(Sys.time(),1,10)),
					"##   Function to calculate mean of x.",
					"## --------------------------------------------JTS",
					"calcMean <- function(x)", "{",
					"   val = mean(x)", "   return(val)", "}",
					"##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcMean"
				)
				writeLines(text=rcode, con=i)
			}
#browser();return()
			if (grepl("Win\\.txt$", ii)) {
				## Create Windows Description file
				wcode = c(
					paste0("## Windows Description file for '", pkgname, "', created ", Sys.time()), 
					"## ===============================================",
					"##",
					paste0("window name=", pkgname, " title=\"", pkgname, " -- main window\""),
					"grid 1 5 sticky=E",
					"   button text=\"Open\" func=doAction action=\"openFile(`.`)\" bg=honeydew",
					"   button text=\"Set WD\" function=.changeWD bg=honeydew action=setwd",
					"   button text=\"R\" func=doAction bg=lemonchiffon   action=\"openFile(`./projects.r`)\"",
					"   button text=\"WDF\" func=doAction bg=lemonchiffon action=\"openFile(`./projectWin.txt`)\"",
					"   button text=\"UG\"  func=doAction action=\"openUG(pkg=`PBSmodelling`)\" bg=lemonchiffon",
					"grid 1 2 sticky=E",
					"   label \"WD:\" sticky=E",
					paste0("   droplist name=currentdir value=\"", getwd(), "\" add=T width=70 func=.changeWDEnter")
				)
			}
			writeLines(text=wcode, con=i)
#browser();return()
		}
	}
}
.win.addTemplates = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	addTemplates(pkgname=pkgname, projpath=projpath)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createPkg


## addCode-----------------------------2022-11-04
##  Add R files to package from current working directory.
## ---------------------------------------------RH
addCode = function(rfiles, wfiles, vfiles, frompath=".", topath="./myNewPkg")
{
	if (missing(rfiles)) ## R code
		rfiles=c("projects.r", "optFuns.r", "utilFuns.r")
	if (missing(wfiles)) ## Windows GUI
		wfiles=c("projectWin.txt")
	if (missing(vfiles)) ## Vignettes
		vfiles=c("PBSproj.Rmd")

	if (!dir.exists(topath))
		createPkg(pkgname=basename(topath), projpath=dirname(topath))

	## Copy R code file for PBSproject
	if (!.is.empty(rfiles)) {
		rpath = paste0(topath,"/R")
		if (!dir.exists(rpath))
			dir.create(rpath)
		zr = sapply(rfiles, function(x){ file.exists(paste0(frompath,"/",x)) })
		rgood = names(zr)[zr]
		if (!.is.empty(rgood)) {
			file.copy(from=paste0(frompath,"/",rgood), to=paste0(rpath,"/",rgood), overwrite=TRUE, copy.date=TRUE)
		}
	}
	## Copy window GUI files for PBSproject
	if (!.is.empty(wfiles)) {
		wpath = paste0(topath,"/inst/win")
		if (!dir.exists(wpath))
			dir.create(wpath, recursive=TRUE)
		zw = sapply(wfiles, function(x){ file.exists(paste0(frompath,"/",x)) })
		wgood = names(zw)[zw]
		if (!.is.empty(wgood)) {
			file.copy(from=paste0(frompath,"/",wgood), to=paste0(wpath,"/",wgood), overwrite=TRUE, copy.date=TRUE)
		}
	}
	## Copy vignettes for PBSproject
	if (!.is.empty(vfiles)) {
		vpath = paste0(topath,"/vignettes")
		if (!dir.exists(vpath))
			dir.create(vpath)
		zv = sapply(vfiles, function(x){ file.exists(paste0(frompath,"/",x)) })
		vgood = names(zv)[zv]
		if (!.is.empty(vgood)) {
			file.copy(from=paste0(frompath,"/",vgood), to=paste0(vpath,"/",vgood), overwrite=TRUE, copy.date=TRUE)
		}
	}
}
.win.addCode = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	## Check that a source repository has been selected
	frompath = try(getWinVal(winName="chooseP")$from, silent=TRUE)
	if (inherits(frompath, "try-error"))
		.win.chooseProj(winName="PBSproj")
	## Check that set of code names have been selected
	code = try(getWinVal(winName="chooseC")$code, silent=TRUE)
	if (inherits(code, "try-error"))
		.win.chooseProj(winName="chooseP")
	## Change R files here, not in main function:
	afiles = names(getWinVal(winName="chooseC")$code)[getWinVal(winName="chooseC")$code]
	rfiles = grep("\\.[rR]$",afiles,value=TRUE)
	#rfiles=c("projects.r", "optFuns.r", "utilFuns.r")
	wfiles =  grep("Win\\.txt$",afiles,value=TRUE)
	#wfiles=c("projectWin.txt")
	vfiles = grep("\\.[Rr]md$",afiles,value=TRUE)
	#vfiles=c("PBSproj.Rmd")
	addCode(rfiles=rfiles, wfiles=wfiles, vfiles=vfiles, frompath=frompath, topath=paste0(projpath,"/",pkgname))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~addCode


.win.editR = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	## Change R files here, not in main function:
	rfile = getWinAct()[1]
	openFile(paste0(projpath,"/",pkgname,"/R/",rfile))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editR
.win.editWin = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	wfile = getWinAct()[1]
	openFile(paste0(projpath,"/",pkgname,"/R/",wfile))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editR

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
source("guiFuns.r")

createWin("projectWin.txt")
.initOptions()

