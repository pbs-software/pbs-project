## PBSproject R code (RH: 221118)
## ==============================

## addTemplates-------------------------2022-11-14
##  Add code templates to R package
## ---------------------------------------------RH
addTemplates = function(pkgname="myNewPkg", projpath=".")
{
	#target = file.path(projpath,pkgname)
	#rpath = paste0(target,"/R")
	#if (!dir.exists(rpath))
	#	dir.create(rpath)
	rpath = projpath
	rtargs = paste0(rpath,"/",pkgname,c(".r","Win.txt"))
	for (i in rtargs) {
		if (file.exists(i)) next  ## don't overwrite existing files
		ii = basename(i)
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
			writeLines(text=wcode, con=i)
		}
#browser();return()
	}
}
.win.addTemplates = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	addTemplates(pkgname=pkgname, projpath=".")
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~addTemplates

.win.editR = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	#openFile(paste0(projpath,"/",pkgname,"/R/",rfile))
	openFile(paste0("./",pkgname,".r"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editR

.win.editWin = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	#openFile(paste0(projpath,"/",pkgname,"/R/",wfile))
	openFile(paste0("./",pkgname,"Win.txt"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editWin

.win.sourceR = function (winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	source(paste0("./",pkgname,".r"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.sourceR

.win.createWin = function (winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	createWin(paste0("./", pkgname, "Win.txt"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.createWin

##=============================
require(PBSmodelling)
#require(devtools)
#require(roxygen2)

createWin("projectWin.txt")
#.initOptions()

