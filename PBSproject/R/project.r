## @project -- R code (created: @date)
## ===================================

## createNew----------------------------2022-11-22
##  Create new project using project.r and projectWin.txt as templates
## ---------------------------------------------RH
createNew = function(pkgname="@project", projpath=".", overwrite=FALSE)
{
	rpath  = projpath
	rtargs = paste0(rpath,"/",pkgname,c(".r","Win.txt"))
	for (i in rtargs) {
		if (file.exists(i) && !overwrite) next  ## don't overwrite existing files
		ii = basename(i)
		if (grepl("\\.r$", ii)) {
			## Create R code form 'project.r'
			rbase = paste0(getwd(),"/project.r")
			if (!file.exists(rbase)){
				mess = paste0("R code file '", rbase, "'\n   does not exist in current working directory.")
				showAlert(mess); stop(mess)
			}
			rcode = readLines(con=rbase)
			pline = setdiff( grep("@project",rcode), grep("STATIC",rcode) ) ## STATIC
			dline = setdiff( grep("@date",rcode), grep("STATIC",rcode) )    ## STATIC
			uline = union(pline,dline)
			rcode[uline] = gsub("@project", pkgname, sub("@date", Sys.time(), rcode[uline])) ## STATIC
			rcode = rcode[-(rev(grep("END CODE",rcode))[1]:length(rcode))]  ## remove recursive calls
#browser();return()
#if (pkgname=="gadfly") {browser();return()}
			writeLines(text=rcode, con=i)
		}
		if (grepl("Win\\.txt$", ii)) {
			## Create Windows Description file
			wbase = paste0(getwd(),"/projectWin.txt")
			if (!file.exists(wbase)){
				mess = paste0("Windows description file '", wbase, "'\n   does not exist in current working directory.")
				showAlert(mess); stop(mess)
			}
			wcode = readLines(wbase)
			pline = setdiff( grep("@project",wcode), grep("STATIC",wcode) ) ## STATIC
			dline = setdiff( grep("@date",wcode), grep("STATIC",wcode) )    ## STATIC
			uline = union(pline,dline)
			wcode[uline] = gsub("@project", pkgname, sub("@date", Sys.time(), wcode[uline])) ## STATIC
			if (pkgname!="runProject") {
				wnew = union (grep("text=New",wcode), grep("createNew",wcode))
				grep("createNew",wcode,invert=T,value=T)
				wcode[wnew] = "\t\tbutton text=Refresh bg=thistle1 sticky=W function=.win.createWin action=createWin"
				wpkg = grep("pkgname", wcode)[1]
				wcode[wpkg] = sub("edit=TRUE", "edit=FALSE noeditbg=gainsboro ", sub("\\\"\\\"",pkgname, wcode[wpkg]) )
			}
#browser(); return()
			writeLines(text=wcode, con=i)
			if (pkgname!="runProject") {
				closeWin("runProject")
				createWin(i)
			}
		}
#browser();return()
	}
}
.win.createNew = function(winName="runProject")  ## window will always be the same when starting new project
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(pkgname)){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	createNew(pkgname=pkgname, projpath=".")
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createNew

.win.editR = function(winName="@project")
{
	getWinVal(scope="L",winName=winName)
	#openFile(paste0(projpath,"/",pkgname,"/R/",rfile))
	rfile = paste0("./",pkgname,".r")
	if (file.exists(rfile))
		openFile(rfile)
	else {
		mess = paste0("R code file '", rfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editR

.win.editWin = function(winName="@project")
{
	getWinVal(scope="L",winName=winName)
	#openFile(paste0(projpath,"/",pkgname,"/R/",wfile))
	openFile(paste0("./",pkgname,"Win.txt"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editWin

.win.sourceR = function (winName="@project")
{
	getWinVal(scope="L",winName=winName)
	source(paste0("./",pkgname,".r"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.sourceR

.win.createWin = function (winName="@project")
{
	getWinVal(scope="L",winName=winName)
	createWin(paste0("./", pkgname, "Win.txt"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.createWin

.is.empty = function(x)
{
	if (is.null(x) || all(is.na(x)) || length(x)==0 || all(x==""))
		return(TRUE)
	else
		return(FALSE)
}

#=================================================

## END CODE

require(PBSmodelling)
#require(devtools)
#require(roxygen2)

createNew(pkgname="runProject", projpath=".", overwrite=TRUE)
source("runProject.r")
createWin("runProjectWin.txt")
#.initOptions()

