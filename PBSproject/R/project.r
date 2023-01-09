## @project -- R code (created: @date)
## ===================================

## START CUT CREATE

## createNew----------------------------2022-11-24
##  Create new project using project.r and projectWin.txt as templates
## ---------------------------------------------RH
createNew = function(projname="@project", projpath=".", overwrite=FALSE)
{
	rpath  = projpath
	rtargs = paste0(rpath,"/",projname,c(".r","Win.txt"))
	for (i in rtargs) {
		if (file.exists(i) && !overwrite){
			.win.refresh()
		} #next  ## don't overwrite existing files
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
			rcode[uline] = gsub("@project", projname, sub("@date", Sys.time(), rcode[uline])) ## STATIC
			rcode = rcode[-(rev(grep("END CODE",rcode))[1]:length(rcode))]  ## remove recursive calls
#browser();return()
			if (projname!="runProject") {
				## remove modified 'createWin' function from new projects
				start.cut = grep("START CUT CREATE",rcode)[1]
				end.cut   = rev(grep("END CUT CREATE",rcode))[1]
				rcode = rcode[-(start.cut:end.cut)]
				rcode[start.cut] = "## ===== 'createNew' removed from user's R code ====="
				rm("createNew", envir=.GlobalEnv)
			}
#if (projname=="gadfly") {browser();return()}
			writeLines(text=rcode, con=i)
			if (projname!="runProject") {
				source(i)
			}
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
			wcode[uline] = gsub("@project", projname, sub("@date", Sys.time(), wcode[uline])) ## STATIC
			if (projname!="runProject") {
#browser(); return()
				wpkg = grep("name=projname", wcode)[1]
				wcode[wpkg] = 
					sub("edit=TRUE", "edit=TRUE noeditbg=gainsboro ",
					sub("\\\"\\\"", paste0("\"",projname,"\""),
					sub("\"<new project name>\"", paste0("\"",projname,"\""),
				wcode[wpkg] )))
				wchk = grep("name=overwrite", wcode) 
				wcode[wchk] = "null" #sub("checked=T", "checked=F", wcode[wchk])
				## Buttons
				wcode = grep("text=Start",wcode,invert=T,value=T)
				#wnew = union (grep("text=Start",wcode), grep("createNew",wcode))
				#wcode[wnew] = "null"
				wcwd = grep("text=\\\"Create WDF\\\"",wcode)
				wcode[wcwd] = "\t\tbutton text=Refresh bg=yellow sticky=W function=.win.refresh action=refresh"
				wcode[(wcwd+2):(length(wcode)+1)] = wcode[(wcwd+1):length(wcode)]
				wcode[wcwd+1] = "\t\tbutton text=New bg=tan1 sticky=W function=.init.createNew action=createNew"
#browser():return()
			}
			writeLines(text=wcode, con=i)
			if (projname!="runProject") {
				closeWin("runProject")
				createWin(i)
			}
		}
#browser();return()
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createNew

## Use the modified 'createWin' using the project specified in 'runProject' window:
.win.createNew = function(winName="runProject")
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(projname) || projname=="<new project name>"){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	createNew(projname=projname, projpath=".", overwrite=overwrite)
}

## END CUT CREATE

## Use the original R code 'createNew.init' (has wild cards):
.init.createNew = function()
{
	createNew.init(projname="runProject", projpath=".", overwrite=TRUE)
	source("runProject.r")
	createWin("runProjectWin.txt")
}

## .win.editR---------------------------2022-11-24
##  Edit the project's R code.
## ---------------------------------------------RH
.win.editR = function(winName="@project")
{
	getWinVal(scope="L",winName=winName)
	if (projname=="<new project name>")
		projname = "runProject"
	rfile = paste0("./",projname,".r")
	if (file.exists(rfile)){
		.do.backup(rfile)
		openFile(rfile)
	} else {
		mess = paste0("R code file '", rfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editR

## .win.editWin-------------------------2022-11-24
##  Edit the project's windows description file.
## ---------------------------------------------RH
.win.editWin = function(winName="@project")
{
	getWinVal(scope="L",winName=winName)
	if (projname=="<new project name>")
		projname = "runProject"
	wfile = paste0("./",projname,"Win.txt")
	if (file.exists(wfile)){
		.do.backup(wfile)
		openFile(wfile)
	} else {
		mess = paste0("Windows description file '", wfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editWin

## .win.sourceR-------------------------2022-11-24
##  Source the project's R code.
## ---------------------------------------------RH
.win.sourceR = function (winName="@project")
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(projname) || projname=="<new project name>"){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	rfile = paste0("./",projname,".r")
	if (file.exists(rfile))
		source(rfile)
	else {
		mess = paste0("R code file '", rfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.sourceR

## .win.createWin-----------------------2022-11-24
##  Create the project's windows description file (not currently used).
## ---------------------------------------------RH
.win.createWin = function (winName="@project")
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(projname) || projname=="<new project name>"){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	wfile = paste0("./",projname,"Win.txt")
	if (file.exists(wfile))
		createWin(wfile)
	else {
		mess = paste0("Windows description file '", wfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.createWin

## .win.refresh-------------------------2022-11-24
##  Source the project's R code AND
##  Create the project's windows description file.
## ---------------------------------------------RH
.win.refresh = function (winName="@project")
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(projname) || projname=="<new project name>"){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	rfile = paste0("./",projname,".r")
	if (file.exists(rfile)){
		source(rfile)
	} else {
		mess = paste0("R code file '", rfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
	wfile = paste0("./",projname,"Win.txt")
	if (file.exists(wfile)){
		createWin(wfile)
	} else {
		mess = paste0("Windows description file '", wfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
	if (projname!=winName)
		closeWin(winName)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.refresh

## .is.empty----------------------------2022-11-22
##  Check whether string is empty or NULL.
## ---------------------------------------------RH
.is.empty = function(x)
{
	if (is.null(x) || all(is.na(x)) || length(x)==0 || all(x==""))
		return(TRUE)
	else
		return(FALSE)
}

## .do.backup---------------------------2022-11-24
##  Edit the project's windows description file.
## ---------------------------------------------RH
.do.backup = function(x, timestamp)
{
	bupdir  = paste0(dirname(x),"/backup/")
	if (!dir.exists(bupdir))
		dir.create(bupdir)
	if (missing(timestamp))
		timestamp = paste0("-(", gsub(":",".", gsub(" ","_", Sys.time())) ,")")
	bupfile = paste0(bupdir, sub("(Win)?\\.(r|txt)", paste0("\\1", timestamp, ".\\2"), basename(x)))
#browser();return()
	file.copy(from=x, to=bupfile, copy.date=TRUE)
}
#=================================================

## END CODE

require(PBSmodelling)
#require(devtools)
#require(roxygen2)

closeWin()
createNew.init = createNew ## save for relaunching from user project GUIs
createNew(projname="runProject", projpath=".", overwrite=TRUE)
source("runProject.r")
createWin("runProjectWin.txt")


