## Project: @projname
##   R code (created: @date)
## ===============================================

## START USER PROJECT CODE
#=================================================
## <insert user code here>
#=================================================
## END USER PROJECT CODE


#=================================================
## START TEMPLATE CODE (pbsProject.r)
##  (hidden utility functions, style=0)
#=================================================

## .win.setPBSext-----------------------2022-12-30
## Set extensions '.r' and '.txt' to open with 
## editor displayed in GUI.
## ---------------------------------------------RH
.win.setPBSext = function(winName="@projname")
{
	getWinVal(scope="L",winName=winName)
	mess = c(
		paste0("setPBSext('r','\"", getWinVal()$editor, "\" %f')"),
		paste0("setPBSext('txt','\"", getWinVal()$editor, "\" %f')")
	)
	eval(parse(text=paste0(mess, collapse="; ")))
}

## .win.clearMost-----------------------2022-12-29
## Clear objects in Global environment
## ---------------------------------------------RH
.win.clearMost = function(keep=c(".First",".Random.seed",".SavedPlots","clr","clr.rgb","qu","so"))
{
	getWinVal(scope="L",winName="@projname")
	act = getWinAct(winName="@projname")[1]
	all.objs = ls(all.names=hidden, pos=.GlobalEnv)
	rm.objs = setdiff(all.objs,keep)
	mess = paste0(strwrap(paste0(rm.objs, collapse=", "),80),collapse="\n")
	if (act=="ls") {
		mess = paste0("\nFollowing objects are available in Global environment:\n", mess )
		message(mess)
	} else if (act=="clear"){
		packList(rm.objs, target="removed.objects")  ## save to PBSmodelling environment just in case needed later
		mess = paste0("\nFollowing objects were removed from Global environment:\n", mess )
		rm(list=rm.objs, envir=.GlobalEnv)
		message(mess)
	}
}

## .win.openProject-------------------2023-01-10
## Open a project specified in '@projname' window
## ---------------------------------------------RH
.win.openProject = function(winName="@projname")
{
	getWinVal(scope="L",winName=winName)
#print(c(projname,newprojname))
#print(c(projpath,newprojpath))
	if (.is.empty(newprojname) || newprojname=="<another project name>"){
		if (overwrite)
			mess = paste0("Supply a package name to create a new package.")
		else
			mess = paste0("Supply a package name to open an existing package.")
		showAlert(mess); stop(mess)
	}
	if (newprojname==projname && newprojpath==projpath){
		mess = paste0("Choose a new project name/path that is not the same as the current project name/path.")
		showAlert(mess); stop(mess)
	}
	openProject(projname=newprojname, projpath=newprojpath, create=overwrite)
}

## .win.editR---------------------------2022-12-29
##  Edit the project's R code (@projname.r).
## ---------------------------------------------RH
.win.editR = function(winName="@projname")
{
	getWinVal(scope="L",winName=winName)
	rfile = paste0(projpath,"/",projname,".r")
	if (file.exists(rfile)){
		.do.backup(rfile)
		openFile(rfile)
	} else {
		mess = paste0("R code file '", rfile, "'\n   does not exist.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editR

## .win.editWin-------------------------2022-12-29
##  Edit the project's windows description file.
## ---------------------------------------------RH
.win.editWin = function(winName="@projname")
{
	getWinVal(scope="L",winName=winName)
	wfile = paste0(projpath,"/",projname,"Win.txt")
	if (file.exists(wfile)){
		.do.backup(wfile)
		openFile(wfile)
	} else {
		mess = paste0("Windows description file '", wfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.editWin

## .win.sourceR-------------------------2022-12-29
##  Source the project's R code.
## ---------------------------------------------RH
.win.sourceR = function (winName="@projname")
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(projname) || projname=="<another project name>"){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	rfile = paste0(projpath,"/",projname,".r")
	if (file.exists(rfile))
		source(rfile)
	else {
		mess = paste0("R code file '", rfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.sourceR

## .win.createWin-----------------------2022-12-29
##  Create the project's windows description file (not currently used).
## ---------------------------------------------RH
.win.createWin = function (winName="@projname")
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(projname) || projname=="<another project name>"){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	wfile = paste0(projpath,"/",projname,"Win.txt")
	if (file.exists(wfile))
		createWin(wfile)
	else {
		mess = paste0("Windows description file '", wfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.createWin

## .win.refresh-------------------------2022-12-29
##  Source the project's R code AND
##  Create the project's windows description file.
## ---------------------------------------------RH
.win.refresh = function (winName="@projname")
{
	getWinVal(scope="L",winName=winName)
	if (.is.empty(projname) || projname=="<another project name>"){
		mess = paste0("Supply a package name to create a new package.")
		showAlert(mess); stop(mess)
	}
	rfile = paste0(projpath,"/",projname,".r")
	if (file.exists(rfile)){
		source(rfile)
	} else {
		mess = paste0("R code file '", rfile, "'\n   does not exist in current working directory.")
		showAlert(mess); stop(mess)
	}
	wfile = paste0(projpath,"/",projname,"Win.txt")
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

## .do.backup---------------------------2022-12-30
##  Make copies of specified file x to subdirectory 'backup'.
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
## END TEMPLATE CODE

