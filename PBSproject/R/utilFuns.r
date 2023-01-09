##================================================
##             UTILITY FUNCTIONS                  
##================================================

## jtget--------------------------------2022-11-02
##  Provide PBSproject wrappers for PBSmodelling functions:
##   tget/tcall/tprint/tput/lisp
##----------------------------------------------RH
jtget   = function(...) {tget  (..., penv=parent.frame(), tenv=.PBSprojEnv)}
jtcall  = function(...) {tcall (..., penv=parent.frame(), tenv=.PBSprojEnv)}
jtprint = function(...) {tprint(..., penv=parent.frame(), tenv=.PBSprojEnv)}
jtput   = function(...) {tput  (..., penv=parent.frame(), tenv=.PBSprojEnv)}
jlisp   = function(...) {lisp  (..., pos =.PBSprojEnv)}


## suggestPath--------------------------2022-11-03
## Suggest a path for a specified program from PATH
## Arguments:
##  progs = vector of program names (without extension)
##  ipath = initial path by user to try before PATH directories
##  file_ext = alternative program extension if other
##    than `.exe` (primarily for Windows users)
## ---------------------------------------------RH
suggestPath <- function(progs, ipath=NULL, file_ext=NULL, bat_ext=NULL)
{
	isWin = .Platform$OS.type=="windows" #; isWin=FALSE
	path_sep = .Platform$path.sep
	#file_sep = ifelse(isWin, "\\", "/" )
	file_sep = ifelse(isWin, "\\", "/" )
	if (is.null (file_ext))
		file_ext = ifelse(isWin, ".exe", "" )
	if (!is.null(bat_ext))
		file_ext = ifelse(isWin, ".bat", ".sh" )
	sys_path = Sys.getenv( "PATH" )
	paths = strsplit(sys_path,path_sep)[[1]]
	paths = gsub(file_sep, "/", paths)

	status = logical()
	pathos = list()
	for (aprog in progs) {
		aprog = paste(aprog,file_ext,sep="")
		inprog = sapply(c(ipath,paths),function(x){file.exists(paste(x,aprog,sep=file_sep))})
		pathos[[aprog]] = inprog
		if (!any(inprog)) {
			target  = "" 
			astatus = FALSE
		} else {
			target = names(inprog)[inprog][1]
			astatus = TRUE
		}
		names(astatus) = target
		status = c(status,astatus)
	}
	attr(status,"pathos") = pathos
	return(status)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~suggestPath

## suggestPath--------------------------2022-11-03
## Suggest a path for a specified program from PATH
## Arguments:
##  progs = vector of program names (without extension)
##  ipath = initial path by user to try before PATH directories
##  file_ext = alternative program extension if other
##    than `.exe` (primarily for Windows users)
## ---------------------------------------------RH
suggestPath <- function(progs, ipath=NULL, file_ext=NULL, bat_ext=NULL)
{
	isWin = .Platform$OS.type=="windows" #; isWin=FALSE
	path_sep = .Platform$path.sep
	#file_sep = ifelse(isWin, "\\", "/" )
	file_sep = ifelse(isWin, "\\", "/" )
	user_sep = "/"
	if (is.null (file_ext))
		file_ext = ifelse(isWin, ".exe", "" )
	if (!is.null(bat_ext))
		file_ext = ifelse(isWin, ".bat", ".sh" )
	sys_path = Sys.getenv( "PATH" )
	paths = strsplit(sys_path,path_sep)[[1]]
	paths = gsub("\\\\|/", user_sep, paths)
	paths = paths[!sapply(paths,.is.empty)]  ## sometimes get "" ???
	ipath = gsub("\\\\|/", user_sep, ipath)
#browser();return()

	status = logical()
	pathos = list()
	for (aprog in progs) {
		aprog = paste0(aprog, file_ext)
		inprog = sapply( unique(c(ipath,paths)), function(x){ file.exists(paste(x,aprog,sep=file_sep)) })
		pathos[[aprog]] = inprog
		if (!any(inprog)) {
			target  = "" 
			astatus = FALSE
		} else {
			target = names(inprog)[inprog][1]
			astatus = TRUE
		}
		names(astatus) = target
		status = c(status,astatus)
	}
#browser();return()
	attr(status,"pathos") = pathos
	return(status)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~suggestPath

## .win.suggestPath---------------------2022-11-03
## Function called by GUI to suggest paths for setup.
## ---------------------------------------------RH
.win.suggestPath=function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	isWin = .Platform$OS.type=="windows" #; isWin=FALSE
	#file_sep = ifelse(isWin, "\\", "/" )
	file_sep = ifelse(isWin, "\\", "/" )
	user_sep = "/"
	file_ext = ifelse(isWin, ".exe", "" )
	bat_ext  = ifelse(isWin, ".bat", ".sh" )
	suggestions = list()
	nPath =function(x){
		if (is.null(x) || x=="") return(NULL)
		else gsub("(\\\\|/)$", "", .normPath(x) )
	}
	if (isWin) {
		projpath.default = "."
		batpath.default  = "C:\\bat"
		unixbin.default  = "C:\\rtools\\bin"
		editpath.default = "C:\\Windows\\System32"
	} else {
		projpath.default = "."
		batpath.default  = "/usr"
		unixbin.default  = "/usr"
		editpath.default = "/usr"
	}

	ipath = paste(c( nPath(projpath), projpath.default), sep=file_sep)
	ipath = gsub("\\\\|/", user_sep, ipath)
	#proj_sugg = suggestPath( progs="project", ipath=ipath )
	#if (proj_sugg)
	#	suggestions[["projpath"]] = gsub(paste(file_sep,"bin$",sep=ifelse(isWin,file_sep,"")),"",names(proj_sugg))
	suggestions[["projpath"]]    = gsub("\\\\|/", user_sep, getwd())

	ipath = paste( c(nPath(batpath), batpath.default), sep=file_sep)
	ipath = gsub("\\\\|/", user_sep, ipath)
	bat_sugg = suggestPath(progs="Rgui", ipath=ipath, bat_ext=".bat")
	if (bat_sugg)
		suggestions[["batpath"]] = gsub( paste(file_sep,"bin$",sep=ifelse(isWin,file_sep,"")), "", names(bat_sugg))
	#else
	#	suggestions[["batver"]] = ""

	ipath = paste( c(nPath(unixbin), unixbin.default), sep=file_sep)
	ipath = gsub("\\\\|/", user_sep, ipath)
	unix_sugg = suggestPath(progs="make", ipath=ipath)
	if (unix_sugg)
		suggestions[["unixbin"]] = names(unix_sugg)

	if (isWin) editors = c("Uedit32","notepad++","cedt","runemacs","notepad")
	else       editors = c("gedit","Vim","vi")
	edit_sugg = FALSE

	## what if user specifies an editor with some weird extension (e.g., non.sense.old, fake_emacs)
	if (!is.null(editor) && editor!="") {
		ipath = gsub("\\\\|/", user_sep, editor)
		path_editor = dirname(editor)
		user_editor = basename(editor)
		edpcs = strsplit(user_editor,"\\.")[[1]]
		if (length(edpcs)==1) user_ext = ""
		else {
			user_ext = paste(".",rev(edpcs)[1],sep="")
			user_editor = paste(rev(rev(edpcs)[-1]),collapse=".")
		}
		ipath = paste( c(nPath(path_editor), editpath.default), sep=file_sep)
		ipath = gsub("\\\\|/", user_sep, ipath)
		edit_sugg = suggestPath(progs=user_editor, ipath=ipath, file_ext=user_ext)
	}
	if (edit_sugg)
		suggestions[["editor"]] = paste(names(edit_sugg)[edit_sugg][1],user_sep,user_editor,user_ext,sep="")
	else {
		edit_sugg = suggestPath(progs=editors, ipath=nPath(editor))
		if (any(edit_sugg))
			suggestions[["editor"]] = paste(names(edit_sugg)[edit_sugg][1],user_sep,editors[edit_sugg][1],file_ext,sep="")
	}
	if (length(suggestions)>0)
		setWinVal(suggestions,winName=winName)
#browser();return()
#suggestions<<-suggestions
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.suggestPath


## miscellanous-------------------------2009-02-11
.addQuotes=function(str)
{
	return(paste("\"",str,"\"",sep=""))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.addQuotes
.asIs=function(x)
{
	if (is.numeric(x)) x=format(x,scientific=FALSE)
	return(x)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.asIs
.callSys <- function(..., wait=TRUE)
{
	## note dots is not interpreted as expected, uses only first in list
	dots=list(...)
	if (.Platform$OS.type=="windows") {
		if("edit"%in%names(dots))
			dots[[1]]=paste("start \"\"",dots[[1]],sep=" ")
		out=shell(dots,intern=TRUE) }
	else {
		cmd <- unlist(list(...))
		if( wait == FALSE )
			out=system(cmd,wait=FALSE)
		else
			out=system(cmd,intern=TRUE)
	}
	invisible(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.callSys
.changeWD <- function( wd )
{
	if( missing( wd ) )
		wd <- selectDir() ## tclvalue(tkchooseDirectory())
	if( wd != "" ) {
		currentdir.values <- sort( unique( c( wd, getWinVal()$currentdir.values ) ) )
		setWinVal( list( currentdir.values = currentdir.values ) )
		setWinVal( list( currentdir = wd ) )
		setwd( wd )
		.load.prefix.droplist()
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.changeWD
.changeWDEnter <- function()
{
	## called by droplist when user hits enter
	wd <- getWinVal()$currentdir
	## remove trailing slash
	wd <- gsub("(\\\\|/)$", "", wd )
	if( file.exists( wd ) )
		.changeWD( wd )
	else {
		showAlert( paste( "unable to set working directory to \"", wd, "\" - does not exist", sep="" ) )
		.changeWD( getwd() )
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.changeWD
.chooseCols=function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	if (is.null(prefix) || prefix=="")
		return()
	inFile=paste(prefix, ".mc.dat", sep="")
	if(!file.exists(inFile)){
		showAlert(paste("Cannot find file", inFile, "in working directory."))
		return()
	}
	if (!exists(inFile,envir=.GlobalEnv) || is.null(attributes(get(inFile))$mtime) ||
			file.info(inFile)$mtime!=attributes(get(inFile))$mtime)
		inData=readRep(prefix,".mc.dat")
	else
		inData=get(inFile)
	flds=names(inData)
	nc=length(flds)
	useCols=jtcall(PBSproj)$useCols
	if (is.null(useCols) || length(useCols)!=nc)
		useCols=rep(TRUE,nc)

	## store feilds as a data.frame - for use with scrolling object widget
	choices <- as.data.frame( useCols )
	rownames( choices ) <- flds

	## converts data.frame scrolling object back into vector and saves to global var
	saveCols <- function()
	{
		choices <- getWinVal(winName="chooseCols")$choices
		jtget(PBSproj)
		PBSproj$useCols=choices[[ 1 ]]
		jtput(PBSproj)
		closeWin("chooseCols")
	}
	toggleSelected <- function()
	{
		winName <- "chooseCols" 
		choices <- getWinVal(winName = winName )$choices
		if( any( choices[[1]] ) )
			choices[[1]] <- choices[[1]] & FALSE #force all false
		else
			choices[[1]] <- choices[[1]] | TRUE #force all true
		setWinVal( list( choices = choices ), winName = winName )
	}
	winDesc = c("window name=chooseCols title=Choose",
		"object choices rowshow=20",
		"grid 1 2",
		"button text=\"select all/none\" func=toggleSelected padx=\"0 10\"",
		"button text=OK bg=skyblue function=saveCols" )
		
	createWin(winDesc, astext = TRUE)
}

## given a vector of paths, return the last directory name of each path
.getDirName <- function( path )
{
	dirname <- gsub("\\\\", "/", path )
	return( unlist( lapply( strsplit( dirname, "/" ), function(x) x[length(x)] ) ) )
}

.load.prefix.droplist <- function()
# Repopulates droplist with all prefixes in current directory
# TODO we might want a "refresh" button on the GUI to call this
# ideally we could have the droplist call a function *BEFORE* 
# it drops down - thus refreshing the list
{
	choices <- findPrefix( ".tpl" )
	setWinVal( list( prefix.values = choices ) )
	if (any(choices=="vonb")) ch1 = grep("vonb",choices) else ch1 = 1
	setWinVal( list( prefix = choices[ ch1 ] ) )
}

.normPath = function(path, winslash="\\", mustWork=FALSE) {
	normalizePath( path, winslash, mustWork )
}

## .version-----------------------------2018-10-02
.version = function(x) {
	if (is.null(x) || is.numeric(x)) return(x)
	xpc = strsplit(x,split="\\.")[[1]]
	npc = !grepl("[[:alpha:]]",xpc)  ## only numerics assuming alternative is alphanumeric
	xnu = as.numeric(paste(xpc[npc],collapse="."))
	return(xnu)
}

.win.findTPL=function(suffix=".tpl",winName="PBSproj"){ 
	choice=findPrefix(suffix) 
	chooseWinVal(choice,"prefix",winname=winName) 
	invisible()
}

.is.empty = function(x)
{
	if (is.null(x) || all(is.na(x)) || length(x)==0 || all(x==""))
		return(TRUE)
	else
		return(FALSE)
}
