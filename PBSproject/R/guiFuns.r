## chooseProj---------------------------2022-11-15
##  Choose a repository for code for transfer.
## ---------------------------------------------RH
chooseProj = function(projpath="", include.cwd=FALSE, display.full.names=FALSE)
{
	pkgs = character()
	if (include.cwd) {
		pkgs = c(pkgs, getwd())
	}
	if (missing(projpath) || .is.empty(projpath)) { ## redundant but c'est la vie
		if (exists(".PBSmodEnv")){
			if ("PBSproj" %in% names(.PBSmodEnv[[".PBSmod"]])) {
				projpath = getWinVal(winName="PBSproj")$projpath
			}
		}
		if (.is.empty(projpath)) {
			projpath = getwd()
		}
		pkgs  = c(pkgs, list.dirs(projpath,full.names=TRUE,recursive=FALSE))
		## Exclude the specified 'pkgname' as this is generally the target
		pkgs  = pkgs[grep(getWinVal(winName="PBSproj")$pkgname, basename(pkgs), invert=TRUE)]
	} else {
		## In case user specifies particular directories outside of projpath
		pkgs = projpath
	}
pkgs = c(pkgs, paste0(projpath,"/",c("sumtingwong", "beautiful_laundrette"))) ## just for testing
	if (.is.empty(pkgs)){
		mess = "No potential candidates detected or specified"
		junk = showAlert(mess); stop(mess)
	}
	npkgs = length(pkgs)
	bnams = basename(pkgs)
	if (any(grepl(getwd(), pkgs))) {
		bpos = grep(getwd(), pkgs)
		bnams[bpos] = paste0(bnams[bpos],"_(cwd)")
	}
	ipacs = character()
	for (i in 1:npkgs) {
		ipacs = c(ipacs, paste0("   radio name=from value=", pkgs[i], " text=", if (display.full.names) pkgs[i] else bnams[i], " mode=character sticky=W padx=5"))
	}
	wlistP <- c(
		"window name=\"chooseP\" title=\"Choose a code repository\" bg=moccasin",
		"label text=\"Potential code candidates\" font=\"11 bold\" sticky=W",
		"label text=\"Choose one:\" sticky=W",
		paste0("grid ", npkgs, " 1 sticky=W"),
		ipacs,
		"grid 1 1 sticky=E",
		#"   button text=\"R file\" bg=mistyrose sticky=E func=openProjFile action=R",
		"   button text=Done bg=green sticky=E func=doAction action=\"closeWin(name=`chooseP`)\"",
		"")
#assign("w1",wlistP,envir=.GlobalEnv) ## debugging
#assign("junk",pkgs,envir=.GlobalEnv) ## debugging
	createWin(wlistP, astext=TRUE)
}
.win.chooseProj = function(winName="PBSproj"){
	chooseProj(projpath="", include.cwd=T, display.full.names=T)
	#chooseProj(projpath=c("C:/Users/haighr/Files/Temp/R/AppE_Equations","C:/Users/haighr/Files/GFish/PSARC22/CAR/Docs/RD/AppF_Results"), include.cwd=T, display.full.names=T)
}
## Create a GUI to choose files for transfer to user's project (RH 221115)
.win.chooseCode = function(winName="chooseP")
{
	frompath = try(getWinVal(winName=winName)$from, silent=TRUE)
	## Check that a source repository has been selected
	frompath = try(getWinVal(winName="chooseP")$from, silent=TRUE)
	if (inherits(frompath, "try-error"))
		.win.chooseProj(winName="PBSproj")
	code.names = list.files(from, pattern="\\.[rR]$|Win\\.txt$|\\.Rmd$")
	code.names = grep("^junk|^crap|^fornow", code.names, invert=TRUE, value=TRUE)
	ncode = length(code.names)
	wlistC <- c(
		"window name=\"chooseC\" title=\"Choose code name(s) to copy\" bg=honeydew",
		"label text=\"Potential code candidates\" font=\"11 bold\" sticky=W",
		"label text=\"Select one or many:\" sticky=W",
		paste0("vector length=", ncode, " mode=logical names=code vecnames=\"", paste0(code.names, collapse=" "), "\" labels=\"", paste0(code.names, collapse=" "), "\" values=\"", paste0(rep("F",ncode),collapse=" "), "\" vertical=T"),
		"grid 1 2 sticky=E",
		"   button text=Add  bg=pink sticky=E function=.win.addCode action=addCode",
		"   button text=Done bg=green sticky=E func=doAction action=\"closeWin(name=`chooseC`)\"",
		"")
#assign("wC",wlistC,envir=.GlobalEnv) ## debugging
	createWin(wlistC, astext=TRUE)
}

## Create a GUI to select files for editing (RH 221115)
.win.editCode = function(winName="PBSproj")
{
	focusWin(winName=winName)
	projpath = try(getWinVal(winName=winName)$projpath, silent=TRUE)
	if (inherits(projpath, "try-error")) 
		projpath = selectDir(usewidget="projpath")
	editor = getWinVal(winName="PBSproj")$editor
	if (!file.exists(editor))
		editor = selectDir(usewidget="editor")

	code.names = list.files(projpath, pattern="\\.[rR]$|Win\\.txt$|\\.Rmd$", recursive=T, full.names=F)
	ncode = length(code.names)
	wlistE <- c(
		"window name=\"editC\" title=\"Choose code name(s) to edit\" bg=lightyellow",
		"label text=\"Potential code candidates\" font=\"11 bold\" sticky=W",
		"label text=\"Select all or some:\" sticky=W",
		paste0("vector length=", ncode, " mode=logical names=code vecnames=\"", paste0(code.names, collapse=" "), "\" labels=\"", paste0(code.names, collapse=" "), "\" values=\"", paste0(rep("F",ncode),collapse=" "), "\" vertical=T sticky=E"),
		"grid 1 2 sticky=E",
		"   button text=Open bg=aliceblue sticky=E func=.win.openE action=.win.openE",
		## Following line of code not dynamic enough:
		#paste0("   button text=Open bg=aliceblue sticky=E func=doAction action=\"shell(cmd=`", noquote(eval(editor)), "  ", paste0(names(getWinVal(winName="editC")$code)[getWinVal(winName="editC")$code], collapse=" "), "`, intern=T)\"" ), ## not dynamic
		"   button text=Done bg=green sticky=E func=doAction action=\"closeWin(name=`editC`)\"",
		"")
#assign("wE",wlistE,envir=.GlobalEnv) ## debugging
	createWin(wlistE, astext=TRUE)
}
## Create a shell command to open user-specified files (RH 221115)
.win.openE   = function(winName="editC") {
	editor    = getWinVal(winName="PBSproj")$editor
	projpath  = getWinVal(winName="PBSproj")$projpath
	code.use  = names(getWinVal(winName=winName)$code)[getWinVal(winName=winName)$code]
	code.full = paste0(projpath, "/", code.use)
	command   = paste0(eval(editor), "  ", paste0(code.full, collapse=" "))
#browser();return()
	shell(cmd=command, intern=TRUE)
}


## openProjFile-------------------------2022-11-15
##  Open a project file for editing.
## ---------------------------------------------RH
openProjFile = function(type="R", from, to)
{
	pnam = getWinVal(winName="chooseP", scope="L")$from
	tdir = paste0(pnam,"/",type)
	if (file.exists(tdir)) {
		tfiles  = list.files(tdir, pattern=paste0("\\.",tolower(type),"$"), full.names=TRUE,recursive=FALSE)
		ntfiles = length(tfiles)
		ipacs = character()
		if (ntfiles>0) {
			bnams = basename(tfiles)
			ipacs = character()
			for (i in 1:ntfiles) {
				ipacs = c(ipacs, paste0("   radio name=tnam value=", tfiles[i], " text=", bnams[i], " mode=character sticky=W padx=5 func=doAction action=\"tnam=getWinVal(winName=`chooseT`)$tnam; switch(getWinVal(winName=`chooseT`)$display, openFile(tnam), .win.display(tnam))\""))
			}
		}
		wlistT <- c(
			"window name=\"chooseT\" title=\"Choose a file\" bg=aliceblue",
			paste0("label text=\"\"", type, "\" file candidates\" font=\"11 bold\" sticky=W width=20"),
			"grid 1 3 sticky=W",
			"   label text=\"Choose one:\" sticky=W",
			"   radio name=display value=1 text=editor mode=numeric sticky=W selected=T",
			"   radio name=display value=2 text=window mode=numeric sticky=W",
			paste0("grid ", ntfiles, " 1 sticky=W"),
			ipacs,
			#paste0("button text=\"Open\" bg=darkolivegreen1 sticky=E func=openFile action=\"",basename(getWinVal(winName="chooseT")$tnam),"\""),
			"")
		createWin(wlistT, astext=TRUE)
#browser();return()
	} else {
		mess = paste0("'", basename(pnam), "' does not appear to be a package\n", "  Missing directory:\n  ", tdir, "\n")
		junk = showAlert(mess)
		stop(mess)
	}
}
.win.display = function(tnam)
{
	#fornow = readLines(tnam)[1:120]
	fornow = readChar(tnam, file.info(tnam)$size)
	fornow = deparse(paste0(gsub("'|`","\\\"",fornow), collapse="xyz"), width.cutoff=500L)
	fornow = gsub("xyz", "\n", fornow)
	
	txt1 = readLines(tnam)#[1:120]
	txt2 = gsub("\\\\","\\\\\\",txt1)    ## need to escape double quotes
	txt3 = gsub("'|`","\\\\'",txt2)      ## 'text' widget does not like single quotes
	txt4 = gsub("\\t","\\\\t",gsub("\\n","\\\\n",txt3))
	txt5 = paste0(txt4,collapse="\n")
#writeLines(fornow,"fornow.r")
#crap = paste0("text name=wtxt3 height=50 width=125 relief=solid edit=TRUE value=", noquote(fornow), " font=\"consolas 10\" bg=mintcream")
#writeLines(crap,"crap.r")
	#fornow = paste0("text name=wtxt3 height=50 width=125 relief=solid edit=TRUE value=\"", paste0(txt1,collapse="\n"), "\" font=\"consolas 10\" bg=mintcream")
	wlistE <- c(
		"window name=\"editT\" title=\"Peruse and edit a file\"",
		paste0("text name=wtxt3 height=50 width=125 relief=solid edit=TRUE value=", noquote(fornow), " font=\"consolas 10\" bg=mintcream"),
		"button function=.runPrHelper text=\"create window from source\" action=\"__USE_EDIT__\" pady=5 padx=20",
		"")
assign("w3",wlistE,envir=.GlobalEnv)
	createWin(wlistE, astext=TRUE)
}
