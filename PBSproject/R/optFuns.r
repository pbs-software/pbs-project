##================================================
##             OPTIONS FUNCTIONS                  
##================================================

.initOptions <- function()
{
	## do not re-iniitialize if `.PBSproj' exists in the package environment
	if(exists(".PBSproj", envir=.PBSprojEnv) && is(jtcall(.PBSproj),"PBSoptions"))
		return()
	readPRJopts()
}

## setupPRJ------------------------------2014-02-26
## Command line initialization to read in path
## information and check for the presence of executables:
## batpath  -- Rgui.bat, etc.bat
## projpath -- nothing specific because it's a user's project
## unixbin  -- make
## editor   -- e.g. notepad, but can be any valid editor.
## ---------------------------------------------RH
setupPRJ = function(pathfile)
{
	if (missing(pathfile) || is.null(pathfile) || !file.exists(pathfile))
	pathfile = "PRJpaths.txt"
	.initOptions()
	readPRJpaths(pathfile)
	checkPRJopts()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~setupPRJ


## checkPRJopts-------------------------2022-11-03
## Checks path locations specified by PRJ options.
## ---------------------------------------------RH
checkPRJopts=function(opts=getOptions( jtcall(.PBSproj) ),
   check=c("projpath","batpath","unixbin","editor"),
   warn=TRUE, popup=FALSE, verify=TRUE)
{
	sAF = options()$stringsAsFactors
	on.exit(options(stringsAsFactors=sAF))
	options(stringsAsFactors=FALSE)
	isWin = .Platform$OS.type=="windows" #; isWin=FALSE
	slash = ifelse(isWin, "\\", "/" )
	
	## assume g++ is always available on Unix machines, so don't check it.

	## Check that .PBSproj has all required paths and that links point to actual files on the hard drive.
	## Check for project, batch, unix, and editor programs
	mess=list()
	for (i in 1:length(opts)) {
		ii = names(opts)[i]
		if (!any(ii==check)) next
		if (.is.empty(opts[[ii]])) {
			ipath = "."
			progs = paste0("no_", ii)
		} else {
			iii = ipath = convSlashes(opts[[ii]], os="unix")
			use.cwd = iii %in% c("",".")
			if (use.cwd) iii = getwd() #"."  ## set to current working directory
			#ipath = convSlashes(iii, os="unix")
			progs = paste0("no_", ii)
			if (ii=="projpath") {
				if (file.exists(ipath))
					progs = paste0("usr_", ii)
			} else if (ii=="batpath") {
				#if (!use.cwd)
				#	ipath = convSlashes(paste0(iii,slash), os="unix")#,"bin")
				progs = list.files(ipath, pattern="\\.bat$")[1:5]
				if (all(is.na(progs))) progs = paste0("no_", ii)
				if (any(is.na(progs))) progs = progs[!is.na(progs)]
#browser();return()
			} else if (ii=="unixbin") {
				progs = paste0(c("grep","make","sed"),ifelse(isWin,".exe",""))
			} else if (ii=="editor") {
				#if (use.cwd)
				#	progs = "editor"
				#else {
					progs = basename(ipath)
					epath = convSlashes(dirname(ipath), os="unix")
					if ( progs %in% list.files(epath) )
						progs = paste0("usr_", ii)
					else
						progs = paste0("no_", ii)
				#}
#browser();return()
			}
		}
		if ( all(grepl("^usr\\_",progs)) ) {
			istatus = TRUE
		} else if ( all(grepl("^no\\_",progs)) ) {
		#all(progs=="editor") || all(progs=="none")) {
			istatus = FALSE
		} else if (!isWin && ii %in% c("unixbin")) {
			ipath = "/usr"; istatus = TRUE
		} else {
			target = paste(ipath,progs,sep=slash)
			istatus = file.exists(target)
		}
		names(istatus) = progs
		mess[[i]] = istatus
		names(mess)[i] = ipath
	}
#browser();return()

	PRJstatus = all(unlist(mess)==TRUE)
	attr(PRJstatus,"status") = mess
	vmess=unlist(mess)
	names(vmess) = paste(rep(names(mess), lapply(mess,length)), unlist(lapply(mess,names)), sep="/")
	attr(PRJstatus,"message") = vmess
	if (warn|popup) {
		if (all(vmess==TRUE)) {
			if(warn && verify) cat("All programs found\n\n")
			#if(popup) showAlert("All programs found","Continue","info") }
		 } else {
			badmess = paste("Programs not found:\n----------------------\n", paste(names(vmess)[!vmess],collapse="\n"),
				"\n\nAlter the path file (default 'PRJpaths.txt') in the working directory.\n",
				"     ~~~OR~~~\n",
				"If using the PBSproj GUI, alter the path entries in the Setup tab.\n\n",sep="")
			if (isWin && popup) {
				badmess <- paste( badmess, "If you need to install PRJ, see 'PRJ Installation' manual in Help dropdown menu.\n\n", sep="" )
			}
			if (warn) cat(badmess)
			if (popup) showAlert(badmess,"User action required","warning") 
		}
	}
#browser();return()
	## check for sed.exe when all programs above are found
	if (isWin && all(vmess)) {
		opts.sed = opts[c("unixbin","projpath","batpath")]
		opts.sed = lapply(opts.sed,function(x){if(x=="") "." else x})
		sedmess=list()
		for (i in 1:length(opts.sed)) {
			ii = names(opts.sed)[i]
			if (!any(ii==check)) next
			iii = ipath = opts.sed[[ii]]
			if (ii=="unixbin" || iii==".")
				progs = paste0("sed",ifelse(isWin,".exe",""))
			else if (ii %in% c("projpath","batpath"))
				progs = paste0("bin",slash,"sed",ifelse(isWin,".exe",""))
			target=paste(ipath,progs,sep=slash)
			istatus = file.exists(target)
			names(istatus) = progs
			sedmess[[i]] = istatus
			names(sedmess)[i] = ipath
		}
		smess=unlist(sedmess)
		names(smess) = paste(rep(names(sedmess), lapply(sedmess,length)), unlist(lapply(sedmess,names)),sep=slash)
		if (warn|popup) {
			if (any(smess)) {
				# if(warn) cat(paste("'sed",ifelse(isWin,".exe'",",")," program found\n\n",sep="")) ## do not warn
			}
			else {
				## always warn
				badsedmess=paste("Exception: the program 'sed",ifelse(isWin,".exe",""),"' was not found on any of these paths:\n",
					paste(gsub(paste0("sed",ifelse(isWin,".exe","")),"",names(smess))[!smess],collapse="\n"),
					"\n\nPlace a copy of 'sed",ifelse(isWin,".exe",""),"' on any one of the paths indicated above.\n\n",sep="")
				if (warn) cat(badsedmess)
				if (popup) showAlert(badsedmess,"User action required","warning") 
			}
		}
	}
	invisible(PRJstatus)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~checkPRJopts

##--------------------------------------2018-10-03
.win.checkPRJopts = function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	verify = getWinAct(winName="PBSproj")[1]=="verify"
	chkstat = checkPRJopts(opts=list(projpath=projpath, batpath=batpath, unixbin=unixbin, editor=editor), popup=TRUE, verify=verify)
	## set label to OK/FIX with coloured background
	setWinVal(list(chkstat=ifelse(chkstat," OK"," Fix")),winName=winName)
	setWidgetColor( "chkstat", winName=winName, bg=ifelse(chkstat,"lightgreen","pink") )
	setWidgetColor( "checkbutton", winName=winName, bg=ifelse(chkstat,"moccasin","pink") )
	.win.checkPRJpath(winName)
	#.win.setPRJver(winName)
	invisible(chkstat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.checkPRJopts

##--------------------------------------2018-10-01
.win.checkPRJpath=function(winName="PBSproj")
{
	jtget(.PBSproj)
	for (i in c("projpath","batpath","unixbin","editor")) {
		wval = getWinVal()[[i]]
		ival = getOptions(.PBSproj,i)
		if (is.null(ival) || wval!=ival) {
			mess = paste0("setOptions(.PBSproj,",i,"=",deparse(wval),")")
			eval(parse(text=mess))
		}
		
	}
	jtput(.PBSproj)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.checkPRJpath


## readPRJopts---------------------------2022-11-03
## Read PRJ options from a file.
## Only called by `.initOptions()'
## ---------------------------------------------RH
readPRJopts <- function(optfile="PRJopts.txt")
{
	## Create instance of option manager - use this to get/set/save/load options
	## First attempt to load options from the package, then attempt to load options from the current dir (which will override pkg options)
	sysfile = system.file(package="PBSproj")
	#if (is.null(sysfile) || all(is.na(sysfile)) || length(sysfile)==0 || all(sysfile==""))
	if (.is.empty(sysfile))
		sysfile = getwd()
	if (.is.empty(getWinVal()$optfile))
		optfile = "PRJopts.txt"
	pkg_fname = paste0( sysfile, "/", optfile)
	.PBSproj.pkgOptions <- new( "PBSoptions", filename = pkg_fname, initial.options = list(projpath="", batpath="", unixbin="", editor=""), gui.prefix="" )
	jtput(.PBSproj.pkgOptions)

	## Load from current dir, using pkgOptions as default values
	.PBSproj <- new( "PBSoptions", filename = optfile, initial.options = getOptions( .PBSproj.pkgOptions ), gui.prefix="")

	## Standardise paths with OS path separators
	allpaths = getOptions(.PBSproj)[c("projpath","batpath","unixbin","editor")]
	ospaths  = lapply(allpaths,convSlashes, os="unix")
	setWinVal(ospaths, winName="PBSproj")
	setWinVal(list(optfile=optfile), winName="PBSproj")
	setWinVal(list(currentdir=getwd()), winName="PBSproj")
	setOptions(.PBSproj, ospaths)
	jtput(.PBSproj)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~readPRJopts


## writePRJopts--------------------------2015-01-23
## Writes PRJ options to a file.
## Demote this to an automatic back-up file if user 
## saves the PRJpaths file.
## ---------------------------------------------RH
writePRJopts <- function(optfile="PRJopts.txt")
{
	## save to current dir
	jtget(.PBSproj)
	saveOptions( .PBSproj, optfile )

	## save to pkg dir (don't change fname)
	opts <- getOptions( .PBSproj )
	jtget(.PBSproj.pkgOptions)
	setOptions(.PBSproj.pkgOptions,opts)
	jtput(.PBSproj.pkgOptions)
	saveOptions( .PBSproj.pkgOptions )
	return(invisible(NULL))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~writePRJopts


## readPRJpaths--------------------------2014-02-25
## Read PRJ paths from a simple 2-column file.
## Allows user to pass in a file easily made by hand.
## Assumes .PBSproj options object exists.
## ---------------------------------------------RH
readPRJpaths = function(pathfile)
{
	sAF = options()$stringsAsFactors
	on.exit(options(stringsAsFactors=sAF))
	options(stringsAsFactors=FALSE)
	if (!missing(pathfile) && !is.null(pathfile) && file.exists(pathfile)) {
		ufile = read.table(file=pathfile,header=FALSE,col.names=c("target","path"))
		uopts = split(ufile$path,ufile$target)
		jtget(.PBSproj)
		setOptions(.PBSproj,uopts)
		jtput(.PBSproj)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~readPRJpaths
.win.readPRJpaths = function(winName="PBSproj")
{
	pathfile = getWinVal()$optfile
	readPRJpaths(pathfile)
	loadOptionsGUI( jtcall(.PBSproj) )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.readPRJpaths


## savePRJpaths--------------------------2018-09-27
## Save PRJ paths to a simple 2-column file.
## Assumes .PBSproj options object exists.
## ---------------------------------------------RH
savePRJpaths = function(pathfile)
{
	sAF = options()$stringsAsFactors
	on.exit(options(stringsAsFactors=sAF))
	options(stringsAsFactors=FALSE)
	if (missing(pathfile) || is.null(pathfile)) pathfile="PRJpaths.txt"
	isWin = .Platform$OS=="windows"
	Uopts = getOptions(jtcall(.PBSproj))
	upath = if (isWin) c("projpath","batpath","unixbin","editor") else c("projpath","batpath","editor")
	uopts = Uopts[upath]
	ufile = t(sapply(upath,function(x,u){
		c(x,paste(rep(" ",10 - nchar(x)),collapse=""),convSlashes(u[[x]], os="unix", addQuotes=TRUE))
		}, u=uopts, USE.NAMES=FALSE))
	write.table(ufile, file=pathfile, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="")
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~savePRJpaths
.win.savePRJpaths = function(winName="PBSproj")
{
	.win.setPRJpath()
	pathfile = getWinVal()$optfile
	savePRJpaths(pathfile)
	writePRJopts() ## automatic backup to `PRJopts.txt' (only if user pushes the Save button)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.savePRJpaths


## setPRJpath----------------------------2018-09-27
## Sets the PRJ path directories.
## --------------------------------------------ACB
setPRJpath <- function( batpath, projpath, unixbin, editor )
#setPRJpath <- function( editor )
{
	.initOptions()
	jtget(.PBSproj)
	if( missing( batpath ) == FALSE )
		setOptions( .PBSproj, batpath = batpath )
	if( missing( projpath ) == FALSE )
		setOptions( .PBSproj, projpath = projpath )
	if( missing( unixbin ) == FALSE )
		setOptions( .PBSproj, unixbin = unixbin )
	if( missing( editor ) == FALSE )
		setOptions( .PBSproj, editor = editor )
	jtput(.PBSproj)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~setPRJpath
.win.setPRJpath=function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	setPRJpath(batpath, projpath, unixbin, editor) 
	.win.checkPRJopts()
	#.win.setPRJver(winName)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.setPRJpath


## .setPath-----------------------------2022-11-03
## Set the temporary environment path to run PRJ.
## -----------------------------------------ACB/RH
.setPath <- function(pathfile)
{
	path_sep <- .Platform$path.sep
	#dir_sep <- ifelse( .Platform$OS.type == "windows", "\\", "/" )
	dir_sep <- ifelse( .Platform$OS.type == "windows", "/", "/" )
	
	## User can specify a 2-column pathfile (no headers)
	if (!missing(pathfile) && !is.null(pathfile) && file.exists(pathfile))
		readPRJpaths(pathfile)

	#admb_home <- getOptions( jtcall(.PBSproj), "batpath" )
	#admb_path <- paste( admb_home, "bin", sep = dir_sep )
	proj_path <- getOptions( jtcall(.PBSproj), "projpath" )

	if( .Platform$OS.type == "windows" ) {
		bat_path  = getOptions( jtcall(.PBSproj), "batpath")
		unix_path = getOptions( jtcall(.PBSproj), "unixbin")
		wsys_path = paste(Sys.getenv()["SystemRoot"],"System32", sep=dir_sep)
		path <- paste(.normPath(admb_path,dir_sep), .normPath(msys_path,dir_sep), .normPath(gcc_path,dir_sep), .normPath(wsys_path,dir_sep), sep=path_sep)
	} else {
		## linux must include original path so programs like cat, sed are found
		sys_path <- Sys.getenv( "PATH" )
		path <- paste(.normPath(proj_path,dir_sep), .normPath(sys_path,dir_sep), sep=path_sep)
	}
	Sys.setenv( PATH = path )
	#Sys.setenv( PRJ_HOME = gsub("/*$","",gsub("\\\\*$","",.normPath(admb_home,dir_sep)) ) ) #ensure no trailing slash (`/') or (`\\') exists
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.setPath


## setPRJver---------------------------2015-01-27
## Sets the PRJ versions.
## Now simplified to always read in versions if admb/g++ exist.
## ---------------------------------------------RH
setPRJver <- function( admbver, gccver )
{
	.initOptions()
	isWin = .Platform$OS.type=="windows" #; isWin=FALSE
	sayWhat = attributes(checkPRJopts(warn=FALSE))$status
	jtget(.PBSproj)
	opts = getOptions(.PBSproj)

	if(all(sayWhat[[1]])) { # check PRJ version
		if(file.exists(paste(opts["batpath"],"/VERSION",sep="")))
			setOptions(.PBSproj, admbver = readLines(paste(opts["batpath"],"/VERSION",sep=""))[1] )
	} else {
		setOptions(.PBSproj, admbver = "")
	}
	if(all(sayWhat[[2]])) { # check g++ version
			cmd = "g++ --version"
			if (isWin) cmd = shQuote(paste(opts["projpath"],"/bin/",cmd,sep=""))
			gccVer = .callSys(cmd)[1]
			## return the whole string minus `g++ '
			gccver = PBSmodelling::.trimWhiteSpace(gsub("g\\+\\+","",gccVer))
			setOptions(.PBSproj, gccver = gccver)
	} else {
		setOptions(.PBSproj, gccver = "")
	}
	jtput(.PBSproj)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~setPRJver
.win.setPRJver=function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	#setPRJver(admbver,gccver) 
	for (i in c("admbver","gccver")) {
		ival = getOptions(jtcall(.PBSproj),i)
		if (is.null(ival)) next
		mess = paste0("setWinVal(list(",i,"=\"",ival,"\"),winName=winName)")
		eval(parse(text=mess))
	}
	#.win.checkPRJopts()
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.setPRJver


## showPRJargs---------------------------2015-01-27
## Show the PRJ arguments
## ---------------------------------------------RH
showPRJargs <- function(prefix,ed=TRUE)
{
	if( .Platform$OS.type == "windows" )
		p.exe <- paste(prefix,".exe", sep="")
	else
		p.exe <- paste("./", prefix, sep="") #TODO verify
	p.arg <- paste(prefix,".arg", sep="")
	p.err <- paste("File",p.exe,"does not exist.\n",sep=" ")
	p.cmd <- paste(p.exe,"-?",sep=" ")

	p.cmd=shQuote(p.cmd)
	if (file.exists(p.exe)) p.out <- .callSys(p.cmd) else p.out <- p.err

	if (ed) {writeLines(p.out,p.arg)
		editPRJfile(p.arg)
	} else {
		cat(paste(p.out,collapse="\n"))
		cat(paste(p.arg,collapse="\n"))
	}
	invisible(p.out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~showPRJargs
.win.showPRJargs=function(winName="PBSproj")
{
	getWinVal(scope="L",winName=winName)
	showPRJargs(prefix=prefix) 
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.showPRJargs
.win.showGUIargs=function(winName="PBSproj")
{
	## display the argument vector in the GUI based on radio selection
	getWinVal(scope="L",winName=winName)
	if (runType=="normal") setWinVal(list(argvec=""),winName=winName)
	else if (runType=="mcmc") setWinVal(list(argvec=paste("-mcmc",.asIs(nsims),"-mcsave",.asIs(nthin),sep=" ")),winName=winName)
	else if (runType=="lprof") setWinVal(list(argvec="-lprof"),winName=winName)
	else setWinVal(list(argvec=argvec),winName=winName) 
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.showGUIargs
