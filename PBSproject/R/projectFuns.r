## =========================================================
## R function to create a project from the command line.
## Function has been demoted to only creating new project files.
## =========================================================

## createProject------------------------2023-01-12
##  Create new project (JTS wants non-GUI start-up function)
## ---------------------------------------------RH
createProject = function(projname="project", projpath=".", style="default", overwrite=TRUE)
{
	rtargs = paste0(projpath, "/", projname, c(".r","Win.txt"))
	oname  = paste0(projpath, "/", projname,"Opts.txt")  ## stored user options 
	if (file.exists(oname))
		readPBSoptions(fname=oname)

	if (!overwrite){
		rnew = paste0(projpath,"/",projname,".r")
		wnew = paste0(projpath,"/",projname,"Win.txt")
		if (file.exists(rnew) && file.exists(wnew)){
			createWin(wnew)
			mess = paste0("Existing project '", projname, " has been opened")
			message(mess)
			#openProject(projname="project", projpath=".")
		} else {
			mess = paste0("Project '", projname, "' files (", projname, ".r, ", projname, "Win.txt) do not exist in working directory <", projpath, ">.")
			showAlert(mess); stop(mess)
		}
	} else {
		## Create new R and WDF from the templates
		tmpldir = system.file(paste0("templates/style", ifelse(style=="default","00",pad0(style,2))), package="PBSproject")
		rdotdir = system.file("templates/rcode", package="PBSproject")
		if (tmpldir=="" || rdotdir=="")
			tmpldir = rdotdir = getwd()  ## temporary for development and debugging
		setPBSoptions("template", list(style=style, tmpldir=tmpldir))

		for (i in rtargs) {
			ii = basename(i)
			## Create R code form 'pbsProject.r'
			if (grepl("\\.[rR]$", ii)) {
				rbase = paste0(tmpldir, "/pbsProject.r")
				rdots = paste0(rdotdir, "/dotProject.r")
				mess = NULL
				if (!file.exists(rbase) || !file.exists(rdots)){
					if (!file.exists(rbase))
						mess = c(mess, paste0("R code file\n'", rbase, "'\n   does not exist in template directory.\n"))
					if (!file.exists(rdots))
						mess = c(mess, paste0("R code file\n'", rdots, "'\n   does not exist in template directory.\n"))
					showAlert(mess); stop(mess)
				}
				rcode = c(readLines(con=rbase), readLines(con=rdots))
				pline = setdiff( grep("@projname",rcode), grep("STATIC",rcode) ) ## STATIC -- no longer used after PBSproject v.0.0.5
				dline = setdiff( grep("@date",rcode), grep("STATIC",rcode) )     ## STATIC
				uline = union(pline,dline)
				rcode[uline] = gsub("@projname", projname, sub("@date", Sys.time(), rcode[uline])) ## STATIC
				#rcode = rcode[-(rev(grep("END CODE",rcode))[1]:length(rcode))]  ## remove recursive calls
				writeLines(text=rcode, con=i)
				setPBSoptions("template", list(rfile=rbase), sublist=TRUE)
			}
			## Create Windows Description file
			if (grepl("Win\\.txt$", ii)) {
				wbase = paste0(tmpldir, "/pbsProjectWin.txt")
				if (!file.exists(wbase)){
					mess = paste0("Windows description file '", wbase, "'\n   does not exist in current working directory.")
					showAlert(mess); stop(mess)
				}
				wcode = readLines(wbase)
				nline = setdiff( grep("@projname",wcode), grep("STATIC",wcode) ) ## STATIC
				pline = setdiff( grep("@projpath",wcode), grep("STATIC",wcode) ) ## STATIC
				dline = setdiff( grep("@date",wcode), grep("STATIC",wcode) )    ## STATIC
				uline = union(union(nline,pline),dline)
				wcode[uline] = gsub("@projpath",projpath,gsub("@projname", projname, sub("@date", Sys.time(), wcode[uline]))) ## STATIC
				writeLines(text=wcode, con=i)
				setPBSoptions("template", list(wfile=wbase), sublist=TRUE)
			}
		}
	}
	out = writePBSoptions(fname=oname)
	invisible(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createProject


## =========================================================
## R function to open a project from the command line.
## =========================================================

## openProject--------------------------2023-01-12
##  Open an existing project
## ---------------------------------------------RH
openProject = function(projname="project", projpath=".", create=FALSE)
{
	rname = paste0(projpath,"/",projname,".r")
	wname = paste0(projpath,"/",projname,"Win.txt")
	oname = paste0(projpath, "/", projname,"Opts.txt")  ## stored user options 
	editor = NULL
	if (file.exists(oname)) {
		readPBSoptions(fname=oname)
		editor = getPBSoptions()$editor
	}
	if (create || (!file.exists(rname) && !file.exists(wname)) ) {
		createProject(projname=projname, projpath=projpath, overwrite=TRUE)
		openProject(projname=projname, projpath=projpath)
	} else {
		if (file.exists(rname) && file.exists(wname)){
			source(rname)
			createWin(wname)
			mess = paste0("Existing project '", projname, "' has been opened")
			message(mess)
			if (!is.null(editor) && editor!="" && editor != getWinVal()$editor) {
				setWinVal(list(editor=editor))
			}
		} else {
			mess = paste0("One of the project files (", projname, ".r or ", projname, "Win.txt) does not exist\n   in the working directory <", projpath, ">.")
			showAlert(mess); stop(mess)
		}
	}
	.win.setPBSopt()  ## use editor in GUI to set extensions, remember editor, and write an options file
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~openProject

