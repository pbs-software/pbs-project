## =====================================
## R function to create a project from
## the command line (RH 221230)
## =====================================

## createProject------------------------2022-12-30
##  Create new project (JTS wants non-GUI start-up function)
## ---------------------------------------------RH
createProject = function(projname="project", projpath=".", overwrite=FALSE)
{
	rpath  = projpath
	rtargs = paste0(rpath,"/",projname,c(".r","Win.txt"))
	if (!overwrite){
		rnew = paste0(projpath,"/",projname,".r")
		wnew = paste0(projpath,"/",projname,"Win.txt")
		if (file.exists(rnew) && file.exists(wnew)){
			createWin(wnew)
			mess = paste0("Existing project ", projname, "has been not been overwritten but opened")
			message(mess)
		} else {
			mess = paste0("Project '", projname, "' files (", projname, ".r, ", projname, "Win.txt) do not exist in working directory <", projpath, ">.")
			showAlert(mess); stop(mess)
		}
	} else {
		## Create new R and WDF from the templates
		for (i in rtargs) {
			ii = basename(i)
			## Create R code form 'projectTemplate.r'
			if (grepl("\\.[rR]$", ii)) {
				rbase = paste0(getwd(),"/projectTemplate.r")
				if (!file.exists(rbase)){
					mess = paste0("R code file\n'", rbase, "'\n   does not exist in current working directory.")
					showAlert(mess); stop(mess)
				}
				rcode = readLines(con=rbase)
				pline = setdiff( grep("@projname",rcode), grep("STATIC",rcode) ) ## STATIC -- no longer used after PBSproject v.0.0.5
				dline = setdiff( grep("@date",rcode), grep("STATIC",rcode) )     ## STATIC
				uline = union(pline,dline)
				rcode[uline] = gsub("@projname", projname, sub("@date", Sys.time(), rcode[uline])) ## STATIC
				#rcode = rcode[-(rev(grep("END CODE",rcode))[1]:length(rcode))]  ## remove recursive calls
				writeLines(text=rcode, con=i)
				source(file=i)
#browser();return()
			}
			## Create Windows Description file
			if (grepl("Win\\.txt$", ii)) {
				wbase = paste0(getwd(),"/projectTemplateWin.txt")
				if (!file.exists(wbase)){
					mess = paste0("Windows description file '", wbase, "'\n   does not exist in current working directory.")
					showAlert(mess); stop(mess)
				}
				wcode = readLines(wbase)
				nline = setdiff( grep("@projname",wcode), grep("STATIC",wcode) ) ## STATIC
				pline = setdiff( grep("@projpath",wcode), grep("STATIC",wcode) ) ## STATIC
				dline = setdiff( grep("@date",wcode), grep("STATIC",wcode) )    ## STATIC
				uline = union(union(nline,pline),dline)
#browser();return()
				wcode[uline] = gsub("@projpath",projpath,gsub("@projname", projname, sub("@date", Sys.time(), wcode[uline]))) ## STATIC
				writeLines(text=wcode, con=i)
				if (projname!="projectTemplate") {
					#closeWin("projectTemplate")
					createWin(i)
				}
			}
#browser();return()
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createProject

#-------------------------------------------------

require(PBSmodelling)
#require(devtools)
#require(roxygen2)

closeWin()
createProject(projname="project", projpath=".", overwrite=TRUE)


