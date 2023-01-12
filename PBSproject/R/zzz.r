# Taking cue from Roger Bivand's maptools:
.PBSprojEnv <- new.env(FALSE, parent=globalenv())  # be sure to exportPattern("^\\.PBS") in NAMESPACE

.onAttach <- function(lib, pkg)
{
	pkg_info = utils::sessionInfo( package="PBSproject" )$otherPkgs$PBSproject
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()

	userguide_path <- system.file( "doc/PBSprojectIntro.pdf", package = "PBSproject" )
	year <- substring(date(),nchar(date())-3,nchar(date()))

	packageStartupMessage("
-----------------------------------------------------------
PBS Project ", pkg_info$Version, " -- Copyright (C) 2022-",year," Fisheries and Oceans Canada

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

All available PBS packages can be found at
https://github.com/pbs-software

To create a new project:
   createProject(projname=\"project\", projpath=\".\")

To open a project:
   openProject(projname=\"project\", projpath=\".\")
-----------------------------------------------------------

")
}
.onUnload <- function(libpath) {
	rm(.PBSprojEnv)
}

# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
	".win.setPBSopt",
#	"A",
#	"B",
	"createWin",
#	"D",
#	"E",
#	"F",
#	"G",
#	"H",
#	"I",
#	"J",
#	"K",
#	"L",
#	"M",
#	"N",
#	"O",
	"pad0",
#	"Q",
	"readPBSoptions",
	"setPBSoptions","showAlert",
#	"T",
#	"U",
#	"V",
	"writePBSoptions"
#	"X",
#	"Y",
#	"Z"
	), package="PBSproject")

