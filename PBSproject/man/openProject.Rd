\name{openProject}
\alias{openProject}
\title{Open a Project}
\description{
  Open a project's GUI, which was previously created by \code{'createProject'}.
  Alternatively open a GUI from any project created before \code{'PBSproject'}; however, functionality of the GUI's widgets may be broken.
}
\usage{
openProject(projname="project", projpath=".", create=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{projname}{\code{character} -- base name of new project (without file name extensions).}
  \item{projpath}{\code{character} -- name of directory/folder where new project will reside.}
  \item{create}{\code{logical} -- if \code{TRUE}, run the function \code{'createProject'} before opening the project.}
}
\details{
  Open a GUI specified by the project name. The R code is not sourced until the user does so via the GUI.
}
\value{
  No value returned.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Instigator and Sage\cr
  Scientist Emeritus PBS, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-01-10}
}
\note{
  The function keeps track of certain options using the PBS options manager. 
  These options are saved to a file called \code{'projectOpts.txt'}, where project is the name of the user-specified project.
}
\seealso{
In package \pkg{PBSproject}:\cr
  \code{\link[PBSproject]{createProject}}

In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{openFile}},
  \code{\link[PBSmodelling]{setPBSext}},
  \code{\link[PBSmodelling]{setPBSoptions}}
}
\examples{
\dontrun{
  openProject(projname="project", projpath=".")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{project}
\concept{GUI}
% Use only one concept per line.
