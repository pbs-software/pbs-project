\name{createProject}
\alias{createProject}
\title{Create a Project}
\description{
  Create a project by making new files (e.g., \code{'project.r'} and \code{'projectWin.txt'}) from templates defined by the \code{'style'} argument.
}
\usage{
createProject(projname="testproject", projpath=".", style="default", overwrite=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{projname}{\code{character} -- base name of new project (without file name extensions).}
  \item{projpath}{\code{character} -- name of directory/folder where new project will reside.}
  \item{style}{\code{character|numeric} -- style number (perhaps name), where \code{'default'} evaluates to 0 or \code{'00'}.}
  \item{overwrite}{\code{logical} -- if \code{TRUE}, overwrite any previous instances of \code{'projname'} on \code{'projpath'}.}
}
\details{
  Only creates a set of project files from the specified template set (only one available at present).
  A GUI is not opened until user executes the function \code{'openProject'}.
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
  Last modified \code{Rd: 2023-01-16}
}
\note{
  The function keeps track of certain options using the PBS options manager. 
  These options are saved to a file called \code{'projectOpts.txt'}, where project is the name of the user-specified project.
}
\seealso{
In package \pkg{PBSproject}:\cr
  \code{\link[PBSproject]{openProject}}

In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{openFile}},
  \code{\link[PBSmodelling]{setPBSext}},
  \code{\link[PBSmodelling]{setPBSoptions}}
}
\examples{
\dontrun{
  createProject(projname="testproject", projpath=".", overwrite=TRUE)
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
