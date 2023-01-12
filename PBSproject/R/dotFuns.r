## ===============================================
## Dot (hidden) functions not dependent on project.
## ===============================================

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
	file.copy(from=x, to=bupfile, copy.date=TRUE)
}

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

