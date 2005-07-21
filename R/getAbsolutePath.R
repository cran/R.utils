###########################################################################/**
# @RdocDefault getAbsolutePath
#
# @title "Gets the absolute pathname string"
#
# @synopsis
#
# \description{
#   @get "title".
# }
#
# \arguments{
#  \item{pathname}{A @character string of the pathname to be converted into
#    an absolute pathname.}
#  \item{workDirectory}{A @character string of the current working directory.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string of the absolute pathname.
# }
#
# @author
#
# \seealso{
#   @seemethod "isAbsolutePath".
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("getAbsolutePath", "default", function(pathname, workDirectory=getwd(), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  getName <- function(pathname, removeSuffix=FALSE, ...) {
    components <- strsplit(pathname, split="[/\\]")[[1]];

    len <- length(components);
    if (len == 0) 
      return("");

    name <- components[len];
    if (name == ".") 
      return("");

    reg <- regexpr("^.:", name);
    if (reg != -1)
      name <- substring(name, attr(reg, "match.length")+1);

    if (removeSuffix)
      name <- gsub("[.][^.]*$", "", name); # Remove the suffix.

    name; 
  } # getName()


  if (is.null(pathname))
    pathname <- ".";

  if (!isAbsolutePath(pathname)) {
    workDirectory <- strsplit(workDirectory, split="[/\\]")[[1]];

    name <- getName(pathname);
    if (name == "" || name == ".")
      name <- NULL;                        # Only, details, but as in Java!

    pathname <- strsplit(pathname, split="[/\\]")[[1]];
    len <- length(pathname);
    if (len != 0)
      pathname <- pathname[-len];

    pathname <- c(workDirectory, pathname, name);
    pathname <- paste(pathname, sep="", collapse=.Platform$file.sep);

    pathname <- filePath(pathname, removeUps=TRUE);
  }

  pathname;
})  

###########################################################################
# HISTORY: 
# 2005-06-16
# o Now getAbsolutePath() removes ".." too.
# 2005-05-29
# o Created by copying code in the File class of the R.io package.
###########################################################################
