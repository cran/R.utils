###########################################################################/**
# @RdocDefault isDirectory
#
# @title "Checks if the file specification is a directory"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#   \item{pathname}{A @character string of the pathname to be checked.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if the file specification is a directory, otherwise
#  @FALSE is returned.
# }
#
# 
#
# @author
#
# \seealso{
#   To check if it is a file see @seemethod "isFile".
#   Internally @see "base::file.info" is used.
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("isDirectory", "default", function(pathname, ...) {
  pathname <- as.character(pathname);
  if (length(pathname) == 0)
    return(FALSE);

  if (identical(pathname, ""))
    pathname <- "."; # As in Java.

  # 1. Remove trailing '/' and check if it is a directory
  pathname <- gsub("[/\\\\]$", "", pathname);
  isdir <- file.info(pathname)$isdir;
  if (identical(isdir, TRUE))
    return(TRUE);

  # 3. Add trailing '/' and check if it is a directory, e.g. "C:/".
  pathname <- paste(pathname, "/", sep="");
  isdir <- file.info(pathname)$isdir;

  identical(isdir, TRUE);
})

###########################################################################
# HISTORY: 
# 2005-05-29
# o Created by copying code in the File class of the R.io package.
###########################################################################
