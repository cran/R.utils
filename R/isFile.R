###########################################################################/**
# @RdocDefault isFile
#
# @title "Checks if the file specification is a file"
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
#  Returns @TRUE if the file specification is a file, otherwise
#  @FALSE is returned.
# }
#
# 
#
# @author
#
# \seealso{
#   To check if it is a directory see @seemethod "isDirectory".
#   Internally @see "base::file.info" is used.
# }
#
# @keyword IO
# @keyword programming
#*/########################################################################### 
setMethodS3("isFile", "default", function(pathname, ...) {
  pathname <- as.character(pathname);
  isdir <- file.info(pathname)$isdir;
  identical(isdir, FALSE);
})

###########################################################################
# HISTORY: 
# 2005-05-29
# o Created by copying code in the File class of the R.io package.
###########################################################################
