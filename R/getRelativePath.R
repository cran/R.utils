###########################################################################/**
# @RdocDefault getRelativePath
#
# @title "Gets the relative pathname relative to a directory"
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
#  \item{relativeTo}{A @character string of the reference pathname,}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string of the relative pathname.
# }
#
# @author
#
# \seealso{
#   @seemethod "getAbsolutePath".
#   @seemethod "isAbsolutePath".
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("getRelativePath", "default", function(pathname, relativeTo=getwd(), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'pathname':
  if (is.null(pathname))
    pathname <- ".";

  if (length(pathname) > 1) {
    throw("Argument 'pathname' must be a single character string: ", 
                                             paste(pathname, collapse=", "));
  }

  if (!isAbsolutePath(pathname))
    throw("Argument 'pathname' is not an absolute pathname: ", pathname);

  # Argument 'relativeTo':
  if (is.null(relativeTo))
    relativeTo <- ".";

  if (length(relativeTo) > 1) {
    throw("Argument 'relativeTo' must be a single character string: ", 
                                           paste(relativeTo, collapse=", "));
  }

  if (!isAbsolutePath(relativeTo))
    relativeTo <- getAbsolutePath(relativeTo);

  # Split the two pathnames into their components
  relativeTo <- unlist(strsplit(relativeTo, split="[\\/]"));
  pathname <- unlist(strsplit(pathname, split="[\\/]"));


  # 1. Check that the pathnames are "compatible".  
  if (!identical(relativeTo[1], pathname[1])) {
    if (regexpr("[A-Z]:", relativeTo[1]) != -1) {
      warning("Cannot infer relative pathname, because the two pathnames are not on the same device.");
      return(paste(pathname, collapse="/"));
    }
  }

  # 2. Remove all matching components in 'relativeTo' and 'pathname'.
  #    The removed parts constitute their common path.
  for (kk in seq(length=length(relativeTo))) {
    aPart <- relativeTo[1];
    bPart <- pathname[1];
    if (!identical(aPart, bPart))
      break;

    relativeTo <- relativeTo[-1];
    pathname <- pathname[-1];
  }

  # 3. If there are more components in 'relativeTo', this means that the
  #    rest of 'relativeTo' is in a different subdirectory than 'pathname'.
  prefix <- rep("..", length.out=length(relativeTo));

  pathname <- c(prefix, pathname);
  pathname <- paste(pathname, collapse="/");

  pathname;
})  

###########################################################################
# HISTORY: 
# 2005-06-27
# o Created.
###########################################################################
