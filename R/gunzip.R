#########################################################################/**
# @RdocDefault gunzip
#
# @title "Gunzip a file"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \arguments{
#  \item{filename}{Pathname of (gzip'ed) input file to be gunzip'ed.}
#  \item{destname}{Pathname of output file.}
#  \item{overwrite}{If the output file already exists, then if 
#    \code{overwrite} is @TRUE the file is silently overwritting, otherwise
#    an exception is thrown.}
#  \item{BFR.SIZE}{The number of bytes read in each chunk.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns the number of (input/compressed) bytes read.
# }
#
# \details{
#   Internally \code{gzfile()} (see @see "base::connections") is used to
#   read chunks of the gzip'ed file, which are then written to the output 
#   file.
# }
#
# @author
#
# @keyword "file"
# @keyword "programming"
#*/######################################################################### 
setMethodS3("gunzip", "default", function(filename, destname=gsub(".gz", "", filename), overwrite=FALSE, BFR.SIZE=1e6, ...) {
  if (filename == destname) 
    stop(sprintf("Argument 'filename' and 'destname' are identical: %s", filename));
  if (!overwrite && file.exists(destname))
    stop(sprintf("File already exists: %s", destname));

  inn <- gzfile(filename, "rb");
  on.exit(close(inn));

  out <- file(destname, "wb"); 
  on.exit(close(out), add=TRUE);

  nbytes <- 0;
  repeat { 
    bfr <- readBin(inn, what=integer(0), size=1, n=BFR.SIZE);
    n <- length(bfr);
    if (n == 0)
      break;
    nbytes <- nbytes + n;
    writeBin(bfr, con=out, size=1); 
  };

  invisible(nbytes);
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2004-02-12
# o Made into a default function.
# 2003-12-31
# o Copied from the com.braju.sma package and made independent of R.oo.
# 2003-10-29
# o BUG FIX: The 2nd on.exit() in gunzip() was overwriting the first one
#   with the result that the open input gzfile() connection was never
#   closed. We ran out of connections after a while.
############################################################################
