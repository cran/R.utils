%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:48 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{readTableIndex}
\alias{readTableIndex.default}
\alias{readTableIndex}

 \title{Reads a single column from file in table format}

 \usage{\method{readTableIndex}{default}(..., indexColumn=1, colClass="character", verbose=FALSE)}

 \description{
  Reads a single column from file in table format, which can then be used as a index-to-row (look-up) map
  for fast access to a subset of rows using \code{\link{readTable}}().
 }

 \arguments{
   \item{indexColumn}{An single \code{\link[base]{integer}} of the index column.}
   \item{colClass}{A single \code{\link[base]{character}} specifying the class of the
     index column.}
   \item{...}{Arguments passed to \code{\link{readTable}}() used internally.}
   \item{verbose}{A \code{\link[base]{logical}} or a \code{\link{Verbose}} object.}
 }

 \value{
   Returns a \code{\link[base]{vector}}.
 }

 \examples{\dontrun{
    # File containing data table to be access many times
    filename <- "somefile.txt"

    # Create a look-up index
    index <- readTableIndex(filename)

    # Keys of interest
    keys <- c("foo", "bar", "wah")

    # Read only those keys and do it fast
    df <- readTable(filename, rows=match(keys, index))
 }}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
  \code{\link{readTable}}().
 }


\keyword{IO}
