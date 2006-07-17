%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  gunzip.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{gunzip}
\alias{gunzip.default}
\alias{gunzip}

\title{Gunzip a file}

\usage{\method{gunzip}{default}(filename, destname=gsub(".gz", "", filename), overwrite=FALSE, BFR.SIZE=1e+06, ...)}

\description{
 Gunzip a file.
}

\arguments{
 \item{filename}{Pathname of (gzip'ed) input file to be gunzip'ed.}
 \item{destname}{Pathname of output file.}
 \item{overwrite}{If the output file already exists, then if
   \code{overwrite} is \code{\link[base:logical]{TRUE}} the file is silently overwritting, otherwise
   an exception is thrown.}
 \item{BFR.SIZE}{The number of bytes read in each chunk.}
  \item{...}{Not used.}
}

\value{
  Returns the number of (input/compressed) bytes read.
}

\details{
  Internally \code{gzfile()} (see \code{\link[base]{connections}}) is used to
  read chunks of the gzip'ed file, which are then written to the output
  file.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}



\keyword{file}
\keyword{programming}
