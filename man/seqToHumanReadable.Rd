%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:38 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{seqToHumanReadable}
\alias{seqToHumanReadable.default}
\alias{seqToHumanReadable}

\title{Gets a short human readable string representation of an vector of indices}

\description{
 Gets a short human readable string representation of an vector of indices.
}

\usage{\method{seqToHumanReadable}{default}(idx, delimiter="-", collapse=", ", ...)}

\arguments{
  \item{idx}{A \code{\link[base]{vector}} of \code{\link[base]{integer}} indices.}
  \item{delimiter}{A \code{\link[base]{character}} string delimiter.}
  \item{collapse}{A \code{\link[base]{character}} string used to collapse subsequences.}
  \item{...}{Not used.}
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\examples{
  print(seqToHumanReadable(1:10))  # "1-10"
  print(seqToHumanReadable(c(1:10, 15:18, 20)))  # "1-10, 15-18, 20"
}


\keyword{attribute}
