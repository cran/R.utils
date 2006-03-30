%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:49 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{seqToIntervals}
\alias{seqToIntervals.default}
\alias{seqToIntervals}

\title{Gets all contigous intervals of a vector of indices}

\description{
 Gets all contigous intervals of a vector of indices.
}

\usage{\method{seqToIntervals}{default}(idx, ...)}

\arguments{
  \item{idx}{A \code{\link[base]{vector}} of \code{\link[base]{integer}} indices.}
  \item{...}{Not used.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  print(seqToIntervals(1:10))  # [1 10]
  print(seqToIntervals(c(1:10, 15:18, 20)))  # [1 10; 15 18; 20 20]
}


\keyword{attribute}
