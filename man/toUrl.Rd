%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:53 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{toUrl}
\alias{toUrl.default}
\alias{toUrl}

\title{Converts a pathname into a URL}

\description{
 Converts a pathname into a URL starting with \code{file://}.
}

\usage{\method{toUrl}{default}(pathname, safe=TRUE, ...)}

\arguments{
  \item{pathname}{A \code{\link[base]{character}} string of the pathname to be made into a URL.}
  \item{safe}{If \code{\link[base:logical]{TRUE}}, certain "unsafe" characters are escaped.}
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} string.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}



\keyword{IO}
\keyword{programming}
