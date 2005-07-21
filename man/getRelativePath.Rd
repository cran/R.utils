%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:58:59 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getRelativePath}
\alias{getRelativePath.default}
\alias{getRelativePath}

\title{Gets the relative pathname relative to a directory}

\usage{\method{getRelativePath}{default}(pathname, relativeTo=getwd(), ...)}

\description{
  Gets the relative pathname relative to a directory.
}

\arguments{
 \item{pathname}{A \code{\link[base]{character}} string of the pathname to be converted into
   an absolute pathname.}
 \item{relativeTo}{A \code{\link[base]{character}} string of the reference pathname,}
 \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} string of the relative pathname.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  \code{\link[R.utils:getAbsolutePath.]{*getAbsolutePath}()}.
  \code{\link[R.utils:isAbsolutePath.]{*isAbsolutePath}()}.
}



\keyword{IO}
\keyword{programming}
