%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:58:53 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{detachPackage}
\alias{detachPackage.default}
\alias{detachPackage}

\title{Detaches a packages by name}

\description{
  Detaches a packages by name, if loaded.
}

\usage{\method{detachPackage}{default}(pkgname, ...)}

\arguments{
 \item{pkgname}{A \code{\link[base]{character}} string of the package name to be detached.}
 \item{...}{Not used.}
}

\value{
  Returns (invisibly) \code{\link[base:logical]{TRUE}} if package was detached, otherwise \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  \code{\link[base]{detach}}().
}


\keyword{programming}
