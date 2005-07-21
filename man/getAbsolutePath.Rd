%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:58:58 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getAbsolutePath}
\alias{getAbsolutePath.default}
\alias{getAbsolutePath}

\title{Gets the absolute pathname string}

\usage{\method{getAbsolutePath}{default}(pathname, workDirectory=getwd(), ...)}

\description{
  Gets the absolute pathname string.
}

\arguments{
 \item{pathname}{A \code{\link[base]{character}} string of the pathname to be converted into
   an absolute pathname.}
 \item{workDirectory}{A \code{\link[base]{character}} string of the current working directory.}
 \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} string of the absolute pathname.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  \code{\link[R.utils:isAbsolutePath.]{*isAbsolutePath}()}.
}



\keyword{IO}
\keyword{programming}
