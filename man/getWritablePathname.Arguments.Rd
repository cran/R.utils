%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:58:45 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getWritablePathname.Arguments}
\alias{getWritablePathname.Arguments}
\alias{Arguments.getWritablePathname}
\alias{getWritablePathname.Arguments}
\alias{getWritablePathname,Arguments-method}


\title{Gets a writable pathname}

\description{
 Gets a writable pathname.
}

\synopsis{getWritablePathname.Arguments(static, ..., overwrite=FALSE, mkdirs=TRUE)}
\usage{Arguments$getWritablePathname(..., overwrite=FALSE, mkdirs=TRUE)}

\arguments{
  \item{...}{Arguments passed to \code{\link[R.utils:getReadablePathname.Arguments]{*getReadablePathname}()}.}
  \item{overwrite}{If \code{\link[base:logical]{TRUE}} and \code{overwrite} is \code{\link[base:logical]{TRUE}}, existing
    files are overwritten. Otherwise, and Exception is thrown.}
  \item{mkdirs}{If \code{\link[base:logical]{TRUE}}, \code{overwrite} is \code{\link[base:logical]{TRUE}}, and the path to
    the file does not exist, it is (recursively) created.}
}

\value{
 Returns a \code{\link[base]{character}} string of the absolute pathname of the file.
 If the argument was invalid an \code{\link[R.oo]{Exception}} is thrown.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  \code{\link[R.utils:getReadablePathname.Arguments]{*getReadablePathname}()}.
  \code{\link[R.utils]{filePath}}.
  \code{\link[R.utils]{mkdirs}}.
  For more information see \code{\link{Arguments}}.
}


\keyword{internal}
\keyword{methods}
\keyword{IO}
