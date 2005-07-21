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

\name{getReadablePathname.Arguments}
\alias{getReadablePathname.Arguments}
\alias{Arguments.getReadablePathname}
\alias{getReadablePathname.Arguments}
\alias{getReadablePathname,Arguments-method}


\title{Gets a readable pathname}

\description{
 Gets a readable pathname.
}

\synopsis{getReadablePathname.Arguments(static, file=NULL, path=NULL, mustExists=TRUE, absolutePath=TRUE, ...)}
\usage{Arguments$getReadablePathname(file=NULL, path=NULL, mustExists=TRUE, absolutePath=TRUE, ...)}

\arguments{
  \item{file}{A \code{\link[base]{character}} string specifying the file.}
  \item{path}{A \code{\link[base]{character}} string specifying the path.}
  \item{mustExists}{If \code{\link[base:logical]{TRUE}}, the pathname must exists and be readable,
    otherwise an exception is thrown. If \code{\link[base:logical]{FALSE}}, no such test is
    performed.}
  \item{absolutePath}{If \code{\link[base:logical]{TRUE}}, the absolute pathname is returned.}
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} string of the absolute pathname of the file.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  \code{\link[R.utils:getWritablePathname.Arguments]{*getWritablePathname}()}
  \code{\link[R.utils]{filePath}}.
  For more information see \code{\link{Arguments}}.
}


\keyword{internal}
\keyword{methods}
\keyword{IO}
