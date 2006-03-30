%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:11 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
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

\synopsis{getReadablePathname.Arguments(static, file=NULL, path=NULL, mustExist=TRUE, absolutePath=FALSE, ...)}
\usage{Arguments$getReadablePathname(file=NULL, path=NULL, mustExist=TRUE, absolutePath=FALSE, ...)}

\arguments{
  \item{file}{A \code{\link[base]{character}} string specifying the file.}
  \item{path}{A \code{\link[base]{character}} string specifying the path.}
  \item{mustExist}{If \code{\link[base:logical]{TRUE}}, the pathname must exists and be readable,
    otherwise an exception is thrown. If \code{\link[base:logical]{FALSE}}, no such test is
    performed.}
  \item{absolutePath}{If \code{\link[base:logical]{TRUE}}, the absolute pathname is returned.}
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} string of the absolute pathname of the file.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.utils:getWritablePathname.Arguments]{*getWritablePathname}()}
  \code{\link[R.utils]{filePath}}.
  For more information see \code{\link{Arguments}}.
}


\keyword{internal}
\keyword{methods}
\keyword{IO}
