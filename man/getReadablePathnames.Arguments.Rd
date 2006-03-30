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

\name{getReadablePathnames.Arguments}
\alias{getReadablePathnames.Arguments}
\alias{Arguments.getReadablePathnames}
\alias{getReadablePathnames.Arguments}
\alias{getReadablePathnames,Arguments-method}


\title{Gets a readable pathname}

\description{
 Gets a readable pathname.
}

\synopsis{getReadablePathnames.Arguments(static, files=NULL, paths=NULL, ...)}
\usage{Arguments$getReadablePathnames(files=NULL, paths=NULL, ...)}

\arguments{
  \item{files}{A \code{\link[base]{character}} \code{\link[base]{vector}} of filenames.}
  \item{paths}{A \code{\link[base]{character}} \code{\link[base]{vector}} of paths.}
  \item{...}{Arguments passed to \code{\link[R.utils:getReadablePathname.Arguments]{*getReadablePathname}()}.}
}

\value{
 Returns a \code{\link[base]{character}} \code{\link[base]{vector}} of the pathnames for the files.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.utils:getReadablePathname.Arguments]{*getReadablePathname}()}
  \code{\link[R.utils]{filePath}}.
  For more information see \code{\link{Arguments}}.
}


\keyword{internal}
\keyword{methods}
\keyword{IO}
