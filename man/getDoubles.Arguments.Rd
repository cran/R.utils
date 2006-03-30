%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:12 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getDoubles.Arguments}
\alias{getDoubles.Arguments}
\alias{Arguments.getDoubles}
\alias{getDoubles.Arguments}
\alias{getDoubles,Arguments-method}

\alias{Arguments.getDouble}
\alias{getDouble.Arguments}
\alias{getDouble,Arguments-method}


\title{Coerces to a double vector and validates}

\description{
 Coerces to a double vector and validates.
}

\synopsis{getDoubles.Arguments(static, ..., disallow=c("NA", "NaN"))}
\usage{Arguments$getDoubles(..., disallow=c("NA", "NaN"))}

\arguments{
  \item{...}{Arguments passed to @method "getNumeric".}
  \item{disallow}{Disallowed values. See @method "getNumerics" for details.}
}

\value{
 Returns a \code{\link[base]{double}} \code{\link[base]{vector}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{Arguments}}.
}


\keyword{internal}
\keyword{methods}
\keyword{IO}
