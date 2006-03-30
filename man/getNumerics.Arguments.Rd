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

\name{getNumerics.Arguments}
\alias{getNumerics.Arguments}
\alias{Arguments.getNumerics}
\alias{getNumerics.Arguments}
\alias{getNumerics,Arguments-method}

\alias{Arguments.getNumeric}
\alias{getNumeric.Arguments}
\alias{getNumeric,Arguments-method}


\title{Coerces to a numeric vector and validates}

\description{
 Coerces to a numeric vector and validates.
}

\synopsis{getNumerics.Arguments(static, x, range=NULL, asMode="numeric", disallow=NULL, ..., .name=NULL)}
\usage{Arguments$getNumerics(x, range=NULL, asMode="numeric", disallow=NULL, ..., .name=NULL)}

\arguments{
  \item{x}{A \code{\link[base]{vector}}.}
  \item{range}{Two \code{\link[base]{numeric}}s for the allowed ranged. If \code{\link[base]{NULL}}, range is
    not checked.}
  \item{asMode}{A \code{\link[base]{character}} specifying the mode to coerce to.}
  \item{disallow}{A \code{\link[base]{character}} \code{\link[base]{vector}} specifying diallowed value sets,
                    i.e. \code{"NA"}, \code{"NaN"}, and/or \code{"Inf"}.}
  \item{...}{Arguments passed to @method "getVector".}
  \item{.name}{A \code{\link[base]{character}} string for name used in error messages.}
}

\value{
 Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{Arguments}}.
}


\keyword{internal}
\keyword{methods}
\keyword{IO}
