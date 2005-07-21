%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 12:00:11 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{pushState.Verbose}
\alias{pushState.Verbose}
\alias{Verbose.pushState}
\alias{pushState.Verbose}
\alias{pushState,Verbose-method}

\alias{Verbose.popState}
\alias{popState.Verbose}
\alias{popState,Verbose-method}


\title{Pushes the current indentation state of the Verbose object}

\description{
  Pushes the current indentation state of the Verbose object, which is controlled by \code{\link[R.utils:enter.Verbose]{*enter}()} and
  \code{\link[R.utils:exit.Verbose]{*exit}()}.  By pushing the state when entering a function and
  using \code{\link[base]{on.exit}}() to pop the state, the correct state will
  set regardless of if the functions returned naturally or via an error.
}

\usage{\method{pushState}{Verbose}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns (invisibly) \code{\link[base:logical]{TRUE}}.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  For more information see \code{\link{Verbose}}.
}


\keyword{internal}
\keyword{methods}
\keyword{programming}
