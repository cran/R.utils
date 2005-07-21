%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:22 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{NullVerbose}
\docType{class}
\alias{NullVerbose}


\title{A Verbose class ignoring everything}

\description{
 Package:  R.utils \cr
\bold{Class NullVerbose}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.utils]{Verbose}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{NullVerbose}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{NullVerbose}\cr
extends \link[R.utils]{Verbose}\cr



 A Verbose class ignoring everything.
}

\usage{NullVerbose(...)}

\arguments{
  \item{...}{Ignored.}
}

\examples{
verbose <- Verbose()
cat(verbose, "A verbose messages")

verbose <- NullVerbose()
cat(verbose, "A verbose messages")   # Ignored
}

\author{
  Henrik Bengtsson, \url{http://www.braju.com/R/}
}



\keyword{classes}
\keyword{programming}
\keyword{IO}
