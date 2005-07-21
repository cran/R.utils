%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:44 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{SmartComments}
\docType{class}
\alias{SmartComments}


\title{Abstract class SmartComments}

\description{
 Package:  R.utils \cr
\bold{Class SmartComments}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\emph{\code{SmartComments}}\cr

\bold{Directly known subclasses:}\cr
LComments, VComments\cr

public abstract static class \bold{SmartComments}\cr
extends \link[R.oo]{Object}\cr



 Abstract class SmartComments.
}

\usage{SmartComments(letter=NA, ...)}

\arguments{
  \item{letter}{A single \code{\link[base]{character}}.}
  \item{...}{Not used.}
}

\details{
 A "smart" source-code comment is an \R comment, which start with a '\#',
 but is followed by a single letter, then a single symbol and a second
 '\#' and then an option character string, and there must not be any code
 before the comment on the same line. In summary, a smart comment line
 has format: \code{<white spaces>\#<letter><symbol>\# <some text>}.

 Example code with two smart comments (VComments):
 \preformatted{
   x <- 2
   \#V1\# threshold=-1
   \#Vc\# A v-comment log message
   cat("Hello world")
 }
 which after compilation becomes
 \preformatted{
   x <- 2
   verbose <- Verbose(threshold=-1)
   if (verbose) { cat(verbose, "A v-comment log message"); }
   cat("Hello world")
 }
}

\author{
  Henrik Bengtsson, \url{http://www.braju.com/R/}
}

\seealso{
  \code{\link{VComments}}.
}



\keyword{classes}
\keyword{programming}
\keyword{IO}
