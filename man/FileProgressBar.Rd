%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FileProgressBar.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{FileProgressBar}
\docType{class}
\alias{FileProgressBar}


 \title{A progress bar that sets the size of a file accordingly}

 \description{
  Package:  R.utils \cr
\bold{Class FileProgressBar}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.utils]{ProgressBar}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{FileProgressBar}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{FileProgressBar}\cr
extends \link[R.utils]{ProgressBar}\cr


 }

 \usage{
FileProgressBar(pathname=NULL, ...)
}

 \arguments{
   \item{pathname}{The pathname of the output file.}
   \item{...}{Other arguments accepted by the \code{\link{ProgressBar}}
     constructor.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{cleanup} \tab  -\cr
 \tab \code{update} \tab  -\cr
}


 \bold{Methods inherited from ProgressBar}:\cr
as.character, getBarString, increase, isDone, reset, setMaxValue, setProgress, setStepLength, setTicks, setValue, update

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, names, objectSize, print, save


 }

 \details{
   A progress bar that sets the size of a file accordingly. This class useful to check the progress of a batch job by
   just querying the size of a file, for instance, via ftp.
 }

 \examples{
  \dontrun{
  
# Creates a progress bar (of length 100) that displays it self as a file.

pb <- FileProgressBar(file.path(tempdir(), "progress.simulation"), max = 10L)
reset(pb)
while (!isDone(pb)) {
  x <- rnorm(3e4)
  increase(pb)
  # Emulate a slow process
  if (interactive()) Sys.sleep(0.1)
  cat(sprintf("File size: \%d bytes\n", file.info(pb$pathname)$size))
  Sys.sleep(0.01)
}

  }
 }

 \author{Henrik Bengtsson}
\keyword{classes}
