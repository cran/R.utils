%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  NullVerbose.R
% 
% by the Rdoc compiler part of the R.oo package.
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

\usage{
NullVerbose(...)
}

\arguments{
  \item{...}{Ignored.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{cat} \tab  -\cr
 \tab \code{enter} \tab  -\cr
 \tab \code{evaluate} \tab  -\cr
 \tab \code{exit} \tab  -\cr
 \tab \code{header} \tab  -\cr
 \tab \code{isOn} \tab  -\cr
 \tab \code{isVisible} \tab  -\cr
 \tab \code{newline} \tab  -\cr
 \tab \code{print} \tab  -\cr
 \tab \code{printf} \tab  -\cr
 \tab \code{ruler} \tab  -\cr
 \tab \code{str} \tab  -\cr
 \tab \code{summary} \tab  -\cr
 \tab \code{writeRaw} \tab  -\cr
}


 \bold{Methods inherited from Verbose}:\cr
as.character, as.double, as.logical, capture, cat, enter, enterf, equals, evaluate, exit, getThreshold, getTimestampFormat, header, isOn, isVisible, less, more, newline, off, on, popState, print, printWarnings, printf, pushState, ruler, setDefaultLevel, setThreshold, setTimestampFormat, str, summary, timestamp, timestampOff, timestampOn, writeRaw

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, names, objectSize, print, save


}

\examples{
verbose <- Verbose()
cat(verbose, "A verbose messages")

verbose <- NullVerbose()
cat(verbose, "A verbose messages")   # Ignored
}

\author{Henrik Bengtsson}



\keyword{classes}
\keyword{programming}
\keyword{IO}
