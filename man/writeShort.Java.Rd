%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:15 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{writeShort.Java}
\alias{writeShort.Java}
\alias{Java.writeShort}
\alias{writeShort.Java}
\alias{writeShort,Java-method}


\title{Writes a short (16 bits) to a connection in Java format}

\description{
  Writes one or several short's (16 bits) to a connection in Java
  format so they will be readable by Java.
  All data types in Java are signed, i.e. a byte can hold a value in
  the range [-32768,32767]. Trying to write a value outside this range
  will automatically be truncated without a warning.
}

\synopsis{writeShort.Java(static, con, s, ...)}
\usage{Java$writeShort(con, s, ...)}

\arguments{
  \item{con}{Binary connection to be written to.}
  \item{s}{Vector of shorts to be written.}
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  For more information see \code{\link{Java}}.
}
\keyword{internal}
\keyword{methods}
