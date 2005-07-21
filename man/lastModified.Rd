%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:19 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{lastModified}
\alias{lastModified.default}
\alias{lastModified}

 \title{Gets the time when the file was last modified}

 \description{
  Gets the time when the file was last modified. The time is returned as a \code{POSIXct} object.
 }

 \usage{\method{lastModified}{default}(pathname, ...)}

 \arguments{
   \item{pathname}{A \code{\link[base]{character}} string of the pathname to be checked.}
   \item{...}{Not used.}
 }

 \value{
  Returns \code{POSIXct} object specifying when the file was last modified.
  If the file does not exist or it is a directory, \code{0} is returned.
 }



 \author{Henrik Bengtsson \url{http://www.braju.com/R/}}

 \seealso{
   Internally \code{\link[base]{file.info}}() is used.
 }



\keyword{IO}
\keyword{programming}
