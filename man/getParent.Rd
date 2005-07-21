%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:58:58 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getParent}
\alias{getParent.default}
\alias{getParent}

 \title{Gets the string of the parent specified by this pathname}

 \description{
  Gets the string of the parent specified by this pathname.
  This is basically, the string before the last path separator of the
  absolute pathname.
 }

 \usage{\method{getParent}{default}(pathname, fsep=.Platform$file.sep, ...)}

 \arguments{
   \item{pathname}{A \code{\link[base]{character}} string of the pathname to be checked.}
   \item{fsep}{A \code{\link[base]{character}} string of the file separator.}
   \item{...}{Not used.}
 }

 \value{
  Returns a \code{\link[base]{character}} string if the parent exists, otherwise \code{\link[base]{NULL}}.
 }



 \author{Henrik Bengtsson \url{http://www.braju.com/R/}}



\keyword{IO}
\keyword{programming}
