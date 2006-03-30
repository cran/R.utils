%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:33 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{mkdirs}
\alias{mkdirs.default}
\alias{mkdirs}

 \title{Creates a directory including any necessary but nonexistent parent directories}

 \description{
  Creates a directory including any necessary but nonexistent parent directories.
 }

 \usage{\method{mkdirs}{default}(pathname, ...)}

 \arguments{
   \item{pathname}{A \code{\link[base]{character}} string of the pathname to be checked.}
   \item{...}{Not used.}
 }

 \value{
  Returns \code{\link[base:logical]{TRUE}} if the directory was succesfully created,
  otherwise \code{\link[base:logical]{FALSE}}.
  Note that if the directory already exists, \code{\link[base:logical]{FALSE}} is returned.
 }



 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   Internally \code{\link[base:files]{dir.create}}() is used.
 }



\keyword{IO}
\keyword{programming}
