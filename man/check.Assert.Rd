%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:14 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{check.Assert}
\alias{check.Assert}
\alias{Assert.check}
\alias{check.Assert}
\alias{check,Assert-method}


 \title{Static method asserting that a generic condition is true}

 \description{
   Static method asserting that a generic condition is true.
 }

 \synopsis{check.Assert(static, condition, message=NULL, ...)}
\usage{Assert$check(condition, message=NULL, ...)}

 \arguments{
  \item{condition}{A condition that should return \code{\link[base:logical]{TRUE}} if ok, or something
   else if not.}
  \item{message}{The error message to be reported on failure. If \code{\link[base]{NULL}},
   a message is automatically created.}
  \item{...}{Not used.}
 }

 \value{
   Returns (invisibly) \code{\link[base:logical]{TRUE}}, or throws an exception.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   For more information see \code{\link{Assert}}.
 }
\keyword{internal}
\keyword{methods}
