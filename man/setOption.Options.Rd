%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:29 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setOption.Options}
\alias{setOption.Options}
\alias{Options.setOption}
\alias{setOption.Options}
\alias{setOption,Options-method}


 \title{Sets an option}

 \description{
  Sets an option in the options tree structure.
 }

 \usage{\method{setOption}{Options}(this, pathname, value, overwrite=TRUE, ...)}

 \arguments{
   \item{pathname}{A single \code{\link[base]{character}} string specifying the path to the
      option.}
   \item{value}{The value to be assigned to the option.}
   \item{overwrite}{If \code{\link[base:logical]{TRUE}}, already existing options are overwritten,
      otherwise not.}
   \item{...}{Not used.}
 }

 \value{Returns (invisibly) the old option value.}

 \author{Henrik Bengtsson \url{http://www.braju.com/R/}}

 \seealso{
   \code{\link[R.utils:hasOption.Options]{*hasOption}()}.
   \code{\link[R.utils:setOption.Options]{*setOption}()}.
   For more information see \code{\link{Options}}.
 }


\keyword{internal}
\keyword{methods}
\keyword{programming}
