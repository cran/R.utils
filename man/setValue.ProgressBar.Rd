%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:35 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setValue.ProgressBar}
\alias{setValue.ProgressBar}
\alias{ProgressBar.setValue}
\alias{setValue.ProgressBar}
\alias{setValue,ProgressBar-method}


 \title{Sets current value}

 \description{
   Sets current value.
   Note that this method does \emph{not} update the bar visually.
 }

 \usage{\method{setValue}{ProgressBar}(this, value, ...)}

 \arguments{
  \item{value}{A \code{\link[base]{numeric}} in [0,maxValue].}
  \item{...}{Not used.}
 }

 \value{
   Returns old value.
 }

 \author{Henrik Bengtsson \url{http://www.braju.com/R/}}

 \seealso{
   \code{\link[R.utils:setProgress.ProgressBar]{*setProgress}()}.
   \code{\link[R.utils:increase.ProgressBar]{*increase}()}.
   \code{\link[R.utils:reset.ProgressBar]{*reset}()}.
   For more information see \code{\link{ProgressBar}}.
 }
\keyword{internal}
\keyword{methods}
