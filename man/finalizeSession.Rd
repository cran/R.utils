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

\name{finalizeSession}
\alias{finalizeSession.default}
\alias{finalizeSession}

\title{Function to call for finalizing the R session}

\description{
  Function to call for finalizing the R session.  When called, all registered "onSessionExit" hooks
  (functions) are called.  To define such hooks, use the
  \code{\link{onSessionExit}}() function.

  This method should not be used by the user.
}

\usage{\method{finalizeSession}{default}(...)}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns (invisibly) the hooks successfully called.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  \code{\link{onSessionExit}}().
}


\keyword{programming}
