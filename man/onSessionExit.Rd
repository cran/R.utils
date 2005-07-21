%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:23 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{onSessionExit}
\alias{onSessionExit.default}
\alias{onSessionExit}

\title{Registers a function to be called when the R session finishes}

\description{
  Registers a function to be called when the R session finishes.
}

\usage{\method{onSessionExit}{default}(fcn, action=c("prepend", "append", "replace"), ...)}

\arguments{
 \item{fcn}{A \code{\link[base]{function}} to be called without argument.}
 \item{action}{A \code{\link[base]{character}} string specifying how the hook function is
   added to list of hooks.}
 \item{...}{Not used.}
}

\value{
  Returns (invisibly) the hooks successfully called.
}

\details{
  Functions registered this way are called when \code{\link{finalizeSession}}() is
  called.  Moreover, when this package is loaded, the \code{.Last()}
  function is modified such that \code{finalizeSession()} is called.
  However, note that \code{.Last()} is \emph{not} guaranteed to be called
  when the \R session finished.  For instance, the user may quit \R by
  calling \code{quit(callLast=FALSE)}.
  Moreover, when \R is run in batch mode, \code{.Last()} is never called.
}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\examples{\dontrun{
  onSessionExit(function(...) {
    cat("Bye bye world!\n");
  })

  quit()
}}

\seealso{
  \code{\link{.Last}()}.
  \code{\link{finalizeSession}}().
}


\keyword{programming}
