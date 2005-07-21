%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:58:52 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{callHooks}
\alias{callHooks.default}
\alias{callHooks}

\title{Call hook functions}

\description{
  Call hook functions.
}

\usage{\method{callHooks}{default}(hookName, ..., removeCalledHooks=FALSE)}

\arguments{
 \item{hookName}{A \code{\link[base]{character}} string of the hook name.}
 \item{...}{Argument passed to the hook function.}
 \item{removeCalledHooks}{If \code{\link[base:logical]{TRUE}}, called hook functions are removed,
    otherwise not.}
}

\value{
  Returns (invisibly) a \code{\link[base]{list}} that is possible named with hook names,
  if possible.  Each element in the list is in turn a \code{\link[base]{list}} with three
  element: \code{fcn} is the hook function called, \code{result}
  is its return value, and \code{exception} is the exception caught
  or \code{\link[base]{NULL}}.
}

\examples{
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Example 1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# First, clean up if called more than once
setHook("myFunction.onEnter", NULL, action="replace")
setHook("myFunction.onExit", NULL, action="replace")

runConference <- function(...) {
  callHooks("myFunction.onEnter")
  cat("Speaker A: Hello there...\n")
  callHooks("myFunction.onExit")
}

setHook("myFunction.onEnter", function(...) {
  cat("Chair: Welcome to our conference.\n")
})

setHook("myFunction.onEnter", function(...) {
  cat("Chair: Please welcome Speaker A!\n")
})

setHook("myFunction.onExit", function(...) {
  cat("Chair: Please thanks Speaker A!\n")
})

runConference()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Example 2
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setHook("randomNumber", NULL, action="replace")
setHook("randomNumber", rnorm)      # By function
setHook("randomNumber", "rexp")     # By name
setHook("randomNumber", "runiff")   # Non-existing name
setHook("randomNumber", .GlobalEnv) # Not a function

res <- callHooks("randomNumber", n=1)
str(res)
cat("Number of hooks: ", length(res), "\n");
isErroneous <- unlist(lapply(res, FUN=function(x) !is.null(x$exception)));
cat("Erroneous hooks: ", sum(isErroneous), "\n");


}

\author{Henrik Bengtsson \url{http://www.braju.com/R/}}

\seealso{
  See \link[base:UserHooks]{UserHooks} how to set hooks.
}


\keyword{programming}
