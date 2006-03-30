###########################################################################/**
# @RdocDefault callHooks
#
# @title "Call hook functions"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{hookName}{A @character string of the hook name.}
#  \item{...}{Argument passed to the hook function.}
#  \item{removeCalledHooks}{If @TRUE, called hook functions are removed, 
#     otherwise not.}
# }
#
# \value{
#   Returns (invisibly) a @list that is possible named with hook names,
#   if possible.  Each element in the list is in turn a @list with three
#   element: \code{fcn} is the hook function called, \code{result} 
#   is its return value, and \code{exception} is the exception caught 
#   or @NULL.
# }
#
# @examples "../incl/callHooks.Rex"
#
# @author
#
# \seealso{
#   See \link[base:UserHooks]{UserHooks} how to set hooks.
# }
#
# @keyword programming
#*/###########################################################################
setMethodS3("callHooks", "default", function(hookName, ..., removeCalledHooks=FALSE) {
  # Argument 'hook':
  hookName <- as.character(hookName);
  if (length(hookName) != 1)
    throw("Argument 'hookName' must be a single character string: ", length(hookName));

  # Argument 'removeCalledHooks':
  removeCalledHooks <- as.logical(removeCalledHooks);
    
  hooks <- getHook(hookName);
  if (length(hooks) == 0)
    return();

  if (!is.list(hooks))
    hooks <- list(hooks);

  toBeDone <- rep(TRUE, length=length(hooks));
  if (removeCalledHooks) {
    on.exit(setHook(hookName, hooks[toBeDone], action="replace"));
  }

  res <- vector(length(hooks), mode="list");
  hookNames <- character(length(hooks));
  for (kk in seq(length=length(hooks))) {
    # Find the hook function
    fcn <- hooks[[kk]];
    tmp <- list(fcn=fcn, result=NULL, exception=NULL);
    if (is.character(fcn)) {
      hookNames[[kk]] <- fcn;
      tryCatch({
        fcn <- get(fcn, mode="function");
      }, error = function(ex) {
        tmp[["fcn"]] <<- NA;
        tmp[["exception"]] <<- ex;
      })
    }

    # Try to call the hook function
    if (!is.null(fcn)) {
      tryCatch({
        result <- fcn(...);
        tmp[["result"]] <- result;
        toBeDone[kk] <- FALSE;
      }, error = function(ex) {
        tmp[["exception"]] <<- ex;
      })
    }
    res[[kk]] <- tmp;
  }

  names(res) <- hookNames;
  invisible(res);
})


############################################################################
# HISTORY:
# 2005-06-15
# o Now callHooks() returns a detailed list of hooks called, their return
#   values and any exceptions caught.
# o Added arguments '...', which are passed to the hook function.
# 2005-06-10
# o Created.
############################################################################
