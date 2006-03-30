###########################################################################/**
# @RdocDefault addFinalizerToLast
#
# @title "Modifies .Last() to call 'finalizeSession()"
#
# \description{
#   @get "title" \emph{before} calling the default \code{.Last()} function.
#
#   Note that \code{.Last()} is \emph{not} guaranteed to be called when
#   the \R session finished.  For instance, the user may quit \R by calling
#   \code{quit(runLast=FALSE)} or run R in batch mode.
#
#   Note that this function is called when the R.utils package is loaded.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns (invisibly) @TRUE if \code{.Last()} was modified, 
#   otherwise @FALSE.
# }
#
# @author
#
# \seealso{
#   @see "onSessionExit".
# }
#
# @keyword programming
#*/###########################################################################
setMethodS3("addFinalizerToLast", "default", function(...) {
  # Get .Last().
  if (exists(".Last", mode="function")) {
    .Last <- get(".Last", mode="function");
    if (identical(attr(.Last, "finalizeSession"), TRUE))
      return(invisible(FALSE));

    # Rename original .Last() function
    assign(".LastOriginal", .Last, envir=.GlobalEnv);

    # Define a new .Last() function
    .Last <- function(...) {
      finalizeSession();
      if (exists(".LastOriginal", mode="function"))
        .LastOriginal();
    }
  } else {
    .Last <- function(...) { 
      finalizeSession();
    }
  }

  attr(.Last, "finalizeSession") <- TRUE;
  assign(".Last", .Last, envir=.GlobalEnv);

  invisible(FALSE);
}, private=TRUE)



############################################################################
# HISTORY:
# 2005-06-10
# o Extra care was need with this function, because otherwise R CMD check
#   would give an error if called by .First.lib() in R.utils.
# o Created.
############################################################################
