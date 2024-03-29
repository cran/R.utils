###########################################################################/**
# @RdocFunction withOptions
#
# @title "Evaluate an R expression with options set temporarily"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{expr}{The R expression to be evaluated.}
#   \item{...}{Named options to be used.}
#   \item{args}{(optional) Additional named options specified as a named @list.}
#   \item{substitute}{If @TRUE, argument \code{expr} is
#    \code{\link[base]{substitute}()}:ed, otherwise not.}
#   \item{envir}{The @environment in which the expression should be evaluated.}
# }
#
# \value{
#  Returns the results of the expression evaluated.
# }
#
# \details{
#   Upon exit (also on errors), this function will reset \emph{all}
#   options to the state of options available upon entry.  This means
#   any options \emph{modified} but also those \emph{added} when
#   evaluating \code{expr} will also be undone upon exit.
# }
#
# @author
#
# @examples "../incl/withOptions.Rex"
#
# \seealso{
#   Internally, @see "base::eval" is used to evaluate the expression.
#   and @see "base::options" to set options.
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
withOptions <- function(expr, ..., args=list(), substitute=TRUE, envir=parent.frame()) {
  # Argument 'expr':
  if (substitute) expr <- substitute(expr)

  # Argument 'args':
  if (!is.list(args)) {
    throw("Argument 'args' is not a list: ", class(args)[1L])
  }

  # Argument 'envir':
  if (!is.environment(envir)) {
    throw("Argument 'envir' is not a list: ", class(envir)[1L])
  }

  # All options specified
  new <- c(list(...), args)

  # Set options temporarily (restore *all* upon exit)
  prev <- options()
  on.exit({
    # Reset existing options
    options(prev)
    # Drop any added ones
    added <- setdiff(names(options()), names(prev))
    if (length(added) > 0L) {
      drop <- vector("list", length=length(added))
      names(drop) <- added
      options(drop)
    }
  })
  if (length(new) > 0L) options(new)

  eval(expr, envir = envir, enclos = baseenv())
} # withOptions()
