.parseArgs <- function(args, defaults=list()) {
  # Local functions
  names <- function(args) {
    keys <- base::names(args)
    if (is.null(keys)) keys <- rep("", times=length(args))
    keys
  } # names()

  # Argument 'args':
  .stop_if_not(is.list(args))

  # Argument 'defaults':
  .stop_if_not(is.list(defaults))
  formals <- names(defaults)
  .stop_if_not(!is.null(formals))


  # Split up named and unnamed arguments
  named <- (names(args) != "")
  argsN <- args[named]
  args <- args[!named]

  argsT <- list()

  # Get the arguments, if they are named
  for (kk in seq_along(formals)) {
    key <- formals[kk]
    keys <- names(argsN)
    if (is.element(key, keys)) {
      idx <- which(keys == key)[1L]
      argsT[[key]] <- argsN[[idx]]
      argsN <- argsN[-idx]
      formals[kk] <- NA
    }
  }
  formals <- formals[!is.na(formals)]


  # Get the remaining arguments by position
  for (kk in seq_along(formals)) {
    key <- formals[kk]
    if (length(args) > 0L) {
      value <- args[[1L]]
      argsT[[key]] <- value
      args <- args[-1L]
    } else {
      if (!is.symbol(defaults[[key]])) {
        value <- defaults[[key]]
        argsT[[key]] <- value
      }
    }
    formals[kk] <- NA
  }
  formals <- formals[!is.na(formals)]

  # Return parsed arguments
  list(args=argsT, namedArgs=argsN, unnamedArgs=args)
} # .parseArgs()
