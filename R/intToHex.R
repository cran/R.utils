setMethodS3("as.character", "hexmode", function(x, ...) {
  hexDigit <- c(0:9, "A", "B", "C", "D", "E", "F")
  isna <- is.na(x)
  y <- x[!isna]
  ans0 <- character(length(y))
  z <- NULL
  while (any(y > 0) | is.null(z)) {
    z <- y%%16
    y <- floor(y/16)
    ans0 <- paste(hexDigit[z + 1], ans0, sep = "")
  }
  ans <- rep(NA, length(x))
  ans[!isna] <- ans0
  ans
})

intToHex <- function(x) {
  y <- as.integer(x);
  class(y) <- "hexmode";
  y <- as.character(y);
  dim(y) <- dim(x);
  y;
}

############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2002-07-08
# * BUG FIX: intToHex(0) gave "". Problem was in as.character.hexmode().
# 2002-05-31
# * intToX() now returns the result with same dimensions as the input.
# 2002-04-13
# * Created from intToOct() and as.character.octmode().
############################################################################
