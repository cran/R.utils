#########################################################################/**
# @RdocDefault seqToIntervals
#
# @title "Gets all contigous intervals of a vector of indices"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#   \item{idx}{A @vector of @integer indices.}
#   \item{...}{Not used.}
# }
#
# @author
#
# \examples{
#   print(seqToIntervals(1:10))  # [1 10]
#   print(seqToIntervals(c(1:10, 15:18, 20)))  # [1 10; 15 18; 20 20]
# }
#
# @keyword "attribute"
#*/#########################################################################t 
setMethodS3("seqToIntervals", "default", function(idx, ...) {
  idx <- as.integer(idx);
  idx <- unique(idx);
  idx <- sort(idx);
  
  res <- NULL;
  if (length(idx) == 0)
    return(res);

  fromValue <- idx[1];
  toValue <- fromValue-1;
  lastValue <- fromValue;

  count <- 0;
  for (kk in seq(along=idx)) {
    value <- idx[kk];
    if (value - lastValue > 1) {
      toValue <- lastValue;
      res <- c(res, fromValue, toValue);
      fromValue <- value;
      count <- count + 1;
    }
    lastValue <- value;
  }

  if (toValue < fromValue) {
    toValue <- lastValue;
    res <- c(res, fromValue, toValue);
  }

  res <- matrix(res, ncol=2, byrow=TRUE);
  colnames(res) <- c("from", "to");

  res;
})

###########################################################################
# HISTORY: 
# 2005-11-01
# o Created from seqToHumanReadable().
###########################################################################
