###########################################################################/**
# @RdocDefault displayCode
#
# @title "Displays the contents of a text file with line numbers and more"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{con}{A @connection, a @character string filename, or a @character 
#   @vector (of length two or more) of source code.}
#   \item{numerate}{If @TRUE, line are numbers, otherwise not.}
#   \item{lines}{If a single @numeric, the maximum number of lines to show.
#     If -1, all lines are shown. If a @vector of @numeric, the lines
#     numbers to display.}
#   \item{wrap}{The (output) column @numeric where to wrap lines.}
#   \item{highlight}{A @vector of line number to be highlighted.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns nothing.
# }
#
# @examples "displayCode.Rex"
#
# \author{
#   Henrik Bengtsson, \url{http://www.braju.com/R/}
# }
#
# @keyword file
# @keyword IO
#*/###########################################################################
setMethodS3("displayCode", "default", function(con, numerate=TRUE, lines=-1, wrap=79, highlight=NULL, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'con':
  if (is.character(con) && length(con) == 1) {
    con <- file(con, open="r");
    on.exit(close(con));
  } else if (inherits(con, "connection")) {
    if (!isOpen(con, rw="read")) {
      con <- open(con, open="r");
      on.exit(close(con));
    }
  } else {
    con <- as.character(con);
  }

  # Argument 'numerate':
  numerate <- as.logical(numerate);

  # Argument 'lines':
  if (!is.numeric(lines))
    throw("Argument 'lines' must be numeric: ", mode(lines));

  lines <- unique(as.integer(lines));

  if (length(lines) == 1) {
    if (is.na(lines))
      lines <- -1;
  } else if (length(lines) > 1) {
    if (any(lines <= 0)) {
      throw("Argument 'lines' must be positive: [", 
                                         min(lines), ",", max(lines), "]");
    }
  }

  # Argument 'wrap':
  if (length(wrap) != 1) {
    throw("Argument 'wrap' must be a single number: ", 
                                               paste(wrap, collapse=", "));
  }

  if (any(!is.finite(wrap)))
    throw("Argument 'wrap' is non-finite: ", wrap);


  # Argument 'highlight':
  if (is.character(highlight)) {
    # Find line number in 'highlight' string.  For example, by passing
    # geterrmessage() we can automatigally highlight the erroneous line.
    pattern <- ".*(line|row)(|s) ([0-9][0-9]*).*";
    if (regexpr(pattern, highlight) != -1) {
      highlight <- gsub(pattern, "\\3", highlight);
      highlight <- as.integer(highlight);
    }
  }

  if (!is.null(highlight) && is.na(highlight)) {
    highlight <- NULL;
  } else {
    highlight <- unique(as.integer(highlight));
  }

  # Read source code lines
  if (is.character(con)) {
    bfr <- con;
  } else {
    bfr <- readLines(con, n=max(lines));
  }

  nlines <- length(bfr);
  if (nlines == 0)
    return();

  # Number the read lines
  numbers <- as.integer(seq(length=nlines));

  # Prepare highlight marks
  marks <- rep(" ", nlines);
  marks[highlight] <- "*";

  if (length(lines) > 1) {
    # Ignore lines not read
    lines <- lines[lines <= length(bfr)];
    bfr <- bfr[lines];
    numbers <- numbers[lines];
    marks <- marks[lines];
  }

  if (all(marks == " "))
    marks <- NULL;

  # Create right-aligned line number strings
  if (numerate) {
    width <- nchar(as.character(nlines));
    fmtstr <- paste("%", width, "d", sep="");
    numbers <- sprintf(fmtstr, numbers);
  } else {
    numbers <- NULL;
  }

  # Create the line prefixes
  if (!is.null(marks) || !is.null(numbers)) {
    prefix <- paste(marks, numbers, "|", sep="");
    width <- nchar(prefix[1]);
    emptyPrefix <- paste(paste(rep(" ", width-1), collapse=""), "|", sep="");
  } else {
    prefix <- NULL;
    width <- 0;
    emptyPrefix <- NULL;
  }

  # Create output lines by wrapping the lines, but not the line numbers
  if (wrap > 0) {
    wrap <- wrap - width;

    bfr2 <- c();
    for (kk in seq(bfr)) {
      if (nchar(bfr[kk]) <= wrap) {
        line <- paste(prefix[kk], bfr[kk], sep="");
      } else {
        # Wrap line at positions:
        wrapAt <- seq(from=1, to=nchar(bfr[kk]), by=wrap);
        line <- c();
        while (length(wrapAt) > 0) {
          line <- c(line, substr(bfr[kk], 1, wrap));
          bfr[kk] <- substring(bfr[kk], wrap+1)
          wrapAt <- wrapAt[-1];
        }
        indent <- c(prefix[kk], rep(emptyPrefix, length=length(line)-1));
        line <- paste(indent, line, sep="");
      }
      bfr2 <- c(bfr2, line);
    }
    bfr <- bfr2;
  }

  bfr <- paste(bfr, collapse="\n");
  bfr <- paste(bfr, "\n", sep="");
  cat(bfr);
})


############################################################################
# HISTORY:
# 2005-06-26
# o Renamed from display File() to displayCode().
# 2005-06-17
# o Added Rdoc help and example.
# o Created.
############################################################################ 
