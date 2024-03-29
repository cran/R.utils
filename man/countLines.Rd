%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  countLines.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{countLines}
\alias{countLines.default}
\alias{countLines}

\title{Counts the number of lines in a text file}

\description{
 Counts the number of lines in a text file by counting the number of occurrences of platform-independent
 newlines (CR, LF, and CR+LF [1]), including a last line with neither.
 An empty file has zero lines.
}

\usage{
\method{countLines}{default}(file, chunkSize=5e+07, ...)
}

\arguments{
  \item{file}{A \code{\link[base:connections]{connection}} or a pathname.}
  \item{chunkSize}{The number of bytes read in each chunk.}
  \item{...}{Not used.}
}

\value{
  Returns an non-negative \code{\link[base]{integer}}.
}

\details{
  Both compressed and non-compressed files are supported.
}

\author{Henrik Bengtsson}

\examples{
pathname <- system.file("NEWS.md", package="R.utils");
n <- countLines(pathname);
n2 <- length(readLines(pathname));
stopifnot(n == n2);
}

\references{
  [1] Page \emph{Newline}, Wikipedia, July 2008.
      \url{https://en.wikipedia.org/wiki/Newline}
}


\keyword{programming}
