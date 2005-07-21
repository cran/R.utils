%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:58:54 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{displayCode}
\alias{displayCode.default}
\alias{displayCode}

\title{Displays the contents of a text file with line numbers and more}

\description{
 Displays the contents of a text file with line numbers and more.
}

\usage{\method{displayCode}{default}(con, numerate=TRUE, lines=-1, wrap=79, highlight=NULL, ...)}

\arguments{
  \item{con}{A \code{\link[base:connections]{connection}}, a \code{\link[base]{character}} string filename, or a \code{\link[base]{character}}
  \code{\link[base]{vector}} (of length two or more) of source code.}
  \item{numerate}{If \code{\link[base:logical]{TRUE}}, line are numbers, otherwise not.}
  \item{lines}{If a single \code{\link[base]{numeric}}, the maximum number of lines to show.
    If -1, all lines are shown. If a \code{\link[base]{vector}} of \code{\link[base]{numeric}}, the lines
    numbers to display.}
  \item{wrap}{The (output) column \code{\link[base]{numeric}} where to wrap lines.}
  \item{highlight}{A \code{\link[base]{vector}} of line number to be highlighted.}
  \item{...}{Not used.}
}

\value{
  Returns nothing.
}

\examples{
file <- system.file("DESCRIPTION", package="R.utils")
cat("-----------------------------------------------------------------\n")
cat(file, ":\n", sep="")
cat("-----------------------------------------------------------------\n")
displayCode(file)
cat("\n")

file <- system.file("CONTENTS", package="R.utils")
cat("-----------------------------------------------------------------\n")
cat(file, ":\n", sep="")
cat("-----------------------------------------------------------------\n")
displayCode(file, numerate=FALSE, lines=100:110, wrap=65)
cat("\n")

file <- system.file("CONTENTS", package="R.utils")
cat("-----------------------------------------------------------------\n")
cat(file, ":\n", sep="")
cat("-----------------------------------------------------------------\n")
displayCode(file, lines=100:110, wrap=65, highlight=c(101,104:108))

}

\author{
  Henrik Bengtsson, \url{http://www.braju.com/R/}
}



\keyword{file}
\keyword{IO}
