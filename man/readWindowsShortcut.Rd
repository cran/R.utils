%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:37 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{readWindowsShortcut}
\alias{readWindowsShortcut.default}
\alias{readWindowsShortcut}

 \title{Reads a Microsoft Windows Shortcut (.lnk file)}

 \usage{\method{readWindowsShortcut}{default}(con, verbose=FALSE, ...)}

 \description{
   Reads a Microsoft Windows Shortcut (.lnk file).
 }

 \arguments{
   \item{con}{A \code{\link[base:connections]{connection}} or a \code{\link[base]{character}} string (filename).}
   \item{verbose}{If \code{\link[base:logical]{TRUE}}, extra information is written while reading.}
   \item{...}{Not used.}
 }

 \value{
   Returns a \code{\link[base]{list}} structure.
 }

 \examples{
filename <- system.file("data-ex/HISTORY.LNK", package="R.utils")
lnk <- readWindowsShortcut(filename)

# Print all information
print(lnk)

# Get the relative path to the target file
history <- file.path(dirname(filename), lnk$relativePath)

# Alternatively, everything in one call
history <- filePath(filename, expandLinks="relative")

file.show(history)

}

 \author{Henrik Bengtsson \url{http://www.braju.com/R/}}

 \seealso{
   \code{\link{filePath}}
 }


\keyword{IO}
