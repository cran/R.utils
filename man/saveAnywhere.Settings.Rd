%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:41 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{saveAnywhere.Settings}
\alias{saveAnywhere.Settings}
\alias{Settings.saveAnywhere}
\alias{saveAnywhere.Settings}
\alias{saveAnywhere,Settings-method}


 \title{Saves settings to file}

 \description{
  Saves settings to file. If the settings was read from file, they are by default
  written back to the same file. If this was not the case, it defaults
  to the settings file in the home directory of the current user.
 }

 \usage{\method{saveAnywhere}{Settings}(this, file=NULL, path="~", ...)}

 \arguments{
   \item{file}{A \code{\link[base]{character}} string or a \code{\link[base:connections]{connection}} where to write too.
      If \code{\link[base]{NULL}}, the file from which the settings were read is used. If
      this was not the case, argument \code{path} is used.}
   \item{path}{The default path, if no settings files are specified.
      This defaults to the current user's home directory.}
   \item{...}{Arguments passed to \code{\link[write.Options]{write}()}
      in superclass Options.}
 }

 \value{Returns nothing.}

 \author{Henrik Bengtsson \url{http://www.braju.com/R/}}

 \seealso{
   \code{\link[R.utils:loadAnywhere.Settings]{*loadAnywhere}()}.
   For more information see \code{\link{Settings}}.
 }


\keyword{internal}
\keyword{methods}
\keyword{programming}
