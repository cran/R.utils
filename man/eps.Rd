%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:16 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{eps}
\alias{eps}


 \title{EPS graphics device}

 \description{
  Device driver for Encapsulated Postscript. This driver is the same as
  the postscript driver where some arguments have different default values.
 }

 \usage{eps(file="Rplot\%03d.ps", horizontal=FALSE, paper="special", ...)}

 \arguments{
   \item{file}{Default file name (pattern).}
   \item{horizontal}{If \code{\link[base:logical]{FALSE}}, an horizontal EPS file is created,
         otherwise a portrait file is created.}
   \item{paper}{}
   \item{...}{Other arguments accepted by \code{postscript()}.}
 }

 \value{
   A plot device is opened; nothing is returned.
 }

 \examples{\dontrun{
   eps("foo.eps")

   # is identical to

   postscript("foo.eps", onefile=TRUE, horizontal=FALSE)

   # and

   dev.print(eps, "foo.eps")

   # is identical to

   dev.print(postscript, "foo.eps", onefile=TRUE, horizontal=FALSE, paper="special")
 }}

 \keyword{device}

 \seealso{
   \code{\link[grDevices]{postscript}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

