%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Thu Jul 21 11:59:51 2005.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2004.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getUsername.System}
\alias{getUsername.System}
\alias{System.getUsername}
\alias{getUsername.System}
\alias{getUsername,System-method}


 \title{Retrieves the name of the user running R}

 \description{
  Retrieves the name of the user running R.
 }

 \synopsis{getUsername.System(static, ...)}
\usage{System$getUsername(...)}

 \value{
  Returns a \code{\link[base]{character}} string.
 }

 \details{
  First this function checks the system environment variables \code{USER},
  and \code{USERNAME}.
  Finally, it tries to query the system command \code{whoami}.
 }

 \seealso{
   \code{\link[R.utils:getHostname.System]{*getHostname}()}.
 }
\keyword{internal}
\keyword{methods}
