%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  
% 
% on Mon Apr 03 17:26:59 2006.
% 
% Generator was the Rdoc class, which is part of the R.oo package written
% by Henrik Bengtsson, 2001-2006.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getHostname.System}
\alias{getHostname.System}
\alias{System.getHostname}
\alias{getHostname.System}
\alias{getHostname,System-method}


 \title{Retrieves the computer name of the current host}

 \description{
  Retrieves the computer name of the current host.
 }

 \synopsis{getHostname.System(static, ...)}
\usage{System$getHostname(...)}

 \value{
  Returns a \code{\link[base]{character}} string.
 }

 \details{
  First, this function checks the system environment variables \code{HOST},
  \code{HOSTNAME}, and \code{COMPUTERNAME}.
  Second, it checks \code{Sys.info()["nodename"]} for host name details.
  Finally, it tries to query the system command \code{uname -n}.
 }

 \seealso{
   \code{\link[R.utils:getUsername.System]{*getUsername}()}.
 }
\keyword{internal}
\keyword{methods}
