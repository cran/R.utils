###########################################################################/**
# @RdocClass System
#
# @title "Static class to query information about the system"
#
# \description{
#  @classhierarchy
#
#  The System class contains several useful class fields and methods. It 
#  cannot be instantiated.
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @author
#*/###########################################################################
setConstructorS3("System", function() {
  extend(Object(), "System")
})



########################################################################/**
# @RdocMethod getHostname
#
# @title "Retrieves the computer name of the current host"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \value{
#  Returns a @character string.
# }
# 
# \details{
#  First this function checks the system environment variables \code{HOST},
#  \code{HOSTNAME}, and \code{COMPUTERNAME}. 
#  Finally, it tries to query the system command \code{uname -n}.
# }
#
# \seealso{
#   @seemethod "getUsername".
# }
#**/#######################################################################
setMethodS3("getHostname", "System", function(static, ...) {
  host <- Sys.getenv(c("HOST", "HOSTNAME", "COMPUTERNAME"));
  host <- host[host != ""];
  if (length(host) == 0) {
    host <- readLines(pipe("/usr/bin/env uname -n"));
  }
  host[1];
}, static=TRUE)


########################################################################/**
# @RdocMethod getUsername
#
# @title "Retrieves the name of the user running R"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \value{
#  Returns a @character string.
# }
# 
# \details{
#  First this function checks the system environment variables \code{USER},
#  and \code{USERNAME}.
#  Finally, it tries to query the system command \code{whoami}.
# }
#
# \seealso{
#   @seemethod "getHostname".
# }
#**/#######################################################################
setMethodS3("getUsername", "System", function(static, ...) {
  host <- Sys.getenv(c("USER", "USERNAME"));
  host <- host[host != ""];
  if (length(host) == 0) {
    host <- readLines(pipe("/usr/bin/env whoami"));
  }
  host[1];
}, static=TRUE)




###########################################################################/**
# @RdocMethod currentTimeMillis
#
# @title "Get the current time in milliseconds"
#
# @synopsis
#
# \description{
#   @get "title".
# }
#
# \value{
#   Returns an @integer.
# }
#
# @author
#
# \seealso{
#   @see "base::Sys.time".
#   @see "base::proc.time".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("currentTimeMillis", "System", function(this, ...) {
  secs <- as.numeric(Sys.time());
  times <- proc.time();
  time <- times[2];  # System CPU time

  # CPU time is not available on Win 98/Me;
  if (is.na(time))
    time <- times[3]; # Total elapsed times
  (secs + time %% 1)*1000;
}, static=TRUE);




###########################################################################/**
# @RdocMethod parseDebian
#
# @title "Parses a string, file or connection for Debian formatted parameters"
#
# @synopsis
#
# \arguments{
#   \item{text}{The text to be parsed. Default value is @NULL.}
#   \item{file}{Name file, a \code{File} object or connection to be parsed.
#     Default value is @NULL.}
#   \item{keys}{The keys (names of the parameters) to be retrieved.
#     If @NULL all fields are returned. Default value is @NULL.}
#
#  Either, \code{text} or \code{file} must be given.
# }
#
# \description{
#   Parses a text, file or a connection for Debian formatted parameters.
#   A file in Debian format contains rows with parameters of the form
#   \code{KEY=VALUE}. It is allowed to have duplicated keys.
# }
#
# \value{
#   Returns a named @list of parameter values.
# }
#
# \details{
# }
#
# \examples{
#  file <- file.path(Package("R.utils")$path, "DESCRIPTION")
#  l <- System$parseDebian(file=file)
#  print(l)
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("parseDebian", "System", function(this, text=NULL, file=NULL, keys=NULL, ...) {
  if (is.null(text) && is.null(file))
    throw("Either argument text or argument file must be specified.");

  # Retrieve the text to be parsed.
  if (is.null(text)) {
    file <- as.character(file);
    text <- scan(file=file, what="", sep="\n", quiet=TRUE);
    text <- paste(text, "", sep="");
  } else {
    text <- unlist(text);
    text <- strsplit(text, "\n");
    text <- unlist(text);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Get the keys (names) and values of the parameters
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  nbrOfLines <- length(text);
  keyMatches <- regexpr("^[^:]*:", text);
  keyLines <- which(keyMatches == 1);
  keyLengths <- attr(keyMatches, "match.length")[keyLines]-1;
  pkeys <- substring(text[keyLines], 1, keyLengths);
  text[keyLines] <- substring(text[keyLines], keyLengths+2);
  valueNbrOfLines <- c(keyLines, 0) - c(0, keyLines);
  valueNbrOfLines <- valueNbrOfLines[-length(valueNbrOfLines)];	
  valueNbrOfLines <- valueNbrOfLines[-1];
  len <- length(valueNbrOfLines);
  valueNbrOfLines[len+1] <- keyLines[len+1]-length(text)+1;
  values <- c();
  for (k in 1:length(keyLines)) {
    valueLines <- keyLines[k] + 1:valueNbrOfLines[k] - 1;
    value <- paste(text[valueLines], sep="", collapse="\n");
    values <- c(values, value);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Some cleanup of values
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 1. Replace all '\r' with '\n'.
  values <- gsub("\r", "\n", values);  
  # 2. At the end of each line, remove all whitespaces and add a space.
  values <- gsub("[ \t]*\n", " \n", values);  
  # 3. At the beginning of each line, remove all whitespaces.
  values <- gsub("\n[ \t]*", "\n", values);  
  # 4. Replace all lines that contains a single '.' with '\r'.
  values <- gsub("\n[.] \n", "\n\r\n", values);  
  values <- gsub("\n[.] \n", "\n\r\n", values);  # since we miss every second!
  # 4. Remove all '\n'.
  values <- gsub("\n", "", values);  
  # 1. Replace all '\r' with '\n' (single '.' lines).
  values <- gsub("\r", "\n", values);  
  # 4. Removes prefix whitespaces
  values <- gsub("^[ \t]", "", values);
  # 5. Removes suffix whitespaces
  # For some reason, the gsub below crashes once in a while, i.e. once every
  # 20:th time. Strange! But, I think I tracked it down to happen when one
  # of the strings in values has zero length. So, by making all zero length
  # strings equal to " " the gsub call won't crash. I think! /hb 2001-05-11
  values[nchar(values) == 0] <- " ";
  values <- gsub("[ \t]*$", "", values);

  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Return the wanted parameters
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.null(keys)) {
    parameters <- as.list(values);
    names(parameters) <- pkeys;
  } else {
    idx <- omit.na(match(keys, pkeys));
    parameters <- as.list(values[idx]);
    names(parameters) <- pkeys[idx];
  }
  
  parameters;
}, static=TRUE);




###########################################################################/**
# @RdocMethod openBrowser
#
# @title "Opens an HTML document using the OS default HTML browser"
#
# @synopsis
#
# \arguments{
#   \item{query}{The path to document to be opened by the browser.}
# }
#
# \description{
#  @get "title". Note that this
#  call is dependent on the operating system (currently only Windows and
#  Unix are supported).
#  The document given by \code{query} can either be a local file or a
#  web page. If the \code{query} was given as non-url string, i.e. as a
#  standard file pathname, the method will automatically check if the
#  file exists and conform the query to a correct url starting with
#  \code{file:}. The used url will be returned as a string.
#
#  Any suggestion how implement this on Apple system are welcome!
# }
#
# \value{
#   Returns the url of the \code{query}.  
# }
#
# \details{
#   It is hard to create a good cross-platform \code{openBrowser()} method,
#   but here is one try.
#
#   In the following text \code{<browser>} is the value returned by
#   \code{getOption("browser")} and \code{<url>} is the URL conformed
#   query, which starts with either \code{file:} or \code{http:}.
#
#   On a \emph{Windows} system, if \code{<browser>} is not @NULL,
#   first
#
#     \code{shell.exec(<browser> <url>)}
#
#   is tried. If this fails, then
#
#     \code{shell.exec(<url>)}
#
#   is tried. Using this latter approach will \emph{not} guarantee that
#   an HTML browser will open the url, e.g. depending on the Windows file
#   associations, a \code{*.txt} file might be opened by NotePad. However,
#   it will most likely open something.
#   If \code{<browser>} contains spaces, make sure it is quoted.
#
#   On \emph{Unix} systems, \code{system()} will be used to call:
#
#   \code{ <browser> -remote "openURL(<url>)" 2> /dev/null || <browser> <url> &}
#
# }
#
# \examples{\dontrun{
#   System$openBrowser("http://www.r-project.org/")
# }}
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("openBrowser", "System", function(this, query, ...) {
  require(R.io);
  
  url <- as.character(query);
  if (regexpr("^[a-z][a-z]*:", url) == -1) {
    # Assume we are dealing with a file
    file <- filePath(url);
    if (!file.exists(file))
      throw("File not found: ", file);
    url <- toUrl(file);
  }

  browser <- getOption("browser");
  if (!is.null(browser)) {
    # Check if 'browser' contains spaces, but the user forgot to quote it.
    if (regexpr(" ", browser) != -1) {
      if (regexpr("^\"", browser) == -1 || regexpr("\"$", browser) == -1) {
        browser <- paste("\"", browser, "\"", sep="");
        msg <- paste("getOption(\"browser\") contains spaces, but it is not quoted:", browser);
        warning(msg);
      }
    }
  }

  OST <- .Platform$OS.type;
  # ---------------------------------------------------------------------
  # W i n d o w s
  # ---------------------------------------------------------------------
  if (OST == "windows") {
    first <- 1;
    tmp <- tolower(url);

    if (is.null(browser) && 
      !startsWith(tmp, "http:") && !startsWith(tmp, "file:") &&
        !endsWith(tmp, ".html") && !endsWith(tmp, ".htm")) {
      first <- 2;
      msg <- paste("The extension of the URL might not be opened in a HTML browser on your Windows system: ", url, sep="");
      warning(msg);
    }

    if (first == 1) {
      # 1. Try to call <url>
      shell.exec(url);
    } else {
      # 2a. Try to call <browser> <url>
      loaded <- FALSE;
      if (!is.null(browser)) {
        # 2a.i.
        cmd <- paste(browser, url);
        res <- system(cmd, wait=FALSE);
        loaded <- (res == 0);
        if (!loaded) {
          # 2a.ii. Check if "start" exists, because that might help us
          start <- "start /minimized";
          tryCatch({
            system(start, intern=TRUE)
          }, error = function(ex) {
            start <<- NULL
          })
          cmd <- paste(start, browser, url);
          res <- system(cmd, wait=FALSE);
          loaded <- (res == 0);
        }
        if (!loaded) {
          warning("Could not find the browser specified in options(). Please make sure it is specified with the absolute path *and* if it contains spaces, it has to be quoted.");
        }
      }
      # 2b. Try to call <url>
      if (!loaded)
        shell.exec(url);
    }
  }  
  # ---------------------------------------------------------------------
  # U n i x
  # ---------------------------------------------------------------------
  else if (OST == "unix") {
    if (is.null(browser))
       throw("options(\"browser\") not set.");
    # 1. Try to call <browser> -remote "openURL(<url>)", which opens the
    #    document in an already existing browser.
    cmd1 <- paste(browser, " -remote \"openURL(", url, ")\" 2>/dev/null", sep="");
    # 2. Try to call <browser> <url>, which opens the document in a new
    #    browser.
    cmd2 <- paste(browser, url);

    # If 1 fails, try 2.
    cmd  <- paste(cmd1, "||", cmd2);
    system(cmd);
  } else {
    throw("Don't know how to open the browser on", OST);
  }

  # Return the url, which was tried to be opened.
  invisible(url);
}, static=TRUE);


#########################################################################/**
# @RdocMethod findGhostscript
#
# @title "Searches for the ghostview binary on the current system"
#
# \description{
#   @get "title". 
#  
#   Currently only Windows and Unix is supported.
# }
#
# @synopsis
#
# \arguments{
#   \item{updateRGSCMD}{If @TRUE and ghostscript is found, then the system
#     environment variable \code{R_GSCMD} is set to the (first) path found. 
#     This variable is used by the \code{bitmap()} device.}
#   \item{...}{Not used.}
# }
#
#
# \value{
#   Returns a @vector of full pathnames where ghostscript is found. 
# }
#
# \examples{
#   print(System$findGhostscript())
# }
#
# @author
#
# \seealso{
#   @see "base::Sys.getenv".
#   @see "grDevices::dev2bitmap".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("findGhostscript", "System", function(static, updateRGSCMD=TRUE, ...) {
  pathname <- NULL;

  OST <- .Platform$OS.type;
  if (OST == "windows") {
    # Default installation is "C:\gs\gs8.15\bin\gswin32c.exe";
    drives <- "C:";
    paths <- "gs";
    pattern <- "gswin32c.exe$";

    for (drive in drives) {
      for (path in paths) {
        path0 <- file.path(drive, path);
        pathname <- list.files(pattern=pattern, path=path0, recursive=TRUE, 
                                                           full.names=TRUE);
        if (length(pathname) > 0)
          break;
      } # for (path ...)
      if (length(pathname) > 0)
        break;
    } # for (drive ...)
  } else if (OST == "unix") {
    pathname <- system("which gs", intern=TRUE);
  } else {
    warning("Unsupported operating system: ", OST);
    return(NULL);
  }

  if (length(pathname) > 0) {
    if (length(pathname) > 1)
      pathname <- pathname[1];
    Sys.putenv("R_GSCMD"=pathname);
  } else if (updateRGSCMD) {
    warning("Ghostscript not found.");
  }

  pathname;
}, static=TRUE)


############################################################################
# HISTORY:
# 2005-07-18
# o Example for parseDebian() used package R.lang. Fixed.
# 2005-05-29
# o Removed many unecessary (never used) methods and fields from the class:
#   fields 'inn', 'err' and 'out' with corresponding methods setIn(), 
#   setErr() and setOut(). In addition, indentityHashCode() getenv(), gc(),
#   exit(), stop(), getProperties(), and getProperty() were removed.
# o Moved System from R.lang to R.utils.
# 2005-03-07
# o Added static method findGhostscript().
# 2004-07-25
# o Added getUsername() and getHostname(). This was formerly in 
#   R.jobs::Jobs, but is better suited here.
# 2003-04-16
# o Updated the Rdocs.
# 2003-04-15
# o Updated openBrowser() to accept any protocols such as mailto: etc.
#   Note that an URL can only be 256 characters long.
# 2002-12-07
# o Added a test for "file://" (similar to "http://") to openBrowser() when
#   called on a Windows system.
# 2002-05-21
# * BUG FIX: In openBrowser() the system() call under Unix does not contain
#   a 'wait' argument!
# 2002-03-29
# * Updated openBrowser() to be somewhat smarter by using 'start' as a
#   third alternative for Windows systems.
# 2002-03-26
# * BUG FIX: Forgot some debug code in openBrowser(), which made the
#   function to fail on non Unix systems.
# 2002-03-08
# * Added Rdoc for class, getenv() and parseDebian().
# * Made <browser> more bullit-proof. If it is not quoted, but needs to be,
#   it is fixed and a warning is given.
# * If 'browser' is set in options() and it is found, system() will also
#   be used for Windows, otherwise system.exec(). The reason for this is
#   that file with "incorrect" extension, e.g. *.txt might be loaded by
#   NotePad instead of Netscape or Internet Explorer.
# 2002-03-07
# * Added openBrowser(), whose main part was written by Robert Gentleman.
#   I added the code which assert that the url is a correct url; this makes
#   use of the File class.
# 2002-03-06
# * Rewritten to make use of setMethodS3()'s.
# * Removed obsolete getInternalReferences()
# 2001-06-07
# * Added some Rdoc comments.
# 2001-05-14
# * Added getInternalReferences() for improving gco() performance.
# 2001-05-09
# * Added parseDebian(). Replaces parse.dcf() which does not work correctly.
# 2001-05-04
# * Now supports formal attributes.
# 2001-05-02
# * Added user.dir to properties.
# 2001-05-01
# * Added getenv().
# * made currentTimeMillis() using Sys.time().
# * Added R.home and R.class.path to the properties.
# 2001-04-30
# * Added setErr(), setIn(), exit(), gc(), and currentTimeMillis().
# 2001-04-29
# * Created.
############################################################################
