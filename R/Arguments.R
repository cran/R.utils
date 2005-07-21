###########################################################################/**
# @RdocClass Arguments
#
# @title "Static class to validate and process arguments"
#
# \description{
#  @classhierarchy
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @author
#
# @keyword programming
#*/########################################################################### 
setConstructorS3("Arguments", function(...) {
  extend(Object(), "Arguments");
})




#########################################################################/**
# @RdocMethod getReadablePathname
#
# @title "Gets a readable pathname"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{file}{A @character string specifying the file.}
#   \item{path}{A @character string specifying the path.}
#   \item{mustExists}{If @TRUE, the pathname must exists and be readable,
#     otherwise an exception is thrown. If @FALSE, no such test is 
#     performed.}
#   \item{absolutePath}{If @TRUE, the absolute pathname is returned.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string of the absolute pathname of the file.
# }
#
# @author
#
# \seealso{
#   @seemethod "getWritablePathname"
#   @see "R.utils::filePath".
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getReadablePathname", "Arguments", function(static, file=NULL, path=NULL, mustExists=TRUE, absolutePath=TRUE, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'file':
  if (inherits(file, "connection"))
    throw("In this context, argument 'file' cannot be a connection.");
  file <- getCharacter(static, file);

  # Argument 'path':
  path <- getCharacter(static, path, length=c(0,1));

  # Argument 'mustExists':
  mustExists <- getLogical(static, mustExists);

  # Argument 'mustExists':
  absolutePath <- getLogical(static, absolutePath);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Process arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pathname <- filePath(path, file);
  if (absolutePath)
    pathname <- getAbsolutePath(pathname);
  if (mustExists) {
    # Check if file exists
    if (!file.exists(pathname))
      throw("File not found: ", pathname);

    # Check if file permissions allow reading
    if (file.access(pathname, mode=4) == -1)
      throw("No permission to read file: ", pathname);
  }
    
  pathname;
}, static=TRUE)


#########################################################################/**
# @RdocMethod getWritablePathname
#
# @title "Gets a writable pathname"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @seemethod "getReadablePathname".}
#   \item{overwrite}{If @TRUE and \code{overwrite} is @TRUE, existing
#     files are overwritten. Otherwise, and Exception is thrown.}
#   \item{mkdirs}{If @TRUE, \code{overwrite} is @TRUE, and the path to
#     the file does not exist, it is (recursively) created.}
# }
#
# \value{
#  Returns a @character string of the absolute pathname of the file.
#  If the argument was invalid an @see "R.oo::Exception" is thrown.
# }
#
# @author
#
# \seealso{
#   @seemethod "getReadablePathname".
#   @see "R.utils::filePath".
#   @see "R.utils::mkdirs".
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getWritablePathname", "Arguments", function(static, ..., overwrite=FALSE, mkdirs=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'overwrite':
  overwrite <- getLogical(static, overwrite);

  # Argument 'mkdirs':
  mkdirs <- getLogical(static, mkdirs);

  # Create pathname
  pathname <- getReadablePathname(static, ..., mustExists=FALSE);

  if (isFile(pathname)) {
    # Check if it is ok to overwrite file
    if (!overwrite) {
      throw("File already exists and will not be overwritten: ", pathname);
    }

    # Check if file permissions allow writing
    if (file.access(pathname, mode=2) == -1)
      throw("No permission to (over-)write file: ", pathname);
  } else {
    # Check if parent directory exists
    parent <- getParent(pathname);
    if (!isDirectory(parent)) {
      # Check if parent directory should be created
      if (!mkdirs)
        throw("Filepath does not exist: ", parent);
      if (!mkdirs(parent))
  	throw("Could not create file path: ", parent);
    }
  }

  pathname;
}, static=TRUE)





#########################################################################/**
# @RdocMethod getVector
#
# @title "Validates a vector"
#
# \description{
#  @get "title" by checking its length (number of elements).
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{A single @vector.}
#   \item{length}{A @numeric @vector of length two or more. If two, it
#     is the minimum and maximum length of \code{x}. Elsewise it is the 
#     set of possible lengths of \code{x}.}
#   \item{.name}{A @character string for name used in error messages.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns the same @vector, if it is valid. Otherwise an exception is 
#  thrown.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getVector", "Arguments", function(static, x, length=NULL, .name=NULL, ...) {
  if (length(length) == 0)
    return(x);

  if (is.null(.name))
    .name <- as.character(deparse(substitute(ss)));

  if (length[1] > 0 && !is.vector(x))
    throw(sprintf("Argument '%s' is not a vector: %s", .name, mode(x)));

  xlen <- length(x);

  if (length(length) == 1)
    length <- c(1,length);

  if (length(length) == 2) {
    if (xlen < length[1] || xlen > length[2]) {
      if (length[1] == length[2] && length[1] == 1) {
        throw(sprintf("Argument '%s' should be a single value not %d values.", .name, xlen));
      } else if (length[1] == length[2]) {
        throw(sprintf("Number of elements in argument '%s' should be exactly %d not %d value(s).", .name, length[1], xlen));
      } else {
        throw(sprintf("Number of elements in argument '%s' is out of range [%d,%d]: %d", .name, length[1], length[2], xlen));
      }
    }
  } else {
    if (!xlen %in% length) {
      throw(sprintf("Number of elements in argument '%s' is not in {%s}: %d", 
                                 .name, seqToHumanReadable(length), xlen, ));
    }
  }

  x;
}, static=TRUE, private=TRUE)




#########################################################################/**
# @RdocMethod getCharacters
# @aliasmethod getCharacter
#
# @title "Coerces to a character vector and validates"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{s}{A @vector.}
#   \item{nchar}{A @numeric @vector of length one or two. If one,
#     the maximum number of characters ("length") in \code{s}. If two, 
#     the minimum and maximum length of \code{s}.}
#   \item{gString}{If @TRUE, each string is treated as a @see "GString".}
#   \item{.name}{A @character string for name used in error messages.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character @vector, if it is valid. Otherwise an exception is 
#  thrown.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getCharacters", "Arguments", function(static, s, length=NULL, trim=FALSE, nchar=NULL, gString=TRUE, .name=NULL, ...) {
  if (is.null(.name))
    .name <- as.character(deparse(substitute(s)));
  s <- getVector(static, s, length=length, .name=.name);

  # Nothing to check?
  if (length(s) == 0)
    return(s);

  # Coerce to GString, the character string, and optionally trim it.
  s <- unlist(lapply(s, FUN=function(x) {
    x <- as.character(GString(x));
    if (trim)
      x <- trim(x);
    x;
  }))

  names(s) <- NULL;

  # Nothing to check?
  if (is.null(nchar))
    return(s);
 
  if (length(nchar) == 1)
    nchar <- c(1,nchar);
  
  # Check the string length of each character string
  for (kk in seq(length=length(s))) {
    slen <- nchar(s[kk]);
    if (slen < nchar[1] || slen > nchar[2]) {
      throw(sprintf("String length of elements #%d in '%s' is out of range [%d,%d]: %d '%s'", kk, .name, nchar[1], nchar[2], slen, s[kk]));
    }
  }

  unlist(s);
}, static=TRUE)

setMethodS3("getCharacter", "Arguments", function(static, ..., length=NULL) {
  getCharacters(static, ..., length=length);
}, static=TRUE)




#########################################################################/**
# @RdocMethod getNumerics
# @aliasmethod getNumeric
#
# @title "Coerces to a numeric vector and validates"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{A @vector.}
#   \item{range}{Two @numerics for the allowed ranged. If @NULL, range is
#     not checked.}
#   \item{asMode}{A @character specifying the mode to coerce to.}
#   \item{disallow}{A @character @vector specifying diallowed value sets,
#                     i.e. \code{"NA"}, \code{"NaN"}, and/or \code{"Inf"}.}
#   \item{...}{Arguments passed to @method "getVector".}
#   \item{.name}{A @character string for name used in error messages.}
# }
#
# \value{
#  Returns a @numeric @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getNumerics", "Arguments", function(static, x, range=NULL, asMode="numeric", disallow=NULL, ..., .name=NULL) {
  if (is.null(.name))
    .name <- as.character(deparse(substitute(x)));
  x <- getVector(static, x, ..., .name=.name);

  # Coerce the mode of 'x'
  if (!is.null(asMode))
    mode(x) <- asMode;

  # Nothing to do?
  if (length(x) == 0)
    return(x);

  if (!is.null(disallow)) {
    if ("NaN" %in% disallow && any(is.nan(x))) {
      throw(sprintf("Argument '%s' contains %d NaN value(s).", 
                                                   .name, sum(is.nan(x))));
    }

    if ("NA" %in% disallow && any(is.na(x) & !is.nan(x))) {
      throw(sprintf("Argument '%s' contains %d NA value(s).", 
                                                    .name, sum(is.na(x))));
    }

    # For conveniency, disallow 'Inf' here too; other range takes care of it.
    if ("Inf" %in% disallow && any(is.infinite(x))) {
      throw(sprintf("Argument '%s' contains %d NA value(s).", 
                                             .name, sum(is.infinite(x))));
    }
  }

  # Nothing to check?
  if (is.null(range))
    return(x);
  
  xrange <- range(x, na.rm=TRUE);
  if (xrange[1] < range[1] || xrange[2] > range[2]) {
    xrange <- as.character(xrange);
    range <- as.character(range);
    throw(sprintf("Range of argument '%s' is out of range [%s,%s]: [%s,%s]", 
                          .name, range[1], range[2], xrange[1], xrange[2]));
  }

  x;
}, static=TRUE)

setMethodS3("getNumeric", "Arguments", function(static, ..., length=1) {
  getNumerics(static, ..., length=length);
}, static=TRUE)




#########################################################################/**
# @RdocMethod getDoubles
# @aliasmethod getDouble
#
# @title "Coerces to a double vector and validates"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @method "getNumeric".}
#   \item{disallow}{Disallowed values. See @method "getNumerics" for details.}
# }
#
# \value{
#  Returns a @double @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getDoubles", "Arguments", function(static, ..., disallow=c("NA","NaN")) {
  getNumerics(static, ..., asMode="double", disallow=disallow);
}, static=TRUE)

setMethodS3("getDouble", "Arguments", function(static, ..., length=1) {
  getDoubles(static, ..., length=length);
}, static=TRUE)




#########################################################################/**
# @RdocMethod getIntegers
# @aliasmethod getInteger
#
# @title "Coerces to a integer vector and validates"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @method "getNumeric".}
#   \item{disallow}{Disallowed values. See @method "getNumerics" for details.}
# }
#
# \value{
#  Returns a @integer @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getIntegers", "Arguments", function(static, ..., disallow=c("NA","NaN")) {
  getNumerics(static, ..., asMode="integer", disallow=disallow);
}, static=TRUE)

setMethodS3("getInteger", "Arguments", function(static, ..., length=1) {
  getIntegers(static, ..., length=length);
}, static=TRUE)



#########################################################################/**
# @RdocMethod getIndices
# @aliasmethod getIndex
#
# @title "Coerces to a integer vector and validates"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @method "getInteger".}
#   \item{range}{Allowed range. See @method "getNumeric" for details.}
# }
#
# \value{
#  Returns a @integer @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getIndices", "Arguments", function(static, ..., range=c(1,Inf)) {
  getIntegers(static, ..., range=range);
}, static=TRUE)

setMethodS3("getIndex", "Arguments", function(static, ..., length=1) {
  getIndices(static, ..., length=length);
}, static=TRUE)




#########################################################################/**
# @RdocMethod getLogicals
# @aliasmethod getLogical
#
# @title "Coerces to a logical vector and validates"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{A @vector.}
#   \item{disallow}{A @character @vector specifying diallowed value sets
#      after coercing, i.e. \code{"NA"}.}
#   \item{...}{Arguments passed to @method "getVector".}
#   \item{.name}{A @character string for name used in error messages.}
# }
#
# \value{
#  Returns a @numeric @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getLogicals", "Arguments", function(static, x, ..., disallow=c("NA", "NaN"), coerce=FALSE, .name=NULL) {
  if (is.null(.name))
    .name <- as.character(deparse(substitute(x)));
  x <- getVector(static, x, ..., .name=.name);

  # Coerce to logicals?
  if (coerce)
    x <- as.logical(x);

  if (!is.null(disallow)) {
    if ("NA" %in% disallow && any(is.na(x))) {
      throw(sprintf("Argument '%s' contains %d NA value(s).", 
                                                    .name, sum(is.na(x))));
    }
  }

  # Assert that 'x' is logical before returning
  if (any(!is.logical(x)))
    throw(sprintf("Argument '%s' is non-logical: %s", .name, class(x)));

  x;
}, static=TRUE)

setMethodS3("getLogical", "Arguments", function(static, ..., length=1) {
  getLogicals(static, ..., length=length);
}, static=TRUE)



#########################################################################/**
# @RdocMethod getVerbose
#
# @title "Coerces to Verbose object"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{verbose}{A single object. If a @see "Verbose", it is immediately
#      returned.  If a @numeric value, it is used as the threshold.
#      Otherwise the object is coerced to a @logical value and if @TRUE,
#      the threshold is \code{defaultThreshold}.}
#   \item{defaultThreshold}{A @numeric value for the default threshold, if
#       \code{verbose} was interpreted as a @logical value.}
#   \item{useNullVerbose}{If \code{verbose} can be interpreted as @FALSE, 
#       return a @see NullVerbose object if @TRUE.}
#   \item{...}{Not used.}
#   \item{.name}{A @character string for name used in error messages.}
# }
#
# \value{
#  Returns a @see Verbose (or a @see "NullVerbose") object.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
#*/#########################################################################
setMethodS3("getVerbose", "Arguments", function(static, verbose, defaultThreshold=-1, useNullVerbose=TRUE, ..., .name=NULL) {
  if (inherits(verbose, "Verbose"))
    return(verbose);

  if (is.null(.name))
    .name <- as.character(deparse(substitute(verbose)));

  if (is.numeric(verbose)) {
    verbose <- getDouble(static, verbose, .name=.name);
    verbose <- Verbose(threshold=verbose);
  } else {
    verbose <- getLogical(static, verbose, .name=.name);
    if (useNullVerbose) {
      verbose <- NullVerbose();
    } else {
      defaultThreshold <- getNumeric(static, defaultThreshold);
      verbose <- Verbose(threshold=defaultThreshold);
    }
  }

  verbose;
}, static=TRUE)



############################################################################
# HISTORY:
# 2005-07-19
# o BUG FIX: getCharacters() would not coerce Object:s correctly.
# 2005-07-07
# o getCharacters() returned attribute 'names' too. Removed.
# 2005-06-20
# o Added argument 'absolutePath' to getReadablePathname().
# 2005-06-18
# o Added static methods getVector(), getNumeric/s(), getDouble/s(), 
#   getInteger/s(), getIndices/getIndex(), and getLogical/s(). These should
#   be very handy. Also added getVector().
#   Not sure if getVector() should be renamed to checkLength(), and even
#   be moved to the Assert class.  Not sure where the assert class is 
#   heading.
# 2005-05-31
# o Created from former File$validateFileAndPath().
############################################################################
