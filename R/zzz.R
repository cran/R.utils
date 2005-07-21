# Allows conflicts. For more information, see library() and
# conflicts() in [R] base.
.conflicts.OK <- TRUE


.First.lib <- function(libname, pkgname) {
  pkg <- Package(pkgname);
  assign(pkgname, pkg, pos=getPosition(pkg));

  # Add a default Verbose object at threshold -1.
  assign("verbose", Verbose(threshold=-1), pos=getPosition(pkg));

  # Make .Last() call finalizeSession() when R finishes.
  tryCatch({
    addFinalizerToLast();
  }, error=function(ex) {
    warning(getMessage(ex));
  })

  onSessionExit(function(...) detachPackage(pkgname));

  cat(getName(pkg), " v", getVersion(pkg), " (", getDate(pkg), ")",
      " successfully loaded. See ?", pkgname, " for help.\n", sep="");
}


.Last.lib <- function(libpath) {
  # Revert to original .Last() function
  if (exists(".LastOriginal", mode="function", envir=.GlobalEnv)) {
    assign(".Last", .LastOriginal, envir=.GlobalEnv);
    rm(".LastOriginal", envir=.GlobalEnv);
  }
} # .Last.lib()

############################################################################
# HISTORY: 
# 2005-06-23
# o Added default Verbose object 'verbose'.
############################################################################
