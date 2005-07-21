# Added '...' to some base functions. These will later be
# turned into default functions by setMethodS3().

getOption <- appendVarArgs(getOption)
inherits <- appendVarArgs(inherits)
parse <- appendVarArgs(parse)


############################################################################
# HISTORY:
# 2005-05-26
# o Added fix for default getOption().
# 2005-02-15
# o Created to please R CMD check.
############################################################################
