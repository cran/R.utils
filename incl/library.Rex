function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
    logical.return = FALSE, warn.conflicts = TRUE, keep.source = getOption("keep.source.pkgs"), 
    verbose = getOption("verbose"), version) 
{
    testRversion <- function(pkgInfo, pkgname) {
        current <- getRversion()
        if (length(Rdeps <- pkgInfo$Rdepends) > 1) {
            target <- Rdeps$version
            res <- eval(parse(text = paste("current", Rdeps$op, 
                "target")))
            if (!res) 
                stop(gettextf("This is R %s, package '%s' needs %s %s", 
                  current, pkgname, Rdeps$op, target), call. = FALSE, 
                  domain = NA)
        }
        if (!is.null(built <- pkgInfo$Built)) {
            if (built$R < "2.0.0") 
                stop(gettextf("package '%s' was built before R 2.0.0: please re-install it", 
                  pkgname), call. = FALSE, domain = NA)
            if (built$R > current) 
                warning(gettextf("package '%s' was built under R version %s", 
                  pkgname, as.character(built$R)), call. = FALSE, 
                  domain = NA)
            if (.Platform$OS.type == "unix") {
                platform <- built$Platform
                if (length(grep("\\w", platform)) && !testPlatformEquivalence(platform, 
                  R.version$platform)) 
                  stop(gettextf("package '%s' was built for %s", 
                    pkgname, platform), call. = FALSE, domain = NA)
            }
        }
        else stop(gettextf("package '%s' has not been installed properly\n", 
            pkgname), gettext("See the Note in ?library"), call. = FALSE, 
            domain = NA)
    }
    checkNoGenerics <- function(env, pkg) {
        nenv <- env
        ns <- .Internal(getRegisteredNamespace(as.name(libraryPkgName(pkg))))
        if (!is.null(ns)) 
            nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE)) 
            TRUE
        else {
            length(objects(env, pattern = "^\\.__M", all = TRUE)) == 
                0
        }
    }
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value", 
            ".Random.seed", ".First.lib", ".Last.lib", ".packageName", 
            ".noGenerics", ".required", ".no_S3_generics")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ob <- objects(lib.pos, all = TRUE)
        if (!nogenerics && .isMethodsDispatchOn()) {
            these <- objects(lib.pos, all = TRUE)
            these <- these[substr(these, 1, 6) == ".__M__"]
            gen <- gsub(".__M__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__M__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        fst <- TRUE
        ipos <- seq(along = sp)[-c(lib.pos, match("Autoloads", 
            sp))]
        for (i in ipos) {
            obj.same <- match(objects(i, all = TRUE), ob, nomatch = 0)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if (length(Classobjs)) 
                  same <- same[-Classobjs]
                if (length(same)) {
                  if (fst) {
                    fst <- FALSE
                    cat(gettextf("\nAttaching package: '%s'\n\n", 
                      package))
                  }
                  cat("\n\tThe following object(s) are masked", 
                    if (i < lib.pos) 
                      "_by_"
                    else "from", sp[i], ":\n\n\t", same, "\n\n")
                }
            }
        }
    }
    libraryPkgName <- function(pkgName, sep = "_") unlist(strsplit(pkgName, 
        sep, fixed = TRUE))[1]
    libraryPkgVersion <- function(pkgName, sep = "_") {
        splitName <- unlist(strsplit(pkgName, sep, fixed = TRUE))
        if (length(splitName) > 1) 
            splitName[2]
        else NULL
    }
    libraryMaxVersPos <- function(vers) {
        if (length(vers) == 0) 
            return(integer(0))
        vers <- package_version(vers)
        min(which(vers == max(vers)))
    }
    runUserHook <- function(pkgname, pkgpath) {
        hook <- getHook(packageEvent(pkgname, "attach"))
        for (fun in hook) try(fun(pkgname, pkgpath))
    }
    bindTranslations <- function(pkgname, pkgpath) {
        popath <- file.path(pkgpath, "po")
        if (!file.exists(popath)) 
            return()
        bindtextdomain(pkgname, popath)
        bindtextdomain(paste("R", pkgname, sep = "-"), popath)
    }
    if (!missing(package)) {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        if (!character.only) 
            package <- as.character(substitute(package))
        if (package %in% c("ctest", "eda", "modreg", "mva", "nls", 
            "stepfun", "ts")) {
            have.stats <- "package:stats" %in% search()
            if (!have.stats) 
                require("stats")
            old <- "stats"
            warning(gettextf("package '%s' has been merged into '%s'", 
                package, old), call. = FALSE, domain = NA)
            return(if (logical.return) TRUE else invisible(.packages()))
        }
        if (package == "mle") {
            have.stats4 <- "package:stats4" %in% search()
            if (!have.stats4) 
                require("stats4")
            old <- "stats4"
            warning(gettextf("package '%s' has been merged into '%s'", 
                package, old), call. = FALSE, domain = NA)
            return(if (logical.return) TRUE else invisible(.packages()))
        }
        if (package == "lqs") {
            warning("package 'lqs' has been moved back to package 'MASS'", 
                call. = FALSE, immediate. = TRUE)
            have.VR <- "package:MASS" %in% search()
            if (!have.VR) {
                if (require("MASS", quietly = TRUE)) 
                  warning("package 'MASS' has now been loaded", 
                    call. = FALSE, immediate. = TRUE)
                else {
                  if (logical.return) 
                    return(FALSE)
                  else stop("package 'MASS' seems to be missing from this R installation")
                }
            }
            return(if (logical.return) TRUE else invisible(.packages()))
        }
        if (!missing(version)) {
            package <- manglePackageName(package, version)
        }
        else {
            pkgDirs <- list.files(lib.loc, pattern = paste("^", 
                package, sep = ""))
            if (length(pkgDirs) > 0) {
                if (!(package %in% pkgDirs)) {
                  vers <- unlist(lapply(pkgDirs, libraryPkgVersion))
                  vpos <- libraryMaxVersPos(vers)
                  if (length(vpos) > 0) 
                    package <- pkgDirs[vpos]
                }
            }
        }
        if (length(package) != 1) 
            stop("'package' must be of length 1")
        pkgname <- paste("package", package, sep = ":")
        newpackage <- is.na(match(pkgname, search()))
        if (newpackage) {
            pkgpath <- .find.package(package, lib.loc, quiet = TRUE, 
                verbose = verbose)
            if (length(pkgpath) == 0) {
                vers <- libraryPkgVersion(package)
                txt <- if (!is.null(vers)) 
                  gettextf("there is no package called '%s', version %s", 
                    libraryPkgName(package), vers)
                else gettextf("there is no package called '%s'", 
                  libraryPkgName(package))
                if (logical.return) {
                  warning(txt, domain = NA)
                  return(FALSE)
                }
                else stop(txt, domain = NA)
            }
            which.lib.loc <- dirname(pkgpath)
            pfile <- system.file("Meta", "package.rds", package = package, 
                lib.loc = which.lib.loc)
            if (!nchar(pfile)) 
                stop(gettextf("'%s' is not a valid package -- installed < 2.0.0?", 
                  libraryPkgName(package)), domain = NA)
            pkgInfo <- .readRDS(pfile)
            testRversion(pkgInfo, package)
            if (is.character(pos)) {
                npos <- match(pos, search())
                if (is.na(npos)) {
                  warning(gettextf("'%s' not found on search path, using pos = 2", 
                    pos), domain = NA)
                  pos <- 2
                }
                else pos <- npos
            }
            .getRequiredPackages2(pkgInfo)
            if (packageHasNamespace(package, which.lib.loc)) {
                tt <- try({
                  ns <- loadNamespace(package, c(which.lib.loc, 
                    lib.loc))
                  dataPath <- file.path(which.lib.loc, package, 
                    "data")
                  env <- attachNamespace(ns, pos = pos, dataPath = dataPath)
                })
                if (inherits(tt, "try-error")) 
                  if (logical.return) 
                    return(FALSE)
                  else stop(gettextf("package/namespace load failed for '%s'", 
                    libraryPkgName(package)), call. = FALSE, 
                    domain = NA)
                else {
                  on.exit(do.call("detach", list(name = pkgname)))
                  nogenerics <- checkNoGenerics(env, package)
                  if (warn.conflicts && !exists(".conflicts.OK", 
                    envir = env, inherits = FALSE)) 
                    checkConflicts(package, pkgname, pkgpath, 
                      nogenerics)
                  if (!nogenerics && .isMethodsDispatchOn() && 
                    !identical(pkgname, "package:methods")) 
                    methods::cacheMetaData(env, TRUE, searchWhere = .GlobalEnv)
                  runUserHook(package, pkgpath)
                  on.exit()
                  if (logical.return) 
                    return(TRUE)
                  else return(invisible(.packages()))
                }
            }
            codeFile <- file.path(which.lib.loc, package, "R", 
                libraryPkgName(package))
            loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
            assign(".packageName", package, envir = loadenv)
            if (file.exists(codeFile)) {
                res <- try(sys.source(codeFile, loadenv, keep.source = keep.source))
                if (inherits(res, "try-error")) 
                  stop(gettextf("unable to load R code in package '%s'", 
                    libraryPkgName(package)), call. = FALSE, 
                    domain = NA)
            }
            else if (verbose) 
                warning(gettextf("package '%s' contains no R code", 
                  libraryPkgName(package)), domain = NA)
            dbbase <- file.path(which.lib.loc, package, "data", 
                "Rdata")
            if (file.exists(paste(dbbase, ".rdb", sep = ""))) 
                lazyLoad(dbbase, loadenv)
            dbbase <- file.path(which.lib.loc, package, "R", 
                "sysdata")
            if (file.exists(paste(dbbase, ".rdb", sep = ""))) 
                lazyLoad(dbbase, loadenv)
            env <- attach(NULL, pos = pos, name = pkgname)
            on.exit(do.call("detach", list(name = pkgname)))
            attr(env, "path") <- file.path(which.lib.loc, package)
            .Internal(lib.fixup(loadenv, env))
            bindTranslations(libraryPkgName(package), pkgpath)
            if (exists(".First.lib", mode = "function", envir = env, 
                inherits = FALSE)) {
                firstlib <- get(".First.lib", mode = "function", 
                  envir = env, inherits = FALSE)
                tt <- try(firstlib(which.lib.loc, package))
                if (inherits(tt, "try-error")) 
                  if (logical.return) 
                    return(FALSE)
                  else stop(gettextf(".First.lib failed for '%s'", 
                    libraryPkgName(package)), domain = NA)
            }
            if (!is.null(firstlib <- getOption(".First.lib")[[package]])) {
                tt <- try(firstlib(which.lib.loc, package))
                if (inherits(tt, "try-error")) 
                  if (logical.return) 
                    return(FALSE)
                  else stop(gettextf(".First.lib failed for '%s'", 
                    libraryPkgName(package)), domain = NA)
            }
            nogenerics <- checkNoGenerics(env, package)
            if (warn.conflicts && !exists(".conflicts.OK", envir = env, 
                inherits = FALSE)) 
                checkConflicts(package, pkgname, pkgpath, nogenerics)
            if (!nogenerics && .isMethodsDispatchOn() && !identical(pkgname, 
                "package:methods")) 
                methods::cacheMetaData(env, TRUE, searchWhere = .GlobalEnv)
            runUserHook(package, pkgpath)
            on.exit()
        }
        if (verbose && !newpackage) 
            warning(gettextf("package '%s' already present in search()", 
                libraryPkgName(package)), domain = NA)
    }
    else if (!missing(help)) {
        if (!character.only) 
            help <- as.character(substitute(help))
        pkgName <- help[1]
        pkgPath <- .find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"), 
            file.path(pkgPath, "INDEX"))
        if (file.exists(vignetteIndexRDS <- file.path(pkgPath, 
            "Meta", "vignette.rds"))) 
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector(length = 3, mode = "list")
        readDocFile <- function(f) {
            if (basename(f) %in% "package.rds") {
                txt <- .readRDS(f)$DESCRIPTION
                nm <- paste(names(txt), ":", sep = "")
                formatDL(nm, txt, indent = max(nchar(nm, type = "w")) + 
                  3)
            }
            else if (basename(f) %in% "vignette.rds") {
                txt <- .readRDS(f)
                if (is.data.frame(txt) && nrow(txt)) 
                  cbind(basename(gsub("\\.[[:alpha:]]+$", "", 
                    txt$File)), paste(txt$Title, paste(rep.int("(source", 
                    NROW(txt)), ifelse(txt$PDF != "", ", pdf", 
                    ""), ")", sep = "")))
                else NULL
            }
            else readLines(f)
        }
        for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
    }
    else {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        db <- matrix(character(0), nr = 0, nc = 3)
        nopkgs <- character(0)
        for (lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for (i in sort(a)) {
                file <- system.file("Meta", "package.rds", package = i, 
                  lib.loc = lib)
                title <- if (file != "") {
                  tmp <- .readRDS(file)
                  if (is.list(tmp)) 
                    tmp <- tmp$DESCRIPTION
                  tmp["Title"]
                }
                else NA
                if (is.na(title)) 
                  title <- " ** No title available (pre-2.0.0 install?)  ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if (length(a) == 0) 
                nopkgs <- c(nopkgs, lib)
        }
        colnames(db) <- c("Package", "LibPath", "Title")
        if ((length(nopkgs) > 0) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages", 
                "libraries %s contain no packages"), pkglist)
            warning(msg, domain = NA)
        }
        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }
    if (logical.return) 
        TRUE
    else invisible(.packages())
}
<environment: namespace:base>
