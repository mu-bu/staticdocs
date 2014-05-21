inst_path <- function(package="staticdocs") {
  
  # handle the case of a list (eg., a package object)
  if( is.list(package) && !is.null(package$package) ) package <- package$package
  if (is.null(dev_meta(package))) {
    # package is probably installed
    system.file(package = package)
  } else {
    # package was probably loaded with devtools
    file.path(getNamespaceInfo(package, "path"), "inst")
  }
}

# Return the staticdocs path for a package
# Could be in pkgdir/inst/staticdocs/ (for non-installed source packages)
# or in pkgdir/staticdocs/ (for installed packages)
pkg_sd_path <- function(package) {
  pathsrc <- file.path(package$path, "inst", "staticdocs")
  pathinst <- file.path(package$path, "staticdocs")
  dir.exists <- function(...) file_test('-d', ...) 
  if (dir.exists(pathsrc))
    pathsrc
  else if (dir.exists(pathinst))
    pathinst
    
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

rows_list <- function(df) {
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, ]))
}

#' @importFrom markdown markdownToHTML
markdown <- function(x = NULL, path = NULL) {
  if (is.null(path)) {
    if (is.null(x) || x == "") return("")
  }
  
  # build call to markdown (cannot use path = NULL)
  ca <- as.call(list(as.name('markdownToHTML')
                    , fragment.only = TRUE
                    , options = c("safelink", "use_xhtml", "smartypants")))
  if (is.null(path)) ca$text <- x
  else ca$file <- path
  # compile markdown 
  eval(ca)
}

cloak_email <- function(x){
	sub('@', ' at ', x, fixed=TRUE)
}

#' @export
git_branch <- function(dir = '.', all = FALSE){
    br <- suppressWarnings(try(system(paste0('cd "', dir, '"; git branch'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE))
    if( length(br) ){
          i <- grep("^\\*", br)
          if( !all ) br <- br[i]
          br <- str_trim(gsub("^\\* *", '', br))
          attr(br, 'current') <- i
    }
    br
}

git_remote <- function(dir = '.', all = FALSE, git = FALSE){
    x <- suppressWarnings(try(system(paste0('cd "', dir, '"; git remote -v'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE))
    if( length(x) ){
        # push only
#        x <- grep("\\(push\\)$", x, value = TRUE)
        i <- grep("^origin.*\\(push\\)$", x)
        if( !all ) x <- x[i]
        x <- str_trim(gsub("^.*git@([^ ]+).*", "\\1", x))
        if( !git ){
            xs <- str_split(x, ":")
            x <- sapply(xs, function(x){
                    sprintf("http://%s/%s", x[1L], gsub('\\.git$', '', x[2L]))  
                })
        }
        x <- unique(x)
    }
    x
}

natorder <- function(x, ...){
    
    mx <- max(nchar(gsub('[^0-9]', '', x)))
    num <- strsplit(x, "[^0-9]+")
    fmt <- gsub('[0-9]+', paste0("%0", mx, "d"), x)
    num <- lapply(num, function(x) as.numeric(x[nchar(x)>0]))
    sx <- mapply(function(f, args){
                if( !length(args) ) return(f)
                do.call('sprintf', c(f, as.list(args)))
            }, fmt, num)
    order(sx, ...)
}

natsort <- function(x, ...){
    x[natorder(x, ...)]
}

capitalize <- function(x){
    paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
}

