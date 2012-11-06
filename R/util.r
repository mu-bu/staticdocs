inst_path <- function(package="staticdocs") {
  
  # handle the case of a list (eg., a package object)
  if( is.list(package) && !is.null(package$package) ) package <- package$package
  if (is.null(dev_meta("staticdocs"))) {
    # staticdocs is probably installed
    system.file(package = package)
  } else {
    # staticdocs was probably loaded with devtools
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
  
  (markdownToHTML(text = x, file = path, fragment.only = TRUE,
    options = c("safelink", "use_xhtml", "smartypants")))
}

cloak_email <- function(x){
	sub('@', ' at ', x, fixed=TRUE)
}