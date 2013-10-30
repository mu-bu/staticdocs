
parse_deps <- function (string, incR=FALSE) 
{
	if (is.null(string)) 
		return()
	pieces <- strsplit(string, ",")[[1]]
	names <- gsub("\\s*\\(.*?\\)", "", pieces)
	names <- gsub("^\\s+|\\s+$", "", names)
	versions_str <- pieces
	have_version <- grepl("\\(.*\\)", versions_str)
	versions_str[!have_version] <- NA
	compare <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
	versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)
	compare_nna <- compare[!is.na(compare)]
	compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", 
			"<")
	if (!all(compare_valid)) {
		stop("Invalid comparison operator in dependency: ", paste(compare_nna[!compare_valid], 
						collapse = ", "))
	}
	deps <- data.frame(name = names, compare = compare, version = versions, 
			stringsAsFactors = FALSE)
	
	if( !incR ) deps <- deps[names != "R", ]
	deps
}

#' Return information about a package
#'
#' @param package name of package, as character vector
#' @return A named list of useful metadata about a package
#' @export
#' @keywords internal
#' @importFrom devtools as.package
package_info <- function(package, base_path = NULL, examples = NULL) {
	
  if( is(package, "package_info") ) return(package)
  
  out <- as.package(package)

  settings <- load_settings(out)
  out$index <- settings$index
  out$icons <- settings$icon
  out$readme <- settings$readme
  
  out$base_path <- base_path %||% settings$base_path %||% 
    stop("base_path not specified", call. = FALSE)
  out$examples <- examples %||% settings$examples %||% TRUE

  if (!is.null(out$url)) {
    href <- as.list(str_trim(str_split(out$url, ",")[[1]]))
	out$urls <- list(href=unname(apply(cbind(href=href), 1, as.list)))
    out$url <- NULL
  }
  
  # Author info
  authors <- list()
  .format_authors <- function(x){
	  m <- str_match(x, "([^<]+)\\s*(<([^>]+)>)?(.*)")
  	  authors <- str_c(m[,2], ifelse( m[,4] != '', str_c('(', cloak_email(m[,4]), ')'), ''), m[,5])
	  list(author=unname(apply(cbind(name=authors), 1, as.list)))
  }
  
  if (!is.null(out$`authors@r`)) {
    contrib <- eval(parse(text = out$`authors@r`))
	authors$contrib <- format(contrib)
    
    # extract maintainer from Authors@R 
    if ( is.null(out$maintainer) ){
        m <- grep("\\[[^]]*cre[^]]*\\]", format(contrib), value = TRUE)
        out$maintainer <- str_trim(gsub("\\[[^]]*cre[^]]*\\]", '', m))
    }
  }
  if (!is.null(out$author)) {
	  authors$author <- str_trim(str_split(out$author, ",")[[1]])
  }
  authors$maintainer <- if (!is.null(out$maintainer)) out$maintainer
  # format
  if( length(authors) ){
    out$authors <- sapply(authors, .format_authors, simplify=FALSE)
  }
  
  # Dependencies 
  all_deps <- parse_deps(out$depends, incR=TRUE)
  Rdep <- if( length(iR <- which(all_deps$name == 'R')) ) paste(all_deps[iR, ], collapse=" ")
  out$dependencies <- list(
	rversion = Rdep,
    depends = str_c(parse_deps(out$depends)$name, collapse = ", "),
    imports = str_c(parse_deps(out$imports)$name, collapse = ", "),
    suggests = str_c(parse_deps(out$suggests)$name, collapse = ", "),
    extends = str_c(parse_deps(out$extends)$name, collapse = ", ")
  )
  out$dependencies <- out$dependencies[!sapply(out$dependencies, identical, '')]
  
  out$rd <- package_rd(package)
  out$rd_index <- topic_index(out$rd)

  structure(out, class = "package_info")
}

topic_index <- function(rd) {
  aliases <- unname(lapply(rd, extract_alias))

  names <- unlist(lapply(rd, extract_name), use.names = FALSE)  
  file_in <- names(rd)
  file_out <- str_replace(file_in, "\\.Rd$", ".html")
  
  data.frame(
    name = names,
    alias = I(aliases),
    file_in = file_in,
    file_out = file_out,
    stringsAsFactors = FALSE
  )
}

extract_alias <- function(x) {
  aliases <- Filter(function(x) attr(x, "Rd_tag") == "\\alias", x)
  vapply(aliases, function(x) x[[1]][[1]], character(1))
}

extract_name <- function(x) {
  alias <- Find(function(x) attr(x, "Rd_tag") == "\\name", x)
  alias[[1]][[1]]
}


#' @S3method print package_info
print.package_info <- function(x, ...) {
  cat("Package: ", x$package, "\n", sep = "")
  cat(x$path, " -> ", x$base_path, "\n", sep = "")
  
  topics <- strwrap(paste(sort(x$rd_index$name), collapse = ", "), 
    indent = 2, exdent = 2, width = getOption("width"))
  cat("Topics:\n", paste(topics, collapse = "\n"), "\n", sep = "")
  
}
