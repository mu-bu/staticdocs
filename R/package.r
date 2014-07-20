
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
  # files to install in root
  out$www <- settings$www
  out$knitr <- settings$knitr
  
  out$base_path <- base_path %||% settings$base_path %||% 
    stop("base_path not specified", call. = FALSE)
  out$examples <- examples %||% settings$examples %||% TRUE
  
  out$urls <- list()
  if (!is.null(out[['url']])) {
    href <- as.list(str_trim(str_split(out$url, ",")[[1]]))
	out$urls <- list(href=unname(apply(cbind(href=href), 1, as.list)))
    out$url <- NULL
  }
  
  # bug report
  if (!is.null(out$bugreports)) {
    out$urls$bugreport <- list(url=out$bugreports)
  }
  
  # extra features
  out$has_src <- file.exists(file.path(package$path, 'src', ''))
  out$has_vignettes <- length(list_vignettes(package)) > 0
  
  # code repositories
  out$repos <- extract_repos(out, settings$repos)
  out$urls$repos <- out$repos['urls']
  
  # Author info
  authors <- list()
  .format_authors <- function(x){
	  m <- str_match(x, "([^<]+)\\s*(<([^>]+)>)?(.*)")
  	  authors <- str_c(m[,2], ifelse( m[,4] != '', str_c('(', cloak_email(m[,4]), ')'), ''), m[,5])
	  list(author=unname(apply(cbind(name=authors), 1, as.list)))
  }
  
  if (!is.null(out$`authors@r`)) {
    contrib <- eval(parse(text = out$`authors@r`))
    # extract maintainer from Authors@R 
    if ( is.null(out$maintainer) ){
        m <- grep("\\[[^]]*cre[^]]*\\]", format(contrib), value = TRUE)
        out$maintainer <- str_trim(gsub("\\[[^]]*cre[^]]*\\]", '', m))
    }
    # drop emails from maintainer contrib
    lapply(seq_along(contrib), function(i){
        co <- contrib[[i]][[1L]]
        if( 'cre' %in% co$role) contrib[i]$email <<- list(NULL)
    })
    
    authors$contrib <- format(contrib)
      
    # extract authors from Authors@R 
    if ( is.null(out[['author']]) ){
        m <- grep("\\[[^]]*aut[^]]*\\]", format(contrib), value = TRUE)
        out$author <- paste0(str_trim(gsub("\\[[^]]*aut[^]]*\\]", '', m)), collapse = ', ')
    }
  }
  if (!is.null(out[['author']])) {
	  authors$author <- str_trim(str_split(out[['author']], ",")[[1]])
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

extract_repos <- function(pkg, settings){
    
    # initialise result list
    repos <- list()
    # preprend settings
    repos <- c(settings, repos)
    
    # load data from DESCRIPTION
    if( !is.null(pkg$scm) ){
        scms <- str_trim(strsplit(pkg$scm, ',')[[1L]])
        drepos <- sapply(scms, function(x){
            if( !nzchar(spec <- gsub("([^:]+):?(.*)", "\\2", x)) ) "TRUE"
            else spec
        })
        repos <- c(repos, setNames(drepos, gsub("([^:]+):?.*", "\\1", scms)))
    }
    
    # append github repo infered from path
    repos <- c(repos, github = git_remote(pkg$path))
    
    # load settings data
    if( !length(repos) ) return()
    
#    print(repos)
#    message('#')
    
    # remove duplicated settings
    names(repos)[grep("^r-?forge$", names(repos))] <- "rforge"
    repos <- repos[!duplicated(names(repos))]
    names(repos) <- tolower(names(repos))

    .is_true_value <- function(x){
        toupper(as.character(x)) %in% c('TRUE', 'YES')
    }
    
#    print(repos)
    # process repo specifications
    lapply(names(repos), function(x){
        repo <- repos[[x]]
        # use package name if TRUE
        if( is.character(repo) || isTRUE(repo) ){
            if( x %in% c('rforge', 'r-forge') ){
                if( .is_true_value(repo) ) repo <- pkg$package
                if( !grepl("/", repo, fixed = TRUE) )
                    repo <- sprintf("http://r-forge.r-project.org/projects/%s", tolower(repo))
                repo <- list(icon = 'r-forge-icon.png', url = repo)
            }else if( grepl("^github", x) ){
                if( .is_true_value(repo) ){
                    repo <- setNames(Sys.info()['user'], NULL)
                    warning("Using github account: ", repo, "\n  NOTE: you may want to specify the user account in the DESCRIPTION file or staticdocs/index.r")
                }
                
                # username only?
                if( !grepl("/", repo, fixed = TRUE) ) repo <- sprintf("%s/%s", repo, pkg$package)
                
                if( !grepl("^http", repo) ) url <- sprintf("https://github.com/%s", repo)
                else url <- repo
                repo <- list(icon = 'github.png', url = url, gran = grepl("+gran", x))
                
                # force key = 'github'
                x <- 'github'

            }else if( x == 'cran' ){
                repo <- list(icon = "r-icon.jpg", url = sprintf("http://cran.r-project.org/package=%s", pkg$package))
                
            }else{
                repo <- list(url = repo)
                
            }
        }
        if( !is.list(repo) ) stop("Invalid repository specification for repo '", x, "': must be a list [", class(repo), "]")
        repos[[x]] <<- repo
    })
    repos <- Filter(length, repos)
    res <- list(urls = lapply(unname(repos), as.list))
#    print(res)
    res$repos <- repos
    res
}
