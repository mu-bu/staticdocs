#' Build complete static documentation for a package.
#'
#' Currently, knitr builds documentation for:
#'
#' \itemize{
#'   \item Rd files.  Files
#'   \item Demos. Must be listed in \file{demos/00index}.
#'   \item Vignettes.
#' }
#'
#' @param package path to source version of package.  See
#'   \code{\link[devtools]{as.package}} for details on how paths and package
#'   names are resolved.
#' @param base_path root directory in which to create documentation
#' @param examples include examples or not?  Examples are particularly
#'   slow to render because all code must be run, so turning them off makes
#'   it easier to tweak templates etc.
#' @export
#' @import stringr
#' @importFrom devtools load_all
#' @aliases staticdocs-package
build_package <- function(package, base_path = NULL, examples = NULL, knitr=TRUE, target = NULL, branch = FALSE) {
  
  
  # init install lib.loc
  install_lib(NULL, global=FALSE)
  on.exit( install_lib(NULL) )

  # install package
  pkg <- as.package(package)
  lib.loc <- install_lib(pkg)
  message("Using libraries:\n", paste0("- ", .libPaths(), collapse = "\n"))
  library(pkg$package, lib.loc = lib.loc, character.only=TRUE)
  
  # generate in branch sub-directory
  if( isTRUE(branch) ) branch <- git_branch(pkg$path)
  pkg$is_devel <- build_is_devel(pkg$version, branch)
  make_landing_page <- FALSE
  if( is.character(branch) ){
      message("Passed/detected branch: ", branch)
      # change release into master (development flow is to make integration changes on branch release/)
      if( grepl("^release", branch) ) branch <- 'master'
      base_path <- file.path(base_path, strsplit(branch, .Platform$file.sep)[[1L]][1])
      make_landing_page <- TRUE
  }
  message("Generating site in: ", base_path)
  #

  package <- package_info(pkg, base_path, examples)
  if (!file.exists(package$base_path)) dir.create(package$base_path)
  copy_bootstrap(base_path)

  message("Writting VERSION file")
  cat(package$version, file = file.path(base_path, 'VERSION'), sep ="\n")
  
  # reset headlinks
  add_headlink(NULL)
  package$navbar <- '{{{navbar}}}'
  
  package$rd_knitr <- knitr
  
  if( !is.null(target) ){
      target <- str_trim(strsplit(target, ",")[[1]])
  }
  tbuild <- function(x) !length(target) || !nzchar(target) || x %in% target  
  
  if( tbuild('topics') )  package$topics <- build_topics(package)
  if( tbuild('vignettes') )  package$vignettes <- build_vignettes(package)
  if( tbuild('demos') )  package$demos <- build_demos(package)
  if( tbuild('md') )  package$mdpages <- build_mdpages(package)
  if( tbuild('readme') )  package$readme <- build_readme(package)
  if( tbuild('references') )  build_references(package)
  if( tbuild('install') )  package$install <- build_install(package)
  if( tbuild('news') )  package$news <- build_news(package)
  if( tbuild('citation') )  package$citation <- build_citation(package)
  
  if( tbuild('index') )  build_index(package)
  
  # render main pages
  build_pages(package)
    
  # render landing page
  if( make_landing_page ){
      build_landing(package, base_path)
  }
  
  if (interactive()) {
    browseURL(normalizePath(file.path(base_path, "index.html")))
  }
  invisible(TRUE)
}

install_lib <- local({
      .lib <- NULL
	  .ol <- NULL
	  .global <- FALSE
      function(package, global=FALSE){
		
		# just return the installation library
		if( missing(package) || (!is.null(package) && !is.null(.lib)) ) return(.lib)
	    else if( is.null(package) ){ # init or restoration
		  if( !is.null(.lib) ){ # restoration
			  if( .global ){
				  message("# Removing package from main library '", .lib, "'")
				  remove.packages(package, lib=.lib)
			  }else{
				  message("# Deleting temporary library '", .lib, "'")
				  unlink(.lib, recursive=TRUE)
			  }
			  # restore old lib path
			  if( !is.null(.ol) ) .libPaths(.ol)
			  .ol <<- NULL
		  }else{ # init
			  .global <<- global
		  }
		  .ol <<- .libPaths()
          .lib <<- NULL
          return(.lib)
      }
	    
	    # install package in temporary library (NB: not working if parallel runs)
		if( !.global ){
			tmplib <- tempfile()
	        dir.create(tmplib)
	        .libPaths(c(tmplib, file.path(package$path, '..', 'lib'), .libPaths()))
			message("# Installing package in temporary library '", tmplib, "'")
	        pkgmaker::quickinstall(package$path, tmplib, vignettes=FALSE)
		}else{			
			# if needs to run examples with parallel computations
			pkglib <- normalizePath(file.path(package$path, '..', 'lib'))
			.libPaths(c(file.path(package$path, '..', 'lib'), .libPaths()))
			instlib <- .libPaths()[if( file.exists(pkglib) ) 2L else 1L]
			message("# Installing package in main library '", instlib, "'")
			pkgmaker::quickinstall(package$path, instlib, vignettes=FALSE)
			tmplib <- dirname(find.package(package$package))
			stopifnot(instlib==tmplib)
			##
		}
				
	    .lib <<- tmplib
	    .lib
	  }
 })


# taken/adapted from knitr::knit_rd (1.2)
knit_examples <- function(p, pkgRdDB, links = tools::findHTMLlinks()) 
{
	library(knitr)
	optc = opts_chunk$get()
	on.exit(opts_chunk$set(optc))
	force(links)
#	topics = names(pkgRdDB)
#	for (p in topics) {
#		message("** knitting documentation of ", p)
		ef <- tempfile()
		tools::Rd2ex(pkgRdDB[[p]], ef, fragment = TRUE)
		# early exit if there are no examples
		if( !file.exists(ef) ) return()
		on.exit(unlink(ef), add=TRUE)
		ex = readLines(ef, warn = FALSE)
		ex = ex[-(1L:grep("### ** Examples", ex, fixed = TRUE))]
		ex = c(paste0("```{r rd_example_", p, "_", tempfile(),", message = TRUE, error = TRUE, warning = TRUE}"), ex, "```")
		opts_chunk$set(fig.path = str_c("figure/", p, "-ex"), tidy = FALSE)
		res = try(knit2html(text = ex, envir = parent.frame(2), fragment.only = TRUE, quiet = TRUE))
		unlink("figure/", recursive = TRUE)
		if (inherits(res, "try-error")) {
			res = ex
			res[1] = "<pre><code class=\"r\">"
			res[length(res)] = "</code></pre>"
		}
		txt = res
#	}
	txt
}


#' Generate all topic pages for a package.
#'
#' @export
#' @inheritParams build_package
#' @param package_info A list containing information about the package,
#'   as generated by \code{\link{package_info}}
#' @keywords internal
build_topics <- function(package) {

  # for each file, find name of one topic
  index <- package$rd_index
  paths <- file.path(package$base_path, index$file_out)

  # create columns for extra topic info
  index$title <- ""
  index$in_index <- TRUE
  
  if( package$rd_knitr ){
      RdDBpath <- file.path(find.package(package$package, lib.loc = install_lib()), "help", package$package)
      message("loading Rd db from: ", RdDBpath)
	pkgRdDB = tools:::fetchRdDB(RdDBpath)
  }
  
  message("Rendering topic man pages")
  # load cache
  cache_file <- load_cache(package, 'topic')
  on.exit( save(topic_cache, file = cache_file) )
  #
  
  for (i in seq_along(index$name)) {
      
    fhtml <- basename(paths[[i]])
    rd <- package$rd[[i]]
    html <- topic_cache$compute(list(fhtml, digest(rd), package$rd_knitr, package$examples), {
                        
          message("Generating ", fhtml)
          html <- to_html(rd, 
            env = new.env(parent = globalenv()), 
            topic = str_replace(basename(paths[[i]]), "\\.html$", ""),
            package = package)
          html$pagetitle <- html$name
    	  if( package$rd_knitr ){# knit examples
    		html$examples <- knit_examples(index$name[i], pkgRdDB)   
    	  }
    		  
          graphics.off()
    
          html
      })

    pkg_fields <- c("package", "version", 'versions')
    html[pkg_fields] <- package[pkg_fields]
    html$indextarget <- "PAGE-MAN.html"

    render_page(package, "topic", html, paths[[i]], navbar=FALSE)
    if ("internal" %in% html$keywords) {
      index$in_index[i] <- FALSE
    }
    index$title[i] <- html$title
  }

  index
}

build_readme <- function(package) {
  if (!is.null(package$readme)) return(markdown(package$readme))
  
  path <- file.path(package$path, "README.md")
  # use description if no README.md is available
  if (!file.exists(path)) return( package$description )
  
  # remove Travis-CI links
  l <- readLines(path)
  l <- str_trim(grep("travis-ci.org", l, value = TRUE, invert = TRUE, fixed = TRUE))
  l <- l[nchar(l) > 0]
  if( !length(l) ) return(package$description)
  #
  
  # compile
  markdown(paste0(l, collapse="\n"))
}

build_install <- function(package = NULL) {
  
   if( is.null(package) ) package <- package_info('.', '.')
   
  install <- ''
  
  # look for github SCM => generate default install_github installation page
  if( length(repos <- package$repos) ){
      
      message("Generating installation procedures")
      
      # CRAN
      if( !is.null(cran <- repos$repos$cran) ){
          message("  * CRAN")
          install <- sprintf("## ![](img/%s) CRAN

Page: %s
                                              
The latest release of the package can be installed from source R-forge repository as follows:
```{r CRAN}
install.packages(\"%s\")
```"
        , cran$icon, cran$url, package$package)
      }
      ##
          
      # GITHUB
      if( !is.null(gh <- repos$repos$github) ){
          
          message("  * GitHub")
          gh_cmd <- function(dev = NULL, args = NULL){
              extra <- ''
              if( !is.null(dev) ) extra <- sprintf(", '%s'", dev)
              if( !is.null(args) ) extra <- sprintf("%s, %s", extra, args)
              gh_data <- str_match(gh$url, "github.com/([^/]+)/([^/]+)")
              sprintf("install_github('%s', '%s'%s)", gh_data[2], gh_data[3], extra)
          }
          
          if( nzchar(install) ) install <- paste0(install, "\n***\n")
          install <- sprintf("%s

## ![](img/%s) GitHub

Repository: %s

The package can be installed from source from the GitHub using the `devtools` package as follows:
```{r gh}
library(devtools)
# latest stable version
%s

# development version
%s
```

### Troubleshooting
The above commands will perform a complete build/installation, which may require a __complete R development environment__.
This should be fine for standard unix-based R installations (Linux, Mac), but is cannot be taken for granted on Windows machines. 

So if this fails, then try doing a quick install by: 
  * skipping vignette generation only with argument `build_vignette = FALSE`.
  * limiting the installation procedure to the strict necessary with argument `quick = TRUE`;

```{r gh_quick}
library(devtools)
# latest stable version
%s
%s

# development version
%s
%s
```"
        , install, gh$icon, gh$url
        , gh_cmd(), gh_cmd('develop')
        , gh_cmd(args = 'quick = TRUE'), gh_cmd(args = 'build_vignettes = FALSE')
        , gh_cmd('develop', args = 'quick = TRUE'), gh_cmd('develop', args = 'build_vignettes = FALSE')
        )
      }
      ##
      
      # R-forge
      if( !is.null(rf <- repos$repos$rforge) ){
          message("  * R-forge")
          
          if( nzchar(install) ) install <- paste0(install, "\n***\n")
          install <- sprintf("%s

## ![](img/%s) R-forge

Project: %s

The latest automated build of the package can be installed from R-forge repository as follows:
```{r rforge}
install.packages(\"%s\", repos=\"http://R-Forge.R-project.org\")
```

### Troubleshooting
This installation method depends on the package's build status on R-forge, which can be checked on the [project page](%s) > 'R packages' or from R:
```{r rforge_check}
'%s' %%in%% rownames(available.packages(contrib.url(\"http://R-Forge.R-project.org\")))
```"
        , install, rf$icon, rf$url, package$package, rf$url, package$package)
    }
    ##
    
      if( nzchar(install) ){
        install <- paste0("```{r setup, echo = FALSE}
library(knitr)
opts_chunk$set(eval = FALSE)
```
", install)
        library(knitr)
        install <- knit(text = install, envir = new.env())
      }
  }
  
  # add README content  
  if( file.exists(path <- file.path(package$path, "README")) ){
       if( nzchar(install) ) install <- paste0(install, "\n***\n")
       install <- paste0(install, paste0(readLines(path), collapse = "\n"))
  }
  if( !nzchar(install) ) return()
  
  render_head_page(package, 'installation', x = install, title = NULL)
  invisible(install)
}

build_news <- function(package) {
    
    #path <- package
    path <- file.path(package$path, "NEWS")
    # use description if no README.md is available
    if (!file.exists(path)) return() 
    
    outfile <- file.path(package$base_path, 'PAGE-NEWS.html') 
	message("Generating ", basename(outfile))
    # re-format NEWS
    news <- readLines(path)
    # remove sections
    news <- grep("^\\*\\*\\*", news, value = TRUE, invert = TRUE)
    i <- c(grep("^(Changes? in .*)", news), length(news) + 1)
    html <- list()
    news <- sapply(seq(1, length(i) - 1), function(j){
        items <- news[seq(i[j]+1, i[j+1] - 1)]
        items <- gsub("^([^ ])", "#### \\1", items)
        items <- gsub("^    o ", "  * ", items)
        list(
            title = news[i[j]]      
            , items = as.list(markdown(items))
            )
    }, simplify = FALSE)
    package$news <- news
    # render dedicated file
	render_page(package, "news", package, outfile)
	# add dedicated head link
	add_headlink(package, basename(outfile), 'News')
    # return news list
    news
}

build_mdpages <- function(package, index, base_path=NULL) {
	# pre-process arguments
	package <- package_info(package, base_path=base_path)
	
	# look for md pages
	path <- pkg_sd_path(package)
	if ( !length(path) || !file.exists(path)) return()
	pmd <- "\\.(R)?md$"
	mdfiles <- list.files(path, pattern=pmd, full.names=TRUE)
	if( !length(mdfiles) ) return()
	
	titles <- sub(pmd, '', basename(mdfiles))
	
    tmplib <- install_lib(package)
	message("Rendering static MD pages (", tmplib, ")")
	
	for(i in seq_along(mdfiles)) {
		mdf <- mdfiles[i]
		basef <- basename(mdf)
		out <- str_c('PAGE-', sub("\\.(R)?md$", ".html", basef))
		outfile <- file.path(package$base_path, out)
		message("Generating page ", basef)
		html <- list()
		if( grepl("\\.rmd", mdf, ignore.case=TRUE) ){
			html$content <- knitr::knit2html(text=readLines(mdf))
		}else html$content <- markdown(path=mdf)
		html$indextarget <- 'index.html'
		html$pagetitle <- NULL
		html$package <- package
		render_page(package, "mdpage", html, outfile)
		# add dedicated head link
		add_headlink(package, basename(outfile), titles[i])
	}
	
	list(demo=unname(apply(cbind(mdfiles, titles), 1, as.list)))
}

# copy style, javascript files
copy_bootstrap <- function(base_path) {
	
  css_path <- file.path(base_path, 'css')
  js_path <- file.path(base_path, 'js')
  bootstrap <- file.path(inst_path(), "bootstrap")
  file.copy(dir(bootstrap, full.names = TRUE), base_path, recursive = TRUE)
  d3 <- file.path(inst_path(), "d3")
  file.copy(dir(d3, full.names = TRUE), base_path, recursive = TRUE)
  # img
  file.copy(dir(file.path(inst_path(), "img"), full.names = TRUE), file.path(base_path, 'img'), recursive = TRUE)
  # knitr files
  #file.copy(system.file("misc", c("highlight.css", "R.css"), package = "knitr"), css_path)
  #file.copy(system.file("misc", "highlight.pack.js", package = "knitr"), js_path)
}


#' List all package vignettes.
#'
#' Copies all vignettes and returns data structure suitable for use with
#' whisker templates.
#'
#' @keywords internal
#' @inheritParams build_package
#' @importFrom tools buildVignettes
#' @return a list, with one element for each vignette containing the vignette
#'   title and file name.
build_vignettes <- function(package) {  
  # Locate source and built versions of vignettes
  path <- dir(file.path(package$path, c("inst/doc", "vignettes")), ".Rnw", 
    full.names = TRUE)
  if (length(path) == 0) return()
  
  # make quick install to ensure everything is there for building
  tmplib <- install_lib(package)
  message("Building vignettes (", tmplib, ")")
  #buildVignettes(dir = package$path)
  
  message("Copying vignettes")
  path <- list.files(file.path(tmplib, package$package, 'doc'), pattern="\\.Rnw$", full.names=TRUE)
  if (length(path) == 0) return()
  src <- str_replace(path, "\\.Rnw$", ".pdf")
  filename <- basename(src)
  message(paste(filename, collapse="\n"))
  
  # create vignettes directory
  dest <- file.path(package$base_path, "vignettes")
  if (!file.exists(dest)) dir.create(dest)
  # copy pdf files
  file.copy(src, file.path(dest, filename), overwrite=TRUE)

  # Extract titles
  title <- vapply(path, FUN.VALUE = character(1), function(x) {
    contents <- str_c(readLines(x), collapse = "\n")
    str_match(contents, "\\\\VignetteIndexEntry\\{(.*?)\\}")[2]
  })  
  
  list(vignette = unname(apply(cbind(filename, title), 1, as.list)))
}

#' Creates a Bibliography Page
#' 
#' @param package package name or object
#' @param base_path output path. If missing it is taken from \code{package}
#' 
#' @keywords internal
#' @importFrom bibtex read.bib
build_references <- function(package, base_path=NULL){
	
	# pre-process arguments
	package <- package_info(package, base_path=base_path)
	
	# look for reference file in inst/
	ref <- file.path(inst_path(package), 'REFERENCES.bib')
	if( !file.exists(ref) ) return()
	
	outfile <- file.path(package$base_path, 'PAGE-REFERENCES.html') 
	message("Generating ", basename(outfile))
	# load bibtex items
    library(bibtex)
	bibs <- read.bib(ref)
    bkeys <- sapply(bibs, function(x) x$key)
    bibs <- bibs[order(bkeys)]
    # format
	hbibs <- format(bibs, style='html')
    package$references <- lapply(seq_along(bibs), function(i){ list(bibitem = hbibs[i], key = bibs[[i]]$key)}) 
	
	# render dedicated file
	render_page(package, "references", package, outfile)
	# add dedicated head link
	add_headlink(package, basename(outfile), 'References')

}

capfirst <- function(s) {
	paste(toupper(substring(s,1,1)), tolower(substring(s,2)), sep='')
}

#' @importFrom utils readCitationFile
build_citation <- function(package){
  citfile <- file.path(inst_path(package), 'CITATION')
  if( !file.exists(citfile) ) return()
  message('Rendering CITATION')
  package[capfirst(names(package))] <- package
  cit <- readCitationFile(citfile, meta = package)
  # only extract first one
  gsub("(^<p>)|(</p>$)", "", format(cit[[1L]], style = 'html'))
}

build_demos <- function(package, index, base_path=NULL) {
  # pre-process arguments
  package <- package_info(package, base_path=base_path)
	
  demo_dir <- file.path(package$path, "demo")
  if (!file.exists(demo_dir)) return()
  
  message("Rendering demos")
  demos <- readLines(file.path(demo_dir, "00Index"))
  
  pieces <- str_split_fixed(demos, "\\s+", 2)
  in_path <- str_c(pieces[, 1], ".r")
  filename <- str_c("demo-", pieces[,1], ".html")
  title <- pieces[, 2]
  
  # load cache
  cache_file <- load_cache(package, 'demo')
  on.exit( save(demo_cache, file = cache_file) )
  #
  
  for(i in seq_along(title)) {
    if( !file.exists(dfile <- in_path[i]) )
      dfile <- sub("\\.r$", ".R", dfile)
#    message("Parsing demo ", basename(dfile))
    demo_code <- readLines(file.path(demo_dir, dfile))
#    demo_expr <- evaluate(demo_code, new.env(parent = globalenv()))

    html <- list()
    html$demo <- demo_cache$compute(list(filename[i], digest(demo_code)), {
                        message("Generating demo ", filename[i])
                        eval_replay_html(demo_code, envir=new.env(parent = globalenv())
                            , package = package, prefix = str_c(pieces[i], "-demo"))
                 })
    html$indextarget <- "PAGE-DEMOS.html"
    html$pagetitle <- title[i]
    html$package <- package
    render_page(package, "demo", html, 
      file.path(package$base_path, filename[i]), navbar=FALSE)
  }
  
  
  package$demos <- list(demo=unname(apply(cbind(filename, title), 1, as.list)))  
  # render dedicated file
  outfile <- file.path(package$base_path, 'PAGE-DEMOS.html')
  message("Generating ", basename(outfile))
  render_page(package, "index-demos", package, outfile)
  # add dedicated head link
  add_headlink(package, basename(outfile), 'Demos')
  
  # return demos
  package$demos

}

build_is_devel <- function(version, branch) {
  
  devel <- FALSE
  if( !is.character(branch) ){
        min <- strsplit(version, '.')[[1]][2]
        devel <- !is.na(min) && min %% 2 == 1
  } else devel <- !grepl('^(release)|(master)', branch)
  
  if( devel ) TRUE
  else NULL
}

build_landing<- function(package, base_path){
    message('Generate landing page')
    mdir <- dirname(base_path)
    outfile <- file.path(mdir, 'index.html')
    v <- list.dirs(mdir, recursive = FALSE)
    
    vs <- lapply(v, function(v){
                br <- basename(v)
                if( file.exists(vf <- file.path(v, 'VERSION') ) ){
                    numv <- readLines(vf)[1L]
                    res <- list(version = numv, branch = br)
                    res$is_devel <- build_is_devel(numv, br)
                    res
                }
            })
    data <- package[c('title', 'description', 'package')]
    vs <- vs[sapply(vs, length) > 0]
    data$versions <- vs[natorder(sapply(vs, function(x) paste0(x$version, ifelse(is.null(x$is_devel), '', 'devel'))))]
    # copy bootstrap css in main dir
    copy_bootstrap(dirname(outfile))
    # render page
    render_page(package, "main", data, outfile, navbar = '', to_top_link = FALSE)
}

# wrap a content into the main layout
wrap_page <- local({
	.cache <- NULL
	function(package, file, layout='layout'){
		if( is.null(package) ){ # reset cache
			.cache <<- NULL
			return()
		}
				
    
	}
})

add_headlink <- local({
	
	.cache <- NULL
	function(package, target, face=target, prepend=FALSE){
		
		if( missing(package) ){
			return( .cache ) 
		}
		if( is.null(package) ){ # reset cache
			.cache <<- NULL
			return()
		}
		
		if( is.null(face) ) face <- target
		.cache <<-
		if( !prepend ){
			c(.cache, list(list(target=target, face=face)))
		}else{
			c(list(list(target=target, face=face)), .cache)
		}
		.cache
	}
})

render_head_page <- function(package, name, outfile = NULL, x = NULL, path = NULL, index = 'index.html', link = capitalize(name), title = link){
    
    if( is.null(outfile) ) outfile <- file.path(package$base_path, sprintf('PAGE-%s.html', toupper(name)))
    message("Generating menu page ", basename(outfile))
    html <- list()
	html$content <- markdown(x = x, path = path)
	html$indextarget <- index
	html$pagetitle <- title
	html$package <- package
    # render
    render_page(package, name, html, outfile)
    # add dedicated head link
    add_headlink(package, basename(outfile), link)
    html$content
}


# Adds Navigation Bar
build_pages <- function(package, base_path=NULL, layout='default') {
	
  # pre-process arguments
  package <- package_info(package, base_path=base_path)
  outpath <- package$base_path
  
  wrap_page(NULL)
  # substitute head links in all html files starting with '-'
  files <- dir(outpath, pattern="^PAGE-.*\\.html", full.names=TRUE)
  files <- c(file.path(outpath,'index.html'), files)
  message("Wrapping pages ", paste(basename(files), collapse=", ")," in layout:", layout)
  
  package$headlinks <- add_headlink()
  navbar <- paste(render_template(package, "navbar", '', package), collapse="\n")
	sapply(files, function(f){
      # collapse contents
      contents <- paste(readLines(f, warn=FALSE), collapse="\n")
      # substitute in layout template
      navbar <- gsub(sprintf('<li>(<a href="%s")', basename(f)), '<li class="active">\\1', navbar)
      rendered <- whisker.render(contents, list(navbar = navbar))
      cat(rendered, file = f)
  })
	invisible()
	
}
