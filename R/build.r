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
build_package <- function(package, base_path = NULL, examples = NULL, rd_knitr=FALSE) {
  
  
  # init install lib
  install_lib(NULL)
  ol <- .libPaths()
  on.exit({
      if( !is.null(tmplib <- install_lib()) ){
        .libPaths(ol)
        unlink(tmplib, recursive=TRUE)
       }
  })

  if( rd_knitr ){
    pkg <- as.package(package)
    install_lib(pkg)
    library(pkg$package, character.only=TRUE)
  }else{
    #load_all(package)
    pkg <- as.package(package)
    install_lib(pkg)
    library(pkg$package, character.only=TRUE)
  }

  package <- package_info(package, base_path, examples)
  if (!file.exists(package$base_path)) dir.create(package$base_path)
  copy_bootstrap(base_path)

  # reset headlinks
  add_headlink(NULL)
  package$navbar <- '{{{navbar}}}'
  
  package$rd_knitr <- rd_knitr
  package$topics <- build_topics(package)
  package$vignettes <- build_vignettes(package)
  package$demos <- build_demos(package)
  package$mdpages <- build_mdpages(package)
  package$readme <- readme(package)
  
  build_references(package)
  package$citation <- build_citation(package)
  
  build_index(package)
  
  # render main pages
  build_pages(package)
  
  if (interactive()) {
    browseURL(normalizePath(file.path(base_path, "index.html")))
  }
  invisible(TRUE)
}

install_lib <- local({
      .lib <- NULL
      function(package){
        if( missing(package) || !is.null(.lib) ) return(.lib)
        if( is.null(package) ){
          .lib <<- NULL
          return(.lib)
        }
        
        # install package
        tmplib <- tempfile()
        dir.create(tmplib)
        .libPaths(c(tmplib, file.path(package$path, '..', 'lib'), .libPaths()))
        pkgmaker::quickinstall(package$path, tmplib, vignettes=TRUE)
        .lib <<- tmplib
        .lib
      }
 })

knit_html <- function(rd, out, links = tools::findHTMLlinks()){
  
  p <- basename(out)
  message("** knitting documentation of ", p)
  f <- tempfile()
  on.exit( unlink(f) )
  tools::Rd2HTML(rd, f, package = '', Links = links, no_links = is.null(links), stages = "render")
  message('DONE')
  txt = readLines(f, warn = FALSE)
  if (length(i <- grep("<h3>Examples</h3>", txt)) == 1L && 
      length(grep("</pre>", txt[i:length(txt)]))) {
    i0 = grep("<pre>", txt)
    i0 = i0[i0 > i][1L] - 1L
    i1 = grep("</pre>", txt)
    i1 = i1[i1 > i0][1L] + 1L
    tools::Rd2ex(rd, ef <- tempfile())
    ex = readLines(ef, warn = FALSE)
    ex = ex[-(1L:grep("### ** Examples", ex, fixed = TRUE))]
    ex = c("```{r}", ex, "```")
    opts_chunk$set(fig.path = str_c("figure/", p, "-"), 
        tidy = FALSE)
    res = try(knit2html(text = ex, envir = parent.frame(2), 
            fragment.only = TRUE))
    if (inherits(res, "try-error")) {
      res = ex
      res[1] = "<pre><code class=\"r\">"
      res[length(res)] = "</code></pre>"
    }
    txt = c(txt[1:i0], res, txt[i1:length(txt)])
    txt = sub("</head>", "\n<link rel=\"stylesheet\" href=\"highlight.css\">\n<script src=\"highlight.pack.js\"></script>\n<script>hljs.initHighlightingOnLoad();</script>\n</head>", 
        txt)
  }
  else message("no examples found for ", p)
  writeLines(txt, out)
}


function (pkg, links = tools::findHTMLlinks(), frame = TRUE) 
{
  library(pkg, character.only = TRUE)
  optc = opts_chunk$get()
  on.exit(opts_chunk$set(optc))
  file.copy(system.file("misc", c("highlight.css", "highlight.pack.js", 
                      "R.css"), package = "knitr"), "./")
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 
                  "help", pkg))
  force(links)
  topics = names(pkgRdDB)
  for (p in topics) {
    message("** knitting documentation of ", p)
    tools::Rd2HTML(pkgRdDB[[p]], f <- tempfile(), package = pkg, 
        Links = links, no_links = is.null(links), stages = "render")
    txt = readLines(f, warn = FALSE)
    if (length(i <- grep("<h3>Examples</h3>", txt)) == 1L && 
        length(grep("</pre>", txt[i:length(txt)]))) {
      i0 = grep("<pre>", txt)
      i0 = i0[i0 > i][1L] - 1L
      i1 = grep("</pre>", txt)
      i1 = i1[i1 > i0][1L] + 1L
      tools::Rd2ex(pkgRdDB[[p]], ef <- tempfile())
      ex = readLines(ef, warn = FALSE)
      ex = ex[-(1L:grep("### ** Examples", ex, fixed = TRUE))]
      ex = c("```{r}", ex, "```")
      opts_chunk$set(fig.path = str_c("figure/", p, "-"), 
          tidy = FALSE)
      res = try(knit2html(text = ex, envir = parent.frame(2), 
              fragment.only = TRUE))
      if (inherits(res, "try-error")) {
        res = ex
        res[1] = "<pre><code class=\"r\">"
        res[length(res)] = "</code></pre>"
      }
      txt = c(txt[1:i0], res, txt[i1:length(txt)])
      txt = sub("</head>", "\n<link rel=\"stylesheet\" href=\"highlight.css\">\n<script src=\"highlight.pack.js\"></script>\n<script>hljs.initHighlightingOnLoad();</script>\n</head>", 
          txt)
    }
    else message("no examples found for ", p)
    writeLines(txt, str_c(p, ".html"))
  }
  unlink("figure/", recursive = TRUE)
  toc = sprintf("- <a href=\"%s\" target=\"content\">%s</a>", 
      str_c(topics, ".html"), topics)
  toc = c(str_c("# ", pkg), "", toc, "", paste("Generated with [knitr](http://yihui.name/knitr) ", 
                  packageVersion("knitr")))
  markdown::markdownToHTML(text = paste(toc, collapse = "\n"), 
      output = "00frame_toc.html", title = str_c("R Documentation of ", 
          pkg), options = NULL, extensions = NULL, stylesheet = "R.css")
  txt = readLines(file.path(find.package(pkg), "html", "00Index.html"))
  unlink("00Index.html")
  writeLines(gsub("../../../doc/html/", "http://stat.ethz.ch/R-manual/R-devel/doc/html/", 
                  txt, fixed = TRUE), "00Index.html")
  if (!frame) {
    unlink(c("00frame_toc.html", "index.html"))
    (if (.Platform$OS.type == "windows") 
        file.copy
      else file.symlink)("00Index.html", "index.html")
    return(invisible())
  }
  writeLines(sprintf("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">\n<html>\n<head><title>Documentation of the %s package</title></head>\n<frameset cols=\"15%%,*\">\n  <frame src=\"00frame_toc.html\">\n  <frame src=\"00Index.html\" name=\"content\">\n</frameset>\n</html>\n", 
                  pkg), "index.html")
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
    library(knitr)
    od <- setwd(package$base_path)
    on.exit( setwd(od) )
    knit_rd(package$package, frame=FALSE)
    setwd(od)
    on.exit()
    return(index)
  }
  
  for (i in seq_along(index$name)) {
    message("Generating ", basename(paths[[i]]))
    
    rd <- package$rd[[i]]
      html <- to_html(rd, 
        env = new.env(parent = globalenv()), 
        topic = str_replace(basename(paths[[i]]), "\\.html$", ""),
        package = package)
      html$pagetitle <- html$name
  
      html$package <- package[c("package", "version")]
      html$indextarget <- "_MAN.html"
      render_page(package, "topic", html, paths[[i]], nonav=TRUE)
      graphics.off()
    
    if ("internal" %in% html$keywords) {
      index$in_index[i] <- FALSE
    }
    index$title[i] <- html$title
  }

  index
}

readme <- function(package) {
  if (!is.null(package$readme)) return(markdown(package$readme))
  
  path <- file.path(package$path, "README.md")
  # use description if no README.md is available
  if (!file.exists(path)) return( package$description )
  
  markdown(path = path)
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
	
	message("Rendering MD pages")
	
	for(i in seq_along(mdfiles)) {
		mdf <- mdfiles[i]
		basef <- basename(mdf)
		out <- str_c('_PAGE-', sub("\\.(R)?md$", ".html", basef))
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

copy_bootstrap <- function(base_path) {
  bootstrap <- file.path(inst_path(), "bootstrap")
  file.copy(dir(bootstrap, full.names = TRUE), base_path, recursive = TRUE)
  d3 <- file.path(inst_path(), "d3")
  file.copy(dir(d3, full.names = TRUE), base_path, recursive = TRUE)
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
	
	outfile <- file.path(package$base_path, '_REFERENCES.html') 
	message("Generating ", basename(outfile))
	# load bibtex items
  library(bibtex)
	bibs <- read.bib(ref)
	# format
	package$references <- lapply(format(bibs, style='html'), function(x) list(bibitems=x))
	
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
  
  for(i in seq_along(title)) {
    if( !file.exists(dfile <- in_path[i]) )
      dfile <- sub("\\.r$", ".R", dfile)
    message("Evaluating demo ", basename(dfile))
    demo_code <- readLines(file.path(demo_dir, dfile))
#    demo_expr <- evaluate(demo_code, new.env(parent = globalenv()))

    message("Generating demo ", filename[i])
    html <- list()
    html$demo <- eval_replay_html(demo_code, envir=new.env(parent = globalenv())
                            , package = package, prefix = str_c(pieces[i], "-demo"))
    html$indextarget <- "_DEMOS.html"
    html$pagetitle <- title[i]
    html$package <- package
    render_page(package, "demo", html, 
      file.path(package$base_path, filename[i]), nonav=TRUE)
  }
  
  
  package$demos <- list(demo=unname(apply(cbind(filename, title), 1, as.list)))  
  # render dedicated file
  outfile <- file.path(package$base_path, '_DEMOS.html')
  message("Generating ", basename(outfile))
  render_page(package, "index-demos", package, outfile)
  # add dedicated head link
  add_headlink(package, basename(outfile), 'Demos')
  
  # return demos
  package$demos

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

# Adds Navigation Bar
build_pages <- function(package, base_path=NULL, layout='default') {
	
  # pre-process arguments
  package <- package_info(package, base_path=base_path)
  outpath <- package$base_path
  
  wrap_page(NULL)
  # substitute head links in all html files starting with '_'
  files <- dir(outpath, pattern="^_.*\\.html", full.names=TRUE)
  files <- c(file.path(outpath,'index.html'), files)
  message("Wrapping pages ", paste(basename(files), collapse=", ")," in layout:", layout)
  
  package$headlinks <- add_headlink()
  navbar <- paste(render_template(package, "navbar", '', package), collapse="\n")
	sapply(files, function(f){
      # collapse contents
      contents <- paste(readLines(f, warn=FALSE), collapse="\n")
      # substitute in layout template
      rendered <- whisker.render(contents, list(navbar=navbar))
      cat(rendered, file = f)
  })
	invisible()
	
}
