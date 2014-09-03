#' Render complete page.
#' 
#' @param name of the template (e.g. index, demo, topic)
#' @param data data for the template
#' @param path location to create file.  If \code{""} (the default), 
#'   prints to standard out.
#' @export
render_page <- function(package, name, data, path = "", navbar=TRUE, to_top_link = TRUE) {
    
  if( isTRUE(to_top_link) ) data$to_top <- list()
  # render template components
  pieces <- c("head", "navbar", "header", "content", "footer")
  components <- lapply(pieces, render_template, package = package, name, data)
  names(components) <- pieces
  # handle navbar
  if( isTRUE(navbar) ) navbar <- "{{{navbar}}}" # will substitute navbar at the end
  if( is.character(navbar) ) components$navbar <- navbar # use provided navbar
  
  # render complete layout
  out <- render_template(package, "layout", name, components)
  # github links to issues
#  if( use_github() ){
#      out <- gsub("((issues?)|(bugs?)|(fixes?))[ ]*#([0-9]+)", "(\\1)[/issues/\\2]", outignore.case = TRUE)
#  }  
  cat(out, file = path)
}

#' @importFrom whisker whisker.render
render_template <- function(package, type, name, data) {
  template <- readLines(find_template(package, type, name))
  if (length(template) <= 1 && str_trim(template) == "") return("")
  
  whisker.render(template, data)
}

# Find template by looking first in package/staticdocs then in 
# staticdocs/templates, trying first for a type-name.html otherwise 
# defaulting to type.html
find_template <- function(package, type, name) {
  paths <- c(
    pkg_sd_path(package),
    file.path(inst_path(), "templates")
  )
  
  names <- c(
    str_c(type, "-", name, ".html"),
    str_c(type, ".html")
  )
  
  locations <- as.vector(t(outer(paths, names, FUN = "file.path")))
  Find(file.exists, locations, nomatch = 
    stop("Can't find template for ", type, "-", name, ".", call. = FALSE))
}
