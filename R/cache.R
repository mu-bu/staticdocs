#' @importFrom digest digest
new_cache <- function() {
  cache <- new.env(parent = emptyenv())
  
  compute <- function(keys, code) {
    hash <- suppressWarnings(digest(keys))
    if (exists(hash, cache, inherits = FALSE)) {
      return(cache[[hash]])
    }
    
    (cache[[hash]] <- force(code))
  }
  
  reset <- function() {
    cache <<- new.env(parent = emptyenv())
  }
  
  get <- function(keys){
	  hash <- suppressWarnings(digest(keys))
	  if (exists(hash, cache, inherits = FALSE)) {
		  return(cache[[hash]])
	  }
  }
  
  list(compute = compute, reset = reset, get = get)
}

misc_cache <- new_cache()
topic_cache <- new_cache()
demo_cache <- new_cache()

#' Clear all staticdocs caches.
#'
#' In order to speed up execution time, staticdocs caches a number of 
#' interim results. This function empties all caches and guarantees that all
#' results are computed afresh.
#' 
#' @export
clear_caches <- function() {
  misc_cache$reset()
  topic_cache$reset()
  demo_cache$reset()
}

load_cache <- function(package, name){
  cache_file <- file.path(package$base_path, 'cache', paste0(name, '.rda'))
  if( file.exists(cache_file) ){
      e <- parent.frame()
      load(cache_file, envir = e)
  }
  dir.create(dirname(cache_file), showWarnings = FALSE)
  cache_file
}
