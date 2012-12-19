# Replay a list of evaluated results, just like you'd run them in a R
# terminal, but rendered as html

replay_html <- function(x, ...) UseMethod("replay_html", x)

eval_replay_html <- function(x, envir, package, prefix, ...){
  
  library(evaluate)
  # remove old files
  ifiles <- list.files(package$base_path, pattern=str_c('^', prefix), full.names=TRUE)
  if( length(ifiles) ){
    message("Removing plot files [", prefix, ']')
    unlink(ifiles)
  }
  # setup sequential graphic file device
  fprefix <- str_c(prefix, '%i.png')
  fformat <- file.path(package$base_path, fprefix)
  on.exit(dev.off())
#  png(fformat, width=400, height=400, res=96)
  png(fformat, width=7, height=7, unit='in', res=72)
  
  ptext <- parse_all(x)
  # loop over the expressions and check if any graphic was generated
  i <- 1
  res <- sapply(ptext$src, function(src, ...){
        expr <- evaluate(src, envir, new_device=FALSE)
        html <- replay_html(expr, package = package, ...)
        while( file.exists(file.path(package$base_path, img <- sprintf(fprefix, i))) ){
          html <- str_c(html,
                  str_c("<p><img src='", img, "' alt='' width='400' height='400' /></p>"))
          i <<- i + 1
        }
        html
      }
  , ...)
  
  # concatenate all results
  str_c(res, collapse="")
}

#' @importFrom evaluate is.source
#' @S3method replay_html list
replay_html.list <- function(x, ...) {
  # Stitch adjacent source blocks back together
  src <- vapply(x, is.source, logical(1))
  # New group whenever not source, or when src after not-src
  group <- cumsum(!src | c(FALSE, src[-1] != src[-length(src)]))
  
  parts <- split(x, group)
  parts <- lapply(parts, function(x) {
    if (length(x) == 1) return(x[[1]])
    src <- str_c(vapply(x, "[[", "src", FUN.VALUE = character(1)), 
      collapse = "")
    structure(list(src = src), class = "source")
  })
  
  pieces <- mapply(replay_html, parts, obj_id = seq_along(parts), 
    MoreArgs = list(...))
  str_c(pieces, collapse = "\n")
}

#' @S3method replay_html character
replay_html.character <- function(x, ...) {
  str_c("<div class='output'>", str_c(x, collapse = ""), "</div>")
}

#' @S3method replay_html value
replay_html.value <- function(x, ...) {
  if (!x$visible) return()
  
  printed <- str_c(capture.output(print(x$value)), collapse = "\n")
  str_c("<div class='output'>", printed, "</div>")
}

#' @S3method replay_html source
replay_html.source <- function(x, ..., package) {
  str_c("<div class='input'>", src_highlight(x$src, package$rd_index),
    "</div>")
}

#' @S3method replay_html warning
replay_html.warning <- function(x, ...) {
  str_c("<strong class='warning'>Warning message:\n", x$message, "</strong>")
}

#' @S3method replay_html message
replay_html.message <- function(x, ...) {
  str_c("<strong class='message'>", str_replace(x$message, "\n$", ""),
   "</strong>")
}

#' @S3method replay_html error
replay_html.error <- function(x, ...) {
  if (is.null(x$call)) {
    str_c("<strong class='error'>Error: ", x$message, "</strong>")
  } else {
    call <- deparse(x$call)
    str_c("<strong class='error'>Error in ", call, ": ", x$message,
     "</strong>")
  }
}

#' @S3method replay_html recordedplot
replay_html.recordedplot <- function(x, package, name_prefix, obj_id, ...) {  
#  name <- str_c(name_prefix, obj_id, ".png")
#  path <- file.path(package$base_path, name)
#  
#  if (!file.exists(path)) { 
#    png(path, width = 400, height = 400, res = 96)
##	TODO: bug here: the device dimension does not match the one used at evaluation time
##	png(path, width=7, height=7, unit='in', res=72)
#    on.exit(dev.off())
#    print(x)
#  }

#  str_c("<p><img src='", name, "' alt='' width='400' height='400' /></p>")
  ''
}
