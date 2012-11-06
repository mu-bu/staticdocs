#' @importFrom highlight highlight renderer_html formatter_html
#' @importFrom parser parser
src_highlight <- function(text, index) {
  if (str_trim(text) == "") return("")

  expr <- NULL
  try(expr <- parser(text = text))
  if (length(expr) == 0) return(text)
  
  # Custom formatter that adds links to function calls
  formatter <- function(tokens, styles, ...) {
    funcall <- styles == "functioncall"
    for(i in which(funcall)) {
      if( !nchar(tokens[i]) ){
        message("Empty help topic near "
               , str_c(tokens[max(1,i-3):min(i+3, length(tokens))]), collapse = "\n")
        next
      } 
      loc <- find_topic(tokens[i], NULL, index = index)
      if (is.null(loc)) {
        message("Can't find help topic ", tokens[i])
        next
      }
      tokens[i] <- make_link(loc, label = tokens[i])
    }
    
    formatter_html(tokens, styles, ...)
  }
  
  renderer <- renderer_html(doc = FALSE, formatter = formatter)
  out <- capture.output(highlight(parser.output = expr, renderer = renderer))
  # Drop pre tag
  out <- out[-c(1, length(out))]
  str_c(out, collapse = "\n")
}
