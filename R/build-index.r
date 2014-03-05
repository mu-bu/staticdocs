#' Build the index page.
#'
build_index <- function(package) {
  out <- file.path(package$base_path, "index.html")
  message("Generating index.html")

  index <- package$index
  topic_index <- package$topics[package$topics$in_index, , drop = FALSE]
  package$topic_index <- rows_list(topic_index)

  # Cross-reference complete list of topics vs. topics found in index page
  topics <- unlist(lapply(index, "[[", "topics"))
  missing <- !(topics %in% topic_index$name)
  if (any(missing)) {
    warning("Can't find index topics: ", paste(topics[missing], 
      collapse = ", "), call. = FALSE)
    topics <- topics[!missing]
  }
  
  other <- !(topic_index$name %in% topics)
  if (any(other)) {
  title <- if(length(topics)) 'Other' else ''
  index <- 
    c(index, list(sd_section(title, NULL, sort(topic_index$name[other]))))
  }
  
  # Render each section
  sections <- lapply(index, build_section, package = package)
  package$sections <- sections
  package$rd <- NULL
  
  #generate dedicated documentation index page
  manout <- file.path(package$base_path, "PAGE-MAN.html")
  message("Generating ", basename(manout))
  render_page(package, "man", package, manout)
  # add head link to index page
  add_headlink(package, basename(manout), 'Documentation', prepend=TRUE)
  
  # Render alias cloud
  package$topiccloud <- build_alias_cloud(topic_index)
  package$sections <- NULL
  
  render_icons(package)
  package$pagetitle <- "Index"
  render_page(package, "index", package, out)
}

build_alias_cloud <- function(index){
  
  aliases <- sapply(index$name, function(topic){
    row <- index[index$name == topic, , drop=FALSE]
    row$alias[[1]]
  })
  l <- sapply(aliases, length)
  words <- paste('"', names(l), ':', l, '"', sep='', collapse=', ')
  
  str_c('<script>
var fill = d3.scale.category20();
  wordScale=d3.scale.log().domain([1, ', max(l), ']).range([10, 100]);
  var w = 600, h = 600;
 var datawords=[', words, '];

d3.layout.cloud().size([w, h])
      .words(datawords.map(function(d) {
		d = d.split(":");
        return {key: d[0], value: d[1] * 1};
      }))
      .spiral("rectangular")
      .padding(2)
      //.rotate(function() { return  ~~(Math.random() * 2) * 90; })
      .text(function(d) { return d.key; })
      .fontSize(function(d) { return wordScale(d.value); })
      .on("end", draw)
      .start();

  function draw(words) {
    d3.select("#cloud").append("svg")
        .attr("width", w)
        .attr("height", h)
      .append("g")
        .attr("transform", "translate(" + [w >> 1, h >> 1] + ")")
      .selectAll("text")
        .data(words)
      .enter().append("text")
        .style("font-size", function(d) { return d.size + "px"; })
        .style("font-family", "Impact")
        .style("fill", function(d, i) { return fill(i); })
        .attr("text-anchor", "middle")
        .attr("class", "cloud-txt")
        .attr("transform", function(d) {
          return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
        })
        .text(function(d) { return d.text; });
  }
</script>')
}

build_section <- function(section, package) {
  find_info <- function(item) {
    match <- package$topics$name == item$name
    if (!any(match)) return(NULL)
    
    row <- package$topics[match, , drop = FALSE]
    item$file_out <- row$file_out

    aliases <- setdiff(row$alias[[1]], row$name)
    if (length(aliases) > 0) {
      item$aliases <- str_c("(", str_c(aliases, collapse = ", "), ")")
    }
    
    if (is.null(item$title)) {
      rd <- package$rd[[row$file_in]]
      item$title <- extract_title(rd)
    }
    
    item$icon <- icon_path(package, item$name)
    item
  }
  
  desc <- section$description
  
  list(
    title = section$name %||% "Missing section title",
    description = markdown(desc),
    items = compact(lapply(section$elements, find_info))
  )
}

extract_title <- function(x) {
  alias <- Find(function(x) attr(x, "Rd_tag") == "\\title", x)
  alias[[1]][[1]]
}

compact <- function (x) Filter(Negate(is.null), x)

