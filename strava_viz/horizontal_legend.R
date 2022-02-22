# from this stackoverflow answer: https://stackoverflow.com/questions/60838128/shiny-r-how-to-make-a-leaflet-legend-horizontal
# manually create a legend
horizontal_legend <- function(values, palette, title, left_label, right_label, bins = 7) {
  
  # validate args
  stopifnot(!is.null(values))
  stopifnot(!is.null(palette))
  stopifnot(!is.null(title))
  stopifnot(!is.null(left_label))
  stopifnot(!is.null(right_label))
  
  # generate color palette using Bins (not sure if it's the best approach)
  # @reference: 
  # https://github.com/rstudio/leaflet/blob/c19b0fb9c60d5caf5f6116c9e30dba3f27a5288a/R/legend.R#L93
  pal <- colorNumeric(palette, values)
  cuts <- if (length(bins) == 1) pretty(values, n = bins) else bins
  n <- length(cuts)
  r <- range(values, na.rm = TRUE)
  # pretty cut points may be out of the range of `values`
  cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
  colors <- pal(c(r[1], cuts, r[2]))
  
  # generate html list object using colors
  legend <- tags$ul(class = "legend")
  legend$children <- lapply(seq_len(length(colors)), function(color) {
    tags$li(
      class = "legend-item legend-color",
      style = paste0(
        "background-color:", colors[color]
      ),
    )
  })
  
  # add labels to list
  legend$children <- tagList(
    tags$li(
      class = "legend-item legend-label left-label",
      as.character(left_label)
    ),
    legend$children,
    tags$li(
      class = "legend-item legend-label right-label",
      as.character(right_label)
    )
  )
  
  # render legend with title
  return(
    tagList(
      tags$span(class = "legend-title", as.character(title)),
      legend
    )
  )
}
