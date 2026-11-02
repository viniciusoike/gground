check_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    cli::cli_warn(
      "Using {.field size} with {name} is deprecated; use {.field linewidth} instead."
    )
    data$linewidth <- data$size
  }
  return(data)
}

as_radius <- function(radius) {
  if (grid::is.unit(radius)) {
    return(radius)
  }
  return(grid::unit(radius, "pt"))
}
