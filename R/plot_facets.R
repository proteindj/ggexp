#' Utility function to facet a plot
#'
#' @param plot ggplot object
#' @param facet_rows column names from original data frame to be used in facet rows if facet_type == "grid" else columns to be faceted
#' @param facet_columns column names from original data frame to be used in facet columns if facet_type == "grid" else columns to be faceted
#' @param facet_type either "wrap" or "grid"
#' @param ... params passed into either facet_wrap or facet_grid, depending on facet_type parameter
#'
#' @return ggplot object
#' @export
plot_facets = function(plot,
                       facet_rows = c(),
                       facet_columns = c(),
                       facet_type = "grid",
                       ...) {
  plot = get(paste0(".plot_facet_", facet_type))(plot, facet_rows, facet_columns, ...)

  return(plot)
}

#' Get dimensions of ggplot plots with facets
#'
#' @param plot ggplot object with facets
#'
#' @importFrom ggplot2 wrap_dims ggplot_build
#'
#' @return
#' @export
compute_facet_dim = function(p) {

  if (is.null(p$facet$params$rows)) {
    built = ggplot_build(p)
    n = length(unique(built$data[[1]]$PANEL))
    par = built$layout$facet$params
    dim = wrap_dims(n, par$nrow, par$ncol)
  } else {
    nrow = 1
    for (row in names(p$facet$params$rows)) {
      nrow = nrow * length(unique(p$data[, row, drop = TRUE]))
    }
    ncol = 1
    for (col in names(p$facet$params$cols)) {
      ncol = ncol * length(unique(p$data[, col, drop = TRUE]))
    }
    dim = c(nrow, ncol)
  }

  return(dim)
}

#' Helper function to add facet_grid to plot
#'
#' @param plot ggplot object to facet
#' @param facet_rows columns in data to make facet rows
#' @param facet_columns columns in data to make facet columns
#'
#' @importFrom ggplot2 facet_grid
#'
#' @return
.plot_facet_grid = function(plot,
                            facet_rows,
                            facet_columns,
                            ...) {
  formula = as.formula(paste0(
    ifelse(
      paste(facet_rows, collapse = " + ") == "",
      ".",
      paste(facet_rows, collapse = " + ")
    ),
    " ~ ",
    ifelse(
      paste(facet_columns, collapse = " + ") == "",
      ".",
      paste(facet_columns, collapse = " + ")
    )
  ))

  plot = plot + facet_grid(formula, ...)

  return(plot)
}

#' Helper function to add facet_wrap to plot
#'
#' @param plot ggplot object to facet
#' @param facet_rows columns in data to facet
#' @param facet_columns same as facet_rows for facet_wrap - just combines these two with rows first then columns
#'
#' @importFrom ggplot2 facet_wrap
#'
#' @return
.plot_facet_wrap = function(plot,
                            facet_rows,
                            facet_columns,
                            ...) {
  plot = plot + facet_wrap(facets = c(facet_rows, facet_columns), ...)

  return(plot)
}
