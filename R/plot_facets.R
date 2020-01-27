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
#'
#' @examples
#' NULL
plot_facets = function(plot,
                       facet_rows = c(),
                       facet_columns = c(),
                       facet_type = "grid",
                       ...) {

  plot = get(paste0(".plot_facet_", facet_type))(plot, facet_rows, facet_columns, ...)

  return(plot)
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
#'
#' @examples
#' NULL
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
#'
#' @examples
#' NULL
.plot_facet_wrap = function(plot,
                            facet_rows,
                            facet_columns,
                            ...) {

  plot = plot + facet_wrap(facets = c(facet_rows, facet_columns), ...)

  return(plot)
}
