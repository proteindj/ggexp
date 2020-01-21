#' Get ggexp default theme
#'
#' @import ggplot2
#'
#' @return ggplot theme
#' @export
#'
#' @examples
theme_ggexp = function() {
  theme_classic() +
    theme(strip.background = element_blank(), strip.placement = "outside")
}

#' Utility function to facet a plot
#'
#' @param plot ggplot object
#' @param facet_rows Column names from original data frame to be used in facet rows if facet_type == "grid" else columns to be faceted
#' @param facet_columns Column names from original data frame to be used in facet columns if facet_type == "grid" else columns to be faceted
#' @param facet_type Either "wrap" or "grid"
#' @param facet_scales Corresponds to scales argument in facet_wrap/facet_grid - either "fixed", free", "free_x", "free_y"
#' @param facet_switch Corresponds to switch argument in facet_wrap/facet_grid - either "x", "y", or "both"
#' @param nrow If facet_wrap, max number of rows in faceted figure
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_facets = function(plot,
                       facet_rows = c(),
                       facet_columns = c(),
                       facet_type = "grid",
                       facet_scales = NULL,
                       facet_switch = NULL,
                       nrow = NULL) {
  if (facet_type == "grid") {
    plot = plot + facet_grid(
      as.formula(paste0(
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
      )),
      scales = facet_scales,
      drop = TRUE,
      switch = facet_switch
    )
  } else {
    plot = plot + facet_wrap(
      facets = c(facet_rows, facet_columns),
      nrow = nrow,
      scales = facet_scales,
      drop = TRUE,
      switch = NULL,
      strip.position = "top"
    )
  }
  return(plot)
}
