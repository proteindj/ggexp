#' Generate ggexp palette with assigned color values based on alphabetized input values and overriden with input palette
#'
#' @param values character vector or factor which to assign colors to
#'
#' @return ggplot color/fill scale object
#' @export
#'
#' @examples
get_palette = function(values, palette = c()) {
  if (!is.numeric(values)) {
    palette_values =
      c(ggsci::pal_d3()(10),
        ggsci::pal_lancet()(9),
        ggsci::pal_npg()(10),
        ggsci::pal_jama()(7))

    levels = unique(values[!values == "NA"])
    palette_custom = palette_values[1:length(levels)]
    names(palette_custom) = sort(levels)
    if (any(is.na(values)) || any(values == "NA")) palette_custom = c(palette_custom, "NA" = "gray70")
    palette_custom = c(palette, palette_custom)[unique(names(c(palette, palette_custom)))]

    palette_custom = ggplot2::scale_color_manual(values = palette_custom, aesthetics = c("colour", "fill"), drop = FALSE)
  } else {
    palette_custom = list(high = "#67001F", mid = "#F7F7F7", low = "#053061", midpoint = 0, limits = c(min(values), max(values)), aesthetics = c("colour", "fill"))
    palette_custom = c(palette, palette_custom)[unique(names(c(palette, palette_custom)))]

    palette_custom = do.call(ggplot2::scale_color_gradient2, palette_custom)
  }

  return(palette_custom)
}

#' Get ggexp default theme
#'
#' @import ggplot2
#'
#' @return ggplot theme
#' @export
#'
#' @examples
get_theme <- function() {
  # theme_light() +
  #   theme(strip.background = element_rect(fill = 'white')) +
  #   theme(strip.text = element_text(colour = 'black')) +
  #   theme(panel.grid.minor = element_blank()) +
  #   theme(strip.background = element_rect(color = "black", size = 0.3))
  theme_classic() +
    theme(strip.background = element_blank())
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
                       facet_rows,
                       facet_columns,
                       facet_type,
                       facet_scales,
                       facet_switch,
                       nrow) {
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
      switch = facet_switch
    )
  }
  return(plot)
}
