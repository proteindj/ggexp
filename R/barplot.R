#' Plot (stacked) barplot with annotations
#'
#' This is a convenience function wrapping around geom_bar, but it makes it easy to make both stacked and unstacked plots and adjust text annotations accordingly.
#'
#' @param data data frame containing dataset to use for plotting
#' @param x column for x-axis
#' @param y column for y-axis
#' @param label column for text annotation
#' @param color column for color
#' @param stacked boolean whether result should be a stacked barplot or not
#' @param constant_height boolean whether stacked bars should have constant height (proportions plotted)
#' @param facet_rows columns for faceting by row
#' @param facet_columns columns for faceting by column
#' @param facet_type either "wrap" or "grid", corresponding to facet_wrap and facet_grid respectively
#' @param facet_switch either NULL, "both", "x", or "y", same as switch argument in facet calls
#' @param facet_scales either "fixed", "free_x", "free_y", or "free", same as scales argument in facet calls
#' @param nrow number of rows in plot, only applies if facet_type == "wrap"
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data = mtcars %>%
#'   mutate(cyl = factor(cyl)) %>%
#'   group_by(cyl, am) %>%
#'   tally()
#'
#' plot_barplot(
#'   data = data,
#'   x = "am",
#'   y = "n",
#'   color = "cyl",
#'   label = "cyl"
#' )
plot_barplot = function(data,
                        x,
                        y,
                        label = NULL,
                        color = NULL,
                        fill = NULL,
                        stacked = TRUE,
                        constant_height = TRUE,
                        text_size = 3,
                        facet_rows = c(),
                        facet_columns = c(),
                        facet_type = "grid",
                        facet_switch = NULL,
                        facet_scales = "free",
                        nrow = 1) {

  data[, x] = factor(data[, x, drop = TRUE], levels = gtools::mixedsort(unique(data[, x, drop = TRUE])))

  if (!is.null(label)) {
    if (is.numeric(data[, label, drop = TRUE])) {
      data[, label] = round(data[, label, drop = TRUE], 2)
    }
  }

  stack_or_fill = ifelse(constant_height, "fill", "stack")

  plot = ggplot(data = data, aes_string(x = x, y = y, color = color)) +
    geom_bar(
      stat = "identity",
      fill = "white",
      position = ifelse(stacked, stack_or_fill, "dodge")
    ) +
    theme_ggexp()

  if (!is.null(label)) {
    if (stacked) {
      plot = plot + geom_text(aes_string(label = label), position = get(paste0("position_", stack_or_fill))(vjust = 0.5), show.legend = FALSE, size = text_size)
    } else {
      plot = plot + geom_text(aes_string(label = label), position = position_dodge(width = 0.9), show.legend = FALSE, size = text_size)
    }
  }

  plot = plot_facets(plot,
                     facet_rows,
                     facet_columns,
                     facet_type,
                     facet_scales,
                     facet_switch,
                     nrow)

  return(plot)
}
