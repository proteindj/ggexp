#' Plot pairwise scatterplots
#'
#' @param data Data frame to plot
#' @param x Columns to use for x-axis
#' @param y Columns to use for y-axis
#' @param color Column for color
#' @param shape Column for shape
#' @param alpha Numeric value between 0 and 1 for alpha
#' @param facet_rows Columns to facet on
#' @param facet_columns Columns to facet on
#' @param facet_type Either "wrap" or "grid", same as ggplot
#' @param ... params passed into either facet_wrap or facet_grid, depending on facet_type parameter
#'
#' @import ggplot2
#' @importFrom dplyr select_if left_join
#' @importFrom purrr pmap_dfr
#'
#' @return ggplot object
#' @export
plot_pairwise_scatterplot = function(data,
                                     x = colnames(data %>% select_if(is.numeric)),
                                     y = x,
                                     combination_groups = c(),
                                     axis_annotations = NULL,
                                     color = NULL,
                                     shape = NULL,
                                     alpha = 1,
                                     point_size = 0.5,
                                     facet_rows = c(),
                                     facet_columns = c(),
                                     facet_type = "grid",
                                     ...) {
  axes_columns = intersect(c(x, y), colnames(data))

  combinations = expand_grid_unique(x, y)

  if (length(combination_groups) != 0 && facet_type == "wrap") {
    combinations$V1 = as.character(combinations$V1)
    combinations$V2 = as.character(combinations$V2)

    axis_annotations$axis = as.character(axis_annotations$axis)

    combinations = combinations %>%
      left_join(axis_annotations, by = c("V1" = "axis")) %>%
      left_join(axis_annotations, by = c("V2" = "axis"))

    for (group in combination_groups) {
      combinations = combinations[combinations[, paste0(group, ".x"), drop = TRUE] ==
                                    combinations[, paste0(group, ".y"), drop = TRUE], , drop = FALSE]
    }
  }

  data = pmap_dfr(combinations, ~ {
    x = ..1
    y = ..2
    cbind(
      data.frame(.xvalue = data[, as.character(x), drop = TRUE], .yvalue = data[, as.character(y), drop = TRUE]),
      data.frame(.xkey = ..1, .ykey = ..2),
      data[, intersect(c(color, facet_rows, facet_columns), colnames(data)), drop = FALSE]
    )
  })

  if (!is.null(axis_annotations)) {
    data = data %>%
      left_join(axis_annotations[, c("axis", combination_groups), drop = FALSE], by = c(".xkey" = "axis"))
  }

  data$.xkey = factor(data$.xkey, levels = x)
  data$.ykey = factor(data$.ykey, levels = y)

  if (length(x) > 1)  {
    facet_columns = c(facet_columns, ".xkey")
    xlab = NULL
  } else {
    xlab = x
  }

  if (length(y) > 1)  {
    facet_rows = c(facet_rows, ".ykey")
    ylab = NULL
  } else {
    ylab = y
  }

  plot = data %>%
    ggplot(., aes_string(x = ".xvalue", y = ".yvalue", color = color, shape = shape)) +
    theme_ggexp() +
    labs(x = xlab, y = ylab) +
    geom_point(alpha = alpha, size = point_size)

  plot$data$.xykey = paste0("x: ", plot$data$.xkey, ", y: ", plot$data$.ykey)

  if (facet_type == "wrap") {
    facet_rows = setdiff(facet_rows, ".ykey")
    facet_columns = setdiff(facet_columns, ".xkey")
    facet_columns = c(facet_columns, ".xkey", ".ykey")
  }

  plot = plot_facets(plot,
                     facet_rows,
                     facet_columns,
                     facet_type,
                     ...)

  return(plot)
}

expand_grid_unique = function(x, y, include.equals = FALSE) {
  x = unique(x)
  y = unique(y)
  g = function(i) {
    z = setdiff(y, x[seq_len(i - include.equals)])
    if (length(z))
      cbind(x[i], z, deparse.level = 0)
  }
  as.data.frame(do.call(rbind, lapply(seq_along(x), g)))
}
