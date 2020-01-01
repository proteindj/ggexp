#' Plot pairwise scatterplots
#'
#' @param data data frame containing dataset to use for plotting
#' @param x string scalar indicating column for x-axis
#' @param y string scalar indicating column for y-axis
#' @param color string scalar indicating column for color
#' @param shape string scalar indicating column for shape
#' @param alpha numeric scalar for alpha of points
#' @param facet_rows string vector indicating columns for faceting by row
#' @param facet_columns string vector indicating columns for faceting by column
#' @param facet_type string scalar that is either "wrap" or "grid", corresponding to facet_wrap and facet_grid respectively
#' @param facet_switch string scalar that is either NULL, "both", "x", or "y", same as switch argument in facet calls
#' @param facet_scales string scalar that is either "fixed", "free_x", "free_y", or "free", same as scales argument in facet calls
#' @param nrow numeric scalar indicating the number of rows in plot, only applies if facet_type == "wrap"
#'
#' @import ggplot2
#' @importFrom dplyr select_if
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_pairwise_scatterplot = function(data,
                            x = colnames(data %>% select_if(is.numeric)),
                            y = x,
                            combination_groups = c(),
                            axis_annotations = NULL,
                            color = NULL,
                            shape = NULL,
                            alpha = 0.3,
                            facet_rows = c(),
                            facet_columns = c(),
                            facet_type = "grid",
                            facet_scales = "free",
                            facet_switch = "both",
                            nrow = 2) {

  axes_columns = intersect(c(x, y), colnames(data))

  combinations = expand_grid_unique(x, y)

  if (length(combination_groups) != 0 && facet_type == "wrap") {
    combinations$V1 = as.character(combinations$V1)
    combinations$V2 = as.character(combinations$V2)

    axis_annotations$axis = as.character(axis_annotations$axis)

    combinations = combinations %>%
      dplyr::left_join(axis_annotations, by = c("V1" = "axis")) %>%
      dplyr::left_join(axis_annotations, by = c("V2" = "axis"))

    for (group in combination_groups) {
      combinations = combinations[combinations[, paste0(group, ".x"), drop = TRUE] ==
                                    combinations[, paste0(group, ".y"), drop = TRUE], , drop = FALSE]
    }
  }

  data = purrr::pmap_dfr(combinations, ~ {
      x = ..1
      y = ..2
      cbind(data.frame(.xvalue = data[, as.character(x), drop = TRUE], .yvalue = data[, as.character(y), drop = TRUE]), data.frame(.xkey = ..1, .ykey = ..2), data[, intersect(c(color, facet_rows, facet_columns), colnames(data)), drop = FALSE])
    }
  )

  if (!is.null(axis_annotations)) {
    data = data %>%
      dplyr::left_join(axis_annotations[, c("axis", combination_groups), drop = FALSE], by = c(".xkey" = "axis"))
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

  if (is.null(color)) {
    palette = c()
  } else if (is.numeric(data[, color, drop = TRUE])) {
    palette = c()
  } else {
    data[, color] = factor(as.character(data[, color, drop = TRUE]), levels = gtools::mixedsort(as.character(unique(data[, color, drop = TRUE]))))
    palette = get_palette(data[, color, drop = TRUE])
  }

  plot = data %>%
    ggplot(., aes_string(x = ".xvalue", y = ".yvalue", color = color)) +
    theme_ggexp() +
    labs(x = xlab, y = ylab) +
    geom_point(alpha = alpha)

  if (length(palette) > 0) {
    plot = plot +
      palette
  }

  plot$data$.xykey = paste0("x: ", plot$data$.xkey, "\ny: ", plot$data$.ykey)

  if (facet_type == "wrap") {
    facet_rows = setdiff(facet_rows, ".ykey")
    facet_columns = setdiff(facet_columns, ".xkey")
    facet_rows = c(facet_rows, ".xykey")
  }

  plot = plot_facets(plot, facet_rows, facet_columns, facet_type, facet_scales, facet_switch, nrow)

  return(plot)
}

expand_grid_unique = function(x, y, include.equals=FALSE) {
  x = unique(x)
  y = unique(y)
  g = function(i) {
    z = setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  as.data.frame(do.call(rbind, lapply(seq_along(x), g)))
}
