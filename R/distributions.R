#' Plot distributions with pairwise annotations and flexibility
#'
#' @param data data frame containing dataset to use for plotting
#' @param pairwise_annotation data frame containing pairwise annotations
#' @param type string scaler indicating type of plot - can be "line", "sina", "quasirandom", "density", "violin", "box", or "ridge"
#' @param x string scalar indicating column for x-axis
#' @param y string scalar indicating column for y-axis
#' @param group string scalar indicating column for group aesthethic, used if type == "line"
#' @param color string scalar indicating column for color
#' @param alpha numeric scalar for alpha of points
#' @param scale string scalar that is either "default" for linearly-spaced scale or "log" for log-spaced
#' @param lower_quantile numeric scalar indicating lower quantile of values, beyond which data points are filtered from plot
#' @param upper_quantile numeric scalar indicating uower quantile of values, beyond which data points are filtered from plot
#' @param tier_width numeric scalar indicating relative distance between tiers for pairwise annotations
#' @param annotate_counts boolean scalar indicating whether to annotate counts per group or not
#' @param pairwise_annotation_label string scalar indicating column of pairwise_annotation data to use for annotation text
#' @param pairwise_annotation_exclude string vector indicating values to not annotate on pairwise annotations
#' @param facet_rows string vector indicating columns for faceting by row
#' @param facet_columns string vector indicating columns for faceting by column
#' @param facet_type string scalar that is either "wrap" or "grid", corresponding to facet_wrap and facet_grid respectively
#' @param facet_switch string scalar that is either NULL, "both", "x", or "y", same as switch argument in facet calls
#' @param facet_scales string scalar that is either "fixed", "free_x", "free_y", or "free", same as scales argument in facet calls
#' @param nrow numeric scalar indicating the number of rows in plot, only applies if facet_type == "wrap"
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
plot_distributions = function(data,
                              pairwise_annotation = NULL,
                              type = "quasirandom",
                              x,
                              y,
                              group = NULL,
                              color = NULL,
                              alpha = 0.5,
                              scale = "default",
                              lower_quantile = 0,
                              upper_quantile = 1,
                              tier_width = 0.16,
                              annotate_counts = TRUE,
                              pairwise_annotation_label = "p_signif",
                              pairwise_annotation_exclude = c(),
                              facet_rows = c(),
                              facet_columns = c(),
                              facet_type = "wrap",
                              facet_scales = "free",
                              facet_switch = NULL,
                              nrow = 1) {
  plot = get(paste0("plot_", type))(data,
                                    x,
                                    y,
                                    color,
                                    group,
                                    alpha)

  if (!is.null(color)) {
    plot = plot + get_palette(data[, color, drop = TRUE])
  }

  plot = plot_scale(plot, scale, type)

  plot = plot_facets(plot,
                     facet_rows,
                     facet_columns,
                     facet_type,
                     facet_scales,
                     facet_switch,
                     nrow)

  if (annotate_counts) {
    counts_annotation = compute_counts_annotation_data(data, x, c(facet_rows, facet_columns))
    plot = plot_counts_annotation(plot, x, counts_annotation, annotate_counts, type)
  }

  if (!is.null(pairwise_annotation) & (pairwise_annotation_label %in% colnames(pairwise_annotation)) &
      !(type %in% c("density", "ridge"))) {
    plot = plot_pairwise_annotation(
      plot,
      pairwise_annotation,
      pairwise_annotation_label,
      pairwise_annotation_exclude,
      tier_width,
      scale
    )
  }

  plot = plot + theme_ggexp()

  return(plot)
}

#' Annotate number of values per group on plot
#'
#' @param plot
#' @param x
#' @param counts_annotation
#' @param annotate_counts
#' @param type
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_counts_annotation = function(plot,
                                  x,
                                  counts_annotation,
                                  annotate_counts,
                                  type) {
  if (annotate_counts &&
      !(type %in% c("density", "ridge"))) {
    plot = plot +
      geom_text(
        data = counts_annotation,
        aes_string(
          label = "n",
          x = x,
          y = -Inf
        ),
        hjust = 0.5,
        vjust = -0.5,
        size = 2,
        color = "black",
        angle = 0
      )
  } else if (annotate_counts && type == "ridge") {
    plot = plot +
      geom_text(
        data = counts_annotation,
        aes_string(label = "n",
                   x = Inf,
                   y = x),
        hjust = 1.3,
        vjust = -1,
        size = 2,
        color = "black",
        angle = 0
      )
  }
  return(plot)
}

#' Compute number of values per group for count annotation
#'
#' @param data
#' @param x
#' @param facet_rows
#' @param facet_columns
#'
#' @return
#'
#' @examples
compute_counts_annotation_data = function(data, x, groups) {
  counts = data[, unique(c(x, groups)), drop = FALSE] %>%
    stats::na.omit()
  counts = counts %>%
    dplyr::group_by(.dots = unique(c(x, groups))) %>%
    dplyr::tally()
  return(counts)
}

#' Transform scale based on scale type and plot type
#'
#' @param plot
#' @param scale
#' @param type
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_scale = function(plot, scale, type) {
  if (scale == "log") {
    plot = plot +
      scale_y_continuous(trans = 'log10')
  }
  if (!(type %in% c("density", "ridge"))) {
    plot = plot + scale_x_discrete(drop = FALSE)
  }
  return(plot)
}

#' Plot line graph
#'
#' @param data
#' @param x
#' @param y
#' @param color
#' @param group
#' @param alpha
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_line = function(data,
                     x = NULL,
                     y = NULL,
                     color = NULL,
                     group = NULL,
                     alpha = 0.5) {
  plot = ggplot(data) +
    geom_line(alpha = alpha,
              aes_string(x = x, y = y, group = group, color = color)) +
    geom_point(alpha = alpha,
               aes_string(x = x, y = y, col = color),
               shape = 1)

  return(plot)
}

#' Plot sina plot
#'
#' @param data
#' @param x
#' @param y
#' @param color
#' @param group
#' @param alpha
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_sina = function(data,
                     x = NULL,
                     y = NULL,
                     color = NULL,
                     group = NULL,
                     alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = x,
    y = y,
    col = color,
    group = group
  )) +
    ggforce::geom_sina(alpha = alpha, shape = 1) +
    geom_boxplot(
      alpha = 0,
      width = 0.3,
      color = "black",
      outlier.size = 0
    )

  return(plot)
}

#' Plot quasirandom plot
#'
#' @param data
#' @param x
#' @param y
#' @param color
#' @param group
#' @param alpha
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_quasirandom = function(data,
                            x = NULL,
                            y = NULL,
                            color = NULL,
                            group = NULL,
                            alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = x,
    y = y,
    col = color,
    group = group
  )) +
    ggbeeswarm::geom_quasirandom(
      method = "tukeyDense",
      alpha = alpha,
      shape = 1,
      dodge.width = 1,
      position = position_dodge(width = 0.75)
    ) +
    geom_boxplot(
      alpha = 0,
      width = 0.3,
      color = "black",
      outlier.size = 0,
      position = position_dodge(width = 1)
    )

  return(plot)
}

#' Plot violin plot
#'
#' @param data
#' @param x
#' @param y
#' @param color
#' @param group
#' @param alpha
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_violin = function(data,
                       x = NULL,
                       y = NULL,
                       color = NULL,
                       group = NULL,
                       alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = x,
    y = y,
    col = color,
    group = group
  )) +
    geom_violin(alpha = alpha, position = position_dodge(width = 1)) +
    geom_boxplot(
      alpha = 0,
      width = 0.3,
      outlier.size = 0,
      position = position_dodge(width = 1)
    )
  return(plot)
}

#' Plot box and whiskers plot
#'
#' @param data
#' @param x
#' @param y
#' @param color
#' @param group
#' @param alpha
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_box = function(data,
                    x = NULL,
                    y = NULL,
                    color = NULL,
                    group = NULL,
                    alpha = 0.5) {
  plot = ggplot(data) +
    geom_boxplot(alpha = 1,
                 width = 0.3,
                 aes_string(
                   x = x,
                   y = y,
                   col = color,
                   group = group
                 ))

  return(plot)
}

#' Plot density plot
#'
#' @param data
#' @param x
#' @param y
#' @param color
#' @param group
#' @param alpha
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_density = function(data,
                        x = NULL,
                        y = NULL,
                        color = NULL,
                        group = NULL,
                        alpha = 0.5) {
  if (!is.null(group) && !is.null(color)) {
    data[, group] = interaction(data[, color, drop = TRUE], data[, group, drop = TRUE])
  }
  if (!is.numeric(data[, color, drop = TRUE])) {
    plot = ggplot(data, aes_string(x = y, col = color, fill = color, group = group)) +
      geom_density(alpha = alpha) +
      geom_rug(alpha = 0.1)
  } else {
    plot = ggplot(data, aes_string(x = y, group = group)) +
      geom_density(alpha = alpha) +
      geom_rug(aes_string(col = color, fill = color), alpha = 0.1)
  }
  return(plot)
}

#' Plot ridge plot
#'
#' @param data
#' @param x
#' @param y
#' @param color
#' @param group
#' @param alpha
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
plot_ridge = function(data,
                      x = NULL,
                      y = NULL,
                      color = NULL,
                      group = NULL,
                      alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = y,
    y = x,
    col = x,
    fill = x
  )) +
    ggridges::geom_density_ridges(alpha = alpha) +
    geom_rug(alpha = 0.1, aes_string(color = color))
  return(plot)
}
