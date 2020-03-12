#' Plot distributions with pairwise annotations and flexibility
#'
#' @param data data frame containing dataset to use for plotting
#' @param pairwise_annotation data frame containing pairwise annotations
#' @param type type of plot - can be "line", "sina", "quasirandom", "density", "violin", "box", or "ridge"
#' @param x column for x-axis
#' @param y column for y-axis
#' @param group column for group aesthethic, used if type == "line"
#' @param color column for color
#' @param alpha alpha of points
#' @param scale either "default" for linearly-spaced scale or "log" for log-spaced
#' @param tier_width relative distance between tiers for pairwise annotations, between 0 and 1
#' @param annotate_counts boolean whether to annotate counts per group or not
#' @param pairwise_annotation_label column of pairwise_annotation data to use for annotation text
#' @param pairwise_annotation_exclude values to not annotate on pairwise annotations
#' @param lower_quantile lower quantile beyond which to limit axis
#' @param upper_quantile upper quantile beyond which to limit axis
#' @param facet_rows columns for faceting by row
#' @param facet_columns columns for faceting by column
#' @param facet_type either "wrap" or "grid", corresponding to facet_wrap and facet_grid respectively
#' @param ... params passed into either facet_wrap or facet_grid, depending on facet_type parameter
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' NULL
plot_distributions = function(data,
                              pairwise_annotation = NULL,
                              type = "quasirandom",
                              x,
                              y,
                              group = NULL,
                              color = NULL,
                              fill = NULL,
                              alpha = 0.5,
                              text_size = 2,
                              scale = "default",
                              tier_width = 0.16,
                              annotate_counts = TRUE,
                              pairwise_annotation_label = "p_signif",
                              pairwise_annotation_exclude = c(),
                              lower_quantile = 0,
                              upper_quantile = 1,
                              drop_outliers = FALSE,
                              facet_rows = c(),
                              facet_columns = c(),
                              facet_type = "grid",
                              ...) {

  data =.fix_outliers(data, lower_quantile, upper_quantile, drop_outliers, c(facet_rows, facet_columns))

  plot = get(paste0(".plot_", type))(data,
                                     x,
                                     y,
                                     color,
                                     fill,
                                     group,
                                     alpha)

  plot = .plot_scale(plot, scale, type)

  plot = plot_facets(plot,
                     facet_rows,
                     facet_columns,
                     facet_type,
                     ...)

  if (annotate_counts) {
    counts_annotation = .compute_counts_annotation_data(data, x, c(facet_rows, facet_columns))
    plot = .plot_counts_annotation(plot, x, counts_annotation, annotate_counts, type, text_size)
  }

  if (!is.null(pairwise_annotation) &
      (pairwise_annotation_label %in% colnames(pairwise_annotation)) &
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

#' Remove or mask outliers based on quantiles in each group
#'
#' @param data data frame containing dataset to use for plotting
#' @param lower_quantile lower quantile beyond which to limit axis
#' @param upper_quantile upper quantile beyond which to limit axis
#' @param drop_outliers whether to drop outliers or not - if FALSE, then outliers are masked to the lower or upper quantile values
#' @param groups columns which to group on in computing outliers
#'
#' @importFrom dplyr group_by mutate filter
#'
#' @return
#'
#' @examples
#' NULL
.fix_outliers = function(data, lower_quantile, upper_quantile, drop_outliers, groups) {

  data = data %>%
    group_by(.dots = groups) %>%
    mutate(upper_quantile = quantile(value, upper_quantile),
           lower_quantile = quantile(value, lower_quantile)) %>%
    mutate(upper_outlier = value > upper_quantile,
           lower_outlier = value < lower_quantile) %>%
    mutate(outlier = lower_outlier | upper_outlier)

  if (drop_outliers) {
    data = data %>%
      filter(!outlier)
  } else {
    data = data %>%
      mutate(value = ifelse(lower_outlier, lower_quantile, value)) %>%
      mutate(value = ifelse(upper_outlier, upper_quantile, value))
  }

  return(data)
}


#' Compute number of values per group for count annotation
#'
#' @param data data frame from which to count observations
#' @param x column for x-axis
#' @param groups groups to facet by
#'
#' @importFrom stats na.omit
#' @importFrom dplyr group_by tally
#'
#' @return
#'
#' @examples
#' NULL
.compute_counts_annotation_data = function(data, x, groups) {
  counts = data[, unique(c(x, groups)), drop = FALSE] %>%
    na.omit()
  counts = counts %>%
    group_by(.dots = unique(c(x, groups))) %>%
    tally()
  return(counts)
}

#' Annotate number of values per group on plot
#'
#' @param plot plot with discrete x-axis to annotate counts on
#' @param x column for x-axis
#' @param counts_annotation annotation returned from compute_counts_annotation_data
#' @param annotate_counts whether to annotate counts or not
#' @param type type of plot, same as plot_distributions type
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
#' NULL
.plot_counts_annotation = function(plot,
                                   x,
                                   counts_annotation,
                                   annotate_counts,
                                   type,
                                   text_size) {
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
        size = text_size,
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

#' Transform scale based on scale type and plot type
#'
#' @param plot ggplot object
#' @param scale either "log" or "default"
#' @param type plot type, same as plot_distributions type
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
#' NULL
.plot_scale = function(plot, scale, type) {
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
#' @param data data frame for plotting
#' @param x column for x-axis
#' @param y column for y-axis
#' @param color column to color points by
#' @param fill column to fill points by
#' @param group column to group points by - line connects by this variable
#' @param alpha alpha for each point
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
#' NULL
.plot_line = function(data,
                      x = NULL,
                      y = NULL,
                      color = NULL,
                      fill = NULL,
                      group = NULL,
                      alpha = 0.5) {
  plot = ggplot(data) +
    geom_line(alpha = alpha,
              aes_string(
                x = x,
                y = y,
                group = group,
                color = color
              )) +
    geom_point(alpha = alpha,
               aes_string(x = x, y = y, col = color),
               shape = 1)
  return(plot)
}

#' Plot sina plot
#'
#' @param data Data frame for plotting
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column to color points by
#' @param group Column to group points by - not relevant for this function
#' @param alpha Alpha for each point
#'
#' @import ggplot2
#' @importFrom ggforce geom_sina
#'
#' @return
#'
#' @examples
#' NULL
.plot_sina = function(data,
                      x = NULL,
                      y = NULL,
                      color = NULL,
                      fill = NULL,
                      group = NULL,
                      alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = x,
    y = y,
    col = color,
    group = group
  )) +
    geom_sina(alpha = alpha,
              shape = 1,
              position = position_dodge(width = 0)) +
    geom_boxplot(
      alpha = 0,
      width = 0.3,
      color = "firebrick",
      outlier.size = 0
    )

  return(plot)
}

#' Plot quasirandom plot
#'
#' @param data Data frame for plotting
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column to color points by
#' @param group Column to group points by - not relevant for this function
#' @param alpha Alpha for each point
#'
#' @import ggplot2
#' @importFrom  ggbeeswarm geom_quasirandom
#'
#' @return
#'
#' @examples
#' NULL
.plot_quasirandom = function(data,
                             x = NULL,
                             y = NULL,
                             color = NULL,
                             fill = NULL,
                             group = NULL,
                             alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = x,
    y = y,
    col = color,
    group = group
  )) +
    geom_quasirandom(
      method = "tukeyDense",
      alpha = alpha,
      shape = 1,
      position = position_dodge(width = 1)
    ) +
    geom_boxplot(
      alpha = 0,
      width = 0.3,
      color = "firebrick",
      outlier.size = 0,
    )
  return(plot)
}

#' Plot violin plot
#'
#' @param data Data frame for plotting
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column to color points by
#' @param group Column to group points by - not relevant for this function
#' @param alpha Alpha for each point
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
#' NULL
.plot_violin = function(data,
                        x = NULL,
                        y = NULL,
                        color = NULL,
                        fill = NULL,
                        group = NULL,
                        alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = x,
    y = y,
    col = color,
    group = group
  )) +
    geom_violin(alpha = alpha) +
    geom_boxplot(
      alpha = 0,
      width = 0.3,
      outlier.size = 0,
      color = "firebrick"
    )
  return(plot)
}

#' Plot box and whiskers plot
#'
#' @param data Data frame for plotting
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column to color points by
#' @param group Column to group points by - not relevant for this function
#' @param alpha Alpha for each point
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
#' NULL
.plot_box = function(data,
                     x = NULL,
                     y = NULL,
                     color = NULL,
                     fill = NULL,
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
#' @param data Data frame for plotting
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column to color points by
#' @param group Column to group points by - not relevant for this function
#' @param alpha Alpha for each point
#'
#' @import ggplot2
#'
#' @return
#'
#' @examples
#' NULL
.plot_density = function(data,
                         x = NULL,
                         y = NULL,
                         color = NULL,
                         fill = NULL,
                         group = NULL,
                         alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = y,
    col = color,
    fill = fill,
    group = group
  )) +
    geom_density(alpha = alpha) +
    geom_rug(alpha = 0.5)
  return(plot)
}

#' Plot ridge plot
#'
#' @param data Data frame for plotting
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column to color points by
#' @param group Column to group points by - not relevant for this function
#' @param alpha Alpha for each point
#'
#' @import ggplot2
#' @importFrom ggridges geom_density_ridges
#'
#' @return
#'
#' @examples
#' NULL
.plot_ridge = function(data,
                       x = NULL,
                       y = NULL,
                       color = NULL,
                       fill = NULL,
                       group = NULL,
                       alpha = 0.5) {
  plot = ggplot(data, aes_string(
    x = y,
    y = x,
    col = color,
    fill = fill
  )) +
    geom_density_ridges(alpha = alpha) +
    geom_rug(alpha = 0.5, aes_string(color = color))
  return(plot)
}
