#' Plot overview plot, showing distributions of all columns in a data frame,
#'
#' @param data
#' @param columns
#' @param facet_rows
#' @param text_size
#'
#' @importFrom purrr map
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#'
#' @return
#' @export
#'
#' @examples
plot_data_overview = function(data, columns, facet_rows, text_size) {

  plots = map(columns, ~ .plot_overview_distribution(data, .x, facet_rows, text_size))

  for (i in 1:(length(plots) - 1)) {

    plots[[i]] = plots[[i]] +
      theme(strip.background = element_blank(),
            strip.text.y = element_blank())

  }

  plots = wrap_plots(plots, ncol = length(columns))

  return(plots)

}

.plot_overview_distribution = function(data, column, facet_rows, text_size) {

  if (length(unique(data[, column, drop = TRUE])) > 5 && is.numeric(data[, column, drop = TRUE])) {

    plot = .plot_overview_continuous_distribution(data, column, facet_rows, text_size)

  } else {

    plot = .plot_overview_discrete_distribution(data, column, facet_rows, text_size)

  }

  return(plot)

}

#' Function to plot distribution of continuous variable
#'
#' @import ggplot2
#' @importFrom dplyr filter group_by tally
.plot_overview_continuous_distribution = function(data, column, facet_rows, text_size = 3) {

  count = data %>%
    filter(!is.na(!!as.name(column))) %>%
    group_by(.dots = facet_rows) %>%
    tally()

  plot = ggplot(data = data, aes_string(x = column)) +
    geom_density() +
    theme_ggexp() +
    labs(y = NULL, x = column) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_text(data = count, aes(x = Inf, y = Inf, label = n), size = text_size, hjust = 1, vjust = 1) +
    geom_rug(alpha = 0.03, length = unit(1, "npc"))

  plot = plot_facets(plot, facet_rows = facet_rows, facet_type = "grid")

  return(plot)

}

#' Function to plot distribution of discrete variable
#'
#' @import ggplot2
#' @importFrom dplyr filter group_by tally ungroup mutate
.plot_overview_discrete_distribution = function(data, column, facet_rows, text_size = 3) {

  order = names(sort(table(data[, column, drop = FALSE], useNA = "ifany"), decreasing = TRUE))

  if ("NA" %in% order) {
    order = c(setdiff(order, "NA"), "NA")
  }

  data = data %>%
    group_by(.dots = c(column, facet_rows)) %>%
    tally() %>%
    ungroup() %>%
    group_by(.dots = facet_rows) %>%
    mutate(freq = n/sum(n)) %>%
    mutate(y = freq + 0.1 * text_size / 3)

  data[, column] = factor(data[, column, drop = TRUE], levels = order)

  plot = ggplot(data = data, aes_string(x = column, y = "freq")) +
    geom_col() +
    theme_ggexp() +
    labs(y = NULL, x = column) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_text(aes(y = y, label = n), size = text_size)

  plot = plot_facets(plot, facet_rows = facet_rows, facet_type = "grid")

  return(plot)

}
