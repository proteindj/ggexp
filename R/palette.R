#' Generate ggexp palette with assigned color values based on alphabetized input values and overriden with input palette
#'
#' @param values
#' @param palette
#' @param return_ggplot_scale
#'
#' @return
#' @export
#'
#' @examples
get_palette = function(values = NULL, palette = c(), return_ggplot_scale = TRUE) {
  palette_values =
    c(
      ggsci::pal_npg()(10),
      ggsci::pal_jama()(7),
      ggsci::pal_d3()(10),
      ggsci::pal_lancet()(9)
    )
  if (is.null(values)) {
    palette_custom = ggplot2::scale_color_manual(
      values = palette_values,
      aesthetics = c("colour", "fill"),
      drop = TRUE
    )
  } else if (!is.numeric(values)) {
    levels = unique(values[!values == "NA"])
    palette_custom = palette_values[1:length(levels)]
    names(palette_custom) = sort(levels)
    if (any(is.na(values)) ||
        any(values == "NA"))
      palette_custom = c(palette_custom, "NA" = "gray70")
    palette_custom = c(palette, palette_custom)[unique(names(c(palette, palette_custom)))]

    if (return_ggplot_scale) {
      palette_custom = ggplot2::scale_color_manual(
        values = palette_custom,
        aesthetics = c("colour", "fill"),
        drop = TRUE
      )
    }
  } else {
    palette_custom = list(
      high = "#67001F",
      mid = "#F7F7F7",
      low = "#053061",
      midpoint = 0,
      limits = c(min(values), max(values)),
      aesthetics = c("colour", "fill")
    )
    palette_custom = c(palette, palette_custom)[unique(names(c(palette, palette_custom)))]

    if (return_ggplot_scale) palette_custom = do.call(ggplot2::scale_color_gradient2, palette_custom)
  }

  return(palette_custom)
}

#' Generate palette for ComplexHeatmap
#'
#' Takes in data frame of annotations and desired colors for each value in each column, and fills in missing values and returns palette in format required by ComplexHeatmap.
#'
#' @param annotations data frame of annotations
#' @param palette list of character vectors, each element in list with the same name as columns in data frame, and each character vector containing the value as the name and color as the value.
#'
#' @return
#' @export
#'
#' @examples
#' NULL
generate_palette_complex_heatmap = function(annotations, palette) {
  for (column in colnames(annotations)) {
    column_values = annotations[, column, drop = TRUE]
    if (is.null(palette[[column]])) {
      # no palette specified
      if (!is.numeric(column_values) |
          all(column_values %in% c(0, 1))) {
        # discrete values or one-hot encoded
        palette[[column]] = .parse_discrete_palette_complex_heatmap(column_values, palette[[column]])
      } else {
        # numeric values
        palette[[column]] = .parse_continuous_palette_complex_heatmap(column_values, palette[[column]])
      }
    } else {
      # palette specified
      if ("colors" %in% names(palette[[column]])) {
        # continuous palette specified
        palette[[column]] = .parse_continuous_palette_complex_heatmap(column_values, palette[[column]])
      } else {
        # discrete palette specified
        palette[[column]] = .parse_discrete_palette_complex_heatmap(column_values, palette[[column]])
      }
    }
  }
  return(palette[colnames(annotations)])
}

#' Generate palette for ggplot2
#'
#' Takes in vector of values and desired colors for each value, and fills in missing values with default colors.
#'
#' @param annotations vector of values
#' @param palette character vector containing the value as the name and color as the value.
#'
#' @return
#' @export
#'
#' @examples
#' NULL
generate_palette_ggplot = function(values, palette) {
  if (is.null(palette) || length(palette) == 0) { # no palette specified
    if (!is.numeric(values)) { # discrete values or one-hot encoded
      palette = .parse_discrete_palette_ggplot(values, palette)
    } else { # numeric values
      palette = .parse_continuous_palette_ggplot(values, palette)
    }
  } else { # palette specified
    if ("colors" %in% names(palette)) { # continuous palette specified
      palette = .parse_continuous_palette_ggplot(values, palette)
    } else { # discrete palette specified
      palette = .parse_discrete_palette_ggplot(values, palette)
    }
  }
}

.parse_continuous_palette = function(values, palette) {
  if (is.null(palette))
    palette = list() # in case no palette specified
  if ("colors" %in% names(palette)) {
    # colors specified
    if ("breaks" %in% names(palette)) {
      # both colors and breaks specified
      if (length(palette$colors) != length(palette$breaks)) {
        # colors and breaks do not match in length
        palette$breaks = seq(
          from = min(values),
          to = max(values),
          length.out = length(palette$colors)
        )
      }
    } else {
      # only colors specified, breaks inferred
      palette$breaks = seq(
        from = min(values, na.rm = TRUE),
        to = max(values, na.rm = TRUE),
        length.out = length(palette$colors)
      )
    }
  } else {
    # no palette specified
    palette$colors = c("white", "firebrick")
    palette$breaks = c(min(values, na.rm = TRUE), max(values, na.rm = TRUE))
  }
  return(palette)
}

.parse_discrete_palette = function(values, palette) {
  if (is.null(palette))
    palette = list()
  complete_palette = .generate_default_discrete_palette(values)
  for (value in unique(values)) {
    if (value %in% names(palette)) {
      complete_palette[[as.character(value)]] = palette[[as.character(value)]]
    }
  }
  return(unlist(complete_palette))
}

.generate_default_discrete_palette = function(values) {
  levels = unique(values[!values == "NA"])
  palette = c(ggsci::pal_aaas()(10), ggsci::pal_igv()(50))
  palette_custom = palette[1:length(levels)]
  names(palette_custom) = sort(levels)
  palette_custom = as.list(c(palette_custom, "NA" = "gray70"))
  return(palette_custom)
}

.parse_discrete_palette_complex_heatmap = function(values, palette) {
  palette = as.list(.parse_discrete_palette(values, palette))
  if (all(values %in% c(0, 1))) {
    palette$`0` = "white"
    palette$`1` = "black"
  }
  palette = unlist(palette)
  palette = palette[!is.na(names(palette))]
  return(palette)
}

.parse_continuous_palette_complex_heatmap = function(values, palette) {
  palette = .parse_continuous_palette(values, palette)
  palette = circlize::colorRamp2(breaks = palette$breaks, colors = palette$colors)
  return(palette)
}

.parse_discrete_palette_ggplot = function(values, palette) {
  palette = .parse_discrete_palette(values, palette)
  return(ggplot2::scale_color_manual(values = palette, aesthetics = c("colour", "fill")))
}

.parse_continuous_palette_ggplot = function(values, palette) {
  palette = .parse_continuous_palette(values, palette)
  palette = ggplot2::scale_color_gradientn(colors = palette$colors, values = scales::rescale(palette$breaks), limits = c(min(values, na.rm = TRUE), max(values, na.rm = TRUE)))
  return(palette)
}


