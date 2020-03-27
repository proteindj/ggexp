#' Plot heatmap using ComplexHeatmap using simplified interface
#'
#' @param matrix matrix to plot
#' @param row_annotations data frame with annotations for the rows of the matrix
#' @param column_annotations data frame with annotations for the columns of the matrix
#' @param split_rows columns of row_annotations to split heatmap rows by
#' @param split_columns columns of column_annottations to  split heatmap columns by
#' @param palette named list of named character vectors with the values being the colors if discrete, or in the form c(colors = XXX, breaks = XXX) if  continuous
#' @param cluster_rows boolean to cluster rows or not
#' @param cluster_columns boolean to cluster columns or not
#' @param show_row_dend boolean to show row dendrogram or not
#' @param show_column_dend boolean to show column dendrogram or not
#' @param show_row_names boolean to show row names or not
#' @param show_column_names boolean to show column names or  not
#' @param show_legend_row columns of row_annotations to show legend for
#' @param show_legend_column columns of column_annotations to show legend for
#' @param legend_position either "right" or "bottom"
#' @param lower_quantile lower quantile of data for colorbar
#' @param upper_quantile upper quantile of data for colorbar
#' @param text_size text size
#' @param row_names_size text size for row names
#' @param column_names_size text size for column names
#' @param value_name name of value, written above color bar
#' @param title title of plot
#'
#' @importFrom ComplexHeatmap Heatmap draw
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar
#'
#' @return
#' @export
plot_heatmap = function(matrix,
                        row_annotations = NULL,
                        column_annotations = NULL,
                        split_rows = NULL,
                        split_columns = NULL,
                        palette = list(),
                        cluster_rows = TRUE,
                        cluster_columns = TRUE,
                        show_row_dend = TRUE,
                        show_column_dend = TRUE,
                        show_row_names = TRUE,
                        show_column_names = TRUE,
                        show_legend_row = colnames(row_annotations),
                        show_legend_column = colnames(column_annotations),
                        legend_position = "side",
                        lower_quantile = 0.01,
                        upper_quantile = 0.99,
                        text_size = 10,
                        row_names_size = 4,
                        column_names_size = 4,
                        value_name = "value",
                        title = character(0),
                        colors = c("royalblue4", "white", "firebrick4"),
                        color_break_functions = list(
                          function(matrix)
                          quantile(matrix, lower_quantile, na.rm = TRUE),
                          function(matrix)
                            quantile(matrix, 0.5, na.rm = TRUE),
                          function(matrix)
                            quantile(matrix, upper_quantile, na.rm = TRUE)
                          )
                        ) {

  row_anno = .create_annotation(row_annotations,
                                "row",
                                show_legend_row,
                                text_size,
                                palette)

  col_anno = .create_annotation(column_annotations,
                                "column",
                                show_legend_column,
                                text_size,
                                palette)

  heatmap_color = colorRamp2(sapply(color_break_functions, function(fn) fn(matrix)), colors)

  heatmap = Heatmap(
    matrix,
    col = heatmap_color,
    row_split = .create_split(split_rows),
    column_split = .create_split(split_columns),
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    show_row_dend = show_row_dend,
    show_column_dend = show_column_dend,
    show_row_names = show_row_names,
    show_column_names = show_column_names,
    row_names_side = "left",
    column_names_side = "top",
    top_annotation = col_anno,
    left_annotation = row_anno,
    column_names_gp = gpar(fontsize = column_names_size),
    row_names_gp = gpar(fontsize = row_names_size),
    row_title_gp = gpar(fontsize = text_size),
    column_title_gp = gpar(fontsize = text_size),
    column_title = title,
    row_dend_gp = gpar(lwd = 0.25),
    column_dend_gp = gpar(lwd = 0.25),
    heatmap_legend_param = list(
      title = value_name,
      title_gp = gpar(fontsize = text_size, font_face = "plain"),
      labels_gp = gpar(fontsize = 0.9 * text_size)
    )
  )

  if (legend_position == "bottom") {
    heatmap = draw(heatmap,
                   heatmap_legend_side = "bottom",
                   merge_legends = TRUE)
  } else {
    heatmap = draw(heatmap, merge_legends = TRUE)
  }

  return(heatmap)
}

#' Create annotation for rows/columns
#'
#' @param data data frame with annotations
#' @param which either "row" or "column" for row annotations and column annotations respectively
#' @param show_legend columns of data to show legend for
#' @param text_size text size
#' @param palette color palette
#'
#' @importFrom ComplexHeatmap HeatmapAnnotation
#' @importFrom grid gpar
#'
#' @return
#'
#' @examples
#' NULL
.create_annotation = function(data,
                              which,
                              show_legend,
                              text_size,
                              palette) {
  if (length(data) == 0) {
    result = NULL
  } else {
    result = HeatmapAnnotation(
      df = data,
      col = .parse_palette_CH(data, palette),
      show_annotation_name = TRUE,
      annotation_name_side = ifelse(which == "row", "bottom", "left"),
      which = which,
      show_legend = colnames(data) %in% show_legend,
      annotation_name_gp = gpar(fontsize = text_size),
      annotation_legend_param = list(
        title_gp = gpar(fontsize = text_size, font_face = "plain"),
        labels_gp = gpar(fontsize = 0.9 * text_size)
      )
    )
  }
  return(result)
}

.create_split = function(data) {
  if (length(data) == 0) {
    result = NULL
  } else {
    result = data
  }
  return(result)
}

.parse_palette_CH = function(annotations, palette) {
  for (column in colnames(annotations)) {
    column_values = annotations[, column, drop = TRUE]
    if (is.null(palette[[column]])) {
      # no palette specified
      if (!is.numeric(column_values) |
          all(column_values %in% c(0, 1))) {
        # discrete values or one-hot encoded
        palette[[column]] = .parse_discrete_palette_CH(column_values, palette[[column]])
      } else {
        # numeric values
        palette[[column]] = .parse_continuous_palette_CH(column_values, palette[[column]])
      }
    } else {
      # palette specified
      if ("colors" %in% names(palette[[column]])) {
        # continuous palette specified
        palette[[column]] = .parse_continuous_palette_CH(column_values, palette[[column]])
      } else {
        # discrete palette specified
        palette[[column]] = .parse_discrete_palette_CH(column_values, palette[[column]])
      }
    }
  }
  return(palette[colnames(annotations)])
}

.parse_discrete_palette_CH = function(values, palette) {
  palette = as.list(.parse_discrete_palette(values, palette))
  if (all(values %in% c(0, 1))) {
    palette$`0` = "white"
    palette$`1` = "black"
  }
  palette = unlist(palette)
  palette = palette[!is.na(names(palette))]
  return(palette)
}

.parse_continuous_palette_CH = function(values, palette) {
  palette = .parse_continuous_palette(values, palette)
  palette = circlize::colorRamp2(breaks = palette$breaks, colors = palette$colors)
  return(palette)
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
