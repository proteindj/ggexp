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
