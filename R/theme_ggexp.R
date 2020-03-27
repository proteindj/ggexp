#' Get ggexp default theme
#'
#' @importFrom ggplot2 theme_classic theme element_blank
#'
#' @return ggplot theme
#' @export
theme_ggexp = function() {
  theme_classic() +
    theme(strip.background = element_blank(), strip.placement = "outside")
}
