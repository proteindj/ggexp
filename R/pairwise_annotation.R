#' Plot pairwise annotation
#'
#' @param plot ggplot object with discrete x-axis
#' @param pairwise_annotation data frame from prepare_pairwise_annotation
#' @param label string scalar indicating column for text annotation
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_pairwise_annotation = function(plot,
                                    pairwise_annotation,
                                    label) {
  plot +
    geom_segment(
      data = pairwise_annotation,
      aes(
        x = group1_num + 0.1,
        xend = group2_num - 0.1,
        y = y.position,
        yend = y.position
      )
    ) +
    geom_text(
      data = pairwise_annotation,
      aes(
        x = (group1_num + group2_num) / 2,
        y = y.position,
        label = !!as.name(label)
      ),
      vjust = -.05
    )
}

#' Prepare pairwise annotation data frame for plotting by assigning tiers and y positions
#'
#' @param data data frame containing dataset on which the annotations will be plotted
#' @param pairwise_annotation data frame containing pairwise annotations (for example, pairwise statistical tests)
#' @param y string scalar indicating column for y-axis
#' @param label string scalar indicating column for text annotation
#' @param values_to_exclude string vector indicating values to not annotate (for example, "ns" for statistical tests)
#' @param groups string vector of columns to group by before assigning optimal y positions
#' @param tier_width numeric scalar indicating relative distance between tiers for pairwise annotations
#' @param scale string scalar that is either "default" for linearly-spaced scale between y tier positions or "log" for log-spaced
#'
#' @return
#' @export
#'
#' @examples
prepare_pairwise_annotation = function(data,
                                       pairwise_annotation,
                                       x,
                                       y,
                                       label,
                                       values_to_exclude = c(),
                                       groups = c(),
                                       tier_width = 0.15,
                                       scale = "default") {
  data = tidyr::nest(data, data = -groups)
  pairwise_annotation = tidyr::nest(pairwise_annotation,
                                    pairwise_annotation = -groups)

  combined = dplyr::left_join(data, pairwise_annotation, by = groups)

  processed_annotation = purrr::map2(
    combined$data,
    combined$pairwise_annotation,
    ~ prepare_pairwise_annotation_single_group(
      .x,
      .y,
      x,
      y,
      label,
      values_to_exclude,
      tier_width,
      scale
    )
  )

  combined$processed_annotation = processed_annotation

  combined = combined[, c(groups, "processed_annotation"), drop = FALSE]
  combined = tidyr::unnest(combined)

  return(combined)
}

#' Prepare pairwise annotation data frame for plotting by assigning tiers and y positions for a single group
#'
#' @param data
#' @param pairwise_annotation
#' @param y
#' @param label
#' @param values_to_exclude
#' @param tier_width
#' @param scale
#'
#' @return
#'
#' @examples
prepare_pairwise_annotation_single_group = function(data,
                                                    pairwise_annotation,
                                                    x,
                                                    y,
                                                    label,
                                                    values_to_exclude,
                                                    tier_width,
                                                    scale) {
  pairwise_annotation = add_group_numbers(pairwise_annotation, data, x)
  pairwise_annotation = pairwise_annotation[!(pairwise_annotation[, label, drop = TRUE] %in% values_to_exclude),]
  pairwise_annotation = assign_tiers(pairwise_annotation)
  tier_mapping = map_tiers(data, pairwise_annotation, y, tier_width, scale)
  pairwise_annotation = add_tier_mapping(pairwise_annotation, tier_mapping)
  return(pairwise_annotation)
}

#' Add group numbers to a pairwise_annotation with pairwise comparisons
#'
#' Adds columns group1_num, group2_num, and rank
#'
#' @param pairwise_annotation data frame with pairwise annotation to be plotted, must have columns group1 and group2
#'
#' @return
#'
#' @examples
add_group_numbers = function(pairwise_annotation, data, x) {

  groups = factor(data[, x, drop = TRUE])

  mapping = data.frame(char = as.character(groups), as.numeric(groups))

  pairwise_annotation = pairwise_annotation %>%
    dplyr::mutate(group1_num = mapping[match(.$group1, mapping$char), 2, drop = F][[1]]) %>%
    dplyr::mutate(group2_num = mapping[match(.$group2, mapping$char), 2, drop = F][[1]])

  return(pairwise_annotation)
}

#' Assign tiers based on optimal plotting location to prevent one tier per comparison
#'
#' @param pairwise_annotation
#'
#' @return
#'
#' @examples
assign_tiers = function(pairwise_annotation) {
  if (nrow(pairwise_annotation) > 0) {
    pairwise_annotation = pairwise_annotation %>%
      dplyr::mutate(difference = abs(group2_num - group1_num)) %>%
      dplyr::arrange(difference, pmin(group1_num, group2_num)) %>%
      dplyr::mutate(rank = seq(1, nrow(.), by = 1))
    pairwise_annotation$tier = 1
    for (i in 1:nrow(pairwise_annotation)) {
      K = as.numeric(pairwise_annotation[i, "rank"])
      G1 = min(as.numeric(pairwise_annotation[i, "group1_num"]), as.numeric(pairwise_annotation[i, "group2_num"]))
      G2 = max(as.numeric(pairwise_annotation[i, "group1_num"]), as.numeric(pairwise_annotation[i, "group2_num"]))
      options = c()
      for (j in 1:nrow(pairwise_annotation)) {
        k = as.numeric(pairwise_annotation[j, "rank"])
        g1 = min(as.numeric(pairwise_annotation[j, "group1_num"]), as.numeric(pairwise_annotation[j, "group2_num"]))
        g2 = max(as.numeric(pairwise_annotation[j, "group1_num"]), as.numeric(pairwise_annotation[j, "group2_num"]))
        t = as.numeric(pairwise_annotation[j, "tier"])
        if (K > k & ((G1 < g1 & g1 < G2) | (G1 < g2 & g2 < G2))) {
          opt = t + 1
        } else {
          opt = 0
        }
        options[j] = opt
      }
      tier = max(options)
      pairwise_annotation[i, "tier"] = tier
    }
  }
  return(pairwise_annotation)
}

#' Map tiers to y positions based on the data
#'
#' @param data
#' @param pairwise_annotation
#' @param y
#' @param tier_width
#' @param scale
#'
#' @return
#'
#' @examples
map_tiers = function(data,
                     pairwise_annotation,
                     y,
                     tier_width,
                     scale) {
  if (nrow(pairwise_annotation) > 0) {
    tier_mapping = data.frame(tier = min(pairwise_annotation$tier):max(pairwise_annotation$tier))

    by = (max(data[, y]) - min(data[, y])) * tier_width

    if (scale == "log") {
      seq_function = seq_log
    } else {
      seq_function = seq
    }

    tier_mapping$y.position = seq_function(
      from = max(data[, y]) + by,
      by = by,
      length.out = (
        max(pairwise_annotation$tier) - min(pairwise_annotation$tier)
      ) + 1
    )
    return(tier_mapping)
  } else {
    return(data.frame())
  }
}

#' Add tier mapping to pairwise annotation data based on tier mapping
#'
#' @param pairwise_annotation
#' @param tier_mapping
#'
#' @return
#'
#' @examples
add_tier_mapping = function(pairwise_annotation, tier_mapping) {
  if (nrow(pairwise_annotation) > 0) {
    if (nrow(tier_mapping) > 1) {
      pairwise_annotation$y.position = tier_mapping[match(pairwise_annotation$tier, tier_mapping$tier), 2, drop = F][[1]]
    } else {
      pairwise_annotation$y.position = tier_mapping$y.position
    }
  }
  return(pairwise_annotation)
}

#' Sequence generation with equal distance in log space
#'
#' @param from
#' @param by
#' @param length.out
#'
#' @return
#'
#' @examples
seq_log = function(from, by, length.out) {
  ys = c(from)
  while (length(ys) < length.out) {
    ys = c(ys, max(ys) * 2 ^ by)
  }
  return(ys)
}
