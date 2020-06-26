#' Plot pairwise annotation
#'
#' @param plot ggplot object with discrete x-axis
#' @param pairwise_annotation data frame from .prepare_pairwise_annotation
#' @param label string scalar indicating column for text annotation
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
plot_pairwise_annotation = function(plot,
                                    pairwise_annotation,
                                    label = "p_signif",
                                    values_to_exclude = c(),
                                    tier_width = 0.15,
                                    scale = "default") {
  x = plot$labels$x
  y = plot$labels$y

  if (is(plot$facet, "FacetGrid")) {
    groups = c(names(plot$facet$params$rows),
               names(plot$facet$params$cols))
  } else {
    groups = names(plot$facet$params$facets)
  }

  pairwise_annotation = .prepare_pairwise_annotation(
    plot$data,
    pairwise_annotation,
    x,
    y,
    label,
    values_to_exclude,
    groups = groups,
    tier_width = tier_width,
    scale = scale
  )

  if (nrow(pairwise_annotation) > 0) {
    plot = plot +
      geom_segment(
        data = pairwise_annotation,
        aes(
          x = group1_num + 0.1,
          xend = group2_num - 0.1,
          y = y.position,
          yend = y.position
        ),
        inherit.aes = FALSE
      ) +
      geom_text(
        data = pairwise_annotation,
        aes(
          x = (group1_num + group2_num) / 2,
          y = y.position,
          label = !!as.name(label)
        ),
        vjust = -.05,
        inherit.aes = FALSE
      )
  }
  return(plot)
}

#' Prepare pairwise annotation data frame for plotting by assigning tiers and y positions
#'
#' @param data data frame containing dataset on which the annotations will be plotted
#' @param pairwise_annotation data frame containing pairwise annotations (for example, pairwise statistical tests)
#' @param y column for y-axis
#' @param label column for text annotation
#' @param values_to_exclude values to not annotate (for example, "ns" for statistical tests)
#' @param groups columns to group by before assigning optimal y positions
#' @param tier_width relative distance between tiers for pairwise annotations
#' @param scale either "default" for linearly-spaced scale between y tier positions or "log" for log-spaced
#'
#' @importFrom dplyr left_join bind_cols
#' @importFrom tidyr nest unnest
#' @importFrom purrr map2
#'
#' @return
.prepare_pairwise_annotation = function(data,
                                        pairwise_annotation,
                                        x,
                                        y,
                                        label,
                                        values_to_exclude = c(),
                                        groups = c(),
                                        tier_width = 0.15,
                                        scale = "default") {
  data = nest(data, data = setdiff(colnames(data), groups))
  pairwise_annotation = nest(pairwise_annotation,
                             pairwise_annotation = setdiff(colnames(pairwise_annotation), groups))

  if (length(groups) > 0) {
    combined = left_join(data, pairwise_annotation, by = groups)
  } else {
    combined = bind_cols(data, pairwise_annotation)
  }

  processed_annotation = map2(
    combined$data,
    combined$pairwise_annotation,
    ~ .prepare_pairwise_annotation_single_group(.x,
                                                .y,
                                                x,
                                                y,
                                                label,
                                                values_to_exclude,
                                                tier_width,
                                                scale)
  )

  combined$processed_annotation = processed_annotation

  combined = combined[, c(groups, "processed_annotation"), drop = FALSE]
  combined = unnest(combined, cols = "processed_annotation")

  return(combined)
}

#' Prepare pairwise annotation data frame for plotting by assigning tiers and y positions for a single group
#'
#' @param data data frame containing dataset on which the annotations will be plotted
#' @param pairwise_annotation data frame containing pairwise annotations (for example, pairwise statistical tests)
#' @param y column for y-axis
#' @param label column for text annotation
#' @param values_to_exclude values to not annotate (for example, "ns" for statistical tests)
#' @param groups columns to group by before assigning optimal y positions
#' @param tier_width relative distance between tiers for pairwise annotations
#' @param scale either "default" for linearly-spaced scale between y tier positions or "log" for log-spaced
#'
#' @return
.prepare_pairwise_annotation_single_group = function(data,
                                                     pairwise_annotation,
                                                     x,
                                                     y,
                                                     label,
                                                     values_to_exclude,
                                                     tier_width,
                                                     scale) {
  pairwise_annotation = .add_group_numbers(pairwise_annotation, data, x)
  pairwise_annotation = pairwise_annotation[!(pairwise_annotation[, label, drop = TRUE] %in% values_to_exclude), ]
  pairwise_annotation = .assign_tiers(pairwise_annotation)
  tier_mapping = .map_tiers(data, pairwise_annotation, y, tier_width, scale)
  pairwise_annotation = .add_tier_mapping(pairwise_annotation, tier_mapping)
  return(pairwise_annotation)
}

#' Add group numbers to a pairwise_annotation with pairwise comparisons
#'
#' Adds columns group1_num, group2_num, and rank
#'
#' @param pairwise_annotation data frame with pairwise annotation to be plotted, must have columns group1 and group2
#'
#' @importFrom dplyr mutate
#'
#' @return
.add_group_numbers = function(pairwise_annotation, data, x) {
  groups = factor(data[, x, drop = TRUE])

  mapping = data.frame(char = as.character(groups), as.numeric(groups))

  pairwise_annotation = pairwise_annotation %>%
    mutate(group1_num = mapping[match(.$group1, mapping$char), 2, drop = F][[1]]) %>%
    mutate(group2_num = mapping[match(.$group2, mapping$char), 2, drop = F][[1]])

  pairwise_annotation[, c("group1_num", "group2_num")] = t(apply(pairwise_annotation[, c("group1_num", "group2_num")], 1, sort))

  return(pairwise_annotation)
}

#' Assign tiers based on optimal plotting location to prevent one tier per comparison
#'
#' @param pairwise_annotation data frame with pairwise annotation to be plotted, must have columns group1 and group2
#'
#' @importFrom dplyr arrange mutate
#'
#' @return
.assign_tiers = function(pairwise_annotation) {
  if (nrow(pairwise_annotation) > 0) {
    pairwise_annotation = pairwise_annotation %>%
      arrange(group2_num) %>%
      mutate(rank = seq(1, nrow(.), by = 1))
    assigned = pairwise_annotation[c(), , drop = FALSE]
    unassigned = pairwise_annotation
    tier = 1
    while (nrow(unassigned) > 0) {
      result = .assign_unassigned_tiers(unassigned)
      assigned = rbind(assigned, result$assigned %>% dplyr::mutate(tier = tier))
      unassigned = result$unassigned
      tier = tier + 1
    }
    pairwise_annotation = assigned
  }
  return(pairwise_annotation)
}

#' Assigned tiers that have not been assigned yet
#'
#' @param pairwise_annotation data frame with pairwise annotation to be plotted, must have columns group1_num and group2_num
#'
#' @return
.assign_unassigned_tiers = function(pairwise_annotation) {
  assigned = pairwise_annotation[c(1), , drop = FALSE]
  unassigned = pairwise_annotation[c(), , drop = FALSE]
  right = pairwise_annotation[1, "group2_num"]
  if (nrow(pairwise_annotation) > 1) {
    for (i in 2:nrow(pairwise_annotation)) {
      row = pairwise_annotation[i, , drop = FALSE]
      if (pairwise_annotation[i, "group1_num"] >= right) {
        right = pairwise_annotation[i, "group2_num"]
        assigned = rbind(assigned, row)
      } else {
        unassigned = rbind(unassigned, row)
      }
    }
  }
  return(list(assigned = assigned, unassigned = unassigned))
}

#' Map tiers to y positions based on the data
#'
#' @param data data frame containing dataset on which the annotations will be plotted
#' @param pairwise_annotation result from .prepare_pairwise_annotations
#' @param y column for y-axis
#' @param tier_width relative distance between tiers for pairwise annotations
#' @param scale either "default" for linearly-spaced scale between y tier positions or "log" for log-spaced
#'
#' @return
.map_tiers = function(data,
                      pairwise_annotation,
                      y,
                      tier_width,
                      scale) {
  if (nrow(pairwise_annotation) > 0) {
    tier_mapping = data.frame(tier = min(pairwise_annotation$tier):max(pairwise_annotation$tier))

    by = (max(data[, y]) - min(data[, y])) * tier_width

    if (scale == "log") {
      seq_function = .seq_log
    } else {
      seq_function = seq
    }

    tier_mapping$y.position = seq_function(
      from = max(data[, y]) + by / 2,
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
#' @param pairwise_annotation data frame from .prepare_pairwise_annotation
#' @param tier_mapping result from .map_tiers
#'
#' @return
.add_tier_mapping = function(pairwise_annotation, tier_mapping) {
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
#' @param from value to start from
#' @param by distance between points
#' @param length.out desired vector length
#'
#' @return
.seq_log = function(from, by, length.out) {
  ys = c(from)
  while (length(ys) < length.out) {
    ys = c(ys, max(ys) * 2 ^ by)
  }
  return(ys)
}
