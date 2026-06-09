packages = c("ggplot2", "dplyr", "see", "RColorBrewer", "ggridges")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

source("./metrics.R")
source("./parameter.R")


# Define color mapping
oi = palette_okabeito(palette = "full")(5)

method_colors = c(
  bf_ps = oi[1],
  hbi = oi[4],
  psis = oi[2],
  waic = oi[3]
#  random = oi[5]
)

method_labels = c(
  # bf_ps = "BF estimation with PS",
  bf_ps = "BFPS",
  psis = "PSIS-LOO",
  waic = "WAIC",
  hbi = "HBI"
# random = "Random"
)

######################################################################################################
########################################### CLASSIFICATION ###########################################
######################################################################################################

plot_metric = function(data_path = "./metrics/metrics_all.csv",
                        metric = "accuracy",
                        output_dir = "./figures/metrics",
                        width = 9,
                        height = 8) {
  # Load data
  df = read.csv(data_path)  %>% 
    filter(method != "random")

  # Check whether the metric exists
  if (!metric %in% c("accuracy", "precision", "recall", "f1")) {
    stop("Metric must be one of: accuracy, precision, recall, f1")
  }

  # Select relevant columns
  df_metric = df %>%
    select(n_items, n_participants, prevalence, method, seed, all_of(metric))
  colnames(df_metric)[6] = "value"

  df_metric$method = factor(df_metric$method, levels = names(method_colors))

  if (metric == "f1") metric = "f1-score"
  # Create plot
  p = ggplot(df_metric, aes(x = factor(n_participants), y = value, fill = method)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.3) +
    facet_grid(
      prevalence ~ n_items,
      # Rename the facet grid axes
      labeller = labeller(
        prevalence = c("equal" = "Equal Prevalence", "extreme" = "Extreme Prevalence"),
        n_items = c("60" = "180 Items","120" = "360 Items")
      )
    ) +
    # Adjust the range of y axis
    ylim(0, 1) +
    scale_fill_manual(
      values = method_colors,
      labels = method_labels
    ) +
    labs(
      x = "Number of participants",
      y = tools::toTitleCase(metric),
      fill = "Method",
      # title = paste(tools::toTitleCase(metric), "Across Conditions for Methods")
    ) +
    theme_bw(base_size = 14) +
    theme(
      strip.text      = element_text(face = "bold"),
      axis.text       = element_text(size = 12),
      axis.title      = element_text(size = 14),
      legend.text     = element_text(size = 10),
      legend.title    = element_text(size = 12),
      legend.position = "right"
    )

  # Create output directory, if doesn't exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save plot
  file_name = paste0("plot_", metric, ".png")
  ggsave(
    filename = file.path(output_dir, file_name),
    plot = p,
    width = width,
    height = height
  )
}


visualise_cm = function(all_labels_df,
                         cm_output_dir = "./figures/cm_plots",
                         inv_output_dir = "./figures/inv_plots",
                         width = 16,
                         height = 6,
                         by_condition = TRUE,
                         overall = TRUE) {
  
  # Create output directories (if doesn't exist yet)
  if (!dir.exists(cm_output_dir)) dir.create(cm_output_dir, recursive = TRUE)
  if (!dir.exists(inv_output_dir)) dir.create(inv_output_dir, recursive = TRUE)

  # From df with all labels, get the ones per method, calculate their cm and add relevant info about condition
  for (m in unique(all_labels_df$method)) {
    col = method_colors[[m]]
    full_label = method_labels[[m]]

    method_df = filter(all_labels_df, method == m)

    conditions = method_df %>%
      distinct(n_participants, n_items, prevalence) %>%
      arrange(n_participants, n_items, prevalence)

    cm_df_all = lapply(seq_len(nrow(conditions)), function(i) {
      n_part = conditions$n_participants[i]
      n_it = conditions$n_items[i]
      prev = conditions$prevalence[i]

      cond_df = filter(
        method_df,
        n_participants == n_part,
        n_items == n_it,
        prevalence == prev
      )

      cm = confusionMatrix(
        data = factor(cond_df$predicted, levels = 1:7),
        reference = factor(cond_df$true, levels = 1:7),
        mode = "prec_recall"
      )

      as.data.frame(cm$table) %>%
        group_by(Reference) %>%
        mutate(Proportion = Freq / sum(Freq)) %>%
        ungroup() %>%
        mutate(
          Prediction = factor(Prediction, levels = 1:7),
          Reference = factor(Reference, levels = 1:7),
          n_participants = n_part,
          n_items = n_it,
          prevalence = prev
        )
    })

    cm_df_all = bind_rows(cm_df_all)

    # Define shared labeller and theme for by condition plots for cm and inv
    cond_labeller = labeller(
      prevalence = c(
        "equal"   = "Equal Prevalence",
        "extreme" = "Extreme Prevalence"
      ),
      n_items = c(
        "60"  = "180 Items",
        "120" = "360 Items"
      ),
      n_participants = c(
        "150" = "N = 150",
        "300" = "N = 300"
      )
    )
 
    # Shared theme
    cond_theme = theme_bw(base_size = 14) +
      theme(
        strip.text      = element_text(face = "bold"),
        panel.grid      = element_blank(),
        axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 13),
        legend.title    = element_text(size = 14),
        legend.position = "right"
      )


    if (by_condition) {
      # First plot confusion matrices: p(fit|sim) 
      p_cm_cond = ggplot(cm_df_all, aes(x = Prediction, y = Reference, fill = Proportion)) +
        geom_tile(colour = "white", linewidth = 0.4) +
        geom_text(aes(label = ifelse(Proportion > 0.01, sprintf("%.2f", Proportion), "")),
          size = 3.5
        ) +
        scale_fill_gradient(
          low = "white", high = col,
          limits = c(0, 1), name = "Proportion"
        ) +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev) +
        facet_grid(prevalence ~ n_items + n_participants, labeller = cond_labeller) +
        labs(
          # title = paste("Confusion Matrices for", full_label),
          x = "Predicted strategy model",
          y = "True strategy model"
        ) + cond_theme

      ggsave(file.path(cm_output_dir, paste0("cm_by_condition_", m, ".png")),
        plot = p_cm_cond, width = width, height = height, dpi = 300
      )

      # Second plot inversion matrices: p(sim|fit)
      inv_df_cond = cm_df_all %>%
        # Drop proportions for cm, recalculate for inv 
        select(-Proportion) %>%
        group_by(Prediction, n_participants, n_items, prevalence) %>%
        mutate(Proportion = Freq / sum(Freq)) %>%
        ungroup()
 
      p_inv_cond = ggplot(inv_df_cond, aes(x = Prediction, y = Reference, fill = Proportion)) +
        geom_tile(colour = "white", linewidth = 0.4) +
        geom_text(aes(label = ifelse(Proportion > 0.01, sprintf("%.2f", Proportion), "")),
          size = 3.5
        ) +
        scale_fill_gradient(low = "white", high = col, limits = c(0, 1), name = "Proportion") +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev) +
        facet_grid(prevalence ~ n_items + n_participants, labeller = cond_labeller) +
        labs(x = "Fitted strategy model", y = "Simulated strategy model") +
        cond_theme
 
      ggsave(file.path(inv_output_dir, paste0("inv_by_condition_", m, ".png")),
        plot = p_inv_cond, width = width, height = height, dpi = 300
      )


    }
    if (overall) {
      cm_df_overall = cm_df_all %>%
        group_by(Prediction, Reference) %>%
        summarise(Freq = sum(Freq), .groups = "drop")
 
      # Calculate overall cm
      cm_proportions = cm_df_overall %>%
        group_by(Reference) %>%
        mutate(Proportion = Freq / sum(Freq)) %>%
        ungroup()
 
      # Calculate overall inv
      inv_proportions = cm_df_overall %>%
        group_by(Prediction) %>%
        mutate(Proportion = Freq / sum(Freq)) %>%
        ungroup()
 
      overall_theme = theme_bw(base_size = 16) +
        theme(
          strip.text      = element_text(face = "bold"),
          panel.grid      = element_blank(),
          axis.text       = element_text(size = 16),
          axis.title      = element_text(size = 18),
          legend.text     = element_text(size = 13),
          legend.title    = element_text(size = 16),
          legend.position = "right"
        )
 
      # Confusion matrix plot
      p_cm_overall = ggplot(cm_proportions, aes(x = Prediction, y = Reference, fill = Proportion)) +
        geom_tile(colour = "white", linewidth = 0.4) +
        geom_text(aes(label = ifelse(Proportion > 0.01, sprintf("%.2f", Proportion), "")),
          size = 6
        ) +
        scale_fill_gradient(low = "white", high = col, limits = c(0, 1), name = "Proportion") +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev) +
        labs(x = "Predicted strategy model", y = "Simulated strategy model") +
        overall_theme
 
      ggsave(file.path(cm_output_dir, paste0("cm_overall_", m, ".png")),
        plot = p_cm_overall, width = 8, height = 6, dpi = 300
      )
 
      # Inversion matrix plot
      p_inv_overall = ggplot(inv_proportions, aes(x = Prediction, y = Reference, fill = Proportion)) +
        geom_tile(colour = "white", linewidth = 0.4) +
        geom_text(aes(label = ifelse(Proportion > 0.01, sprintf("%.2f", Proportion), "")),
          size = 6
        ) +
        scale_fill_gradient(low = "white", high = col, limits = c(0, 1), name = "Proportion") +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev) +
        labs(x = "Fitted strategy model", y = "Simulated strategy model") +
        overall_theme
 
      ggsave(file.path(inv_output_dir, paste0("inv_overall_", m, ".png")),
        plot = p_inv_overall, width = 8, height = 6, dpi = 300
      )

    }
  }
}


######################################################################################################
###################################### PARAMETER ESTIMATES ###########################################
######################################################################################################

bar_param_plot = function(params_df,
                          metric = c("rmse", "bias"),
                          mode = c("all", "by_model", "avg_model"),
                          output_dir = "./figures/params",
                          width = 14,
                          height = 8) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  model_order = c("internal", "external", "sequential", "integrative")

  # Label mapping for x-axis ticks to match the notation in the manuscript
  param_math = c(
    "b0"   = "beta[0]",
    "bext" = "beta[ext]",
    "bint" = "beta[int]",
    "z"    = "xi"
  )
  model_display = c(
    "internal"    = "Internal",
    "external"    = "External",
    "sequential"  = "Sequential",
    "integrative" = "Integrative"
  )

  facet_labeller = labeller(
    prevalence     = c("equal" = "Equal Prevalence", "extreme" = "Extreme Prevalence"),
    n_items        = c("60" = "180 Items", "120" = "360 Items"),
    n_participants = c("150" = "N = 150", "300" = "N = 300")
  )

  # Calculate bias
  df = params_df %>%
    mutate(
      method = factor(method, levels = names(method_colors)),
      .value = if (metric == "bias") predicted_value - true_value else rmse
    )

  # Get rmse ready for plotting
  if (metric == "rmse") {
    if (mode == "avg_model") {
      df = df %>%
        group_by(n_participants, n_items, prevalence, method, model) %>%
        summarise(mean_val = mean(.value, na.rm = TRUE),
                  se_val   = sd(.value, na.rm = TRUE) / sqrt(n()),
                  .groups  = "drop") %>%
        mutate(model = factor(model, levels = model_order))
    } else {
      df = df %>%
        group_by(n_participants, n_items, prevalence, method, model, param_name) %>%
        summarise(mean_val = mean(.value, na.rm = TRUE),
                  se_val   = sd(.value, na.rm = TRUE) / sqrt(n()),
                  .groups  = "drop")
    }
  } else {
    if (mode == "avg_model") {
      df = df %>% mutate(model = factor(model, levels = model_order))
    }
  }

  y_label     = if (metric == "rmse") "Mean RMSE" else "Bias"
  file_prefix = paste0("bar_", metric)
  base_size   = if (metric == "bias" && mode != "by_model") 14 else 12

  # Theme specifications
  if (mode == "avg_model") {
    x_label = "Model"
    x_angle = 30
    x_size  = 12
  } else {
    x_label = "Parameter"
    x_angle = 40
    x_size  = 12
  }

  # Function to plot the given data
  make_plot = function(data) {
    # Name x axis ticks as model name
    if (mode == "avg_model") {
      data$model = factor(data$model, levels = model_order)
      x_col = "model"
      p_x_scale = scale_x_discrete(labels = model_display)

    } else if (mode == "by_model") {
      # Name x axis ticks as param notation
      param_order = data %>%
        distinct(param_name) %>%
        arrange(param_name) %>%
        pull(param_name)
      data$param_name = factor(data$param_name, levels = param_order)

      tick_map = setNames(
        sapply(param_order, function(p) if (p %in% names(param_math)) param_math[[p]] else p),
        param_order
      )
      p_x_scale = scale_x_discrete(
        labels = function(x) parse(text = tick_map[x])
      )
      x_col = "param_name"

    } else {
      data = data %>%
        mutate(model = factor(model, levels = model_order)) %>%
        arrange(model, param_name) %>%
        mutate(
          x_tick_key = paste0(
            model_display[as.character(model)],
            "|||",
            sapply(param_name, function(p) if (p %in% names(param_math)) param_math[[p]] else p)
          ),
          x_tick_key = factor(x_tick_key, levels = unique(x_tick_key))
        )
      x_col = "x_tick_key"
 
      # Name the x axis ticks as model name - parameter notation
      tick_levels = levels(data$x_tick_key)
      tick_map_all = setNames(
        sapply(tick_levels, function(lbl) {
          parts      = strsplit(lbl, "\\|\\|\\|")[[1]]
          model_part = parts[1] 
          param_part = parts[2]  
          paste0('"', model_part, '"', "*'-'*", param_part)
        }),
        tick_levels
      )
      p_x_scale = scale_x_discrete(
        labels = function(x) parse(text = tick_map_all[x])
      )
 
      # Dashed vertical lines between models
      vline_pos = data %>%
        distinct(model, x_tick_key) %>%
        group_by(model) %>%
        summarise(last_pos = max(as.integer(x_tick_key)), .groups = "drop") %>%
        arrange(last_pos) %>%
        filter(model != last(model_order[model_order %in% unique(as.character(data$model))])) %>%
        pull(last_pos)
    }

    # If RMSE plot as errorbar
    if (metric == "rmse") {
      p = ggplot(data, aes(x = .data[[x_col]], y = mean_val, fill = method)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        geom_errorbar(
          aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
          position  = position_dodge(width = 0.8),
          width     = 0.25,
          linewidth = 0.5
        )
    } else {
      # If bias plot as boxplot
      p = ggplot(data, aes(x = .data[[x_col]], y = .value, fill = method)) +
        geom_boxplot(
          position      = position_dodge(width = 0.8),
          width         = 0.7,
          outlier.size  = 0.8,
          outlier.alpha = 0.5
        ) +
        geom_hline(yintercept = 0, linewidth = 0.7, colour = "grey60", linetype = "dashed")
    }

    if (mode == "all" && exists("vline_pos") && length(vline_pos) > 0) {
      p = p + geom_vline(
        xintercept = vline_pos + 0.5,
        linetype   = "dashed",
        colour     = "grey60",
        linewidth  = 0.4
      )
    }

    p = p + switch(paste(metric, mode),
      "rmse by_model"  = scale_y_continuous(limits = c(0, 1.7),  breaks = seq(0, 1.7, 0.2)),
      "bias by_model"  = scale_y_continuous(limits = c(-6, 6),   breaks = seq(-6, 6, 1),
                                            expand = expansion(mult = c(0.1, 0.1))),
      "bias all"       = scale_y_continuous(limits = c(-6, 6),   breaks = seq(-6, 6, 1),
                                            expand = expansion(mult = c(0.1, 0.1))),
      "bias avg_model" = scale_y_continuous(limits = c(-6, 6),   breaks = seq(-6, 6, 1),
                                            expand = expansion(mult = c(0.1, 0.1))),
      NULL
    )

    p +
      p_x_scale +
      facet_grid(prevalence ~ n_items + n_participants, labeller = facet_labeller) +
      scale_fill_manual(values = method_colors, labels = method_labels) +
      labs(x = x_label, y = y_label, fill = "Method") +
      theme_bw(base_size = base_size) +
      theme(
        strip.text         = element_text(face = "bold"),
        axis.text.x        = element_text(angle = x_angle, hjust = 1, size = x_size),
        legend.position    = "right",
        panel.grid.major.x = element_blank(),
        axis.text          = element_text(size = base_size),
        axis.title         = element_text(size = base_size + 2),
        legend.text        = element_text(size = 13),
        legend.title       = element_text(size = base_size)
      )
  }

  if (mode == "by_model") {
    for (m in model_order) {
      df_model = df %>% filter(model == m)
      if (nrow(df_model) == 0) next
      ggsave(
        filename = file.path(output_dir, paste0(file_prefix, "_", m, ".png")),
        plot     = make_plot(df_model),
        width    = width, height = height, dpi = 300
      )
    }
  } else {
    filename = if (mode == "avg_model") {
      paste0(file_prefix, "_avg_model.png")
    } else {
      paste0(file_prefix, ".png")
    }
    ggsave(
      filename = file.path(output_dir, filename),
      plot     = make_plot(df),
      width    = width, height = height, dpi = 300
    )
  }
}

# plot_param_ci = function(params_df,
#                         output_dir = "./figures/empirical_params",
#                         width = 10,
#                         height = 8
# ) {
#   if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

#   # One plot per model x param x condition combination
#   conditions = params_df %>%
#     distinct(model, param_name, prevalence, n_participants, n_items)

#   for (i in seq_len(nrow(conditions))) {
#     cond = conditions[i, ]

#     df = params_df %>%
#       filter(
#         model         == cond$model,
#         param_name    == cond$param_name,
#         prevalence    == cond$prevalence,
#         n_participants == cond$n_participants,
#         n_items       == cond$n_items
#       ) %>%
#       mutate(method = factor(method, levels = names(method_labels)))

#     # Rank participants within each method by their point estimate
#     df = df %>%
#       group_by(method) %>%
#       mutate(rank = rank(predicted_value)) %>%
#       ungroup()

#     p = ggplot(df, aes(y = rank)) +
#       geom_segment(
#         aes(x = ci_lower, xend = ci_upper, yend = rank, color = within_ci),
#         linewidth = 0.4
#       ) +
#       geom_point(aes(x = predicted_value), size = 0.6, color = "grey30") +
#       geom_vline(aes(xintercept = true_value), linetype = "dashed", linewidth = 0.5) +
#       scale_color_manual(
#         values = c("TRUE" = "grey60", "FALSE" = "red"),
#         labels = c("TRUE" = "Contains true", "FALSE" = "Misses true"),
#         name   = NULL
#       ) +
#       facet_wrap(~ method, labeller = labeller(method = method_labels)) +
#       labs(
#         x = cond$param_name,
#         y = "Participant",
#         title = paste0(
#           tools::toTitleCase(cond$model), " — ", cond$param_name,
#           " | ", cond$prevalence, " prevalence",
#           " | N=", cond$n_participants,
#           ", ", cond$n_items * 3, " items"
#         )
#       ) +
#       theme_bw(base_size = 13) +
#       theme(
#         strip.text      = element_text(face = "bold"),
#         legend.position = "bottom",
#         panel.grid.major.x = element_blank()
#       )

#     fname = paste0(
#       "ci_", cond$model, "_", cond$param_name, "_",
#       cond$prevalence, "_N", cond$n_participants, "_items", cond$n_items * 3, ".png"
#     )

#     ggsave(file.path(output_dir, fname), p, width = width, height = height, dpi = 300)
#   }
# }

######################################################################################################
######################################### RUNTIME ####################################################
######################################################################################################

plot_runtime = function(df_combined,
                       output_dir = "./figures/runtime",
                       width = 9,
                       height = 8) {

  
  p = ggplot(df_combined, aes(x = factor(n_participant), y = runtime_min,
                             fill = method, color = method)) +    
    geom_boxplot(
      position = position_dodge(width = 0.85),
      width = 0.85,
      linewidth = 0.4,
      outlier.size = 1.5,
      outlier.alpha = 0.5,
      alpha = 0.1
    ) +
    scale_fill_manual(values = method_colors, labels = method_labels, guide = "none") +
    scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
    facet_grid(prevalence_type ~ n_items,
        labeller = labeller(prevalence_type = c("equal" = "Equal Prevalence",
                                                "extreme" = "Extreme Prevalence"),
                            n_items = c("60" = "180 Items",
                                        "120" = "360 Items"))) +
    scale_y_log10(breaks = c(1, 2, 5, 15, 30, 60, 120, 240, 360), labels = function(x) paste0(x, "m")) +
    labs(
        x = "Number of participants",
        y = "Runtime (log scale)",
        fill = "Method"
      ) +
      theme_bw(base_size = 14) +
      theme(
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "right",
        panel.grid.minor.y = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.minor.x = element_blank(),
      )

  p_method = ggplot(df_combined, aes(
    x = method,
    y = runtime_min,
    fill = method
  )) +
    geom_boxplot(
      width = 0.6,
      linewidth = 0.4,
      outlier.shape = 16,
      outlier.size = 1.5,
      outlier.alpha = 0.5
    ) +
    scale_fill_manual(values = method_colors, labels = method_labels) +
    # scale_y_log10(
    #   breaks = c(1, 2, 5, 15, 30, 60, 120, 240, 360),
    #   labels = function(x) paste0(x, "m")
    # ) +
    labs(
      x = "Method",
      y = "Runtime (min)"
    ) +
    scale_x_discrete(labels = method_labels) +
    theme_bw(base_size = 14) 

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  ggsave(
    filename = file.path(output_dir, "plot_runtime_log.png"),
    plot = p,
    width = width,
    height = height
  )
  ggsave(
  filename = file.path(output_dir, "plot_runtime_by_method.png"),
  plot = p_method,
  width = width,
  height = height
)

  invisible(p)
}


######################################################################################################
########################################## EMPIRICAL STUDY ###########################################
######################################################################################################

posterior_proportion = function(posterior_df,
                                output_dir = "./figures/models",
                                width = 14,
                                height = 7
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  model_labels = c(
    "internal"    = "Internal",
    "external"    = "External",
    "sequential"  = "Sequential",
    "integrative" = "Integrative",
    "guessing"    = "Guessing",
    "bias.d1"     = "Right-side Bias",
    "bias.d2"     = "Left-side Bias"
  )

  model_levels = unname(model_labels)

  posterior_df = posterior_df %>%
    mutate(model = factor(recode(model, !!!model_labels), levels = model_levels))

  for (m in unique(posterior_df$method)) {
    df = posterior_df %>% filter(method == m)

    participant_order = df %>%
      group_by(participant_id) %>%
      slice_max(proportion, n = 1, with_ties = FALSE) %>%  
      ungroup() %>%
      arrange(model, desc(proportion)) %>%                 
      pull(participant_id)

    df = df %>% mutate(participant_id = factor(participant_id, levels = participant_order))

    p = ggplot(df, aes(x = participant_id, y = proportion, fill = model)) +
      geom_col(width = 1, color = "white", linewidth = 0.1) + 
      scale_fill_brewer(palette = "Set2", breaks = model_levels) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_discrete(expand = c(0, 0)) +
      labs(x = "Participant", y = "Posterior probability", fill = "Model") +
      theme_bw(base_size = 16) +
      theme(
        axis.text.y        = element_text(size = 14),
        axis.text.x        = element_blank(),
        axis.ticks.x       = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "right"
        
      )

    ggsave(
      file.path(output_dir, paste0("posterior_", m, ".png")),
      p, width = width, height = height, dpi = 300
    )
  }
}

model_prevalence = function(assign_files,
                            output_dir = "./figures/models",
                            width = 10,
                            height = 6
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  strat_labels = c("1" = "Internal", "2" = "External", "3" = "Sequential",
                   "4" = "Integrative", "5" = "Guessing", "6" = "Bias D1", "7" = "Bias D2")

  model_levels = unname(strat_labels) 

  read_assignments = function(path, method_name) {
    raw = read.csv(path)
    colnames(raw) = tolower(gsub("\\s+", "_", colnames(raw)))

    assignments = if (method_name == "bf_ps") {
      apply(raw, 2, function(x) Mode(x)[1])
    } else {
      pred_col = intersect(c("strat_labels", "strat_label", "strategy", "strat"), colnames(raw))[1]
      if (is.na(pred_col)) stop("No valid strategy column found for method: ", method_name)
      raw[[pred_col]]
    }

    factor(recode(as.character(assignments), !!!strat_labels), levels = model_levels)
  }

  prevalence_df = bind_rows(lapply(names(assign_files), function(method_name) {
    path = assign_files[[method_name]]

    if (!file.exists(path)) {
      message("Missing: ", path)
      return(NULL)
    }

    assignments = read_assignments(path, method_name)

    data.frame(model = assignments, method = method_name) %>%
      count(method, model, .drop = FALSE) %>%
      group_by(method) %>%
      mutate(prevalence = n / sum(n)) %>%
      ungroup()
  }))

  p = ggplot(prevalence_df, aes(x = model, y = prevalence, fill = method)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = method_colors, labels = method_labels) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(x = "Strategy Model", y = "Proportion of participants", fill = "Method") +
    theme_bw(base_size = 16) +
    theme(
      axis.text.x        = element_text(size = 14, angle = 30, hjust = 1),
      axis.text.y        = element_text(size = 14),
      legend.position    = "right",
      strip.text         = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )

  ggsave(
    file.path(output_dir, "model_prevalence.png"),
    p, width = width, height = height, dpi = 300
  )
}

plot_empirical_params = function(
  param_dirs   = NULL,
  assign_dirs  = NULL,
  param_samples = NULL,
  param_mapping,
  level,
  output_dir = "./figures/empirical_params",
  width  = 12,
  height = 8,
  psis_waic_name = "empirical_"
) {

    param_math = c(
    "b0"   = "beta[0]",
    "bext" = "beta[ext]",
    "bint" = "beta[int]",
    "z"    = "xi"
  )
  model_display = c(
    "internal"    = "Internal",
    "external"    = "External",
    "sequential"  = "Sequential",
    "integrative" = "Integrative"
  )

  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  if (level == "group") {
    
    all_samples = bind_rows(lapply(names(param_dirs), function(method_name) {
      samples_list = get_group_param_samples(method_name, param_dirs, param_mapping, psis_waic_name = psis_waic_name)
      
      bind_rows(lapply(names(samples_list), function(model_label) {
        df = samples_list[[model_label]]
        if (length(df) == 0 || nrow(df) == 0) return(NULL)
        
        params = param_mapping[[model_label]]$params
        
        df %>%
          as.data.frame() %>%
          mutate(method = method_name, model = model_label) %>%
          tidyr::pivot_longer(all_of(params), names_to = "param", values_to = "value") %>%
          filter(!is.na(value))
      }))
    })) %>%
      mutate(method = factor(method, levels = names(method_colors)))
    
    if (nrow(all_samples) == 0) { message("No group parameter data found."); return(invisible(NULL)) }
    
      for (m in names(param_mapping)) {
        df_model = filter(all_samples, model == m)
        if (nrow(df_model) == 0) next
        
        p = ggplot(df_model, aes(x = value, y = method, fill = method, color = method)) +
          geom_density_ridges(alpha = 0.5, linewidth = 0.6, scale = 0.9) +
          scale_fill_manual(values = method_colors, labels = method_labels, aesthetics = c("fill", "color")) +
          scale_y_discrete(labels = method_labels, limits = rev(names(method_colors))) +
          # New, try this
          facet_wrap(~ param, scales = "free",
           labeller = as_labeller(param_math, default = label_parsed)) + 
          labs(x = "Estimate", y = NULL, fill = "Method", color = "Method") +
          theme_bw(base_size = 14) +
          theme(
            strip.text     = element_text(face = "bold"),
            legend.position = "right",
            axis.text.y    = element_blank()
          )
        
        ggsave(file.path(output_dir, paste0("group_params_empirical_", m, ".png")),
              p, width = width, height = height, dpi = 300)
      }
    
  } else if (level == "individual") {
    
    params_df = bind_rows(lapply(names(param_dirs), function(method_name) {
      predicted_path = assign_dirs[[method_name]]
      param_path     = param_dirs[[method_name]]
      
      if (!file.exists(predicted_path) || !file.exists(param_path)) {
        message("Missing files for: ", method_name)
        return(NULL)
      }
      
      bind_rows(lapply(names(param_mapping), function(model_label) {
        model_info   = param_mapping[[model_label]]
        model_params = model_info$params
        
        params_file = switch(method_name,
          hbi  = file.path(param_path, paste0("empirical_data_merged_params_", model_info$model_number, ".csv")),
          psis = file.path(param_path, paste0("empirical_", model_label, ".csv")),
          waic = file.path(param_path, paste0("empirical_", model_label, ".csv")),
          param_path
        )
        
        if (!file.exists(params_file)) { message("Missing: ", params_file); return(NULL) }
        
        tryCatch(
          get_params_data(params_file, predicted_path, method_name, model_params) %>%
            as.data.frame() %>%
            mutate(method = method_name, model = model_label) %>%
            tidyr::pivot_longer(all_of(model_params), names_to = "param", values_to = "value") %>%
            filter(!is.na(value)),
          error = function(e) { message("ERROR ", method_name, "/", model_label, ": ", e$message); NULL }
        )
      }))
    })) %>%
      mutate(method = factor(method, levels = names(method_colors)))
    
    if (nrow(params_df) == 0) { message("No parameter data found."); return(invisible(NULL)) }
    
    for (m in names(param_mapping)) {
      df_model = filter(params_df, model == m)
      if (nrow(df_model) == 0) next

       p = ggplot(df_model, aes(x = value, y = method, fill = method, color = method)) +
          geom_density_ridges(alpha = 0.5, linewidth = 0.6, scale = 0.9) +
          scale_fill_manual(values = method_colors, labels = method_labels, aesthetics = c("fill", "color")) +
          scale_y_discrete(labels = method_labels, limits = rev(names(method_colors))) +
          facet_wrap(~ param, scales = "free") +
          labs(x = "Estimate", y = NULL, fill = "Method", color = "Method",
              title = tools::toTitleCase(m)) +
          theme_bw(base_size = 14) +
          theme(
            strip.text     = element_text(face = "bold"),
            legend.position = "right",
            axis.text.y    = element_blank()
          )

      # p = ggplot(df_model, aes(x = value, fill = method, color = method)) +
      #   geom_density(alpha = 0.35, linewidth = 0.6) +
      #   scale_fill_brewer(palette = "Set2", labels = method_labels, aesthetics = c("fill", "color")) +
      #   facet_wrap(~ param, scales = "free") +
      #   labs(x = "Estimate", y = "Density", fill = "Method", color = "Method",
      #        title = tools::toTitleCase(m)) +
      #   theme_bw(base_size = 14) +
      #   theme(strip.text = element_text(face = "bold"), legend.position = "right")
      
      ggsave(file.path(output_dir, paste0("params_empirical_", m, ".png")),
             p, width = width, height = height, dpi = 300)
    
    }
  }
}

