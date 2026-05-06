packages = c("ggplot2", "dplyr", "see")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

source("./metrics.R")
source("./parameter.R")


# Define color mapping
oi = palette_okabeito(palette = "full")(4)

method_colors = c(
  bf_ps = oi[1],
  psis  = oi[2],
  waic  = oi[3],
  hbi   = oi[4]
)

method_labels = c(
  # bf_ps = "BF estimation with PS",
  bf_ps = "BF",
  psis  = "PSIS-LOO",
  waic  = "WAIC",
  hbi   = "HBI"
)

plot_metric = function(data_path = "./metrics/metrics_all.csv",
                        metric = "accuracy",
                        output_dir = "./figures/metrics",
                        width = 9,
                        height = 8) {
  # Load data
  df = read.csv(data_path)

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
        n_items = c("60" = "60 Items Per Condition", "120" = "120 Items Per Condition")
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
      title = paste(tools::toTitleCase(metric), "Across Conditions for Methods")
    ) +
    theme_bw() +
    theme(strip.text = element_text(face = "bold"))

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
                         output_dir = "./figures/cm_plots",
                         width = 16,
                         height = 6,
                         by_condition = TRUE,
                         overall = TRUE) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

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

    if (by_condition) {
      p = ggplot(cm_df_all, aes(x = Prediction, y = Reference, fill = Proportion)) +
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
        facet_grid(
          prevalence ~ n_items + n_participants,
          labeller = labeller(
            prevalence = c(
              "equal" = "Equal Prevalence",
              "extreme" = "Extreme Prevalence"
            ),
            n_items = c(
              "60" = "60 Items",
              "120" = "120 Items"
            ),
            n_participants = c(
              "150" = "N = 150",
              "300" = "N = 300"
            )
          )
        ) +
        labs(
          title = paste("Confusion Matrices for", full_label),
          x = "Predicted strategy",
          y = "True strategy"
        ) +
        theme_bw(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          strip.text = element_text(face = "bold"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 8),
          legend.position = "right"
        )

      ggsave(file.path(output_dir, paste0("cm_by_condition_", m, ".png")),
        plot = p, width = width, height = height, dpi = 300
      )
    }
    if (overall) {
      cm_df_overall = cm_df_all %>%
        group_by(Prediction, Reference) %>%
        summarise(Freq = sum(Freq), .groups = "drop") %>%
        group_by(Reference) %>%
        mutate(Proportion = Freq / sum(Freq)) %>%
        ungroup()

      p = ggplot(cm_df_overall, aes(x = Prediction, y = Reference, fill = Proportion)) +
        geom_tile(colour = "white", linewidth = 0.4) +
        geom_text(aes(label = ifelse(Proportion > 0.01, sprintf("%.2f", Proportion), "")),
          size = 4
        ) +
        scale_fill_gradient(
          low = "white",
          high = col,
          limits = c(0, 1),
          name = "Proportion"
        ) +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev) +
        labs(
          title = paste("Overall Confusion Matrix for", full_label),
          x = "Predicted strategy",
          y = "True strategy"
        ) +
        theme_bw()

      ggsave(file.path(output_dir, paste0("cm_overall_", m, ".png")),
        plot = p, width = 8, height = 6, dpi = 300
      )
    }
  }
}

# source("./parameter.R")

# # For now ignore models of non-interest
# param_mapping = list(
#     "internal" = list(
#         "model_number" = 1,
#         "params" = c("b0", "bint")
#     ),
#     "external" = list(
#         "model_number" = 2,
#         "params" = c("bext")
#     ),
#     "sequential" = list(
#         "model_number" = 3,
#         "params" = c("b0", "bint", "bext", "z")
#     ),
#     "integrative" = list(
#         "model_number" = 4,
#         "params" = c("b0", "bint", "bext")
#     )
# )

# param_info = list(
#   b0   = list(mean = 0,    sd = 0.5,  a = -Inf, b = Inf),
#   bint = list(mean = 2.5,  sd = 0.75, a = 0,    b = Inf),
#   bext = list(mean = 1.75, sd = 0.5,  a = 0.5,  b = Inf),
#   z    = list(mean = 1,    sd = 0.5,  a = 0.3,  b = 1.3),
#   guess= list(mean = 0.5,  sd = 0.02, a = 0.4,  b = 0.6),
#   bias1= list(mean = 0.05, sd = 0.02, a = 0,    b = 0.15),
#   bias2= list(mean = 0.95, sd = 0.02, a = 0.85, b = 1)
# )

# # TESTS
# df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv",
#                                 predicted_assign_path = "./results_data/model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv",
#                                 param_path = "./results_data/parameter_estimates/hbi/1234",
#                                 param_mapping = param_mapping,
#                                 method_name = "hbi")

# df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv",
#                                 predicted_assign_path = "./results_data/model_assignments/PSIS-LOO/1234/150_180_equal_strategy_assignments.csv",
#                                 param_path = "./results_data/parameter_estimates/non_hierarchical/PSIS-LOO/1234",
#                                 param_mapping = param_mapping,
#                                 method_name = "psis")

# param_path = "./results_data/parameter_estimates/non_hierarchical/PSIS-LOO/1234/150_180_equal_internal.csv"
# model_params = c("b0", "bint")

# result = get_params_data(param_path, method_name = "psis", model_params = model_params)

# param_path = "./results_data/parameter_estimates/product_space/1234/150_180_equal_params.csv"
# predicted_assign_path = "./results_data/model_assignments/posterior_prob/1234/150_180_equal_strategy_assignments.csv"
# model_params = c("b0", "bint")

# result = get_params_data(param_path, predicted_assign_path = predicted_assign_path, method_name = "ps", model_params = model_params)

scatter_params = function(params_df,
                           output_dir = "./figures/params",
                           width = 14,
                           height = 10) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  params_df = params_df %>%
    mutate(
      method = factor(method, levels = names(method_colors)),
      fill_col = ifelse(within_ci, method_colors[as.character(method)], "white"),
      ci_label = factor(
        ifelse(within_ci, "Within CI", "Outside CI"),
        levels = c("Within CI", "Outside CI")
      )
    )

  for (mod in unique(params_df$model)) {
    df_model = filter(params_df, model == mod)

    p = ggplot(df_model, aes(
      x = true_value,
      y = predicted_value,
      colour = method,
      fill = fill_col,
      shape = ci_label
    )) +
      geom_abline(
        slope = 1, intercept = 0,
        linetype = "dashed", colour = "grey50", linewidth = 0.6
      ) +
      geom_point(
        position = position_jitter(width = 0.05, height = 0.05, seed = 42),
        size = 1.8,
        stroke = 0.6,
        alpha = 0.6
      ) +
      scale_fill_identity() +
      scale_colour_manual(
        values = method_colors,
        labels = method_labels,
        name = "Method"
      ) +
      scale_shape_manual(
        values = c("Within CI" = 21, "Outside CI" = 21),
        name = "95% CI"
      ) +
      facet_grid(
        method ~ param_name,
        scales = "free",
        labeller = labeller(method = method_labels)
      ) +
      labs(
        title = paste("Parameter Recovery —", tools::toTitleCase(mod), "Model"),
        x = "True value",
        y = "Predicted value"
      ) +
      guides(
        colour = "none",
        shape = guide_legend(
          order = 1,
          override.aes = list(
            colour = c("Within CI" = "grey40", "Outside CI" = "grey40"),
            fill = c("Within CI" = "grey40", "Outside CI" = "white")
          )
        )
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold"),
        legend.position = "right"
      )

    file_name = paste0("scatter_params_", mod, ".png")
    ggsave(
      filename = file.path(output_dir, file_name),
      plot = p,
      width = width,
      height = height,
      dpi = 300
    )
  }
}


bar_rmse = function(params_df,
                     output_dir = "./figures/params",
                     width = 14,
                     height = 8) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  df = params_df %>%
    mutate(
      param_label = paste0(model, "-", param_name),
      method = factor(method, levels = names(method_colors))
    ) %>%
    group_by(n_participants, n_items, prevalence, method, model, param_label) %>%
    summarise(
      mean_rmse = mean(rmse, na.rm = TRUE),
      se_rmse = sd(rmse, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  model_order = c("internal", "external", "sequential", "integrative")

  param_order = df %>%
    distinct(model, param_label) %>%
    mutate(model = factor(model, levels = model_order)) %>%
    arrange(model, param_label) %>%
    pull(param_label)

  df$param_label = factor(df$param_label, levels = param_order)

  p = ggplot(df, aes(x = param_label, y = mean_rmse, fill = method)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_errorbar(
      aes(
        ymin = mean_rmse - se_rmse,
        ymax = mean_rmse + se_rmse
      ),
      position = position_dodge(width = 0.8),
      width = 0.25,
      linewidth = 0.5
    ) +
    geom_vline(
      xintercept = which(!duplicated(gsub("-.*", "", param_order)))[-1] - 0.5,
      linetype = "dashed",
      colour = "grey60",
      linewidth = 0.4
    ) +
    facet_grid(
      prevalence ~ n_items + n_participants,
      labeller = labeller(
        prevalence = c(
          "equal" = "Equal Prevalence",
          "extreme" = "Extreme Prevalence"
        ),
        n_items = c(
          "60" = "60 Items",
          "120" = "120 Items"
        ),
        n_participants = c(
          "150" = "N = 150",
          "300" = "N = 300"
        )
      )
    ) +
    scale_fill_manual(
      values = method_colors,
      labels = method_labels
    ) +
    labs(
      title = "Parameter Recovery RMSE by Method and Parameter",
      x = "Parameter",
      y = "Mean RMSE",
      fill = "Method"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
      legend.position = "right",
      panel.grid.major.x = element_blank()
    )

  ggsave(
    filename = file.path(output_dir, "bar_rmse.png"),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
}

bar_bias = function(params_df,
                     output_dir = "./figures/params",
                     width = 14,
                     height = 8) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  df = params_df %>%
    mutate(
      bias = predicted_value - true_value,
      param_label = paste0(model, "-", param_name),
      method = factor(method, levels = names(method_colors))
    ) %>%
    group_by(n_participants, n_items, prevalence, method, model, param_label) %>%
    summarise(
      mean_bias = mean(bias, na.rm = TRUE),
      se_bias = sd(bias, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  model_order = c("internal", "external", "sequential", "integrative")

  param_order = df %>%
    distinct(model, param_label) %>%
    mutate(model = factor(model, levels = model_order)) %>%
    arrange(model, param_label) %>%
    pull(param_label)

  df$param_label = factor(df$param_label, levels = param_order)

  p = ggplot(df, aes(x = param_label, y = mean_bias, fill = method)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_errorbar(
      aes(
        ymin = mean_bias - se_bias,
        ymax = mean_bias + se_bias
      ),
      position = position_dodge(width = 0.8),
      width = 0.25,
      linewidth = 0.5
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.7,
      colour = "grey20"
    ) +
    geom_vline(
      xintercept = which(!duplicated(gsub("-.*", "", param_order)))[-1] - 0.5,
      linetype = "dashed",
      colour = "grey60",
      linewidth = 0.4
    ) +
    facet_grid(
      prevalence ~ n_items + n_participants,
      labeller = labeller(
        prevalence = c(
          "equal" = "Equal Prevalence",
          "extreme" = "Extreme Prevalence"
        ),
        n_items = c(
          "60" = "60 Items",
          "120" = "120 Items"
        ),
        n_participants = c(
          "150" = "N = 150",
          "300" = "N = 300"
        )
      )
    ) +
    scale_fill_manual(
      values = method_colors,
      labels = method_labels
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(
      title = "Parameter Recovery Bias by Method and Parameter",
      x = "Parameter",
      y = "Mean Bias",
      fill  = "Method"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
      legend.position = "right",
      panel.grid.major.x = element_blank()
    )

  ggsave(
    filename = file.path(output_dir, "bar_bias.png"),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
}
