packages = c("ggplot2", "dplyr", "see")

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
      strip.text = element_text(face = "bold"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
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
              "60" = "180 Items",
              "120" = "360 Items"
            ),
            n_participants = c(
              "150" = "N = 150",
              "300" = "N = 300"
            )
          )
        ) +
        labs(
          # title = paste("Confusion Matrices for", full_label),
          x = "Predicted strategy model",
          y = "True strategy model"
        ) +
        theme_bw(base_size = 14) +
        theme(
          # plot.title = element_text(face = "bold", hjust = 0.5),
          strip.text = element_text(face = "bold"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 14),
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
          size = 6
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
          # title = paste("Overall Confusion Matrix for", full_label),
          x = "Predicted strategy model",
          y = "True strategy model"
        ) +
        theme_bw(base_size = 16) +
        theme(
          # plot.title = element_text(face = "bold", hjust = 0.5),
          strip.text = element_text(face = "bold"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 16),
          legend.position = "right"
        )

      ggsave(file.path(output_dir, paste0("cm_overall_", m, ".png")),
        plot = p, width = 8, height = 6, dpi = 300
      )
    }
  }
}


bar_param_plot = function(
  params_df,
  metric = c("rmse", "bias"),
  mode = c("all", "by_model", "avg_model"),
  output_dir = "./figures/params",
  width = 14,
  height = 8
) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  model_order = c("internal", "external", "sequential", "integrative")
  facet_labeller = labeller(
    prevalence = c("equal" = "Equal Prevalence", "extreme" = "Extreme Prevalence"),
    n_items = c("60" = "180 Items", "120" = "360 Items"),
    n_participants = c("150" = "N = 150", "300" = "N = 300")
  )

  df = params_df %>%
    mutate(
      method = factor(method, levels = names(method_colors)),
      .value = if (metric == "bias") predicted_value - true_value else rmse
    )

  # If mode is average by model, summarise the metric
  if (mode == "avg_model") {
    df = df %>%
      group_by(n_participants, n_items, prevalence, method, model) %>%
      summarise(mean_val = mean(.value, na.rm = TRUE),
                se_val = sd(.value, na.rm = TRUE) / sqrt(n()),
                .groups = "drop") %>%
      mutate(model = factor(model, levels = model_order))
    x_var   = "model"
    x_label = "Model"
    x_angle = 30
    x_size  = 12
  } else {
    df = df %>%
      mutate(param_label = paste0(model, "-", param_name)) %>%
      group_by(n_participants, n_items, prevalence, method, model, param_label) %>%
      summarise(mean_val = mean(.value, na.rm = TRUE),
                se_val = sd(.value, na.rm = TRUE) / sqrt(n()),
                .groups = "drop")
    x_var   = "param_label"
    x_label = "Parameter"
    x_angle = 40
    x_size  = 12
  }

  y_label = if (metric == "rmse") "Mean RMSE" else "Mean Bias"
  file_prefix = paste0("bar_", metric)
  base_size = if (metric == "bias" && mode != "by_model") 14 else 12

  # Define a function to build the plot
  make_plot = function(data) {
    param_order = NULL

    if (mode != "avg_model") {
      param_order = data %>%
        distinct(model, param_label) %>%
        mutate(model = factor(model, levels = model_order)) %>%
        arrange(model, param_label) %>%
        pull(param_label)
      data$param_label = factor(data$param_label, levels = param_order)
    }

    p = ggplot(data, aes(x = .data[[x_var]], y = mean_val, fill = method)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_errorbar(
        aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
        position = position_dodge(width = 0.8),
        width = 0.25,
        linewidth = 0.5
      )

    if (metric == "bias") {
      p = p + geom_hline(yintercept = 0, linewidth = 0.7, colour = "grey20")
    }

    if (mode == "all" && !is.null(param_order)) {
      p = p + geom_vline(
        xintercept = which(!duplicated(gsub("-.*", "", param_order)))[-1] - 0.5,
        linetype = "dashed",
        colour = "grey60",
        linewidth = 0.4
      )
    }

    # Y axis scale for a given mode and metric
    p = p + switch(paste(metric, mode),
      "rmse by_model" = scale_y_continuous(limits = c(0, 1.7), breaks = seq(0, 1.7, 0.2)),
      "bias by_model" = scale_y_continuous(limits = c(-1.5, 0.7), breaks = seq(-1.5, 0.7, 0.2),
                                           expand = expansion(mult = c(0.1, 0.1))),
      "bias all" = scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))),
      "bias avg_model"= scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))),
      NULL
    )

    p +
      facet_grid(prevalence ~ n_items + n_participants, labeller = facet_labeller) +
      scale_fill_manual(values = method_colors, labels = method_labels) +
      labs(x = x_label, y = y_label, fill = "Method") +
      theme_bw(base_size = base_size) +
      theme(
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = x_angle, hjust = 1, size = x_size),
        legend.position = "right",
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = base_size),
        axis.title = element_text(size = base_size + 2),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = base_size)
      )
  }

  if (mode == "by_model") {
    for (m in model_order) {
      df_model = df %>% filter(model == m)
      if (nrow(df_model) == 0) next
      ggsave(
        filename = file.path(output_dir, paste0(file_prefix, "_", m, ".png")),
        plot = make_plot(df_model),
        width = width, height = height, dpi = 300
      )
    }
  } else {
    filename = if (mode == "avg_model") paste0(file_prefix, "_avg_model.png") else paste0(file_prefix, ".png")
    ggsave(
      filename = file.path(output_dir, filename),
      plot = make_plot(df),
      width = width, height = height, dpi = 300
    )
  }
}

bar_param_plot = function(
  params_df,
  metric = c("rmse", "bias"),
  mode = c("all", "by_model", "avg_model"),
  output_dir = "./figures/params",
  width = 14,
  height = 8
) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  model_order = c("internal", "external", "sequential", "integrative")
  facet_labeller = labeller(
    prevalence = c("equal" = "Equal Prevalence", "extreme" = "Extreme Prevalence"),
    n_items = c("60" = "180 Items", "120" = "360 Items"),
    n_participants = c("150" = "N = 150", "300" = "N = 300")
  )

  df = params_df %>%
    mutate(
      method = factor(method, levels = names(method_colors)),
      .value = if (metric == "bias") predicted_value - true_value else rmse
    )

  if (metric == "rmse") {
    if (mode == "avg_model") {
      df = df %>%
        group_by(n_participants, n_items, prevalence, method, model) %>%
        summarise(mean_val = mean(.value, na.rm = TRUE),
                  se_val = sd(.value, na.rm = TRUE) / sqrt(n()),
                  .groups = "drop") %>%
        mutate(model = factor(model, levels = model_order))
    } else {
      df = df %>%
        mutate(param_label = paste0(model, "-", param_name)) %>%
        group_by(n_participants, n_items, prevalence, method, model, param_label) %>%
        summarise(mean_val = mean(.value, na.rm = TRUE),
                  se_val = sd(.value, na.rm = TRUE) / sqrt(n()),
                  .groups = "drop")
    }
  } else {
    # bias: always keep raw values; set up grouping vars only
    if (mode == "avg_model") {
      df = df %>% mutate(model = factor(model, levels = model_order))
    } else {
      df = df %>% mutate(param_label = paste0(model, "-", param_name))
    }
  }

  if (mode == "avg_model") {
    x_var   = "model"
    x_label = "Model"
    x_angle = 30
    x_size  = 12
  } else {
    x_var   = "param_label"
    x_label = "Parameter"
    x_angle = 40
    x_size  = 12
  }

  y_label   = if (metric == "rmse") "Mean RMSE" else "Bias"
  file_prefix = paste0("bar_", metric)
  base_size = if (metric == "bias" && mode != "by_model") 14 else 12

  make_plot = function(data) {
    param_order = NULL

    if (mode != "avg_model") {
      param_order = data %>%
        distinct(model, param_label) %>%
        mutate(model = factor(model, levels = model_order)) %>%
        arrange(model, param_label) %>%
        pull(param_label)
      data$param_label = factor(data$param_label, levels = param_order)
    }

    if (metric == "rmse") {
      p = ggplot(data, aes(x = .data[[x_var]], y = mean_val, fill = method)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        geom_errorbar(
          aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
          position = position_dodge(width = 0.8),
          width = 0.25,
          linewidth = 0.5
        )
    } else {
      p = ggplot(data, aes(x = .data[[x_var]], y = .value, fill = method)) +
        geom_boxplot(
          position = position_dodge(width = 0.8),
          width = 0.7,
          outlier.size = 0.8,
          outlier.alpha = 0.5
        ) +
        geom_hline(yintercept = 0, linewidth = 0.7, colour = "grey60", linetype = "dashed")
    }

    if (mode == "all" && !is.null(param_order)) {
      p = p + geom_vline(
        xintercept = which(!duplicated(gsub("-.*", "", param_order)))[-1] - 0.5,
        linetype = "dashed",
        colour = "grey60",
        linewidth = 0.4
      )
    }

    p = p + switch(paste(metric, mode),
      "rmse by_model"  = scale_y_continuous(limits = c(0, 1.7), breaks = seq(0, 1.7, 0.2)),
      "bias by_model"  = scale_y_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1), expand = expansion(mult = c(0.1, 0.1))),
      "bias all"       = scale_y_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1), expand = expansion(mult = c(0.1, 0.1))),
      "bias avg_model" = scale_y_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1), expand = expansion(mult = c(0.1, 0.1))),
      NULL
    )

    p +
      facet_grid(prevalence ~ n_items + n_participants, labeller = facet_labeller) +
      scale_fill_manual(values = method_colors, labels = method_labels) +
      labs(x = x_label, y = y_label, fill = "Method") +
      theme_bw(base_size = base_size) +
      theme(
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = x_angle, hjust = 1, size = x_size),
        legend.position = "right",
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = base_size),
        axis.title = element_text(size = base_size + 2),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = base_size)
      )
  }

  if (mode == "by_model") {
    for (m in model_order) {
      df_model = df %>% filter(model == m)
      if (nrow(df_model) == 0) next
      ggsave(
        filename = file.path(output_dir, paste0(file_prefix, "_", m, ".png")),
        plot = make_plot(df_model),
        width = width, height = height, dpi = 300
      )
    }
  } else {
    filename = if (mode == "avg_model") paste0(file_prefix, "_avg_model.png") else paste0(file_prefix, ".png")
    ggsave(
      filename = file.path(output_dir, filename),
      plot = make_plot(df),
      width = width, height = height, dpi = 300
    )
  }
}