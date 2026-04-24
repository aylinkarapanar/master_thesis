packages = c( "ggplot2", "dplyr", "see")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

source("./metrics.R")

plot_metric = function(data_path = "./metrics/metrics_all.csv",
                        metric = "accuracy",
                        output_dir = "./figures",
                        width = 18,
                        height = 16) {
  
  # Load data
  df = read.csv(data_path)
  
  # Check whether the metric exists
  if (!metric %in% c("accuracy","precision","recall","f1")) {
    stop("Metric must be one of: accuracy, precision, recall, f1")
  }
  
  # Select relevant columns
  df_metric = df %>%
    select(n_items, n_participants, prevalence, method, seed, all_of(metric))
  colnames(df_metric)[6] = "value"
  
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
    # Define colours
    scale_fill_okabeito(
      labels = c(
        posterior_prob = "Posterior Prob",
        psis = "PSIS-LOO",
        waic = "WAIC",
        hbi = "HBI"
      )
    ) +
    labs(
      x = "Number of participants",
      y = tools::toTitleCase(metric),
      fill = "Method",
      title = paste("Performance of ", tools::toTitleCase(metric), "Across Conditions")
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
  
  return(p)
}

# TODO: general cm and cm by condition

labels = get_labels("./data/1234/150_180_equal_participants.csv",  "./model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv", method_name)

df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv", 
                                predicted_data_path = "./model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv",
                                param_path = "./parameter_estimates/hbi/1234", 
                                param_mapping = param_mapping)