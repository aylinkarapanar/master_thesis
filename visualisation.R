packages = c( "ggplot2", "dplyr", "see")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

source("./metrics.R")
source("./parameter.R")

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
        bf_ps = "BF estimation with PS",
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

labels = get_labels("./data/1234/150_180_equal_participants.csv", "./results_data/model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv", method_name = "hbi")

source("./parameter.R")

# For now ignore models of non-interest
param_mapping = list(
    "internal" = list(
        "model_number" = 1,
        "params" = c("b0", "bint")
    ),
    "external" = list(
        "model_number" = 2,
        "params" = c("bext")
    ),
    "sequential" = list(
        "model_number" = 3,
        "params" = c("b0", "bint", "bext", "z")
    ),
    "integrative" = list(
        "model_number" = 4,
        "params" = c("b0", "bint", "bext")
    )
)

param_info = list(
  b0   = list(mean = 0,    sd = 0.5,  a = -Inf, b = Inf),
  bint = list(mean = 2.5,  sd = 0.75, a = 0,    b = Inf),
  bext = list(mean = 1.75, sd = 0.5,  a = 0.5,  b = Inf),
  z    = list(mean = 1,    sd = 0.5,  a = 0.3,  b = 1.3),
  guess= list(mean = 0.5,  sd = 0.02, a = 0.4,  b = 0.6),
  bias1= list(mean = 0.05, sd = 0.02, a = 0,    b = 0.15),
  bias2= list(mean = 0.95, sd = 0.02, a = 0.85, b = 1)
)

df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv",
                                predicted_assign_path = "./results_data/model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv",
                                param_path = "./results_data/parameter_estimates/hbi/1234", 
                                param_mapping = param_mapping,
                                method_name = "hbi")

# TODO: get cm tables and sum/concatenate over all the datasets and by condition
# TODO: then visualise
