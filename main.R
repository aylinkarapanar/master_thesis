###############################################################################
################################ SET-UP #####################################
##############################################################################
packages = c("data.table", "caret", "dplyr", "stringr", "benchmarkme", "purrr", "tidyr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

initial_seed = 1234
n_rep = 50

# Store the seeds that will be used
seed_vector = seq(from = initial_seed, to = initial_seed + n_rep - 1)

# Create a df to keep track of the runs done
# runs = data.frame(
#  file   = character(),
#  bf_ps  = logical(),
#  waic   = logical(),
#  loo    = logical(),
#  hbi    = logical(),
#  stringsAsFactors = FALSE
# )

# Save it to CSV
# write.csv(runs, "runs.csv", row.names = FALSE)

###############################################################################
############################ DATA SIMULATION ##################################
##############################################################################

source("./data_simulation.R")

prevalence_list = list(
  equal = rep(1 / 7, 7),
  extreme = c(0.6, 0.15, 0.1, 0.05, 0.05, 0.025, 0.025)
)

simulation_conditions = expand.grid(
  n_participant = c(150, 300),
  n_items = c(60, 120),
  prevalence_type = names(prevalence_list),
  stringsAsFactors = FALSE
)

simulation_conditions$prevalence =
  prevalence_list[simulation_conditions$prevalence_type]

setDT(simulation_conditions)

# lapply(seed_vector, function(seed) {

#   set.seed(seed)

#   for (i in 1:nrow(simulation_conditions)) {
#   simulate_strategy_data(
#     seed = seed,
#     n_participants = simulation_conditions$n_participant[[i]],
#     n_items = simulation_conditions$n_items[[i]],
#     strategy_probs = simulation_conditions$prevalence[[i]],
#     condition = "all",
#     # Hyperparameters
#     #bint_mean = simulation_conditions$params[[i]]$bint_mean,
#     #bext_mean = simulation_conditions$params[[i]]$bext_mean,
#     #z_mean = simulation_conditions$params[[i]]$z_mean,]

#     # Describe the prevalence and parameter values for naming the files
#     prevalence_desc = simulation_conditions$prevalence_type[[i]],
#     #param_desc = simulation_conditions$params_type[[i]]
#   )
# }
# })

################################################################################
###################### Bayes Factor Estimation Using Product Space ##############
################################################################################

source("./BF_PS.R")

# Create time_df to store the runtime of the fucntion
# time_df = copy(simulation_conditions)

# time_df = simulation_conditions[rep(1:nrow(simulation_conditions), each = n_rep), ]
# time_df$prevalence = NULL
# time_df$seed = rep(seed_vector, times = nrow(simulation_conditions))
# time_df$total_runtime = NA
# time_df$no_of_cores = NA
# time_df$name = NA

# # Create the runtime csv
# write.csv(time_df, file = "./runtime/runtime_bf_ps.csv", row.names = FALSE)

# Get all the data files in the data folder
# data_files = list.files("./data", pattern = "_data\\.csv$", full.names = TRUE, recursive = TRUE)

# Apply the function to all the data files which were not already used
# lapply(data_files, function(f) {
#   runs = read.csv("runs.csv", stringsAsFactors = FALSE)

#   row_idx = which(runs$file == f)
#   already_done = any(runs$file == f & runs$bf_ps == TRUE, na.rm = TRUE)

#   if (!already_done) {
#     bf_ps(
#       data_file = f,
#       jags_text = "./JAGS_models/JAGS_hierarchical.txt",
#       n_iter    = 20000,
#       n_burnin  = 5000,
#       n_thin    = 100,
#       jags_seed = initial_seed
#     )
#     if (length(row_index)>0) {
#       runs$bf_ps[row_idx] = TRUE
#     } else {
#       runs = rbind(
#       runs,
#       data.frame(file = f, bf_ps = TRUE, waic = NA, loo = NA, hbi = NA))
#     }

#     write.csv(runs, "runs.csv", row.names = FALSE)
#   }
# })

################################################################################
########################### WAIC AND PSIS-LOO ##################################
################################################################################
source("./WAIC_and_PSIS.R")

# time_df = simulation_conditions[rep(1:nrow(simulation_conditions), each = n_rep), ]
# time_df$prevalence = NULL
# time_df$seed = rep(seed_vector, times = nrow(simulation_conditions))
# time_df$no_of_cores = NA
# time_df$name = NA

# write.csv(time_df, "./runtime/runtime_waic_loo.csv", row.names = FALSE)

# data_files = list.files("./data", pattern = "_data\\.csv$", full.names = TRUE, recursive = TRUE)
# data_files = rev(data_files)
# # List of models with their txt file paths and parameters
# models = list(
#   "internal" = list(
#     "file_path" = "./JAGS_models/JAGS_internal.txt",
#     "params" = c("b0", "b0mean", "b0sd",
#                  "bint", "bintmean", "bintsd",
#                  "loglik")
#   ),
#   "external" = list(
#     "file_path" =  "./JAGS_models/JAGS_external.txt",
#     "params" = c("bext", "bextmean", "bextsd",
#                  "loglik")
#   ),
#   "sequential" = list(
#     "file_path" =  "./JAGS_models/JAGS_sequential.txt",
#     "params" = c("b0", "b0mean", "b0sd",
#                  "bint", "bintmean", "bintsd",
#                  "bext", "bextmean", "bextsd",
#                  "z", "zmean", "zsd",
#                  "loglik")
#   ),
#   "integrative" = list(
#     "file_path" = "./JAGS_models/JAGS_integrative.txt",
#     "params" = c("b0", "b0mean", "b0sd",
#                  "bint", "bintmean", "bintsd",
#                  "bext", "bextmean", "bextsd",
#                  "loglik")

#   ),
#   "guess" = list(
#     "file_path" = "./JAGS_models/JAGS_guess.txt",
#     "params" = c("guess", "guessmean", "guesssd",
#                  "loglik")
#   ),
#   "bias1" = list(
#     "file_path" = "./JAGS_models/JAGS_bias1.txt",
#     "params" = c("bias1", "bias1mean", "bias1sd",
#                  "loglik")

#   ),
#   "bias2" = list(
#     "file_path" = "./JAGS_models/JAGS_bias2.txt",
#     "params" = c("bias2", "bias2mean", "bias2sd",
#                  "loglik")
#   )
# )

# lapply(data_files, function(f) {
#   runs = read.csv("runs.csv", stringsAsFactors = FALSE)

#   row_idx = which(runs$file == f)
#   already_done = any(runs$file == f & runs$waic == TRUE & runs$loo == TRUE, na.rm = TRUE)

#   if (already_done) {
#   cat("Already done:", f, "\n")}

#   if (!already_done) {
#     cat("Running: ", f, "\n")
#     model_selection_waic_loo(
#       data_file   = f,
#       model_list  = models,
#       n_iter      = 5000,
#       n_burnin    = 1000,
#       n_thin      = 5,
#       jags_seed   = initial_seed,
#       time_data_file = "./runtime/runtime_waic_loo.csv"
#     )}

#     if (length(row_idx) > 0) {
#       runs$waic[row_idx] = TRUE
#       runs$loo[row_idx] = TRUE
#     } else {
#     runs = rbind(
#       runs,
#       data.frame(file = f, bf_ps = NA, waic = TRUE, loo = TRUE, hbi = NA)
#   )}
#     write.csv(runs, "runs.csv", row.names = FALSE)
#   })

# ################################################################################
# ########################## RANDOM ASSIGNMENT  ##################################
# ################################################################################
source("./random_assignment.R")

data_files = list.files("./data",
  pattern = "_participants\\.csv$",
  full.names = TRUE, recursive = TRUE
)

generate_random_assignments(data_files,
  output_dir = "./results_data/model_assignments/random",
  seed = initial_seed
)

# ################################################################################
# ########################## CLASSIFICATION PERFORMANCES #########################
# ################################################################################
source("./metrics.R")

# Find all the simulated data files
data_files = list.files("./data", pattern = "_participants\\.csv$", full.names = TRUE, recursive = TRUE)

# Define the file path to the assignments
method_dirs = list(
  bf_ps  = "./results_data/model_assignments/bf_ps",
  psis   = "./results_data/model_assignments/PSIS-LOO",
  waic   = "./results_data/model_assignments/WAIC",
  hbi    = "./results_data/model_assignments/hbi",
  random = "./results_data/model_assignments/random"
)

performance_df = data.frame()

# Collect labels during the loop
all_labels = list()

for (true_path in data_files) {
  seed_name = basename(dirname(true_path))
  base_core = sub("_participants\\.csv$", "", basename(true_path))
  message("Calculating metrics for the file: ", true_path)

  # Find the strategy assignment files using method directories and true data file
  for (method_name in names(method_dirs)) {
    predicted_path = file.path(
      method_dirs[[method_name]], seed_name,
      paste0(base_core, "_strategy_assignments.csv")
    )

    if (file.exists(predicted_path)) {
      performance_df = bind_rows(
        performance_df,
        make_metrics_df(
          true_data_path = true_path,
          predicted_data_path = predicted_path,
          method_name = method_name,
          save_as_csv = TRUE,
          save_together = TRUE
        )
      )

      labs = get_labels(true_path, predicted_path, method_name)
      parts = str_split(base_core, "_")[[1]]

      labs$method = method_name
      labs$n_participants = as.numeric(parts[1])
      labs$n_items = as.numeric(parts[2]) / 3
      labs$prevalence = parts[3]
      labs$seed = seed_name

      all_labels = c(all_labels, list(labs))
    } else {
      # Report missing data files to see which method was not run on which files
      message("Missing file: ", predicted_path)
    }
  }
}

# Merge the collected labels
all_labels_df = bind_rows(all_labels)

if (!dir.exists("./metrics")) dir.create("./metrics", recursive = TRUE)
write.csv(all_labels_df, "./metrics/labels_all.csv", row.names = FALSE)

# #################################################################################
# ##################### PARAMETER ESTIMATIONS #####################################
# #################################################################################
source("./parameter.R")

# Find all the simulated data files
data_files = list.files("./data", pattern = "_participants\\.csv$", full.names = TRUE, recursive = TRUE)

# For now, we ignore models of non-interest and focus on 4 models of interest
# Mapping of which models uses which parameters
param_mapping = list(
  "internal"    = list("model_number" = 1, "params" = c("b0", "bint")),
  "external"    = list("model_number" = 2, "params" = c("bext")),
  "sequential"  = list("model_number" = 3, "params" = c("b0", "bint", "bext", "z")),
  "integrative" = list("model_number" = 4, "params" = c("b0", "bint", "bext"))
)

# Parameter estimates dirs
param_dirs = list(
  bf_ps = "./results_data/parameter_estimates/product_space",
  psis  = "./results_data/parameter_estimates/non_hierarchical/PSIS-LOO",
  waic  = "./results_data/parameter_estimates/non_hierarchical/WAIC",
  hbi   = "./results_data/parameter_estimates/hbi"
)

# Assignment dirs
assign_dirs = list(
  bf_ps = "./results_data/model_assignments/bf_ps",
  psis  = "./results_data/model_assignments/PSIS-LOO",
  waic  = "./results_data/model_assignments/WAIC",
  hbi   = "./results_data/model_assignments/hbi"
)

all_ind_params = list()

for (true_path in data_files) {
  seed_name = basename(dirname(true_path))
  base_core = sub("_participants\\.csv$", "", basename(true_path))
  message("Calculating param metrics for: ", true_path)

  for (method_name in names(param_dirs)) {
    predicted_path = file.path(
      assign_dirs[[method_name]], seed_name,
      paste0(base_core, "_strategy_assignments.csv")
    )

    param_path = if (method_name == "bf_ps") {
      file.path(
        param_dirs[[method_name]], seed_name,
        paste0(base_core, "_params.csv")
      )
    } else {
      file.path(param_dirs[[method_name]], seed_name)
    }

    if (!file.exists(predicted_path)) {
      message(" Missing assignment: ", predicted_path)
      next
    }
    if (!file.exists(param_path)) {
      message(" Missing param path: ", param_path)
      next
    }

    result = tryCatch(
      calculate_param_metrics(
        true_data_path = true_path,
        predicted_assign_path = predicted_path,
        param_path = param_path,
        method_name = method_name,
        param_mapping = param_mapping
      ),
      error = function(e) {
        message("  ERROR in ", method_name, " / ", base_core, ": ", e$message)
        data.frame()
      }
    )
    if (nrow(result) > 0) all_ind_params = c(all_ind_params, list(result))
  }
}

ind_params_df = bind_rows(all_ind_params)

if (!dir.exists("./metrics")) dir.create("./metrics", recursive = TRUE)
write.csv(ind_params_df, "./metrics/ind_params_all.csv", row.names = FALSE)

true_param_list = list(
  internal = list(b0 = 0, bint = 2.5),
  external = list(bext = 1.75),
  sequential = list(b0 = 0, bint = 2.5, bext = 1.75, z = 1),
  integrative = list(b0 = 0, bint = 2.5, bext = 1.75)
)

all_group_params = list()

group_param_dirs = list(
  bf_ps = "./results_data/parameter_estimates/product_space",
  psis  = "./results_data/parameter_estimates/non_hierarchical/PSIS-LOO",
  waic  = "./results_data/parameter_estimates/non_hierarchical/WAIC",
  hbi   = "./results_data/hbi_output"
)

for (true_path in data_files) {
  seed_name = basename(dirname(true_path))
  base_core = sub("_participants\\.csv$", "", basename(true_path))
  message("Calculating group param metrics for: ", true_path)

  for (method_name in names(group_param_dirs)) {
    param_path = if (method_name == "bf_ps") {
      file.path(group_param_dirs[[method_name]], seed_name, paste0(base_core, "_params.csv"))
    } else if (method_name == "hbi") {
      file.path(group_param_dirs[[method_name]], seed_name, base_core) # subdir named after base_core
    } else {
      file.path(group_param_dirs[[method_name]], seed_name)
    }

    if (!file.exists(param_path)) {
      message("  Missing param path: ", param_path)
      next
    }

    result = tryCatch(
      calculate_group_params(
        true_param_list = true_param_list,
        param_path      = param_path,
        method_name     = method_name,
        param_mapping   = param_mapping,
        base_core       = base_core,
        seed_name       = seed_name
      ),
      error = function(e) {
        message("  ERROR in ", method_name, " / ", base_core, ": ", e$message)
        data.frame()
      }
    )
    if (nrow(result) > 0) all_group_params = c(all_group_params, list(result))
  }
}

group_params_df = bind_rows(all_group_params)
write.csv(group_params_df, "./metrics/group_params_all.csv", row.names = FALSE)

# ################################################################################
# ########################## VISUALISATIONS ######################################
# ################################################################################
source("./visualisation.R")

metrics = c("accuracy", "precision", "recall", "f1")

# Boxplot for classification metrics
lapply(metrics, function(m) {
  plot_metric(
    data_path  = "./metrics/metrics_all.csv",
    metric     = m,
    output_dir = "./figures/metrics",
    width      = 10,
    height     = 6
  )
})

# Heatmap for confusion matrix of classifications
visualise_cm(
  all_labels_df = all_labels_df %>% filter(method != "random"),
  cm_output_dir = "./figures/cm_plots",
  inv_output_dir = "./figures/inv_plots",
  by_condition = TRUE,
  overall = TRUE
)

# ind_params_df = read.csv("./metrics/ind_params_all.csv")
bar_param_plot(ind_params_df, metric = "rmse", mode = "all", width = 16, height = 8, output_dir = "./figures/params/individual")
bar_param_plot(ind_params_df, metric = "rmse", mode = "by_model", width = 10, height = 6, output_dir = "./figures/params/individual")
bar_param_plot(ind_params_df, metric = "rmse", mode = "avg_model", width = 12, height = 6, output_dir = "./figures/params/individual")

bar_param_plot(ind_params_df, metric = "bias", mode = "all", width = 20, height = 10, output_dir = "./figures/params/individual")
bar_param_plot(ind_params_df, metric = "bias", mode = "by_model", width = 14, height = 8, output_dir = "./figures/params/individual")
bar_param_plot(ind_params_df, metric = "bias", mode = "avg_model", width = 14, height = 8, output_dir = "./figures/params/individual")

group_params_df = read.csv("./metrics/group_params_all.csv")

bar_param_plot(group_params_df, metric = "rmse", mode = "all", width = 16, height = 8, output_dir = "./figures/params/group")
bar_param_plot(group_params_df, metric = "rmse", mode = "by_model", width = 10, height = 6, output_dir = "./figures/params/group")
bar_param_plot(group_params_df, metric = "rmse", mode = "avg_model", width = 12, height = 6, output_dir = "./figures/params/group")

bar_param_plot(group_params_df, metric = "bias", mode = "all", width = 20, height = 10, output_dir = "./figures/params/group")
bar_param_plot(group_params_df, metric = "bias", mode = "by_model", width = 14, height = 8, output_dir = "./figures/params/group")
bar_param_plot(group_params_df, metric = "bias", mode = "avg_model", width = 14, height = 8, output_dir = "./figures/params/group")


# plot_param_ci(params_df = params_df)
runtime_list = list(
  bf_ps = "./runtime/runtime_bf_ps.csv",
  psis  = "./runtime/runtime_waic_loo.csv",
  waic  = "./runtime/runtime_waic_loo.csv",
  hbi   = "./runtime/runtime_hbi.csv"
)

# Load and combine all CSVs
all_runtime_dfs = lapply(names(runtime_list), function(method_name) {
  df = read.csv(runtime_list[[method_name]])
  df = df[df$prevalence_type != "empirical", ]

  if (method_name == "waic") {
    df$total_runtime = df$total_runtime_assign + df$total_runtime_refit_waic
  } else if (method_name == "psis") {
    df$total_runtime = df$total_runtime_assign + df$total_runtime_refit_loo
  }
  df$method = method_name
  df[, c("n_participant", "n_items", "prevalence_type", "seed", "total_runtime", "method")]
})

runtime_df = do.call(rbind, all_runtime_dfs)
runtime_df$runtime_min = runtime_df$total_runtime / 60

plot_runtime(runtime_df,
  width = 10,
  height = 6
)

# ##################################################################################
# ####################### NUMERICAL SUMMARIES #######################################
# ##################################################################################
runtime_df %>%
  group_by(method) %>%
  summarise(
    n = n(),
    mean = mean(runtime_min, na.rm = TRUE),
    sd = sd(runtime_min, na.rm = TRUE),
    median = median(runtime_min, na.rm = TRUE),
    q25 = quantile(runtime_min, 0.25, na.rm = TRUE),
    q75 = quantile(runtime_min, 0.75, na.rm = TRUE),
    iqr = IQR(runtime_min, na.rm = TRUE),
    min = min(runtime_min, na.rm = TRUE),
    max = max(runtime_min, na.rm = TRUE)
  ) %>%
  arrange(median)

runtime_df %>%
  group_by(method, prevalence, n_participants, n_items) %>%
  summarise(
    n = n(),
    mean = mean(runtime_min, na.rm = TRUE),
    sd = sd(runtime_min, na.rm = TRUE),
    median = median(runtime_min, na.rm = TRUE),
    q25 = quantile(runtime_min, 0.25, na.rm = TRUE),
    q75 = quantile(runtime_min, 0.75, na.rm = TRUE),

    min = min(runtime_min, na.rm = TRUE),
    max = max(runtime_min, na.rm = TRUE)
  ) %>%
  arrange(median)

print("Group-level Parameters")
group_params_df %>%
  mutate(param_label = paste0(model, "-", param_name)) %>%
  group_by(prevalence, n_participants, n_items, method, param_label) %>%
  summarise(
    mean_rmse = round(mean(rmse, na.rm = TRUE), 2),
    sd_rmse = round(sd(rmse, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(prevalence, n_participants, n_items, param_label, method) %>%
  print(n = Inf)

print("Individual Parameters")
ind_params_df %>%
  mutate(param_label = paste0(model, "-", param_name)) %>%
  group_by(prevalence, n_participants, n_items, method, param_label) %>%
  summarise(
    mean_rmse = round(mean(rmse, na.rm = TRUE), 2),
    sd_rmse = round(sd(rmse, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(prevalence, n_participants, n_items, param_label, method) %>%
  print(n = Inf)

print("Group-level Parameters")
group_params_df %>%
  group_by(method) %>%
  summarise(
    mean_rmse = round(mean(rmse, na.rm = TRUE), 2),
    sd_rmse   = round(sd(rmse, na.rm = TRUE), 2),
    .groups   = "drop"
  ) %>%
  print(n = Inf)

print("Individual Parameters")
ind_params_df %>%
  group_by(method, model) %>%
  summarise(
    mean_rmse = round(mean(rmse, na.rm = TRUE), 2),
    sd_rmse   = round(sd(rmse, na.rm = TRUE), 2),
    .groups   = "drop"
  ) %>%
  print(n = Inf)



performance_df %>%
  group_by(prevalence, n_participants, n_items, method) %>%
  summarise(
    mean_accuracy  = round(mean(accuracy, na.rm = TRUE), 2),
    sd_accuracy    = round(sd(accuracy, na.rm = TRUE), 2),
    mean_precision = round(mean(precision, na.rm = TRUE), 2),
    sd_precision   = round(sd(precision, na.rm = TRUE), 2),
    mean_recall    = round(mean(recall, na.rm = TRUE), 2),
    sd_recall      = round(sd(recall, na.rm = TRUE), 2),
    mean_f1        = round(mean(f1, na.rm = TRUE), 2),
    sd_f1          = round(sd(f1, na.rm = TRUE), 2),
    .groups        = "drop"
  ) %>%
  arrange(prevalence, n_participants, n_items, method) %>%
  print(n = Inf, width = Inf)

overall_results = all_labels_df %>%
  mutate(
    true = factor(true),
    predicted = factor(predicted, levels = levels(true))
  ) %>%
  group_by(method) %>%
  summarise(
    cm = list(
      confusionMatrix(
        data = predicted,
        reference = true,
        mode = "prec_recall"
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    overall_accuracy = map_dbl(cm, ~ .x$overall["Accuracy"]),
    mean_precision   = map_dbl(cm, ~ mean(.x$byClass[, "Precision"], na.rm = TRUE)),
    mean_recall      = map_dbl(cm, ~ mean(.x$byClass[, "Recall"], na.rm = TRUE)),
    mean_f1          = map_dbl(cm, ~ mean(.x$byClass[, "F1"], na.rm = TRUE))
  ) %>%
  mutate(across(
    c(overall_accuracy, mean_precision, mean_recall, mean_f1),
    ~ round(.x, 2)
  )) %>%
  select(method, overall_accuracy, mean_precision, mean_recall, mean_f1)

print(overall_results, width = Inf)

classification_results = all_labels_df %>%
  mutate(
    true = factor(true),
    predicted = factor(predicted, levels = levels(true))
  ) %>%
  group_by(method) %>%
  summarise(
    cm = list(
      confusionMatrix(
        data = predicted,
        reference = true,
        mode = "prec_recall"
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    by_class = map(cm, ~ {
      bc = as.data.frame(.x$byClass)
      bc$class = rownames(bc)

      tab = .x$table
      total = sum(tab)

      class_acc = map_dbl(rownames(tab), function(cl) {
        TP = tab[cl, cl]
        FP = sum(tab[, cl]) - TP
        FN = sum(tab[cl, ]) - TP
        TN = total - TP - FP - FN

        (TP + TN) / total
      })

      bc$class_accuracy = class_acc
      bc
    })
  ) %>%
  select(method, by_class) %>%
  unnest(by_class) %>%
  mutate(
    class_accuracy = round(class_accuracy, 2),
    Precision = round(Precision, 2),
    Recall = round(Recall, 2),
    F1 = round(F1, 2)
  ) %>%
  select(
    method,
    class,
    class_accuracy,
    Precision,
    Recall,
    F1
  )

print(classification_results, n = Inf, width = Inf)

# ##########################################################################################
# ############################## EMPIRICAL STUDY ##########################################
# ##########################################################################################
source("./BF_PS.R")
empirical_data = "./data/empirical_data_merged.csv"

bf_ps_empirical = bf_ps(data_file = empirical_data,
                        jags_text = "./JAGS_models/JAGS_hierarchical_ppc.txt",
                        n_iter    = 20000,
                        n_burnin  = 5000,
                        n_thin    = 100,
                        jags_seed = initial_seed
                      )
install.packages("bfw")
library(bfw)
install.packages("coda")
library(coda)

coda_chains = as.mcmc(bf_ps_empirical)

# Convergence diagnostics
DiagMCMC(data.MCMC = coda_chains, par.name = "b0mean")
DiagMCMC(data.MCMC = coda_chains, par.name = "fit")

pp_check_r2jags = function(samples,
                           observed  = "fit",
                           simulated = "fitnew",
                           xlab = "Observed discrepancy",
                           ylab = "Simulated discrepancy",
                           main = "Posterior Predictive Check") {
  
  sims = samples$BUGSoutput$sims.matrix
  
  # Extract posterior draws
  fit_obs = sims[, observed]
  fit_new = sims[, simulated]
  
  # Bayesian p-value: proportion of iterations where simulated > observed
  bp_value = mean(fit_new > fit_obs)
  
  # Plot
  lims = range(c(fit_obs, fit_new))
  
  plot(
    fit_obs, fit_new,
    xlim  = lims,
    ylim  = lims,
    xlab  = xlab,
    ylab  = ylab,
    main  = main,
    pch   = 16,
    col   = adjustcolor("steelblue", alpha.f = 0.3),
    cex   = 0.6,
    asp   = 1
  )
  
  abline(0, 1, col = "red", lwd = 2)   # diagonal: perfect fit line
  
  legend("topleft",
         legend = paste0("Bayesian p = ", round(bp_value, 3)),
         bty = "n",
         cex = 1.2)
  
  message(paste0("Bayesian p-value: ", round(bp_value, 3)))
  
  invisible(list(fit_obs  = fit_obs,
                 fit_new  = fit_new,
                 bp_value = bp_value))
}


params = c(
  "b0", "b0mean", "b0sd",
  "bint", "bintmean", "bintsd",
  "bext", "bextmean", "bextsd",
  "z", "zmean", "zsd",
  "guess", "guessmean", "guesssd",
  "bias1", "bias1mean", "bias1sd",
  "bias2", "bias2mean", "bias2sd",
  "strat", "r", "alpha",
  "fit", "fitnew"
)

pp_check_r2jags(bf_ps_empirical,
                observed  = "fit",
                simulated = "fitnew")



model_selection_waic_loo(
  data_file = empirical_data,
  model_list = models,
  n_iter = 5000,
  n_burnin = 1000,
  n_thin = 5,
  jags_seed = initial_seed,
  time_data_file = "./runtime/runtime_waic_loo.csv"
)

# ##########################################################################################
# ############################## DATA VIZ FOR EMPIRICAL STUDY ##########################################
# ##########################################################################################
source("./visualisation.R")

# Model Prevalence
model_prevalence(assign_files = list(
  bf_ps = "./results_data/model_assignments/bf_ps/empirical/empirical_strategy_assignments.csv",
  hbi   = "./results_data/model_assignments/hbi/empirical/empirical_data_merged_strategy_assignments.csv",
  psis  = "./results_data/model_assignments/PSIS-LOO/empirical/empirical_strategy_assignments.csv",
  waic  = "./results_data/model_assignments/WAIC/empirical/empirical_strategy_assignments.csv"
))

# Posterior Probailities
load_posterior_file = function(file, method_name) {
  read.csv(file) %>%
    mutate(participant_id = row_number()) %>%
    tidyr::pivot_longer(
      cols = -participant_id,
      names_to = "model",
      values_to = "proportion"
    ) %>%
    mutate(
      model = tolower(model),
      method = method_name
    )
}

posterior_df = bind_rows(
  load_posterior_file("./results_data/model_assignments/bf_ps/prob_strat/empirical/empirical_data_merged_prob_strat.csv", "BFPS"),
  load_posterior_file("results_data/model_assignments/hbi/prob_strat/empirical/empirical_data_merged_prob_strat.csv", "HBI")
)

posterior_proportion(
  posterior_df,
  output_dir = "./figures/models",
  width      = 10,
  height     = 6
)

# Parameters
ind_param_dirs = list(
  bf_ps = "./results_data/parameter_estimates/product_space/empirical/empirical_params.csv",
  hbi   = "./results_data/parameter_estimates/hbi/empirical",
  psis  = "./results_data/parameter_estimates/non_hierarchical/PSIS-LOO/empirical",
  waic  = "./results_data/parameter_estimates/non_hierarchical/WAIC/empirical"
)
assign_dirs = list(
  bf_ps = "./results_data/model_assignments/bf_ps/empirical/empirical_strategy_assignments.csv",
  hbi   = "./results_data/model_assignments/hbi/empirical/empirical_data_merged_strategy_assignments.csv",
  psis  = "./results_data/model_assignments/PSIS-LOO/empirical/empirical_strategy_assignments.csv",
  waic  = "./results_data/model_assignments/WAIC/empirical/empirical_strategy_assignments.csv"
)

param_mapping = list(
  "internal"    = list("model_number" = 1, "params" = c("b0", "bint")),
  "external"    = list("model_number" = 2, "params" = c("bext")),
  "sequential"  = list("model_number" = 3, "params" = c("b0", "bint", "bext", "z")),
  "integrative" = list("model_number" = 4, "params" = c("b0", "bint", "bext")),
  "guess"       = list("model_number" = 5, "params" = c("guess")),
  "bias1"       = list("model_number" = 6, "params" = c("bias1")),
  "bias2"       = list("model_number" = 7, "params" = c("bias2"))
)

group_param_dirs = list(
  bf_ps = "./results_data/parameter_estimates/product_space/empirical/empirical_params.csv",
  hbi   = "./results_data/hbi_output/empirical/empirical_data_merged/empirical_data_merged_full_output.pkl",
  psis  = "./results_data/parameter_estimates/non_hierarchical/PSIS-LOO/empirical",
  waic  = "./results_data/parameter_estimates/non_hierarchical/WAIC/empirical"
)

# hbi_samples = get_group_param_samples("hbi", group_param_dirs, param_mapping)
# bf_ps_samples = get_group_param_samples("bf_ps", group_param_dirs, param_mapping)
# waic_samples = get_group_param_samples("waic", group_param_dirs, param_mapping)
# psis_samples = get_group_param_samples("psis", group_param_dirs, param_mapping)

# Group level plots
plot_empirical_params(
  param_dirs    = group_param_dirs,
  param_mapping = param_mapping,
  level         = "group",
  output_dir    = "./figures/empirical_params/group"
)

# Individual level plots
plot_empirical_params(
  param_dirs    = ind_param_dirs,
  assign_dirs   = assign_dirs,
  param_mapping = param_mapping,
  level         = "individual",
  output_dir    = "./figures/empirical_params/individual"
)


group_param_dirs = list(
  bf_ps = "results_data/parameter_estimates/product_space/1234/150_180_equal_params.csv",
  hbi   = "results_data/hbi_output/1234/150_180_equal/150_180_equal_full_output.pkl",
  psis  = "results_data/parameter_estimates/non_hierarchical/PSIS-LOO/1234/150_180_equal",
  waic  = "results_data/parameter_estimates/non_hierarchical/WAIC/1234/150_180_equal"
)

plot_empirical_params(
  param_dirs    = group_param_dirs,
  param_mapping = param_mapping,
  level         = "group",
  output_dir    = "./figures/empirical_params/test",
  psis_waic_name = "150_180_equal_"
)
