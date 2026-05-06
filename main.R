###############################################################################
################################ SET-UP #####################################
##############################################################################

initial_seed = 1234
n_rep = 50

# Store the seeds that will be used
seed_vector = seq(from = initial_seed, to = initial_seed + n_rep - 1)

# Create a df to keep track of the runs done
# runs = data.frame(
#  file = character(),           # path to the data file
#  bf_ps = logical(),
#  waic = logical(),
#  loo = logical(),
#  hbi = logical(),
#  stringsAsFactors = FALSE
# )

# Save it to CSV
# write.csv(runs, "runs.csv", row.names = FALSE)


packages = c("data.table", "caret", "dplyr", "stringr", "benchmarkme")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)


###############################################################################
############################ DATA SIMULATION ##################################
##############################################################################

# source("./data_simulation.R")

prevalence_list = list(
  equal = rep(1 / 7, 7),
  # moderate = c(0.3, 0.2, 0.15, 0.15, 0.1, 0.05, 0.05),
  extreme = c(0.6, 0.15, 0.1, 0.05, 0.05, 0.025, 0.025)
)

# params_list = list(
#  high = list(
#    bint_mean = 3,
#    bext_mean = 3,
#    z_mean = 0.8
#  ),
#  moderate = list(
#    bint_mean = 1.5,
#    bext_mean = 1.5,
#    z_mean = 0.8
#  ),
#  low = list(
#    bint_mean = 0.5,
#    bext_mean = 0.5,
#    z_mean = 0.2
#  )
# )

simulation_conditions = expand.grid(
  n_participant = c(150, 300),
  n_items = c(60, 120),
  prevalence_type = names(prevalence_list),
  # params_type = names(params_list),
  stringsAsFactors = FALSE
)

simulation_conditions$prevalence =
  prevalence_list[simulation_conditions$prevalence_type]

# simulation_conditions$params =
#  params_list[simulation_conditions$params_type]


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

# TODO: IMPORTANT jags parallel does not use all the cores, n.cores used = n.chains
# source("./BF_PS.R")

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
# source("./WAIC_and_PSIS.R")

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
#       data_file = f,
#       model_list = models,
#       n_iter = 5000,
#       n_burnin = 1000,
#       n_thin = 5,
#       jags_seed = initial_seed,
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
# ########################## CLASSIFICATION PERFORMANCES #########################
# ################################################################################
source("./metrics.R")

# Find all the simulated data files
data_files = list.files("./data", pattern = "_participants\\.csv$", full.names = TRUE, recursive = TRUE)

# Define the file path to the assignments
method_dirs = list(
  bf_ps = "./results_data/model_assignments/bf_ps",
  psis = "./results_data/model_assignments/PSIS-LOO",
  waic = "./results_data/model_assignments/WAIC",
  hbi = "./results_data/model_assignments/hbi"
)

performance_df = data.frame()

# Collect labels during the loop
all_labels = list()

for (true_path in data_files) {
  seed_name = basename(dirname(true_path))
  base_core = sub("_participants\\.csv$", "", basename(true_path))
  message("Calculating metrics for the file: ", true_path)

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
      message("Missing file: ", predicted_path)
    }
  }
}

all_labels_df = bind_rows(all_labels)

# #################################################################################
# ##################### PARAMETER ESTIMATIONS #####################################
# #################################################################################
source("./parameter.R")

# param_info = list(
#   b0 = list(mean = 0, sd = 0.5, a = -Inf, b = Inf),
#   bint = list(mean = 2.5, sd = 0.75, a = 0, b = Inf),
#   bext = list(mean = 1.75, sd = 0.5, a = 0.5, b = Inf),
#   z = list(mean = 1, sd = 0.5, a = 0.3, b = 1.3),
#   guess = list(mean = 0.5, sd = 0.02, a = 0.4, b = 0.6),
#   bias1 = list(mean = 0.05, sd = 0.02, a = 0, b = 0.15),
#   bias2 = list(mean = 0.95, sd = 0.02, a = 0.85, b = 1)
# )


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

all_params = list()

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
        true_data_path        = true_path,
        predicted_assign_path = predicted_path,
        param_path            = param_path,
        method_name           = method_name,
        param_mapping         = param_mapping
      ),
      error = function(e) {
        message("  ERROR in ", method_name, " / ", base_core, ": ", e$message)
        data.frame()
      }
    )
    if (nrow(result) > 0) all_params = c(all_params, list(result))
  }
}

params_df = bind_rows(all_params)

if (!dir.exists("./metrics")) dir.create("./metrics", recursive = TRUE)
write.csv(params_df, "./metrics/params_all.csv", row.names = FALSE)

# df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv",
#                                 predicted_assign_path = "./model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv",
#                                 param_path = "./parameter_estimates/hbi/1234",
#                                 param_mapping = param_mapping)

# ################################################################################
# ########################## VISUALISATIONS ######################################
# ################################################################################
source("./visualisation.R")

metrics = c("accuracy", "precision", "recall", "f1")

# Boxplot for classification metrics
lapply(metrics, function(m) {
  plot_metric(
    data_path = "./metrics/metrics_all.csv",
    metric = m,
    output_dir = "./figures/metrics",
    width = 8, 
    height = 6
  )
})

# Heatmap for confusion matrix of classifications
visualise_cm(all_labels_df,
  output_dir = "./figures/cm_plots",
  by_condition = TRUE,
  overall = TRUE
)

# Scatter plot for parameter estimates
scatter_params(params_df, output_dir = "./figures/params")

bar_rmse(params_df, width = 20, height = 12)

bar_bias(params_df, width = 20, height = 12)

params_df %>%
  mutate(param_label = paste0(model, "-", param_name)) %>%
  group_by(prevalence, n_participants, n_items, method, param_label) %>%
  summarise(
    mean_rmse = round(mean(rmse, na.rm = TRUE), 3),
    sd_rmse = round(sd(rmse, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(prevalence, n_participants, n_items, param_label, method) %>%
  print(n = Inf)

params_df %>% 
  group_by(method) %>%
  summarise(
    mean_rmse = round(mean(rmse, na.rm = TRUE), 3),
    sd_rmse = round(sd(rmse, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print(n = Inf)



performance_df %>%
  group_by(prevalence, n_participants, n_items, method) %>%
  summarise(
    mean_accuracy = round(mean(accuracy, na.rm = TRUE), 3),
    sd_accuracy = round(sd(accuracy, na.rm = TRUE), 3),
    mean_precision = round(mean(precision, na.rm = TRUE), 3),
    sd_precision = round(sd(precision, na.rm = TRUE), 3),
    mean_recall = round(mean(recall, na.rm = TRUE), 3),
    sd_recall = round(sd(recall, na.rm = TRUE), 3),
    mean_f1 = round(mean(f1, na.rm = TRUE), 3),
    sd_f1 = round(sd(f1, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(prevalence, n_participants, n_items, method) %>%
  print(n = Inf, width = Inf)
