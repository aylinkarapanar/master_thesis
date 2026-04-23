###############################################################################
################################ SET-UP #####################################
##############################################################################

# TODO: rename posterior prob as BF_PS
initial_seed = 1234
n_rep = 50

# Store the seeds that will be used
seed_vector = seq(from = initial_seed, to = initial_seed + n_rep - 1)

# Create a df to keep track of the runs done
#runs = data.frame(
#  file = character(),           # path to the data file
#  posterior_prob = logical(),   # TRUE if posterior probability was calculated
#  waic = numeric(),             # placeholder for WAIC
#  loo = numeric(),              # placeholder for LOO
#  hbi = numeric(),              # placeholder for HBI
#  stringsAsFactors = FALSE
#)

# Save it to CSV
#write.csv(runs, "runs.csv", row.names = FALSE)


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

#params_list = list(
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
#)

simulation_conditions = expand.grid(
  n_participant = c(150, 300),
  n_items = c(60, 120),
  prevalence_type = names(prevalence_list),
  #params_type = names(params_list),
  stringsAsFactors = FALSE
)

simulation_conditions$prevalence =
  prevalence_list[simulation_conditions$prevalence_type]

#simulation_conditions$params =
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
######################## POSTERIOR PROBABILITIES ###############################
################################################################################

# TODO: IMPORTANT jags parallel does not use all the cores, n.cores used = n.chains
# source("./posterior_probabilities.R")

# Create time_df to store the runtime of the fucntion
# time_df = copy(simulation_conditions)

# time_df = simulation_conditions[rep(1:nrow(simulation_conditions), each = n_rep), ]
# time_df$prevalence = NULL
# time_df$seed = rep(seed_vector, times = nrow(simulation_conditions))
# time_df$total_runtime = NA
# time_df$no_of_cores = NA
# time_df$name = NA

# # Create the runtime csv 
# write.csv(time_df, file = "./runtime/runtime_posterior_prob.csv", row.names = FALSE)

# Get all the data files in the data folder
# data_files = list.files("./data", pattern = "_data\\.csv$", full.names = TRUE, recursive = TRUE)

# Apply the function to all the data files which were not already used
# lapply(data_files, function(f) {
#   runs = read.csv("runs.csv", stringsAsFactors = FALSE)
  
#   row_idx = which(runs$file == f)
#   already_done = any(runs$file == f & runs$posterior_prob == TRUE, na.rm = TRUE)

#   if (!already_done) {
#     posterior_probability(
#       data_file = f,
#       jags_text = "./JAGS_models/JAGS_hierarchical.txt",
#       n_iter    = 20000,
#       n_burnin  = 5000,
#       n_thin    = 100,
#       jags_seed = initial_seed
#     )
#     if (length(row_index)>0) {
#       runs$posterior_prob[row_idx] = TRUE
#     } else {
#       runs = rbind(
#       runs,
#       data.frame(file = f, posterior_prob = TRUE, waic = NA, loo = NA, hbi = NA))
#     }
  
#     write.csv(runs, "runs.csv", row.names = FALSE)
#   }
# })

################################################################################
########################### WAIC AND PSIS-LOO ##################################
################################################################################
# source("./WAIC_and_PSIS.R")

#time_df = simulation_conditions[rep(1:nrow(simulation_conditions), each = n_rep), ]
#time_df$prevalence = NULL
#time_df$seed = rep(seed_vector, times = nrow(simulation_conditions))
#time_df$no_of_cores = NA
#time_df$name = NA

#write.csv(time_df, "./runtime/runtime_waic_loo.csv", row.names = FALSE)

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
#       data.frame(file = f, posterior_prob = NA, waic = TRUE, loo = TRUE, hbi = NA)
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
method_dirs = list(posterior_prob = "./results_data/model_assignments/posterior_prob/",
                   psis = "./results_data/model_assignments/PSIS-LOO/",
                   waic = "./results_data/model_assignments/WAIC/",
                   hbi = "./results_data/model_assignments/hbi")
                   
performance_df = data.frame()

for (true_path in data_files) {
  seed_name = basename(dirname(true_path)) 
  base_core = sub("_participants\\.csv$", "", basename(true_path))
  message("Calculating metrics for the file: ", true_path)

  for (method_name in names(method_dirs)) {
    predicted_path = file.path(method_dirs[[method_name]], seed_name,
                               paste0(base_core, "_strategy_assignments.csv"))
    
    if (file.exists(predicted_path)) {
      performance_df = bind_rows(
        performance_df,
        make_metrics_df(
          true_data_path = true_path,
          predicted_data_path = predicted_path,
          method_name = method_name,
          save_as_csv = TRUE,
          save_together = TRUE
        ))
      
    } else {
      message("Missing file: ", predicted_path)
    }
  }
}


# ################################################################################
# ########################## VISUALISATIONS ######################################
# ################################################################################
source("./visualisation.R")

metrics = c("accuracy","precision","recall","f1")

lapply(metrics, function(m) {
  plot_metric(data_path = "./metrics/metrics_all.csv",
              metric = m)
})



# TODO: overall confusion matrix

# cm = confusionMatrix(
#   data = predicted_labels$strat_label,
#   reference = true_labels$strat_label,
#   mode = "prec_recall"
# )

# cm_df = as.data.frame(as.table(cm$table))
# colnames(cm_df) = c("Prediction", "Reference", "Freq")

# cm_plot = ggplot(cm_df, aes(
#   x = factor(Reference, levels = 1:7),
#   y = factor(Prediction, levels = 7:1),
#   fill = Freq
# )) +
#   geom_tile() +
#   geom_text(aes(label = Freq)) +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme_minimal() +
#   labs(title = paste("Confusion Matrix"),
#        x = "True",
#        y = "Predicted")
