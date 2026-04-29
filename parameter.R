# First, get the labels for the individuals and the corresponding parameters 
# Second, get the true individual parameters from the true model or assigned model?
# Or only look at correctly assigned models' params?
# Calculate RMSE and CI coverage
packages = c("caret", "stringr", "DescTools")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

source("./metrics.R")

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

# TODO: adjust this for truncated distributions 
# Helper function to calculate the CI
get_ci_bounds = function(param_name, param_info, level = 0.95) {
  info = param_info[[param_name]]
  
  z = qnorm((1 + level) / 2)
  
  lower = info$mean - z * info$sd
  upper = info$mean + z * info$sd
  
  # apply truncation
  lower = max(lower, info$a)
  upper = min(upper, info$b)
  
  return(c(lower, upper))
}

# TODO: add product space and non-hierarchical param estimations
# TODO: for PS should you take the mean of the param values over the iterations of MCMC?
calculate_param_metrics = function(
                                   true_data_path, 
                                   predicted_assign_path,
                                   param_path,
                                   method_name = "hbi",
                                   param_mapping) {

  results_list = list()
  
  seed_name = basename(dirname(predicted_assign_path)) 
  base = basename(predicted_assign_path)
  base = str_remove(base, "\\_strategy_assignments.csv$")
  parts = str_split(base, "_")[[1]]
  
  n_participant  = as.numeric(parts[1])
  n_items        = as.numeric(parts[2]) / 3
  prevalence_type = parts[3]
  
  true_data = read.csv(true_data_path)
  labels = get_labels(true_data_path, predicted_assign_path, method_name)
  correct_idx = which(labels$true == labels$predicted)

  if (method_name == "hbi"){


  }
  # Loop over models
  for (model_label in names(param_mapping)) {
    # print(model_label)
    model_info   = param_mapping[[model_label]]
    model_number = model_info$model_number
    model_params = model_info$params
    
    model_idx = which(as.numeric(labels$true) == model_number)
    model_correct_idx = intersect(model_idx, correct_idx)
    
    # print(model_correct_idx)
    if (length(model_correct_idx) == 0) next
    
    # Find and read the parameter estimation
    params_file = file.path(param_path, paste0(base, "_params_", model_number, ".csv"))
    if (!file.exists(params_file)) next
    params_data = read.csv(params_file, header = TRUE)

    print(params_file)
    
    colnames(params_data) = model_params

    print(head(params_data))
    print(dim(params_data))
    print(model_correct_idx)
    # Loop over the correctly identified individuals
    for (i in model_correct_idx) {
      for (param_name in model_params) {
        
        if (!(param_name %in% colnames(true_data))) next
        
        pred_val = params_data[i, param_name]

        print(pred_val)

        true_val = as.numeric(true_data[i, param_name])
        pred_val = as.numeric(pred_val)

        rmse_val = caret::RMSE(true_val, pred_val)

        ci_bounds = get_ci_bounds(param_name, param_info, level = 0.95)
        print(ci_bounds)
        ci_lower = ci_bounds[1]
        ci_upper = ci_bounds[2]
        
        inside_ci = pred_val >= ci_lower & pred_val <= ci_upper
        
        results_list[[length(results_list) + 1]] = data.frame(
          seed = seed_name,
          n_participants = n_participant,
          n_items = n_items,
          prevalence = prevalence_type,
          method = method_name,
          model = model_label,
          individual_id = i,
          param_name = param_name,
          true_value = true_data[i, param_name],
          predicted_value = pred_val,
          rmse = rmse_val,
          within_ci = inside_ci,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(results_list) == 0) return(data.frame())
  results_df = do.call(rbind, results_list)
  return(results_df)
}

# df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv", 
#                                 predicted_data_path = "./model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv",
#                                 param_path = "./parameter_estimates/hbi/1234", 
#                                 param_mapping = param_mapping)

# head(df)
# tail(df)
