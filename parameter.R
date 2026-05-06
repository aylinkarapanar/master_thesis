packages = c("caret", "stringr", "DescTools", "HDInterval")

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
  b0 = list(mean = 0, sd = 0.5, a = -Inf, b = Inf),
  bint = list(mean = 2.5, sd = 0.75, a = 0, b = Inf),
  bext = list(mean = 1.75, sd = 0.5, a = 0.5, b = Inf),
  z = list(mean = 1, sd = 0.5, a = 0.3, b = 1.3),
  guess = list(mean = 0.5, sd = 0.02, a = 0.4, b = 0.6),
  bias1 = list(mean = 0.05, sd = 0.02, a = 0, b = 0.15),
  bias2 = list(mean = 0.95, sd = 0.02, a = 0.85, b = 1)
)

get_ci_bounds = function(samples, level = 0.95) {
  h = HDInterval::hdi(samples, credMass = level)
  c(lower = h[1], upper = h[2])
}

# Helper function to calculate parameter values for product space method
get_bf_ps_params = function(data, predicted_assign_path, model_params) {
  # data = read.csv(param_path, header = TRUE)
  strat = read.csv(predicted_assign_path, header = TRUE)
  # Convert the strategy assignment into binary
  # where the assigned strategy is the most common one in the column
  # if it matches to the current cell 1, if not 0
  strat_binary = as.data.frame(apply(strat, 2, function(col) {
    assigned_label = Mode(col)
    as.integer(col == assigned_label)
  }))

  # print(head(strat_binary))
  # print(dim(strat_binary))
  split_names = strsplit(names(data), ".", fixed = TRUE)
  param_part = sapply(split_names, "[", 1)
  index_part = as.integer(sapply(split_names, "[", 2))
  keep = param_part %in% model_params

  n_participants = max(index_part[keep])
  params_data = matrix(NA, nrow = n_participants, ncol = length(model_params))
  colnames(params_data) = model_params

  for (k in which(keep)) {
    idx = index_part[k]
    param = param_part[k]
    # print(head(strat_binary[, idx]))
    # print(head(data[, k]))

    # Take the mean of the cells of parameter values where the stratgy was correctly assigned cells
    value = mean(strat_binary[, idx] * data[, k], na.rm = TRUE)
    params_data[idx, param] = value
  }

  # print(head(params_data))
  return(params_data)
}


# Helper function to process csv files with parameter values
get_params_data = function(params_file, predicted_assign_path = NULL, method_name, model_params) {
  data = read.csv(params_file, header = TRUE)

  if (method_name == "hbi") {
    params_data = data
    colnames(params_data) = model_params
  } else if (method_name %in% c("psis", "waic")) {
    # Average over MCMC iterations, then reshape to participants x params
    means = colMeans(data)

    # Split the column names to parameter name and index
    split_names = strsplit(names(means), ".", fixed = TRUE)
    param_part = sapply(split_names, "[", 1)
    index_part = as.integer(sapply(split_names, "[", 2))

    # Keep only columns belonging to the defined model's params (exclude parameter mean, sd or loglik)
    keep = param_part %in% model_params
    n_participants = max(index_part[keep])
    params_data = as.data.frame(matrix(NA, nrow = n_participants, ncol = length(model_params)))
    colnames(params_data) = model_params

    # Fill in each value at the right [participant, param] position
    for (k in which(keep)) {
      params_data[index_part[k], param_part[k]] = means[k]
    }
  } else if (method_name == "bf_ps") {
    # separate function would be better to keep it clean here
    params_data = get_bf_ps_params(data, predicted_assign_path, model_params)
  }

  return(params_data)
}


# get_params_data(param_path = "./results_data/parameter_estimates/product_space/1234/150_180_equal_params.csv",
#               predicted_assign_path = "./results_data/model_assignments/posterior_prob/1234/150_180_equal_strategy_assignments.csv",
#               model_params = c("b0", "bint"), method_name = "ps")

calculate_param_metrics = function(true_data_path,
                                    predicted_assign_path,
                                    param_path,
                                    method_name,
                                    param_mapping) {
  results_list = list()

  seed_name = basename(dirname(predicted_assign_path))
  base = basename(predicted_assign_path)
  base = str_remove(base, "\\_strategy_assignments.csv$")
  parts = str_split(base, "_")[[1]]

  n_participant = as.numeric(parts[1])
  n_items = as.numeric(parts[2]) / 3
  prevalence_type = parts[3]

  true_data = read.csv(true_data_path)
  labels = get_labels(true_data_path, predicted_assign_path, method_name)
  correct_idx = which(labels$true == labels$predicted)

  # Loop over models
  for (model_label in names(param_mapping)) {
    # print(model_label)
    model_info = param_mapping[[model_label]]
    model_number = model_info$model_number
    model_params = model_info$params

    model_idx = which(as.numeric(labels$true) == model_number)
    model_correct_idx = intersect(model_idx, correct_idx)

    # print(model_correct_idx)
    if (length(model_correct_idx) == 0) next

    # Find and read the parameter estimation
    params_file = if (method_name == "hbi") {
      file.path(param_path, paste0(base, "_params_", model_number, ".csv"))
    } else if (method_name %in% c("psis", "waic")) {
      file.path(param_path, paste0(base, "_", model_label, ".csv"))
    } else if (method_name == "bf_ps") {
      param_path
    }

    # print(params_file)
    # print(file.exists(params_file))
    if (!file.exists(params_file)) next

    params_data = get_params_data(
      params_file = params_file,
      predicted_assign_path = predicted_assign_path,
      method_name = method_name,
      model_params = model_params
    )


    # print(head(params_data))
    # print(dim(params_data))
    # print(model_correct_idx)

    # Loop over the correctly identified individuals
    for (i in model_correct_idx) {
      # For HBI, use the global index since params_data has all the participants
      # For PSIS/WAIC, use local index, since params_data has only model-assigned participants
      row_idx = if (method_name %in% c("hbi", "bf_ps")) i else which(model_idx == i)

      for (param_name in model_params) {
        if (!(param_name %in% colnames(true_data))) next
        # Not correct!!! use the full MCMC samples
        # For HBI consider processing pkl files

        # samples = params_data[, param_name]
        # ci_bounds = get_ci_bounds(samples, level = 0.95)

        pred_val = as.numeric(params_data[row_idx, param_name])
        true_val = as.numeric(true_data[i, param_name])

        if (is.na(pred_val) || is.na(true_val)) next

        rmse_val = caret::RMSE(pred_val, true_val)
        # inside_ci = pred_val >= ci_bounds[1] & pred_val <= ci_bounds[2]

        results_list[[length(results_list) + 1]] = data.frame(
          seed = seed_name,
          n_participants = n_participant,
          n_items = n_items,
          prevalence = prevalence_type,
          method = method_name,
          model = model_label,
          individual_id = i,
          param_name = param_name,
          true_value = true_val,
          predicted_value = pred_val,
          rmse = rmse_val,
          # within_ci = inside_ci,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(results_list) == 0) {
    return(data.frame())
  }
  results_df = do.call(rbind, results_list)
  return(results_df)
}

# df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv",
#                                 predicted_assign_path = "./model_assignments/hbi/1234/150_180_equal_strategy_assignments.csv",
#                                 param_path = "./parameter_estimates/hbi/1234",
#                                 param_mapping = param_mapping)


# df = calculate_param_metrics(true_data_path = "./data/1234/150_180_equal_participants.csv",
#                               predicted_assign_path = "./results_data/model_assignments//1234/150_180_equal_strategy_assignments.csv",
#                               param_path = "./results_data/parameter_estimates/product_space/1234/150_180_equal_params.csv",
#                               param_mapping = param_mapping,
#                               method_name = "bf_ps")


# get_params_data(param_path = "./results_data/parameter_estimates/product_space/1234/150_180_equal_params.csv",
#               ,
#               model_params = c("b0", "bint"), method_name = "bf_ps")

# head(df)
# tail(df)
