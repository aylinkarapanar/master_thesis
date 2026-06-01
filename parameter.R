packages = c("caret", "stringr", "DescTools", "bayestestR", "reticulate")
lapply(packages, library, character.only = TRUE)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

source("./metrics.R")
source_python("eat_the_pickle.py")

# get_samples = function(method_name, param_path, base, model_number, model_label, model_params, i) {
#   if (method_name == "hbi") {
#     f = file.path(param_path, "samples", paste0(base, "_posterior_", model_number, ".csv"))
#     if (!file.exists(f)) return(NULL)
#     data = read.csv(f)
#     col  = paste0(model_params[1], ".", i)  
#     if (!(col %in% colnames(data))) return(NULL)
#     return(data)

#   } else if (method_name == "bf_ps") {
#     f = param_path 
#     if (!file.exists(f)) return(NULL)
#     return(read.csv(f))

#   } else if (method_name %in% c("psis", "waic")) {
#     f = file.path(param_path, paste0(base, "_", model_label, ".csv"))
#     if (!file.exists(f)) return(NULL)
#     return(read.csv(f))
#   }
# }

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
    params_data = get_bf_ps_params(data, predicted_assign_path, model_params)
  }

  return(params_data)
}

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

    if (!file.exists(params_file)) next

    params_data = get_params_data(
      params_file = params_file,
      predicted_assign_path = predicted_assign_path,
      method_name = method_name,
      model_params = model_params
    )

    # Loop over the correctly identified individuals
    for (i in model_correct_idx) {
      # For HBI, use the global index since params_data has all the participants
      # For PSIS/WAIC, use local index, since params_data has only model-assigned participants
      row_idx = if (method_name %in% c("hbi", "bf_ps")) i else which(model_idx == i)

      for (param_name in model_params) {
        if (!(param_name %in% colnames(true_data))) next

        pred_val = as.numeric(params_data[row_idx, param_name])
        true_val = as.numeric(true_data[i, param_name])

        if (is.na(pred_val) || is.na(true_val)) next

        # samples_data = get_samples(method_name, param_path, base,
        #                           model_number, model_label, model_params, i)
        # if (is.null(samples_data)) next

        # Get the column for this participant x parameter
        col = paste0(param_name, "[", i, "]")
        # if (!(col %in% colnames(samples_data))) next
        # samples = samples_data[[col]]
        # samples = samples[!is.na(samples)]

        # pred_val = mean(samples)
        # ci = bayestestR::ci(samples, method = "HDI", ci = 0.95)

        rmse_val = caret::RMSE(pred_val, true_val)

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
          # ci_lower = ci$CI_low,
          # ci_upper = ci$CI_high,
          # within_ci = true_val >= ci$CI_low & true_val <= ci$CI_high,
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

error = function(e) {
  message("  ERROR in ", method_name, " / ", base_core, ": ", e$message)
  data.frame()
}

calculate_group_params = function(true_param_list,
                                  param_path,        
                                  method_name,
                                  param_mapping,
                                  base_core = NULL,
                                  n_samples = 1000) {
  results = list()

  if (method_name == "hbi") {
    file_path   = file.path(param_path, paste0(base_core,  "_full_output.pkl"))
    pickle_data = eat_the_pickle(file_path)
    mean_list   = pickle_data$group_mean

    est_data = lapply(names(param_mapping), function(model_name) {
      model_idx = param_mapping[[model_name]]$model_number
      params    = param_mapping[[model_name]]$params
      means     = as.numeric(mean_list[[model_idx]])
      df        = as.data.frame(matrix(means, nrow = 1))
      colnames(df) = params
      df
    })
    names(est_data) = names(param_mapping)

  } else if (method_name == "bf_ps") {
    param_df = read.csv(param_path)  

    est_data = lapply(names(param_mapping), function(model_name) {
      params    = param_mapping[[model_name]]$params
      col_names = paste0(params, "mean")
      cols      = col_names[col_names %in% colnames(param_df)]
      if (length(cols) == 0) return(list())
      df = as.data.frame(as.list(colMeans(param_df[, cols, drop = FALSE], na.rm = TRUE)))
      colnames(df) = params[col_names %in% colnames(param_df)]
      df
    })
    names(est_data) = names(param_mapping)

  } else if (method_name %in% c("waic", "psis")) {
    est_data = lapply(names(param_mapping), function(model_name) {
      suppressWarnings(tryCatch({
        file_path = file.path(param_path, paste0(base_core, "_", model_name, ".csv"))
        param_df  = read.csv(file_path)
        params    = param_mapping[[model_name]]$params
        col_names = paste0(params, "mean")
        cols      = col_names[col_names %in% colnames(param_df)]
        if (length(cols) == 0) return(list())
        df = as.data.frame(as.list(colMeans(param_df[, cols, drop = FALSE], na.rm = TRUE)))
        colnames(df) = params[col_names %in% colnames(param_df)]
        df
      }, error = function(e) {
        message(sprintf("  Skipping model '%s': no file found", model_name))
        list()
      }))
    })
    names(est_data) = names(param_mapping)

  } else {
    stop(sprintf("Unknown method_name: '%s'", method_name))
  }

  for (model_name in names(param_mapping)) {
    if (!(model_name %in% names(est_data)))        next
    if (!(model_name %in% names(true_param_list))) next
    est_vals  = est_data[[model_name]]
    true_vals = true_param_list[[model_name]]
    if (length(est_vals) == 0) next

    for (param in names(true_vals)) {
      if (!(param %in% colnames(est_vals))) next
      pred = as.numeric(est_vals[[param]])
      true = as.numeric(true_vals[[param]])
      results[[length(results) + 1]] = data.frame(
        # seed            = seed_name,
        # n_participants  = n_participant,
        # n_items         = n_items,
        # prevalence      = prevalence_type,
        method          = method_name,
        model           = model_name,
        param_name      = param,
        true_value      = true,
        predicted_value = pred,
        rmse            = sqrt((pred - true)^2),
        bias            = pred - true
      )
    }
  }

  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}

calculate_group_params = function(true_param_list,
                                  param_path,
                                  method_name,
                                  param_mapping,
                                  base_core = NULL,
                                  seed_name = NULL,
                                  n_samples = 1000) {
  results = list()

  # Parse metadata from base_core the same way as calculate_param_metrics
  parts           = str_split(base_core, "_")[[1]]
  n_participant   = as.numeric(parts[1])
  n_items         = as.numeric(parts[2]) / 3
  prevalence_type = parts[3]
  seed_name       = if (is.null(seed_name)) NA_character_ else seed_name

  if (method_name == "hbi") {
    file_path   = file.path(param_path, paste0(base_core, "_full_output.pkl"))
    pickle_data = eat_the_pickle(file_path)
    mean_list   = pickle_data$group_mean

    est_data = lapply(names(param_mapping), function(model_name) {
      model_idx = param_mapping[[model_name]]$model_number
      params    = param_mapping[[model_name]]$params
      means     = as.numeric(mean_list[[model_idx]])
      df        = as.data.frame(matrix(means, nrow = 1))
      colnames(df) = params
      df
    })
    names(est_data) = names(param_mapping)

  } else if (method_name == "bf_ps") {
    param_df = read.csv(param_path)

    est_data = lapply(names(param_mapping), function(model_name) {
      params    = param_mapping[[model_name]]$params
      col_names = paste0(params, "mean")
      cols      = col_names[col_names %in% colnames(param_df)]
      if (length(cols) == 0) return(list())
      df = as.data.frame(as.list(colMeans(param_df[, cols, drop = FALSE], na.rm = TRUE)))
      colnames(df) = params[col_names %in% colnames(param_df)]
      df
    })
    names(est_data) = names(param_mapping)

  } else if (method_name %in% c("waic", "psis")) {
    est_data = lapply(names(param_mapping), function(model_name) {
      suppressWarnings(tryCatch({
        file_path = file.path(param_path, paste0(base_core, "_", model_name, ".csv"))
        param_df  = read.csv(file_path)
        params    = param_mapping[[model_name]]$params
        col_names = paste0(params, "mean")
        cols      = col_names[col_names %in% colnames(param_df)]
        if (length(cols) == 0) return(list())
        df = as.data.frame(as.list(colMeans(param_df[, cols, drop = FALSE], na.rm = TRUE)))
        colnames(df) = params[col_names %in% colnames(param_df)]
        df
      }, error = function(e) {
        message(sprintf("  Skipping model '%s': no file found", model_name))
        list()
      }))
    })
    names(est_data) = names(param_mapping)

  } else {
    stop(sprintf("Unknown method_name: '%s'", method_name))
  }

  for (model_name in names(param_mapping)) {
    if (!(model_name %in% names(est_data)))        next
    if (!(model_name %in% names(true_param_list))) next
    est_vals  = est_data[[model_name]]
    true_vals = true_param_list[[model_name]]
    if (length(est_vals) == 0) next

    for (param in names(true_vals)) {
      if (!(param %in% colnames(est_vals))) next
      pred = as.numeric(est_vals[[param]])
      true = as.numeric(true_vals[[param]])
      results[[length(results) + 1]] = data.frame(
        seed            = seed_name,
        n_participants  = n_participant,
        n_items         = n_items,
        prevalence      = prevalence_type,
        method          = method_name,
        model           = model_name,
        param_name      = param,
        true_value      = true,
        predicted_value = pred,
        rmse            = sqrt((pred - true)^2),
        bias            = pred - true
      )
    }
  }

  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}