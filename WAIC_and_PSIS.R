packages = c("R2jags", "loo", "dplyr", "stringr", "benchmarkme", "see")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

# Helper function for fitting JAGS models
fit_jags_ic = function(jags_text,
                       params,
                       datalist,
                       n_iter = 20,
                       n_chains = 3,
                       n_burnin = 10,
                       n_thin = 1,
                       jags_seed = 1234) {
  start = Sys.time()

  samples = jags.parallel(
    jags_text,
    inits = NULL,
    parameters.to.save = params,
    data = datalist,
    n.iter = n_iter,
    n.chains = n_chains,
    n.burnin = n_burnin,
    n.thin = n_thin,
    jags.seed = jags_seed
  )
  
  runtime = as.numeric(difftime(Sys.time(), start, units = "secs"))

  summary_mat = samples$BUGSoutput$summary
  summary_mat = summary_mat[!grepl("^loglik", rownames(summary_mat)), ]
  
  rhat_idx = summary_mat[, "Rhat"] > 1.01
  ess_idx  = summary_mat[, "n.eff"] < 1000

  rhat_df = if (any(rhat_idx)) {
    data.frame(
      parameter = rownames(summary_mat)[rhat_idx],
      type = "Rhat",
      value = summary_mat[rhat_idx, "Rhat"],
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  ess_df = if (any(ess_idx)) {
    data.frame(
      parameter = rownames(summary_mat)[ess_idx],
      type = "ESS",
      value = summary_mat[ess_idx, "n.eff"],
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  warnings_df = do.call(rbind, list(rhat_df, ess_df))

  loglik_matrix = as.matrix(samples$BUGSoutput$sims.list$loglik)
  
  loo_result = loo(loglik_matrix)$pointwise[, "looic"]
  waic_result = waic(loglik_matrix)$pointwise[, "waic"]
  
  return(list("loo" = loo_result, "waic" = waic_result, "runtime" = runtime, "warning_assign" = warnings_df))
}

model_selection_waic_loo = function(data_file,
                                    model_list = models,
                                    n_iter = 5000,
                                    n_chains = 3,
                                    n_burnin = 1000,
                                    n_thin = 10,
                                    jags_seed = 1234,
                                    time_data_file = "./runtime/runtime_waic_loo.csv") {
  
  seed_name = basename(dirname(data_file)) 

   if (seed_name == "data") seed_name = "empirical" 

  output_roots = c(
    params_loo = "./parameter_estimates/non_hierarchical/PSIS-LOO",
    params_waic = "./parameter_estimates/non_hierarchical/WAIC",
    model_loo = "./model_assignments/PSIS-LOO/",
    model_waic = "./model_assignments/WAIC/",
    loo_df = "./model_assignments/PSIS-LOO/PSIS-LOO_df/",
    waic_df = "./model_assignments/WAIC/WAIC_df/",
    traceplots_loo = "./traceplots/loo",
    traceplots_waic = "./traceplots/waic",
    warnings = "./parameter_estimates/non_hierarchical/warnings"
  )
  
  lapply(output_roots, function(root) {
    dir.create(file.path(root, seed_name), recursive = TRUE, showWarnings = FALSE)
  })

  data_long = read.csv(data_file) %>%
    mutate(ID = as.integer(factor(ID))) %>%
    arrange(ID)
    
  participant_ranges = data_long %>%
    mutate(row_id = row_number()) %>%   
    group_by(ID) %>%
    summarize(
      start_idx = min(row_id),
      end_idx   = max(row_id)
    )
  
  datalistmod = list(
    ID   = data_long$ID,
    resp = data_long$resp,
    diff = data_long$diff,
    cue  = data_long$cue,
    cond = data_long$condition,
    Nobs = nrow(data_long),
    n    = length(unique(data_long$ID)),
    start_idx = participant_ranges$start_idx,
    end_idx = participant_ranges$end_idx
  )
  
  n_participant = length(unique(data_long$ID))
  
  looic_df = data.frame(ID = 1:n_participant)
  waic_df = data.frame(ID = 1:n_participant)
  runtime_vector = numeric(length(model_list))
  warning_assign = list()
  
  for (i in seq_along(model_list)) {
    
    model_name = names(model_list)[i]
    cat("Fitting:", model_name, "\n")
    
    results = fit_jags_ic(
      jags_text = model_list[[model_name]]$file_path,
      params = model_list[[model_name]]$params,
      datalist = datalistmod,
      n_iter = n_iter,
      n_chains = n_chains,
      n_burnin = n_burnin,
      n_thin = n_thin,
      jags_seed = jags_seed
    )
    
    looic_df[[model_name]] = results$loo
    waic_df[[model_name]] = results$waic
    runtime_vector[i] = results$runtime
    warning_assign[[model_name]] = cbind(
      model = model_name,
      results$warning_assign
    )
  }
  
  # Determine best model per participant
  model_cols = setdiff(colnames(looic_df), "ID")
  
  looic_strat = data.frame(ID = looic_df$ID,
                           strat_label = apply(looic_df[, model_cols], 1, which.min))
  
  waic_strat = data.frame(ID = waic_df$ID,
                          strat_label = apply(waic_df[, model_cols], 1, which.min))
  
  
  # Name the output files
  make_output_base = function(data_file) {
    base_name = basename(data_file)
    core_name = str_remove(base_name, "_data\\.csv$")
    return(core_name)
  }
  
    is_empirical = grepl("empirical", data_file)

  if (is_empirical) {
    output_base = "empirical"
    n_participant = NA
    n_items = NA
    prevalence_type = "empirical"

  } else {

    output_base = make_output_base(data_file)
    parts = str_split(output_base, "_")[[1]]

    n_participant = as.numeric(parts[1])
    n_items = as.numeric(parts[2]) / 3
    prevalence_type = parts[3]
  }

  write.csv(
    looic_strat,
    file = file.path(output_roots["model_loo"], seed_name,
                     paste0(output_base, "_strategy_assignments.csv")),
    row.names = FALSE
  )

  write.csv(
    looic_df,
    file = file.path(output_roots["loo_df"], seed_name,
                     paste0(output_base, "_df.csv")),
    row.names = FALSE
  )

  write.csv(
    waic_strat,
    file = file.path(output_roots["model_waic"], seed_name,
                     paste0(output_base, "_strategy_assignments.csv")),
    row.names = FALSE
  )
  
  write.csv(
    waic_df,
    file = file.path(output_roots["waic_df"], seed_name,
                     paste0(output_base, "_df.csv")),
    row.names = FALSE
  )
  
  write.csv(
    do.call(rbind, warning_assign),
    file = file.path(output_roots["warnings"], seed_name,
                    paste0(output_base, "_assign_warnings.csv")),
    row.names = FALSE
  )

  
  # Calculate the model parameter estimates by running the models again
  # But only with the individuals assigned to those models
 
  param_estimates_loo = list()
  samples_loo = list()
  
  model_cols = setdiff(colnames(looic_df), "ID")
  
  runtime_refit_loo = numeric(length(model_list))
  warnings_df_loo = vector("list", length(model_list))

  for (i in seq_along(model_list)) {
    
    cat("Refitting model for group according to loo:", names(model_list)[i], "\n")
    
    # Find the IDs assigned to this model 
    ids_in_group_loo = looic_strat$ID[looic_strat$strat_label == i]

    if (length(ids_in_group_loo) == 0) next
    
    # Subset data
    data_subset = data_long %>% filter(ID %in% ids_in_group_loo) %>% arrange(ID)

    # Map original IDs to sequential indices
    unique_ids = sort(unique(data_subset$ID))
    id_map = setNames(seq_along(unique_ids), unique_ids)
    data_subset$ID = id_map[as.character(data_subset$ID)]

    # Recompute participant ranges
    participant_ranges_sub = data_subset %>%
      mutate(row_id = row_number()) %>%
      group_by(ID) %>%
      summarize(start_idx = min(row_id), end_idx = max(row_id))

    datalist_sub = list(
      ID = data_subset$ID,          
      resp = data_subset$resp,
      diff = data_subset$diff,
      cue = data_subset$cue,
      cond = data_subset$condition,
      Nobs = nrow(data_subset),
      n = length(unique(data_subset$ID)),  
      start_idx = participant_ranges_sub$start_idx,
      end_idx = participant_ranges_sub$end_idx
    )
    
    #str(datalist_sub)
    #length(datalist_sub$ID)
    #length(datalist_sub$start_idx)
    #length(datalist_sub$end_idx)

    start_time = Sys.time()
  
    # Refit the model with the filtered data for that model
    samples = jags.parallel(
      model_list[[i]]$file_path,
      parameters.to.save = model_list[[i]]$params,
      data = datalist_sub,
      inits = NULL,
      n.iter = n_iter,
      n.chains = n_chains,
      n.burnin = n_burnin,
      n.thin = n_thin,
      jags.seed = jags_seed
    )

    runtime = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    runtime_refit_loo[i] = runtime
    param_estimates_loo[[i]] = samples$BUGSoutput$sims.matrix
    samples_loo[[i]] = samples
    summary_mat = samples$BUGSoutput$summary
    
    rhat_idx = summary_mat[, "Rhat"] > 1.01
    ess_idx  = summary_mat[, "n.eff"] < 1000
    
    rhat_df = if (any(rhat_idx)) {
      data.frame(
        parameter = rownames(summary_mat)[rhat_idx],
        type = "Rhat",
        value = summary_mat[rhat_idx, "Rhat"],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

    ess_df = if (any(ess_idx)) {
      data.frame(
        parameter = rownames(summary_mat)[ess_idx],
        type = "ESS",
        value = summary_mat[ess_idx, "n.eff"],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
    
    warnings_df_loo[[i]] = {
    combined = do.call(rbind, list(rhat_df, ess_df))
    
    if (is.null(combined)) {
      combined = data.frame(
        model = character(0),
        parameter = character(0),
        type = character(0),
        value = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      combined$model = names(model_list)[i]  
      combined = combined[, c("model", "parameter", "type", "value")]
    }
    
    combined
  }
  }
  
  okabe_ito = okabeito_colors(1:7)

  # Open one PDF for all traceplots
  pdf(file.path(output_roots["traceplots_loo"], paste0(output_base, "_traceplots_loo.pdf")))

  for (i in seq_along(samples_loo)) {
    
    # Skip empty models
    if (is.null(samples_loo[[i]])) next
    
    # Get the full JAGS object
    jags_fit = samples_loo[[i]]
    
    # Select parameters you want to plot
    model_params = model_list[[i]]$params
    param_names = intersect(colnames(jags_fit$BUGSoutput$sims.matrix), model_params)
    param_names = param_names[stringr::str_ends(param_names, "mean")]

    if (length(param_names) == 0) next

    # Traceplot directly on the JAGS object
    R2jags::traceplot(
      jags_fit,
      varname = param_names,
      mfrow = c(4, 2),
      ask = FALSE,
      col = okabe_ito,
      lty = 1,
      lwd = 1
    )
  }

  dev.off()

  write.csv(
  do.call(rbind, warnings_df_loo),
  file = file.path(output_roots["warnings"], seed_name,
                   paste0(output_base, "_refit_warnings_loo.csv")),
  row.names = FALSE)


  for (i in seq_along(param_estimates_loo)) {
    
    if (is.null(param_estimates_loo[[i]])) next
    
    write.csv(
      param_estimates_loo[[i]],
      file = file.path(output_roots["params_loo"], seed_name,
                     paste0(output_base, "_", names(model_list)[i], ".csv")),
      row.names = FALSE
    )
  }

  param_estimates_waic = list()
  samples_waic = list()
  runtime_refit_waic = numeric(length(model_list))
  warnings_df_waic = vector("list", length(model_list))

  for (i in seq_along(model_list)) {
    
    cat("Refitting model for group according to WAIC:", names(model_list)[i], "\n")
    
    ids_in_group_waic = waic_strat$ID[waic_strat$strat_label == i]
    
    if (length(ids_in_group_waic) == 0) next
    
    data_subset = data_long %>%
      filter(ID %in% ids_in_group_waic) %>%
      arrange(ID)
    
    unique_ids = sort(unique(data_subset$ID))
    id_map = setNames(seq_along(unique_ids), unique_ids)
    data_subset$ID = id_map[as.character(data_subset$ID)]

    # Recompute participant ranges
    participant_ranges_sub = data_subset %>%
      mutate(row_id = row_number()) %>%
      group_by(ID) %>%
      summarize(start_idx = min(row_id), end_idx = max(row_id))

    datalist_sub = list(
      ID = data_subset$ID,          
      resp = data_subset$resp,
      diff = data_subset$diff,
      cue = data_subset$cue,
      cond = data_subset$condition,
      Nobs = nrow(data_subset),
      n = length(unique(data_subset$ID)),  
      start_idx = participant_ranges_sub$start_idx,
      end_idx = participant_ranges_sub$end_idx
    )
    
    start_time = Sys.time()
    
    samples = jags.parallel(
      model_list[[i]]$file_path,
      parameters.to.save = model_list[[i]]$params,
      data = datalist_sub,
      inits = NULL,
      n.iter = n_iter,
      n.chains = n_chains,
      n.burnin = n_burnin,
      n.thin = n_thin,
      jags.seed = jags_seed
    )
    
    runtime = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    runtime_refit_waic[i] = runtime
    param_estimates_waic[[i]] = samples$BUGSoutput$sims.matrix
    samples_waic[[i]] = samples
    summary_mat = samples$BUGSoutput$summary
    
    rhat_idx = summary_mat[, "Rhat"] > 1.01
    ess_idx  = summary_mat[, "n.eff"] < 1000
    
    rhat_df = if (any(rhat_idx)) {
      data.frame(
        parameter = rownames(summary_mat)[rhat_idx],
        type = "Rhat",
        value = summary_mat[rhat_idx, "Rhat"],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

    ess_df = if (any(ess_idx)) {
      data.frame(
        parameter = rownames(summary_mat)[ess_idx],
        type = "ESS",
        value = summary_mat[ess_idx, "n.eff"],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
    
    warnings_df_waic[[i]] = {
    combined = do.call(rbind, list(rhat_df, ess_df))
    
    if (is.null(combined)) {
      combined = data.frame(
        model = character(0),
        parameter = character(0),
        type = character(0),
        value = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      combined$model = names(model_list)[i]
      combined = combined[, c("model", "parameter", "type", "value")]
    }
    
    combined
  }
  }

  pdf(file.path(output_roots["traceplots_waic"], paste0(output_base, "_traceplots_waic.pdf")))


  for (i in seq_along(samples_waic)) {
    
    # Skip empty models
    if (is.null(samples_waic[[i]])) next
    
    # Get the full JAGS object
    jags_fit = samples_waic[[i]]
    
    # Select parameters you want to plot
    model_params = model_list[[i]]$params
    param_names = intersect(colnames(jags_fit$BUGSoutput$sims.matrix), model_params)
    param_names = param_names[stringr::str_ends(param_names, "mean")]

    if (length(param_names) == 0) next

    # Traceplot directly on the JAGS object
    R2jags::traceplot(
      jags_fit,
      varname = param_names,
      mfrow = c(4, 2),
      ask = FALSE,
      col = okabe_ito,
      lty = 1,
      lwd = 1
    )
  }

  dev.off()

  for (i in seq_along(param_estimates_waic)) {
  
    if (is.null(param_estimates_waic[[i]])) next
    
    write.csv(
      param_estimates_waic[[i]],
      file = file.path(output_roots["params_waic"], seed_name,
                      paste0(output_base, "_", names(model_list)[i], ".csv")),
      row.names = FALSE
    )
  }

  write.csv(
    do.call(rbind, warnings_df_waic),
    file = file.path(output_roots["warnings"], seed_name,
                    paste0(output_base, "_waic_refit_warnings.csv")),
    row.names = FALSE
  )

  time_df = read.csv(time_data_file)

  if (is_empirical) {

    new_row = data.frame(
      n_participant = NA,
      n_items = NA,
      prevalence_type = "empirical",
      seed = seed_name,

      no_of_cores = get_cpu()$no_of_cores,
      name = get_cpu()$model_name,

      total_runtime_assign = sum(runtime_vector),
      total_runtime_refit_loo = sum(runtime_refit_loo),
      total_runtime_refit_waic = sum(runtime_refit_waic),

      stringsAsFactors = FALSE
    )


    for (i in seq_along(model_list)) {

      new_row[[paste0(names(model_list)[i], "_assign")]] =
        runtime_vector[i]

      new_row[[paste0(names(model_list)[i], "_refit_loo")]] =
        runtime_refit_loo[i]

      new_row[[paste0(names(model_list)[i], "_refit_waic")]] =
        runtime_refit_waic[i]
    }

    time_df = rbind(time_df, new_row)

  } else {

    row_index = which(
      time_df$n_participant == n_participant &
      time_df$n_items == n_items &
      time_df$prevalence_type == prevalence_type &
      time_df$seed == seed_name
    )

    if (length(row_index) > 0) {
    time_df[row_index, "no_of_cores"] = get_cpu()$no_of_cores
    time_df[row_index, "name"] = get_cpu()$model_name

    time_df[row_index, paste0(names(model_list), "_assign")] = as.data.frame(t(runtime_vector))
    time_df[row_index, "total_runtime_assign"] = sum(runtime_vector)

    time_df[row_index, paste0(names(model_list), "_refit_loo")] = as.data.frame(t(runtime_refit_loo))
    time_df[row_index, paste0(names(model_list), "_refit_waic")] = as.data.frame(t(runtime_refit_waic))

    time_df[row_index, "total_runtime_refit_loo"] = sum(runtime_refit_loo)
    time_df[row_index, "total_runtime_refit_waic"] = sum(runtime_refit_waic)
    }

  write.csv(time_df, time_data_file, row.names = FALSE)
  }
}