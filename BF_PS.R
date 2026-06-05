
packages = c("R2jags", "stringr", "benchmarkme", "ggplot2",  "see")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)


bf_ps = function(data_file,
                jags_text,
                n_iter = 20000,
                n_burnin = 5000,
                n_thin = 10,
                jags_seed,
                time_data_file = "./runtime/runtime_bf_ps.csv") {

  seed_name = basename(dirname(data_file))  
  
  if (seed_name == "data") seed_name = "empirical"

  output_roots = c(
    parameter_estimates = "./results_data/parameter_estimates/product_space",
    model_assignments = "./results_data/model_assignments/bf_ps",
    prob_strat = "./results_data/model_assignments/bf_ps/prob_strat",
    warnings = "./results_data/parameter_estimates/product_space/warnings",
    traceplots = "./traceplots/bf_ps"
  )
  
  lapply(output_roots, function(root) {
    dir.create(file.path(root, seed_name), recursive = TRUE, showWarnings = FALSE)
  })

  # Import data and format it
  data_long = read.csv(data_file)
  
  datalistmod = list(
    ID   = data_long$ID,
    resp = data_long$resp,
    diff = data_long$diff,
    cue  = data_long$cue,
    cond = data_long$condition,
    Nobs = nrow(data_long),
    n    = length(unique(data_long$ID)),
    nz   = 7 ,
    alpha0 = rep(1, 7)
  )
  
  
  N = length(unique(data_long$ID))
  
  # Start each chain at a different model
  initial_values_list = list(
    list(strat = rep(1, N)),
    list(strat = rep(2, N)),
    list(strat = rep(3, N)),
    list(strat = rep(4, N)),
    list(strat = rep(5, N)),
    list(strat = rep(6, N)),
    list(strat = rep(7, N))
  )
  
  initial_values = function(chain) {
    initial_values_list[[chain]]
  }
  
  
  params = c("b0", "b0mean", "b0sd",
             "bint", "bintmean", "bintsd",
             "bext", "bextmean", "bextsd",
             "z", "zmean", "zsd",
             "guess", "guessmean", "guesssd",
             "bias1", "bias1mean", "bias1sd",
             "bias2", "bias2mean", "bias2sd",
             "strat", "r", "alpha"
  )
  
  start = Sys.time()
  
  print(paste0("Fitting ", data_file))

  samples = jags.parallel(
    model.file = jags_text,
    inits = initial_values,
    parameters.to.save = params,
    data = datalistmod,
    n.iter = n_iter,
    n.chains = length(initial_values_list),
    n.burnin = n_burnin,
    n.thin = n_thin,
    jags.seed = jags_seed
  )

  runtime = as.numeric(difftime(Sys.time(), start, units = "secs"))

  summary_mat = samples$BUGSoutput$summary
  
  rhat_idx = summary_mat[, "Rhat"] > 1.01
  ess_idx  = summary_mat[, "n.eff"] < 1000

  rhat_df = data.frame(
    parameter = rownames(summary_mat)[rhat_idx],
    type = "Rhat",
    value = summary_mat[rhat_idx, "Rhat"],
    stringsAsFactors = FALSE
  )

  ess_df = data.frame(
    parameter = rownames(summary_mat)[ess_idx],
    type = "ESS",
    value = summary_mat[ess_idx, "n.eff"],
    stringsAsFactors = FALSE
  )

  warnings_df = rbind(rhat_df, ess_df)

  sims_mat = samples$BUGSoutput$sims.matrix
  strat_mat = sims_mat[, grep("^strat\\[", colnames(sims_mat))]
  
  prob_strat = t(apply(strat_mat, 2, function(x)
    prop.table(table(
      factor(x, levels = 1:7)
    ))))
  
  colnames(prob_strat) = c(
    "internal",
    "external",
    "sequential",
    "integrative",
    "guessing",
    "bias D1",
    "bias D2"
  )
  
  # Name the output files
  make_output_base = function(data_file) {
    base_name = basename(data_file)
    core_name = str_remove(base_name, "_data\\.csv$")
    return(core_name)
  }
  
  is_empirical = grepl("empirical", data_file)

  if (is_empirical) {
    output_base = "empirical_data_merged"
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
  
  time_df = read.csv("./runtime/runtime_bf_ps.csv")
  if (is_empirical) {

    new_row = data.frame(
    n_participant    = NA,
    n_items          = NA,
    prevalence_type  = "empirical",
    seed             = seed_name,
    total_runtime    = runtime,
    no_of_cores      = get_cpu()$no_of_cores,
    name             = get_cpu()$model_name,
    stringsAsFactors = FALSE
  )
  
  time_df = rbind(time_df, new_row)
  write.csv(time_df, time_data_file, row.names = FALSE)
  
  message("Empirical runtime:")
  print(new_row)
  
  } else {

    row_index = which(
      time_df$n_participant == n_participant &
      time_df$n_items == n_items &
      time_df$prevalence_type == prevalence_type &
      time_df$seed == seed_name
    )
  }

  # Save parameter estimates
  write.csv(
    sims_mat,
    file = file.path(output_roots["parameter_estimates"], seed_name,
                     paste0(output_base, "_params.csv")),
    row.names = FALSE
  )
  
  # Save strategy assignments
  write.csv(
    strat_mat,
    file = file.path(output_roots["model_assignments"], seed_name,
                     paste0(output_base, "_strategy_assignments.csv")),
    row.names = FALSE
  )
  
  # Save strategy probabilities
  write.csv(
    prob_strat,
    file = file.path(output_roots["prob_strat"], seed_name,
                     paste0(output_base, "_prob_strat.csv")),
    row.names = FALSE
  )

  # Save warnings
  write.csv(
  warnings_df,
  file = file.path(output_roots["warnings"], seed_name,
                   paste0(output_base, "_warnings.csv")),
  row.names = FALSE
)

# Get Okabe-Ito palette
  okabe_ito = okabeito_colors(1:7) 

  # Save traceplots
  pdf(file.path(output_roots["traceplots"], seed_name,
                paste0(output_base, "_traceplot.pdf")),
      width = 12, height = 10)
  
  traceplot(
    samples,
    mfrow = c(4, 2),  
    varname = c("b0mean","bintmean","bextmean","zmean",
                "guessmean","bias1mean","bias2mean"),
    ask = FALSE,
    col = okabe_ito,
    lty = 1,
    lwd = 1
  )

  dev.off()

  pdf(file.path(output_roots["traceplots"], seed_name,
              paste0(output_base, "_strat_probabilities.pdf")),
    width = 12, height = 10)
    
  par(mar = c(5, 4, 4, 8), xpd = TRUE)

  barplot(t(prob_strat),
          beside = FALSE,
          col = okabe_ito,
          main = "Posterior Strategy Probabilities",
          xlab = "Participant",
          ylab = "Probability")

  legend("topright",
        inset = c(-0.1, 0),
        legend = colnames(prob_strat),
        fill = okabe_ito,
        bty = "n")


  dev.off()

  return(samples)
}
