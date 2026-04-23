packages = c("truncnorm", "dplyr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

# Function to create a response dataset for a given condition (nh, h75, h85) 
# with defined number of participants, number of items, and model assignment probabilities
simulate_strategy_data = function(
    seed,
    n_participants = 100,
    # if one condition is specified then n_items will be simulated, if "all" is given then 3 x n_items will be simulated
    n_items = 120, # 120 items per condition
    
    # Default strategy probabilities are equal
    strategy_probs = rep(1/7, 7),
    condition = "nh", # h75, h85, or all to get a dataset with all of the conditions

    # Hyperparameters
    # mean, standart deviation, lower truncation point (a), upper truncation point (b)
    b0_mean = 0, b0_sd = 0.5, b0_a = -Inf, b0_b = Inf,
    bint_mean = 2.5, bint_sd = 0.75, bint_a = 0, bint_b = Inf,
    bext_mean = 1.75, bext_sd = 0.5, bext_a = 0.5, bext_b = Inf,
    z_mean = 1, z_sd = 0.5, z_a = 0.3, z_b = 1.3, 
    guess_mean = 0.5, guess_sd = 0.02, guess_a = 0.4, guess_b = 0.6,
    bias1_mean = 0.05, bias1_sd = 0.02, bias1_a = 0, bias1_b = 0.15,
    bias2_mean = 0.95, bias2_sd = 0.02, bias2_a = 0.85, bias2_b = 1,

    # Describe the prevalence and parameter values for naming the files
    prevalence_desc = "equal"
    ){
  
  set.seed(seed)

  original_wd = getwd()
  
  main_dir = file.path(original_wd, "data")   
  dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)

  seed_dir = file.path(main_dir, as.character(seed))
  dir.create(seed_dir, recursive = TRUE, showWarnings = FALSE)

  # Difficulty values (Zdiff): standardized difference between D1 and D2
  # Range from -1.622 to 1.622 (according to empirical data)
  diff_per_item = seq(-1.622, 1.622, length.out = n_items)
  diff_values = rep(diff_per_item, n_participants)
  
  # Assign cue values based on condition;
  # nh denotes no hints with cue = 0,
  # h75 denotes 75% hints with 75% correct cues and 25% incorrect
  # -1 = hint points to D1, +1 = hint points to D2
  # h85 denotes 85% hints with 85% correct and 15% incorrect
  
  # Helper function to generate hints
  generate_hints = function(diff_values, hint_accuracy) {
    n = length(diff_values)
    correct_answer = as.numeric(diff_values > 0)
    
    hint_correct = rbinom(n, 1, hint_accuracy)
    
    correct_cue = ifelse(correct_answer == 1, 1, -1)
    incorrect_cue = ifelse(correct_answer == 1, -1, 1)
    
    cues = ifelse(hint_correct == 1, correct_cue, incorrect_cue)
    
    return(cues)
  }
  
  # Simulate cue values for given condition
  
  if (condition == "nh") {
    cue_per_item = rep(0, n_items)
    condition_per_item = rep("nh", n_items)
    
  } else if (condition == "h75") {
    cue_per_item = generate_hints(diff_per_item, hint_accuracy = 0.75)
    condition_per_item = rep("h75", n_items)
    
  } else if (condition == "h85") {
    cue_per_item = generate_hints(diff_per_item, hint_accuracy = 0.85)
    condition_per_item = rep("h85", n_items)
    
  } else if (condition == "all") {
    cue_nh  = rep(0, n_items)
    cue_h75 = generate_hints(diff_per_item, hint_accuracy = 0.75)
    cue_h85 = generate_hints(diff_per_item, hint_accuracy = 0.85)
    
    cue_per_item = c(cue_nh, cue_h75, cue_h85)
    condition_per_item = c(rep("nh", n_items), rep("h75", n_items), rep("h85", n_items))
  }
  
  cue_values = rep(cue_per_item, n_participants)
  condition_labels = rep(condition_per_item, n_participants)
  
  # Assign strategy models with the defined model prevalence
  strategies = sample(1:7, n_participants, replace = TRUE, prob = strategy_probs)
  
  participants = data.frame(
    participant_id = 1:n_participants,
    strategy = strategies,
    b0 = NA,
    bint = NA,
    bext = NA,
    z = NA,
    guess = NA,
    bias1 = NA,
    bias2 = NA
  )
  
  # Simulate individual parameters given the specified values
  participants$b0   = rnorm(n_participants, b0_mean, b0_sd)
  participants$bint = rtruncnorm(n_participants, 
                                 mean = bint_mean, sd = bint_sd, a = bint_a)
  participants$bext = rtruncnorm(n_participants, 
                                 mean = bext_mean, sd = bext_sd, a = bext_a)
  participants$z    = rtruncnorm(n_participants, 
                                 mean = z_mean, sd = z_sd, a = z_a, b = z_b)
  participants$guess = rtruncnorm(n_participants,
                                  mean = guess_mean, sd = guess_sd,
                                  a = guess_a, b = guess_b)
  participants$bias1 = rtruncnorm(n_participants,
                                  mean = bias1_mean, sd = bias1_sd,
                                  a = bias1_a, b = bias1_b)
  participants$bias2 = rtruncnorm(n_participants,
                                  mean = bias2_mean, sd = bias2_sd,
                                  a = bias2_a, b = bias2_b)
  
  data_long = data.frame(
    ID = rep(1:n_participants, each = n_items),
    diff = diff_values,
    cue = cue_values,
    condition = condition_labels
  )
  
  strategy = participants$strategy
  b0 = participants$b0
  bint = participants$bint
  bext = participants$bext
  z = participants$z
  guess = participants$guess
  bias1 = participants$bias1
  bias2 = participants$bias2
  
  # For each participant calculate the theta (probability of giving the right answer)
  # given the strategy model they were assigned
  n = nrow(data_long)
  theta = numeric(n)
  
  ppn = data_long$ID
  strat = strategy[ppn]
  
  # Strategies 5–7 (constant theta) - Guessing, Bias 1, Bias 2
  theta[strat == 5] = guess[ppn[strat == 5]]
  theta[strat == 6] = bias1[ppn[strat == 6]]
  theta[strat == 7] = bias2[ppn[strat == 7]]
  
  # Strategy 1 - Internal
  idx = strat == 1
  tmp = b0[ppn[idx]] +
    bint[ppn[idx]] * data_long$diff[idx]
  theta[idx] = plogis(tmp)
  
  # Strategy 2 - External
  idx = strat == 2
  tmp = bext[ppn[idx]] * data_long$cue[idx]
  theta[idx] = plogis(tmp)
  
  # Strategy 3 - Sequential
  idx = strat == 3
  use_int = z[ppn[idx]] < abs(data_long$diff[idx])
  
  tmp = numeric(sum(idx))
  
  tmp[use_int] = b0[ppn[idx]][use_int] +
    bint[ppn[idx]][use_int] * data_long$diff[idx][use_int] +
    bext[ppn[idx]][use_int] * data_long$cue[idx][use_int]
  
  tmp[!use_int] = bext[ppn[idx]][!use_int] *
    data_long$cue[idx][!use_int]
  
  theta[idx] = plogis(tmp)
  
  # Strategy 4 - Integrative
  idx = strat == 4
  tmp = b0[ppn[idx]] +
    bint[ppn[idx]] * data_long$diff[idx] +
    bext[ppn[idx]] * data_long$cue[idx]
  theta[idx] = plogis(tmp)
  
  # Create the responses using the calculated thetas above
  response = rbinom(n, 1, theta)
  
  data_long$resp = response
  data_long$strategy = strategy[data_long$ID]
  data_long$correct_answer = as.numeric(data_long$diff > 0)
  data_long$is_correct = data_long$resp == data_long$correct_answer
  
  true_params_individual = list(
    strategy = strategies,
    b0 = b0,
    bint = bint,
    bext = bext,
    z = z,
    guess = guess,
    bias1 = bias1,
    bias2 = bias2
  )
  
  true_params_population = list(
    b0mean = b0_mean,
    b0sd = b0_sd,
    bintmean = bint_mean,
    bintsd = bint_sd,
    bextmean = bext_mean,
    bextsd = bext_sd,
    zmean = z_mean,
    zsd = z_sd,
    guessmean = guess_mean,
    guesssd = guess_sd,
    bias1mean = bias1_mean,
    bias1sd = bias1_sd,
    bias2mean = bias2_mean,
    bias2sd = bias2_sd
  )
  
  results = list(data = data_long, participants = participants)
  

filename_base <- paste(n_participants,
                    ifelse(condition == "all", n_items * 3, n_items),
                    prevalence_desc,
                    #param_desc,
                    sep = "_"
)
  
  write.csv(results$data, file.path(seed_dir, paste0(filename_base, "_data.csv")), row.names = FALSE)
  write.csv(results$participants, file.path(seed_dir, paste0(filename_base, "_participants.csv")), row.names = FALSE)
  
  return(results)
}


