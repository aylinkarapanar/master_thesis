# Generate random assignments for the simulated data to compare
# of the other methods
generate_random_assignments = function(data_files,
                                       output_dir = "./results_data/model_assignments/random",
                                       n_strategies = 7,
                                       seed = 1234) {
  set.seed(seed)

  for (true_path in data_files) {
    true_data = read.csv(true_path)
    id_col = if ("ID" %in% colnames(true_data)) "ID" else "participant_id"
    n_subjects = nrow(true_data)

    # Random assignment by sampling uniformly from 1 to 7
    assignments = data.frame(
      ID = true_data[[id_col]],
      strat_labels = sample(1:n_strategies, size = n_subjects, replace = TRUE)
    )

    seed_name = basename(dirname(true_path))          
    base_core = sub("_participants\\.csv$", "", basename(true_path))
    out_folder = file.path(output_dir, seed_name)

    if (!dir.exists(out_folder)) dir.create(out_folder, recursive = TRUE)

    out_file = file.path(out_folder,
                         paste0(base_core, "_strategy_assignments.csv"))
    write.csv(assignments, out_file, row.names = FALSE)
  }
}