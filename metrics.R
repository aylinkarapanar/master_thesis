packages = c( "stringr", "dplyr", "caret", "DescTools")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

lapply(packages, library, character.only = TRUE)

# Helper function to get the true and predicted labels
get_labels = function(true_data_path,
                      predicted_data_path,
                      method_name = NULL) {

    true_data = read.csv(true_data_path)
  
    # Determine ID column
    id_col = if ("ID" %in% colnames(true_data)) "ID" else "participant_id"
    # Determine strategy column
    strategy_col = if ("strategy" %in% colnames(true_data)) "strategy" else if ("strat" %in% colnames(true_data)) "strat" else stop("No strategy column found")

    true_labels = true_data %>%
    rename(ID = all_of(id_col)) %>%
    mutate(strat_labels = factor(.data[[strategy_col]], levels = 1:7)) %>%
    select(ID, strat_labels)

    # Load predicted labels
    if (!is.null(predicted_data_path)) {
        if (method_name == "bf_ps") {
        predicted_strategy = read.csv(predicted_data_path) %>%
        # Tie happened on only 11 cases
            # apply(2, Mode)
            # Therefore in case of a tie, first mode is taken
            apply(2, function(x) Mode(x)[1])
        
        predicted_labels = data.frame(
            ID = 1:length(predicted_strategy),
            strat_labels = factor(predicted_strategy, levels = 1:7)
        )
        
        } else {
            predicted_raw = read.csv(predicted_data_path)

            # normalize column names 
            colnames(predicted_raw) = sapply(colnames(predicted_raw), function(x) {
                if (x == "ID") return(x) 
                tolower(gsub("\\s+", "_", x))
                })

            # detect strategy column
            pred_col = if ("strat_labels" %in% colnames(predicted_raw)) {
                "strat_labels"
                } else if ("strat_label" %in% colnames(predicted_raw)) {
                "strat_label"
                } else if ("strategy" %in% colnames(predicted_raw)) {
                "strategy"
                } else if ("strat" %in% colnames(predicted_raw)) {
                "strat"
                } else {
                stop("No valid strategy column found in predicted data")
                }

            predicted_labels = predicted_raw %>%
            rename(strat_labels = all_of(pred_col)) %>%
            mutate(strat_labels = factor(strat_labels, levels = 1:7))
        }
        
    } else {
        stop("Predicted labels or data path must be provided")
    }

    
    # Align by ID
    true_labels = true_labels[order(true_labels$ID), ]
    predicted_labels = predicted_labels[order(predicted_labels$ID), ]
    
    # Return the labels
    return(labels = data.frame(ID = true_labels$ID,
                                true = true_labels$strat_labels,
                                predicted = predicted_labels$strat_labels))
}


make_metrics_df = function(true_data_path,
                            predicted_data_path,
                            method_name = NULL,
                            metrics = c("accuracy","precision","recall","F1"),
                            save_as_csv = TRUE,
                            save_together = TRUE,
                            output_dir = "./metrics") {
  
    # Extract condition info from the file name
    if (is.null(method_name)) {
        method_name = str_split_i(dirname(predicted_data_path), "/", 2)
    }
    seed_name = basename(dirname(predicted_data_path)) 
    base = basename(predicted_data_path)
    base = str_remove(base, "_participants\\.csv$")
    parts = str_split(base, "_")[[1]]
    
    n_participant  = as.numeric(parts[1])
    n_items        = as.numeric(parts[2]) / 3
    prevalence_type = parts[3]
    params_type     = parts[4]
    
    # Load the labels
    labels = get_labels(true_data_path, predicted_data_path, method_name)
    
    # Confusion matrix
    # cm = confusionMatrix(
    #     data = labels$strat_labels,
    #     reference = labels$strat_labels,
    #     mode = "prec_recall"
    # )
    
    cm = confusionMatrix(
        data = labels$predicted,
        reference = labels$true,
        mode = "prec_recall"
    )
    # Compute metrics
    results = data.frame(
        n_items = n_items,
        n_participants = n_participant,
        prevalence = prevalence_type,
        method = method_name,
        seed = seed_name
    )
  
    if ("accuracy" %in% metrics) results$accuracy = cm$overall["Accuracy"]
    if ("precision" %in% metrics) results$precision = mean(cm$byClass[, "Precision"], na.rm = TRUE)
    if ("recall" %in% metrics) results$recall = mean(cm$byClass[, "Recall"], na.rm = TRUE)
    if ("F1" %in% metrics) results$f1 = mean(cm$byClass[, "F1"], na.rm = TRUE)
    
    # Save CSV
    if (save_as_csv) {
    # Ensure directory exists
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
  
    if (save_together) {
    # Save all of the metrics
        file_path = file.path(output_dir, "metrics_all.csv")
        if (!file.exists(file_path)) {
                write.table(
                    results,
                    file = file_path,
                    sep = ",",
                    row.names = FALSE,
                    col.names = TRUE,
                    append = FALSE
                )
            } else {
                write.table(
                    results,
                    file = file_path,
                    sep = ",",
                    row.names = FALSE,
                    col.names = FALSE,
                    append = TRUE
                )
            }
    } else {
    # Save each metric separately
        for (m in metrics) {
            df = results[, c("n_items","n_participants","prevalence","method","seed", m)]
            write.csv(df, file.path(output_dir, paste0("metrics_", m, ".csv")), row.names = FALSE)
            }
        }
    }
  
  return(results)
}
