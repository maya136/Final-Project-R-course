# Functions


# Goal: merge double columns
merge_boys_and_girls_columns <- function(df, colname) {
  col1 <- colname 
  col2 <- paste0(colname, ".1")  # Corresponding .1 column
  
  if (col1 %in% colnames(df) & col2 %in% colnames(df)) {  
    df[[col1]] <- ifelse(is.na(df[[col1]]), df[[col2]], df[[col1]])
    df[[col2]] <- NULL
    
  }
  
  return(df)
}

  
# Goal: calculate sum for each metric in baseline
baseline_metrics_calculate <- function(df, DASS_cols, SDQ_cols, CSHQ_cols) {

  DASS_cols_to_sum <- names(df)[names(df) %in% DASS_cols]
  df$sum_dass <- rowSums(df[, DASS_cols_to_sum], na.rm = TRUE)
  
  SDQ_cols_to_sum <- names(df)[names(df) %in% SDQ_cols]
  df$sum_sdq <- rowSums(df[, SDQ_cols_to_sum], na.rm = TRUE)

  CSHQ_cols_to_sum <- names(df)[names(df) %in% CSHQ_cols]
  df$sum_cshq <- rowSums(df[, CSHQ_cols_to_sum], na.rm = TRUE)
  
  return(df)
}


# Goal: calculate sum for each metric in intervention
intervention_metrics_calculate <- function(df) {
  COMPLIANCE_cols_parent = c("VideoWatch", "ConnectP", "EncourageUse")
  COMPLIANCE_cols_child = c("ConnectC", "TakeToBed", "DCPractice", "Cooperate", 
                          "AltEnding", "IRTDaysPractice", "MinutesPractice")
  
  COMPLIANCE_cols_to_sum <- names(df)[names(df) %in% COMPLIANCE_cols_parent]
  df$sum_parent <- rowSums(df[, COMPLIANCE_cols_to_sum], na.rm = TRUE)
  
  COMPLIANCE_cols_to_sum <- names(df)[names(df) %in% COMPLIANCE_cols_child]
  df$sum_child <- rowSums(df[, COMPLIANCE_cols_to_sum], na.rm = TRUE)

  return(df)
}


# Goal: sort ids - drop ones that dont have 2 questionnaires
sort_by_participant_id <- function(baseline, control, intervention) {
  
  baseline$ParticipantId <- as.character(baseline$ParticipantId)
  control$ParticipantId <- as.character(control$ParticipantId)
  intervention$ParticipantId <- as.character(intervention$ParticipantId)
  
  baseline_ids <- unique(baseline$ParticipantId)
  control_ids <- unique(control$ParticipantId)
  intervention_ids <- unique(intervention$ParticipantId)
  
  valid_in_control <- intersect(baseline_ids, control_ids)
  valid_in_intervention <- intersect(baseline_ids, intervention_ids)
  
  valid_ids <- unique(c(valid_in_control, valid_in_intervention))
  
  # keep only real participants
  baseline_filtered <- baseline[baseline$ParticipantId %in% valid_ids, , drop = FALSE]
  control_filtered <- control[control$ParticipantId %in% valid_ids, , drop = FALSE]
  intervention_filtered <- intervention[intervention$ParticipantId %in% valid_ids, , drop = FALSE]
  
  return(list(baseline = baseline_filtered, control = control_filtered, intervention = intervention_filtered))
}

