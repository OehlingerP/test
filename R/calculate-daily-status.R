#' Construct a structured daily status result
#'
#' Helper constructor that standardizes the output format of the daily
#' status evaluation. The result contains one primary status and, optionally,
#' one secondary advisory status.
#'
#' @param primary_status char; Identifier of the highest-priority
#'   active status.
#' @param primary_action char; Action associated with the
#'   primary status (e.g., "STOP_RUNNING").
#' @param primary_blocks Character vector. Training modalities blocked
#'   by the primary status.
#' @param primary_status_message char; High-level summary
#'   message for the primary status.
#' @param primary_condition_messages char; Messages corresponding
#'   to the triggered conditions of the primary status.
#' @param secondary_status char; Identifier of the secondary
#'   advisory status (if present).
#' @param secondary_action char; Action associated with the
#'   secondary status.
#' @param secondary_blocks char; Training modalities blocked
#'   by the secondary status.
#' @param secondary_status_message char; High-level summary
#'   message for the secondary status.
#' @param secondary_condition_messages char; Messages corresponding
#'   to the triggered conditions of the secondary status.
#'
#' @return A named list containing primary and optional secondary status
#'   metadata and messages.
daily_status_result <- function(primary_status,
                                primary_priority = NULL,
                                primary_action = NULL,
                                primary_color  = NULL,
                                primary_blocks = NULL,
                                primary_status_message = NULL,
                                primary_condition_messages = NULL,
                                secondary_status = NULL,
                                secondary_priority = NULL, 
                                secondary_action = NULL,
                                secondary_color  = NULL,
                                secondary_blocks = NULL,
                                secondary_status_message = NULL,
                                secondary_condition_messages = NULL) {
  out <- list(
    primary_status = primary_status,
    primary_priority = primary_priority,
    primary_action = primary_action,
    primary_color  = primary_color,
    primary_blocks = primary_blocks,
    primary_status_message = primary_status_message,
    primary_condition_messages = primary_condition_messages,
    secondary_status = secondary_status,
    secondary_priority = secondary_priority,
    secondary_action = secondary_action,
    secondary_color  = secondary_color,
    secondary_blocks = secondary_blocks,
    secondary_status_message = secondary_status_message,
    secondary_condition_messages = secondary_condition_messages
  )
  
  # Replace NULLs with a placeholder for consistency
  out <- lapply(out, function(x) {
    if (is.null(x) || length(x) == 0) NA  # or list() if you prefer a list placeholder
    else x
  })
  
  return(out)
  
}

#' Calculate the resolved daily training status
#'
#' Evaluates all defined daily rules, determines active statuses, and
#' resolves them according to priority. The highest-priority active status
#' becomes the primary status. If the primary status allows warnings and
#' additional statuses are active, the next-highest priority status is
#' included as a secondary advisory status.
#'
#' If no status is active, the baseline status is returned.
#'
#' @param ls_rules Named list of daily rule definitions including metadata
#'   such as priority, blocks, actions, and status messages.
#' @param ls_notes Named list of datasets used for rule evaluation.
#'
#' @return A structured daily status object created by
#'   `daily_status_result()`.
calculate_daily_status <- function(ls_rules, ls_notes){
  
  status_names <- names(ls_rules)
  
  ls_cond <- lapply(status_names, function(status){
    rule_specs <- prepare_rule_specs(ls_rules[[status]]$conditions,
                                     ls_notes)
    evaluate_rule(rule_specs)
  })
  
  names(ls_cond) <- status_names
  
  ls_cond <- ls_cond[lapply(ls_cond, length) > 0]
  
  status_active <- names(ls_cond)
  
  if(length(status_active) == 0){
    daily_status_result(
      primary_status         = "baseline",
      primary_action         = ls_rules$baseline$action,
      primary_blocks         = ls_rules$baseline$blocks,
      primary_status_message = ls_rules$baseline$status_message
    )
  }
  
  ordered_status <- status_active[order(sapply(ls_rules[status_active], `[[`, "priority"))]
  
  meta_primary <- ls_rules[[ordered_status[1]]]
  cond_primary <- ls_cond[[ordered_status[1]]]
  
  if(meta_primary$allows_warnings && length(ordered_status) > 1){
    meta_secondary <- ls_rules[[ordered_status[2]]]
    cond_secondary <- ls_cond[[ordered_status[2]]]
  } else {
    meta_secondary <- NULL
    cond_secondary <- NULL
  }
  
  out <- daily_status_result(primary_status = ordered_status[1],
                             primary_priority = meta_primary$priority,
                             primary_action = meta_primary$action,
                             primary_color  = meta_primary$status_color,
                             primary_blocks = meta_primary$blocks,
                             primary_status_message = meta_primary$status_message,
                             primary_condition_messages = sapply(cond_primary, `[[`, "condition_message"),
                             secondary_status = ifelse(length(ordered_status) > 1, ordered_status[2], NA),
                             secondary_priority = meta_secondary$priority,
                             secondary_action = meta_secondary$action,
                             secondary_color  = meta_secondary$status_color,
                             secondary_blocks = meta_secondary$blocks,
                             secondary_status_message = meta_secondary$status_message,
                             secondary_condition_messages = sapply(cond_secondary, `[[`, "condition_message"))
  
  return(out)
  
}
