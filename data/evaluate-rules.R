#' Prepare rule specifications for condition evaluation
#'
#' Transforms a list of rule condition definitions into a list of argument
#' specifications suitable for `check_condition()`. Each specification
#' contains the relevant dataset slice, comparison parameters, and the
#' associated condition message.
#'
#' @param ls_rules_conditions List of condition definitions for a single
#'   status, typically parsed from JSON.
#' @param ls_notes Named list of datasets used for evaluation (e.g.,
#'   daily metrics, session metrics).
#'
#' @return A list of argument lists, one per condition, ready to be passed
#'   to `check_condition()`.
prepare_rule_specs <- function(ls_rules_conditions, ls_notes) {
  
  lapply(ls_rules_conditions, function(cond){
    
    list(
      x             = ls_notes[[cond$dataset]],
      vars          = unlist(cond$vars),
      threshold     = cond$threshold,
      frequency     = cond$frequency,
      lookback_days = cond$lookback_days,
      comparison    = cond$comparison,
      type          = cond$type,
      condition_message = cond$condition_message
    )
    
  })
  
}

#' Evaluate rule conditions for a single status
#'
#' Applies `check_condition()` to each prepared rule specification and
#' returns only those conditions that are active. The corresponding
#' condition messages are attached to the evaluation results.
#'
#' @param rule_specs List of condition argument specifications produced
#'   by `prepare_rule_specs()`.
#'
#' @return A list of evaluation results for active conditions only.
#'   Each element contains the output of `check_condition()` plus the
#'   associated condition message.
evaluate_rule <- function(rule_specs) {
  out <- lapply(rule_specs, function(spec) {
    x <- do.call(check_condition, spec[setdiff(names(spec), "condition_message")])
    x$condition_message <- spec$condition_message
    if(any(x$active_indicator)) x
  })
  
  out[lapply(out,length)>0]

}