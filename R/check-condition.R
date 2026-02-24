#' Condition result helper
condition_result <- function(active_indicator,
                             threshold = NULL,
                             frequency = NULL,
                             lookback_days = NULL,
                             comparison = NULL,
                             type = NULL,
                             n_obs_used = NULL, 
                             start_date = NULL, 
                             end_date = NULL) {
  list(
    active_indicator = active_indicator,
    threshold = threshold,
    frequency = frequency,
    lookback_days = lookback_days,
    comparison = comparison,
    type = type,
    n_obs_used = n_obs_used, 
    start_date = start_date,
    end_date = end_date
  )
}

#' Condition check input helper
condition_check_inputs <- function(x,
                                   vars,
                                   threshold,
                                   frequency,
                                   lookback_days,
                                   comparison,
                                   type) {
  
  # ---- General structure checks ----
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  
  if (!"date" %in% colnames(x)) {
    stop("x must contain a 'date' column")
  }
  
  if (!all(vars %in% colnames(x))) {
    stop("All vars must be present in x")
  }
  
  if (!is.numeric(lookback_days) || 
      length(lookback_days) != 1 ||
      (lookback_days)%%1 != 0) {
    stop("lookback_days must be a single integer value")
  }
  
  if (!type %in% c("threshold", "change", "session", "char")) {
    stop("type must be one of: threshold, change, session, char")
  }
  
  # ---- Type-specific requirements ----
  
  if (type %in% c("threshold", "session")) {
    
    if (is.null(threshold) || 
        !is.numeric(threshold)  || 
        length(threshold) != 1) {
      stop("threshold must be numeric for type 'threshold' or 'session'")
    }
    
    if (is.null(frequency) || 
        !is.numeric(frequency) || 
        length(frequency) != 1) {
      stop("frequency must be numeric for type 'threshold' or 'session'")
    }
    
    if (is.null(comparison) || !comparison %in% c("geq", "leq")) {
      stop("comparison must be 'geq', or 'leq' for type 'threshold' or 'session'")
    }
  }
  
  if (type == "change") {
    
    if (is.null(threshold) || 
        !is.numeric(threshold) || 
        length(threshold) != 1) {
      stop("threshold must be numeric for type 'change'")
    }
    
    if (is.null(comparison) || !comparison %in% c("geq", "leq")) {
      stop("comparison must be 'geq', or 'leq' for type 'change'")
    }
    
    if (length(unique(x$date)) < 2) {
      stop("x must contain at least two different dates for type 'change'")
    }
    
  }
  
  if (type == "char") {
    
    if (is.null(threshold) || 
        !is.character(threshold)) {
      stop("threshold must be character for type 'char'")
    }
    
    if (is.null(frequency) || 
        !is.numeric(frequency) || 
        length(frequency) != 1) {
      stop("frequency must be numeric for type 'char'")
    }
    
    if (is.null(comparison) || !comparison %in% c("eq", "neq")) {
      stop("comparison must be 'eq' or 'neq' for type 'char'")
    }
    
  }
  
  invisible(TRUE)
}



#' Condition date filter helper
#' @param x data.frame; created by `compute_daily_metrics()`
#' @param vars char; vars for which to check condition
#' @param lookback_days num; number of days to evaluate (from max date in x)

condition_date_filter <- function(x, vars, lookback_days){
  if("session_id" %in% colnames(x)){
    x <- x[, c("date", "session_id", vars)] 
  } else {
    x <- x[, c("date", vars)] 
  }
  x %>%
    filter(date >= max(date, na.rm = T) - lookback_days) %>%
    arrange(date)
}

#' Checks whether a certain indicator has crossed a specific `threshold`, 
#'    `frequency` times over the last `lookback_days` days.
condition_type_threshold <- function(x, vars, threshold, frequency, comparison){
  
  if(comparison == "geq"){
    counts <- colSums(x[, vars] >= threshold, na.rm = T)
  } else {
    counts <- colSums(x[, vars] <= threshold, na.rm = T)
  }
  active <- counts >= frequency
  active
  
}

#' Checks whether a certain indicator has changed by `threshold` compared to 
#'    `lookback_days` days ago.
condition_type_change <- function(x, vars, threshold, frequency, comparison){
  
  x <- x %>%
    tidyr::fill(-date, .direction = "downup") 
  
  first_row <- x[which.min(x$date), vars]
  last_row <- x[which.min(x$date), vars]

  if(nrow(first_row) != 1) stop("Non-unique start date in x for type 'change'")
  if(nrow(last_row) != 1) stop("Non-unique end date in x for type 'change'") 
  
  if(comparison == "geq"){
    active <- last_row - first_row >= threshold
  } else {
    active <- last_row - first_row <= threshold
  }
  
  active
  
}

#' Checks whether a certain indicator has crossed a specific `threshold`, 
#'    `frequency` times over the last `lookback_days` days. In contrast to 
#'    `condition_type_threshold()`, this function looks at workout specific data.
#' @details
#' For example, a frequency of 2 would mean that if the threshold is crossed 2 
#'    times in the last sessions the indicator would become active. Those 
#'    sessions could be on the same day (not possible in change threshold)
condition_type_session <- function(x, vars, threshold, frequency, comparison){
  
  if(!"session_id" %in% colnames(x)) stop("session_id missing in data")
  
  condition_type_threshold(x[, setdiff(colnames(x), "session_id")], vars,
                           threshold, frequency, comparison)
  
}

#' Checks whether a specific character value (`threshold`) has been recorded
#'    `frequency` times over the las `lookback_days` days.
condition_type_char <- function(x, vars, threshold, frequency, comparison){
  cols <- setdiff(colnames(x), c("date", "session_id"))
  if(!all(apply(x[, cols], 2, is.character))) stop("Character variables needed for type 'char'")
  
  if(comparison == "eq"){
    counts <- colSums(as.data.frame(apply(x[, vars, drop = F], 2, function(x) x %in% threshold, simplify = F)), na.rm = T)
  } else {
    counts <- colSums(as.data.frame(apply(x[, vars, drop = F], 2, function(x) !x %in% c(threshold, NA), simplify = F)), na.rm = T)
  }
  
  active <- counts >= frequency

} 

#' Calculate readiness to train conditions from daily data.
#' @param x data.frame; created by `compute_daily_metrics()`
#' @param vars char; vars for which to check condition
#' @param threshold num; numerical threshold to trigger action 
#'    (larger- or lower-equal; see comparison)
#' @param frequency num; how often the threshold must be reached to trigger
#'    action
#' @param lookback_days num; number of days to evaluate (from max date in x)
#' @param comparison char; geq = greater equal (default) or leq = lower equal
#' @param type char; type of check to conduct: threshold, change, session, char

check_condition <- function(x, 
                            vars, 
                            threshold, 
                            frequency = NULL, 
                            lookback_days,
                            comparison = NULL, 
                            type){
  
  condition_check_inputs(x,
                         vars,
                         threshold,
                         frequency,
                         lookback_days,
                         comparison,
                         type)
  
  x <- condition_date_filter(x, vars, lookback_days)
  
  type_map <- list(
    threshold = condition_type_threshold,
    change    = condition_type_change,
    session   = condition_type_session,
    char      = condition_type_char
  )
  
  active <- type_map[[type]](x, vars, threshold, frequency, comparison)

  condition_result(active,
                   threshold,
                   frequency,
                   lookback_days,
                   comparison,
                   type,
                   n_obs_used = nrow(x), 
                   start_date = min(x$date), 
                   end_date = max(x$date))
}
