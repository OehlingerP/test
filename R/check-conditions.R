#' Condition result helper
condition_result <- function(active_indicator,
                             threshold = NULL,
                             frequency = NULL,
                             lookback_days = NULL,
                             direction = NULL,
                             type = NULL) {
  list(
    active_indicator = active_indicator,
    threshold = threshold,
    frequency = frequency,
    lookback_days = lookback_days,
    direction = direction,
    type = type
  )
}

#' Condition check input helper
condition_check_inputs <- function(){
  if(!is.numeric(threshold) & !all(apply(x, 2, is.character))) stop("threshold must be numeric")
  if(!is.numeric(frequency) & !is.null(frequency)) stop("frequency must be numeric")
  if(!is.numeric(lookback_days)) stop("lookback_days must be numeric")
  if(!direction %in% c("geq", "leq") & !is.null(direction)) stop("direction must be leq or geq")
  if(!all(vars %in% colnames(x))) stop("All vars must be part of x")
  if(!type %in% c("threshold", "change", "session", "char")) stop("type must be one of threshold, change, session, char")
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
    filter(date >= max(x$date, na.rm = TRUE) - lookback_days) %>%
    arrange(date)
}

#' Checks whether a certain indicator has crossed a specific `threshold`, 
#'    `frequency` times over the last `lookback_days` days.
type_threshold <- function(x, threshold, frequency){
  
  if(direction == "geq"){
    counts <- colSums(x[, -1] >= threshold, na.rm = T)
  } else {
    counts <- colSums(x[, -1] <= threshold, na.rm = T)
  }
  active <- names(counts[which(counts >= frequency)])
  active
  
}

#' Checks whether a certain indicator has changed by `threshold` compared to 
#'    `lookback_days` days ago.
type_change <- function(x, threshold){
  
  x <- x %>%
    tidyr::fill(-date, .direction = "downup") %>%
    filter(date %in% c(max(x$date), min(x$date)))
  
  change <- unlist(x[2, -1] - x[1, -1])
  active <- names(change[which(change >= threshold)])
  active
  
}

#' Checks whether a certain indicator has crossed a specific `threshold`, 
#'    `frequency` times over the last `lookback_days` days. In contrast to 
#'    `type_threshold()`, this function looks at workout specific data.
#' @details
#' For example, a frequency of 2 would mean that if the threshold is crossed 2 
#'    times in the last sessions the indicator would become active. Those 
#'    sessions could be on the same day (not possible in change threshold)
type_session <- function(x, threshold, frequency){
  
  if(!"session_id" %in% colnames(x)) stop("session_id missing in data")
  type_threshold(x[, -2])
  
}

#' Checks whether a specific character value (`threshold`) has been recorded
#'    `frequency` times over the las `lookback_days` days.
type_char <- function(x){
  if(!all(apply(x, 2, is.character))) stop("Provide character data")
  counts <- colSums(x[, -c(1, 2)] == threshold, na.rm = T)
  active <- names(counts[which(counts >= frequency)])
  active
} 

#' Calculate readiness to train conditions from daily data.
#' @param x data.frame; created by `compute_daily_metrics()`
#' @param vars char; vars for which to check condition
#' @param threshold num; numerical threshold to trigger action 
#'    (larger- or lower-equal; see direction)
#' @param frequency num; how often the threshold must be reached to trigger
#'    action
#' @param lookback_days num; number of days to evaluate (from max date in x)
#' @param direction char; geq = greater equal (default) or leq = lower equal
#' @param type char; type of check to conduct: threshold, change, session, char

check_condition <- function(x, 
                            vars, 
                            threshold, 
                            frequency = NULL, 
                            lookback_days,
                            direction = NULL, 
                            type){
  
  condition_check_inputs()
  
  x <- condition_date_filter(x, vars, lookback_days)
  
  if(type == "threshold"){
    active <- type_threshold(x, threshold, frequency)
  } else if(type == "change"){
    active <- type_change(x, threshold)
  } else if(type == "session"){
    active <- type_session(x, threshold, frequency)
  } else {
    active <- type_char(x, threshold, frequency)
  }

  list(
    active_indicator = active,
    threshold = threshold,
    frequency = frequency,
    lookback_days = lookback_days,
    direction = direction,
    type = type
  )
  
}

vars = c("stride")
threshold = "None" 
frequency = 3
lookback_days = 2 
type = "char"

check_condition(x, 
                vars = c("pain_pre", "pain_pos", "pain_dur", "tight_pre"), 
                threshold = 2, 
                frequency = 1, 
                lookback_days = 3, 
                direction = "geq",
                type = "threshold")

