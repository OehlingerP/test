# class(x)
# 
# x <- ls_notes$num$daily$w
# vars <- c("pain_pre", "pain_pos", "pain_max", "tight_pre")
# threshold = 3
# threshold_times = 1
# change_threshold = 2
# n_days <- 2

condition_function_daily <- function(x, 
                                          vars,
                                          threshold = NULL, 
                                          threshold_times = 1,
                                          change_threshold = NULL,
                                          n_days) {
  
  active_vars_threshold <- NULL
  active_vars_trend <- NULL
  
  # General checks -----
  if(all(!vars %in% colnames(x))) stop("var must be a column of x")
  
  if (!inherits(x, "google_drive_workout_notes")) {
    stop("x must be a google_drive_workout_notes object.")
  }
  
  if (!inherits(x, "daily")) {
    stop("x must be the daily min/max aggregates.
         Select correct list object (class = daily)")
  }
  
  if (!inherits(x, "num_wide")) {
    stop("x must be numerical data in wide format. 
         Select correct list object (class = num_wide)")
  }
  
  # select relevant vars ----
  x <- x[, c("date", vars)] %>%
    filter(date >= max(x$date, na.rm = TRUE) - n_days) %>%
    arrange(date)
  
  # threshold condition -----
  if(!is.null(threshold)){
    if(!is.numeric(threshold)) stop("threshold must be numeric")
    if(!is.numeric(threshold_times)) stop("threshold_times must be numeric")
    
    y <- colSums(x[, -1] >= threshold, na.rm = T)
    active_vars_threshold <- names(y[which(y >= threshold_times)])
  }
    
  # trend condition -----
  if(!is.null(change_threshold)){
    if(!is.numeric(threshold)) stop("threshold must be numeric")
    if(!is.numeric(threshold_times)) stop("threshold_times must be numeric")
    
    # replace NA's by previous values
    x <- x %>%
      tidyr::fill(-date, .direction = "downup") %>%
      filter(date %in% c(max(x$date), min(x$date)))
    
    y <- unlist(x[2, -1] - x[1, -1])
    active_vars_trend <- names(y[which(y >= change_threshold)])
    
  }
  
  list(
    active_threshold = active_vars_threshold, 
    active_trend = active_vars_trend,
    text_threshold = ifelse(length(active_vars_threshold) > 1,
                            paste0("The indicators ",
                                   paste(active_vars_threshold, collapse = ", "),
                                   ", are above the threshold of ", 
                                   threshold, ", ", threshold_times, 
                                  " times in the last ", n_days, " days."), 
                            ifelse(length(active_vars_threshold) > 0,
                                   paste0("The indicator ",
                                          active_vars_threshold,
                                          " is above the threshold of ", 
                                          threshold, ", ", threshold_times, 
                                          " times in the last ", n_days, " days."),  
                                   NULL)),
    text_trend = ifelse(length(active_vars_trend) > 0,
                        paste0("The indicators ",
                               paste(active_vars_trend, collapse = ", "),
                               ", changed by more than ", 
                               change_threshold, ", in the last ", 
                               n_days, " days."), 
                        NULL)
  )
}

