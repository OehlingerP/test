#' Function to extract athlete comments from Google Drive Workout Notes Sheet 
#' @param id char; id of Google Drive sheet

extract_workout_notes <- function(id) {
  
  raw_workouts <- read_sheet(id, sheet = "Workouts") %>%
    mutate(across(everything(), as.character))
  
  df_dates_workouts <- raw_workouts %>%
    filter(name == "Session Number") %>%
    tidyr::pivot_longer(-name, names_to = "date") %>%
    select(date, value) %>%
    rename("session_id" = value)
  
  df_values_workouts <- raw_workouts %>%
    filter(name != "Session Number") %>%
    tidyr::pivot_longer(-name, names_to = "date") %>% 
    mutate(value = ifelse(value %in% c("NULL", "NA"), NA, value))
  
  raw_condition <- read_sheet(id, sheet = "Condition") %>%
    mutate(across(everything(), as.character)) %>%
    tidyr::pivot_longer(-name, names_to = "date") %>%
    mutate(session_id = 1) %>%
    select(date, session_id, name, value)
  
  out <- df_dates_workouts %>%
    left_join(df_values_workouts) %>%
    rbind(raw_condition) %>%
    mutate(value_num = as.numeric(gsub(" = .*", "", value)),
           scale_desc = ifelse(!is.na(value_num), gsub("*. = ", "", value), NA), 
           value_char = ifelse(is.na(value_num), value, as.character(value_num)), 
           date = as.Date(gsub("\\.*", "", date))) %>%
    filter(date < Sys.Date())  %>%
    left_join(jsonlite::fromJSON("data/long-to-short-names.json", simplifyDataFrame = T))
  
  out
}

#' Create data.frames in wide format for session (workout; multiple entries per 
#'     day possible) level data. 
#' @param raw data.farme; data.frame created by `extract_workout_notes()`

build_session_data <- function(raw) {
  
  if(!all(c("value_num", "date", "session_id", 
            "var_name", "value_char", "name") %in% 
          colnames(raw))) stop("Not all columns required provided")
  
  num_session <- raw %>%
    filter(!is.na(value_num)) %>%
    select(date, session_id, var_name, value_num) %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value_num)
    
  chr_session <- raw %>%
    filter(!is.na(value_char), 
           !name %in% c("Area", "Side")) %>%
    select(date, session_id, var_name, value_char) %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value_char)
  
  list(
    num_session = num_session,
    chr_session = chr_session
  )
  
}

#' Compute within day metrics for numerical variables
#' @param num_session data.frame; created by `build_session_data()`

compute_session_metrics <- function(num_session) {
  
  num_session %>%
    mutate(
      pain_inc_pre_dur = pain_dur - pain_pre,
      pain_inc_pre_pos = pain_pos - pain_pre,
      pain_inc_dur_pos = pain_pos - pain_dur
    ) %>%
    # PAIN TRENDS - BETWEEN SESSIONS IN A DAY
    group_by(date) %>%
    mutate(
      pain_day_pre = last(pain_pre) - first(pain_pre)
    ) %>%
    ungroup()
  
}

#' Compute daily metrics for numerical variables
#' @param num_session data.frame; created by `build_session_data()`

compute_daily_metrics <- function(num_session) {
  
  num_session %>%
    tidyr::pivot_longer(c(-date, -session_id), names_to = "var_name") %>%
    group_by(date, var_name) %>%
    summarise(min_val = min(value, na.rm = T), 
              max_val = max(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(value = ifelse(var_name %in% c("readiness", "sleep"), 
                          min_val, 
                          max_val)) %>%
    filter(!is.infinite(value)) %>%
    select(-min_val, -max_val) %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value) %>%
    group_by(date) %>%
    mutate(pain_max = max(c(pain_pre, pain_pos, pain_dur)),
           tight_max = max(c(tight_pre, tight_pos, tight_dur))) %>%
    ungroup()
  
}

#' Wrapper function for all the above to have one function to prepare workout
#'    notes
#' @param id char; id of Google Drive sheet 

prepare_workout_data <- function(id) {
  
  raw <- extract_workout_notes(id)
  
  session <- build_session_data(raw)
  
  num_session_metrics <- compute_session_metrics(session$num_session)
  
  num_daily <- compute_daily_metrics(num_session_metrics)
  
  list(
    raw         = raw,
    num_session = num_session_metrics,
    num_daily   = num_daily,
    chr_session = session$chr_session
  )
}

