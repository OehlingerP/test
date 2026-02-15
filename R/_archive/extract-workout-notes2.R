#' Function to extract athlete comments from Google Drive Workout Notes Sheet 
#' @param id char; id of Google Drive sheet 
#' 
#' @return 
#' @format A data frame with x rows and 9 variables:
#' \describe{
#'   \item{date}{Date of the Workout}
#'   \item{session_id}{Workout Number of the Day}
#'   \item{name}{Long variable description}
#'   \item{value}{The value the athlete entered (including the explanation)}
#'   \item{value_num}{Only the numerical value if the variable reflects a numerical scale. Otherwise NA.}
#'   \item{scale_desc}{Description of the scale.}
#'   \item{value_char}{All character values including numerical values stored as character.}
#'   \item{days_since}{Today - Date}
#'   \item{var_name}{Short description of variable}
#' }
#' @export

extract_workout_notes <- function(id){
  
  x <- read_sheet(id)
  
  x <- as.data.frame(apply(x, 2, as.character)) 
  
  df_dates <- x %>%
    filter(name == "Session Number") %>%
    tidyr::pivot_longer(-name, names_to = "date") %>%
    select(date, value) %>%
    rename("session_id" = value)
  
  df_notes <- x %>%
    filter(name != "Session Number") %>%
    tidyr::pivot_longer(-name, names_to = "date")
  
  x <- df_dates %>%
    left_join(df_notes) %>%
    mutate(value_num = as.numeric(gsub(" = .*", "", value)),
           scale_desc = ifelse(!is.na(value_num), gsub("*. = ", "", value), NA), 
           value_char = ifelse(is.na(value_num), value, as.character(value_num)), 
           date = as.Date(gsub("\\.*", "", date))) %>%
    filter(date <= Sys.Date())  %>%
    left_join(readRDS("data/long_to_short_names.RDS"))
  
  # Only numerical vars in long format (colnames are short var names)
  x_num_session <- x %>%
    filter(!is.na(value_num)) %>%
    select(date, session_id, var_name, value_num) %>%
    rename("value" = value_num) %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value) %>%
    # PREPARE INTRADAY TREND INDICATORS
    # PAIN TRENDS - BY SESSION
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
  
  # CALCULATE DAILY MIN/MAX DATASET (INSTEAD OF MULTIPLE SESSIONS PER DAY)
    x_num_daily <- x_num_session %>%
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
      
  # all vars; only four cols; values stored as char
  x_char_long <- x %>%
    filter(!name %in% c("Area", "Side")) %>%
    select(date, session_id, var_name, value_char) %>%
    rename("value" = value_char) %>%
    mutate(value = ifelse(value %in% c("NULL", "NA"), NA, value))
  
  # All vars; values stored as char; wide format (colnames are short var names)
  x_char_wide <- x_char_long %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value) 
  
  # define classes for each data.frame
  num_daily_l <- tidyr::pivot_longer(x_num_daily, -date, names_to = "var_name")
  num_daily_w <- x_num_daily
  num_session_l <- tidyr::pivot_longer(x_num_session, 
                                       -c(date, session_id),
                                       names_to = "var_name")
  num_session_w <- x_num_session
  
  out <- list(
    "num" = list(
      "daily" = list(
        "l" = num_daily_l,
        "w" = num_daily_w
      ),
      "session" = list(
        "l" = num_session_l,
        "w" = num_session_w
      )
    ),
    "chr" = list(
      "l" = x_char_long,
      "w" = x_char_wide
    ),
    "raw" = x
  )
  
  class(out) <- c("google_drive_workout_notes", class(out))
  
  return(out)
  
}


