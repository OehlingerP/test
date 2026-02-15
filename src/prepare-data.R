#' Workout Notes Data
#'
#' A dataset containing all the data entered by the athlete
#'
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

# import workout notes
  df_notes <- extract_workout_notes(gs4_find("Workout Notes")$id) %>%
    filter(date <= Sys.Date())  %>%
    mutate(days_since = as.numeric(status_date-date)) %>%
    left_join(readRDS("data/long_to_short_names.RDS"))
    
  # Only numerical vars in long format (colnames are short var names)
  notes_num_long <- df_notes %>%
    filter(!is.na(value_num)) %>%
    select(date, session_id, var_name, value_num)
  
  # Only numerical vars; wide format (colnames are short var names)
  notes_num_wide <- notes_num_long %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value_num) 

  # all vars; only four cols; values stored as char
  notes_chr_long <- df_notes %>%
    filter(!name %in% c("Area", "Side")) %>%
    select(date, session_id, var_name, value_char)
  
  # All vars; values stored as char; wide format (colnames are short var names)
  notes_chr_wide <- notes_chr_long %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value_char) 
  