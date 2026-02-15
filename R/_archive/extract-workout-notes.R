#' Function to extract athlete comments from Google Drive Workout Notes Sheet 
#' @param id char; id of Google Drive sheet 
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
           date = as.Date(gsub("\\.*", "", date)))
  
  class(x) <- c("google_drive_workout_notes", class(x))
  
  return(x)
  
}


