#' Function to extract athlete comments from TrainingPeaks .csv-file and store
#'    in data.frame
#' @param x data.frame; data with athlete comment column
#' @param name char; name of column containing athlete comments.
#' @export

extract_athlete_comments <- function(x, name){
  
  v <- x[, name]
  
  ls_v <- strsplit(v, " \\*|--- |- |: |\\*")
  
  ls_v <- ls_v[sapply(ls_v, length) == 53]
  
  ls_comments <- lapply(seq_along(ls_v), function(i){
    
    k <- ls_v[[i]]
    
    k <- trimws(k, "both")
    
    # extract meta data
    meta <- unlist(strsplit(k[2], " "))
    
    entry_length = c(3, 6, 5, 2, 3, 3)
    
    idx_types <- cumsum(c(4, head(entry_length, -1)*2+1))
    
    idx_questions <- sequence(entry_length)*2-1+rep(idx_types, entry_length)
    
    data.frame("WorkoutID" = i, 
               "date" = as.Date(meta[1], "%d/%m/%Y"), 
               "name" = paste(meta[2], meta[3]), 
               "type" = rep(k[idx_types], times = entry_length), 
               "question" = k[idx_questions], 
               "value" = k[idx_questions+1])
    
  })
  
  out <- list()
  
  out$long <- data.table::rbindlist(ls_comments)

  # prepare data in wide format
  df_wide <- out$long %>%
    select(-type) %>%
    filter(!question %in% c("Area", "Side")) %>%
    tidyr::pivot_wider(names_from = question, values_from = value) 
  
  idx <- grep("0-10", colnames(df_wide))
  
  df_wide[, idx] <- apply(df_wide[, idx], 2, as.numeric)
  
  out$wide <- df_wide
  
  return(out)
  
}
