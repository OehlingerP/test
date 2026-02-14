#' Get the current training status.
#' @param x data.frame; of class "google_drive_workout_notes" (using 
#'    `extract_workout_notes()`)
#' @param status_date date; date for which to calculate training status

get_training_status <- function(x, status_date){
  
  if (!inherits(x, "google_drive_workout_notes")) {
    stop("x must be a google_drive_workout_notes object.")
  }
  
  x <- x %>%
    filter(date <= status_date) %>%
    mutate(days_since = as.numeric(status_date-date))

  # Numerical Rules
  x_num <- x %>%
    filter(!is.na(value_num)) %>%
    mutate(name = ifelse(grepl("Tight", name), "Tightness", name),
           name = ifelse(grepl("Pain", name), "Pain", name)) %>%
    group_by(date, name, days_since) %>%
    summarise(min_val = min(value_num), 
              value = max(value_num)) %>%
    ungroup() %>%
    mutate(value = ifelse(name %in% 
                            c("Overall readiness", 
                              "Sleep quality last night"),
                          min_val, 
                          value)) %>%
    select(-min_val) %>%
    tidyr::pivot_wider(names_from = name, values_from = value) 
  
  # Character Rules
  x_char <- x %>%
    filter(!is.na(value_char), name != "Area", name != "Side") %>%
    select(date, days_since, session_id, name, value_char) %>%
    tidyr::pivot_wider(names_from = name, values_from = value_char) 
  
  # Stop Training - Pain -------------------------------------------------
  x_stop_pain <- left_join(
    # Numerical Rules
    x_num  %>%
      filter(days_since <= 3) %>%
      mutate(pain_flag = Pain > 4) %>%
      select(date, days_since, pain_flag), 
    # Character Rules
    x_char  %>%
      filter(days_since <= 3) %>%
      mutate(stride_flag = 
               !`Stride changes` %in% c("None","","NA","NULL"),
             downhill_flag = `Downhill tolerance` == "Intolerable") %>%
      group_by(date, days_since) %>%
      summarize(across(c(stride_flag, downhill_flag), ~as.logical(max(.)))) %>%
      select(date, days_since, stride_flag, downhill_flag)
  ) 
  
  # Stop Training - Persistent irritation --------------------------------
  x_stop_irr <- x_num  %>%
    filter(days_since <= 3) %>%
    mutate(pain_flag = Pain >= 4, 
           trend_pain_tight_flag = sum(Pain > 3) + sum(Tightness > 6) > 1) %>%
    select(date, days_since, pain_flag, trend_pain_tight_flag)
  
  # Reduce Running - Pain -----------------------------------------------
  x_reduce_pain <- left_join(
    # Numerical Rules
    x_num  %>%
      filter(days_since <= 5) %>%
      mutate(pain_flag = Pain >= 3, 
             trend_tight_flag = sum(Tightness > 5) > 2) %>%
      select(date, days_since, pain_flag, trend_tight_flag), 
    # Character Rules
    x_char  %>%
      filter(days_since <= 5) %>%
      mutate(downhill_flag = `Downhill tolerance` == "Reduced") %>%
      group_by(date, days_since) %>%
      summarize(across(c(downhill_flag), ~as.logical(max(.)))) %>%
      select(date, days_since, downhill_flag)
  ) 

  # Reduce Training - Fatigue -------------------------------------------
  x_reduce_fatigue <- x_num  %>%
    filter(days_since <= 2) %>%
    mutate(fatigue_flag = `Accumulated fatigue` >= 7, 
           readiness_flag = `Overall readiness` <= 3) %>%
    select(date, days_since, fatigue_flag, readiness_flag)
    
  # Reduce Training - Sleep ---------------------------------------------
  x_reduce_sleep <- x_num  %>%
    filter(days_since <= 1) %>%
    mutate(trend_sleep = sum(`Sleep quality last night` <= 3) > 1) %>%
    select(date, days_since, trend_sleep)
  
  # Increase Cross Volume -----------------------------------------------
  x_increase_cross <- left_join(
    # Numerical Rules
    x_num  %>%
      filter(days_since <= 4) %>%
      mutate(pain_flag = Pain < 3,
             tight_flag = Tightness < 6,
             fatigue_flag = `Accumulated fatigue` < 5) %>%
      select(date, days_since, pain_flag, tight_flag, fatigue_flag), 
    # Character Rules
    x_char  %>%
      filter(days_since <= 4) %>%
      mutate(motivation_flag = `Motivation before` != "Low") %>%
      group_by(date, days_since) %>%
      summarize(across(c(motivation_flag), ~as.logical(max(.)))) %>%
      select(date, days_since, motivation_flag)
  ) %>% 
    left_join(
      x_num  %>%
        filter(days_since <= 2) %>%
        mutate(trend_sleep = sum(`Sleep quality last night` <= 3) < 2) %>%
        select(date, days_since, trend_sleep)
    )

  # Increase Run Volume ------------------------------------------------
  x_increase_run <- left_join(
    # Numerical Rules
    x_num  %>%
      filter(days_since <= 5) %>%
      mutate(pain_flag = Pain < 2,
             tight_flag = Tightness < 6,
             fatigue_flag = `Accumulated fatigue` < 5) %>%
      select(date, days_since, pain_flag, tight_flag, fatigue_flag), 
    # Character Rules
    x_char  %>%
      filter(days_since <= 5) %>%
      mutate(motivation_flag = `Motivation before` != "Low",
             stride_flag = `Stride changes` == "None", 
             downhill_flag = `Downhill tolerance` %in% c("Not tested", "Good")) %>%
      group_by(date, days_since) %>%
      summarize(across(c(motivation_flag), ~as.logical(max(.)))) %>%
      select(date, days_since, motivation_flag)
  ) %>% 
    left_join(
      x_num  %>%
        filter(days_since <= 2) %>%
        mutate(trend_sleep = sum(`Sleep quality last night` <= 3) < 2) %>%
        select(date, days_since, trend_sleep)
    )
  
  rule_stop_pain <- colSums(x_stop_pain[, -c(1, 2)], na.rm = T) > 0
  rule_stop_irr  <- colSums(x_stop_irr[, -c(1, 2)], na.rm = T) > 0
  rule_reduce_pain <- colSums(x_reduce_pain[, -c(1, 2)], na.rm = T) > 0
  rule_reduce_fatigue  <- colSums(x_reduce_fatigue[, -c(1, 2)], na.rm = T) > 1
  rule_reduce_sleep <- colSums(x_reduce_sleep[, -c(1, 2)], na.rm = T) > 0
  rule_increase_cross <- colSums(x_increase_cross[, -c(1, 2)], na.rm = T) >= 3
  rule_increase_run <- apply(x_increase_run[, -c(1, 2)], 2, function(x) all(x, na.rm = T))
  
  # RECOMMEND ACTION
  if (any(rule_stop_pain)) {
    
    return(list(
      title = "NO RUNNING",
      flag = "RED",
      duration = "2-3 days minimum",
      reason = "Sudden Critical Pain - Cannot be ignored",
      active_flags = names(rule_stop_pain)[rule_stop_pain == T],
      action = c(
        "Strength + low-impact cross-training only if Pain ≤ 3",
        "If Pain Pre ≥ 4 next morning, move to Full Break protocol"
      )
    ))
  } else if(any(rule_stop_irr)){
    
    return(list(
      title = "NO RUNNING",
      flag = "RED",
      duration = "3-5 days",
      reason = "Persistent Irritation - Avoid bigger injury",
      active_flags = names(rule_stop_irr)[rule_stop_irr == T],
      action = c(
        "Strength + low-impact cross-training only if Pain ≤ 3",
        "If Pain Pre ≥ 4 next morning, move to Full Break protocol"
      )
    ))
    
  } else if(any(rule_reduce_pain) & any(rule_reduce_fatigue)){
    
    return(list(
      title = "REDUCE OVERALL VOLUME & REDUCE RUNNING VOLUME",
      flag = "RED",
      duration = "3-4 Days",
      reason = c("Highly Fatigued - Be very careful not to get sick or injured.",
                 "Smaller Niggles Emerge - First signs of injury. Your system cannot cope with the current load."),
      active_flags = c(
        names(rule_reduce_pain)[rule_reduce_pain == T],
        names(rule_reduce_fatigue)[rule_reduce_fatigue == T]),
      action = c(
        "Reduce total training load 40–60%",
        "Only easy aerobic", 
        "No muscular strain > 5", 
        "Running allowed only if structural metrics are green (Pain ≤ 1)", 
        "Reduce run volume 30–50%",
        "Remove intensity and downhill", 
        "Maintain aerobic load via cycling"
      )
    ))
    
  } else if(any(rule_reduce_fatigue)){
    
    return(list(
      title = "REDUCE OVERALL VOLUME",
      flag = "YELLOW",
      duration = "3-4 Days",
      reason = "Highly Fatigued - Be very careful not to get sick or injured.",
      active_flags = names(rule_reduce_fatigue)[rule_reduce_fatigue == T],
      action = c(
        "Reduce total training load 40–60%",
        "Only easy aerobic", 
        "No muscular strain > 5", 
        "Running allowed only if structural metrics are green (Pain ≤ 1)"
      )
    ))
    
  } else if(any(rule_reduce_pain)){
    
    return(list(
      title = "REDUCE RUNNING VOLUME",
      flag = "YELLOW",
      duration = "Reassess daily.",
      reason = "Smaller Niggles Emerge - First signs of injury. Your system cannot cope with the current load.",
      active_flags = names(rule_reduce_pain)[rule_reduce_pain == T],
      action = c(
        "Reduce run volume 30–50%",
        "Remove intensity and downhill", 
        "Maintain aerobic load via cycling"
      )
    ))
    
  } else if(rule_reduce_sleep){
    return(list(
      title = "REDUCE OVERALL VOLUME",
      flag = "YELLOW",
      duration = "Reassess daily.",
      reason = "Insomnia - Dampens recovery and increases injury and sickness risk.",
      active_flags = names(rule_reduce_sleep)[rule_reduce_sleep == T],
      action = c(
        "Reduce volume today",
        "Reduce overall stress", 
        "Your main training goal today is to sleep well"
      )
    ))
  } else if(all(rule_increase_cross)){
    return(list(
      title = "INCREASE CROSS VOLUME",
      flag = "GREEN",
      duration = "Until Status Changes",
      reason = "Well Rested, No Concerning Pain",
      active_flags = c("Why we do not increase run volume:", 
                       names(rule_increase_run)[!rule_increase_run]),
      action = c(
        "Add cycling or strength volume",
        "Keep run volume constant"
      )
    ))
  } else if(all(rule_increase_run)){
    return(list(
      title = "INCREASE RUN VOLUME",
      flag = "GREEN",
      duration = "Until Status Changes",
      reason = "Well Rested, No Pain, No Other Concerns",
      active_flags = "",
      action = c(
        "Increase run volume 5–10% max",
        "Do not increase intensity in same week"
      )
    ))
  } else {
    
    return(list(
      title = "BASELINE",
      flag = "GRAY",
      duration = "Until Status Changes",
      reason = c("Nothing concerning but not optimal"),
      active_flags = c("Why we do not increase run volume:", 
                       names(rule_increase_run)[!rule_increase_run]),
      action = c(
        "Continue with planned training",
        "If new week should be planned, repeat the previous week until status improves."
      )
    ))
  }
  
}









# # filter for duration
# x_d1 <- x_num %>% filter(days_since <= 1)
# x_d2 <- x_num %>% filter(days_since <= 2)
# x_d3 <- x_num %>% filter(days_since <= 3)
# x_d4 <- x_num %>% filter(days_since <= 4)
# x_d5 <- x_num %>% filter(days_since <= 5)
# 
# x_d5 <- x_d5 %>%
#   mutate(
#     # Reduce Training - Pain
#     reduce_pain = 
#       (Pain >= 3 | sum(Tightness >= 6) > 1), 
#     # Increase Run Volume
#     increase_run = 
#       Pain < 2 &
#       Tightness < 6 &
#       `Accumulated fatigue` < 5
#   )
# 
# x_d4 <- x_d4 %>%
#   mutate(
#     # Increase Cross Volume
#     increase_cross = 
#       Pain < 3 &
#       Tightness < 6 &
#       `Accumulated fatigue` < 5
#   )
# 
# 
# x_d3 <- x_d3 %>%
#   mutate(
#     # Stop Training
#     stop = Pain >= 5,
#     # Stop Training - Persistent Irritation
#     stop_irr = (Pain >= 4 | 
#                   sum(Pain > 3) + sum(Tightness > 6) > 1))
# 
# x_d2 <- x_d2 %>%
#   mutate(
#     # Reduce Training - Fatigue
#     reduce_fatigue =
#       `Accumulated fatigue` >= 8 |
#       `Overall readiness` <= 3, 
#     # Increase Cross
#     increase_cross = sum(`Sleep quality last night` <= 3) < 2,
#     # Increase Run
#     increase_run = increase_cross
#   ) 
# 
# x_d1 <- x_d1 %>%
#   mutate(
#     # Reduce Training - Sleep
#     reduce_sleep = sum(`Sleep quality last night` <= 3) > 1
#   )
# 
# 
# # filter for duration
# x_d3_char <- x_char %>% filter(days_since <= 3)
# x_d4_char <- x_char %>% filter(days_since <= 4)
# x_d5_char <- x_char %>% filter(days_since <= 5)