#' Get the current training status.
#' @param x data.frame; created with extract-athlete-comments (wide format)
#' @param status_date date; date for which to calculate training status

get_training_status <- function(x, status_date){
  
  x <- x %>%
    filter(date <= status_date) %>%
    mutate(days_since = as.numeric(status_date-date))
  
  # Criterias are enforced top-down
  # Criteria 1: (RED FLAG)
  immediate_stop <- ifelse(
    x %>%
      filter(days_since <= 3) %>%
      filter(`Pain During (0-10)` >= 5 |
               (`Pain During (0-10)`-`Pain Pre (0-10)` >= 2 & `Pain Pre (0-10)` > 0) |
               `Pain Post (0-10)` >= 5 |
               (`Stride changes` != "None" & `Stride changes` != "") |
               `Downhill tolerance` == "Intolerable"
      ) %>%
      nrow() > 0,
    T, 
    F
  )
  
  # Criteria 2: (RED FLAG)
  persistent_irritation <- ifelse(
    x %>%
      filter(days_since <= 3) %>%
      filter(`Pain Pre (0-10)` >= 4 |
               `Pain During (0-10)` == 4) %>%
      nrow() > 0 |
      x %>%
      filter(days_since <= 3) %>%
      mutate(tight = max(`Tightness Pre (0-10)`, 
                         `Tightness During (0-10)`, 
                         `Tightness Post (0-10)`)) %>% 
      group_by(date) %>%
      summarize(pain = max(`Pain Pre (0-10)`), 
                tight = max(tight)) %>%
      ungroup() %>%
      filter(tight >= 7 |
               pain >= 4) %>%
      select(date) %>%
      unique() %>%
      nrow() > 2,
    T, 
    F
  )
  
  # Criteria 3: (YELLOW FLAG)
  run_reduction_pain <- ifelse(
    x %>%
      filter(days_since <= 5) %>%
      filter(`Pain Pre (0-10)` >= 3 |
             `Pain During (0-10)` >= 3 |
             `Downhill tolerance` == "Reduced") %>%
      nrow() > 0 |
    x %>%
      filter(days_since <= 5) %>%
      filter(`Tightness During (0-10)` >= 6) %>%
      select(date) %>%
      unique() %>%
      nrow() > 1,
    T, 
    F
  )
  
  # Criteria 4: (YELLOW FLAG)
  run_reduction_fatigue <- ifelse(
    x %>%
      filter(days_since <= 2) %>%
      filter(`Accumulated fatigue (0-10)` >= 8 |
             `Overall readiness (0-10)` <= 3) %>%
      select(date) %>%
      unique() %>%
      nrow() > 1,
    T, 
    F
  )
  
  # Criteria 5: (YELLOW FLAG)
  run_reduction_sleep <- ifelse(
    x %>%
      filter(days_since <= 1) %>%
      filter(`Sleep quality last night (0-10)` <= 3) %>%
      select(date) %>%
      unique() %>%
      nrow() > 1,
    T, 
    F
  )
  
  
  # Criteria 6: (GREEN FLAG)
  increase_cross_volume <- ifelse(
    x %>%
      filter(days_since <= 4) %>%
      filter(`Pain During (0-10)` > 2 |
             `Pain Pre (0-10)` > 2 |
             `Pain Post (0-10)` > 2 |
             `Tightness Pre (0-10)` >= 4 |
             `Accumulated fatigue (0-10)` > 4 |
             `Motivation before` == "Low"
      ) %>%
      select(date) %>%
      unique() %>%
      nrow() <= 1 &
    x %>%
      filter(days_since <= 2) %>%
      filter(`Sleep quality last night (0-10)` <= 3) %>%
      select(date) %>%
      unique() %>%
      nrow() <= 1,
    T, 
    F
  )
  
  # Criteria 7: (GREEN FLAG)
  increase_run_volume <- ifelse(
    x %>%
      filter(days_since <= 5) %>%
      filter(`Pain During (0-10)` > 1 |
             `Pain Pre (0-10)` > 1 |
             `Pain Post (0-10)` > 1 |
             `Tightness Pre (0-10)` >= 3 |
             `Accumulated fatigue (0-10)` > 4 |
             `Motivation before` == "Low" |
             `Stride changes` != "None" |
             !`Downhill tolerance` %in% c("Not tested", "Good") |
             !Compliance %in% c("As planned", "Modified (increased)")
      ) %>%
      nrow() == 0 &
    x %>%
      filter(days_since <= 2) %>%
      filter(`Sleep quality last night (0-10)` <= 3) %>%
      select(date) %>%
      unique() %>%
      nrow() <= 1,
    T, 
    F
  )
  
  # Criteria 8: (GRAY FLAG)
  baseline_controlled_training <- ifelse(
    any(immediate_stop, 
        persistent_irritation,
        run_reduction_pain,
        run_reduction_fatigue,
        increase_cross_volume,
        increase_run_volume), 
    F, 
    T
  )
  
  # RECOMMEND ACTION
  if (immediate_stop) {
    return(list(
      title = "NO RUNNING",
      flag = "RED",
      duration = "2-3 days minimum",
      reason = "Sudden Critical Pain - Cannot be ignored", 
      action = c(
        "Strength + low-impact cross-training only if Pain ≤ 3",
        "If Pain Pre ≥ 4 next morning, move to Full Break protocol"
      )
    ))
  } else if(persistent_irritation){
    return(list(
      title = "NO RUNNING",
      flag = "RED",
      duration = "3-5 days",
      reason = "Persistent Irritation - Avoid bigger injury",
      action = c(
        "Strength + low-impact cross-training only if Pain ≤ 3",
        "If Pain Pre ≥ 4 next morning, move to Full Break protocol"
      )
    ))
  } else if(run_reduction_fatigue & run_reduction_pain){
    return(list(
      title = "REDUCE OVERALL VOLUME & REDUCE RUNNING VOLUME",
      flag = "RED",
      duration = "3-4 Days",
      reason = c("Highly Fatigued - Be very careful not to get sick or injured.",
                 "Smaller Niggles Emerge - First signs of injury. Your system cannot cope with the current load."),
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
  } else if(run_reduction_fatigue){
    return(list(
      title = "REDUCE OVERALL VOLUME",
      flag = "YELLOW",
      duration = "3-4 Days",
      reason = "Highly Fatigued - Be very careful not to get sick or injured.",
      action = c(
        "Reduce total training load 40–60%",
        "Only easy aerobic", 
        "No muscular strain > 5", 
        "Running allowed only if structural metrics are green (Pain ≤ 1)"
      )
    ))
  } else if(run_reduction_pain){
    return(list(
      title = "REDUCE RUNNING VOLUME",
      flag = "YELLOW",
      duration = "Reassess daily.",
      reason = "Smaller Niggles Emerge - First signs of injury. Your system cannot cope with the current load.",
      action = c(
        "Reduce run volume 30–50%",
        "Remove intensity and downhill", 
        "Maintain aerobic load via cycling"
      )
    ))
  } else if(run_reduction_sleep){
    return(list(
      title = "REDUCE OVERALL VOLUME",
      flag = "YELLOW",
      duration = "Reassess daily.",
      reason = "Insomnia - Dampens recovery and increases injury and sickness risk.",
      action = c(
        "Reduce volume today",
        "Reduce overall stress", 
        "Your main training goal today is to sleep well"
      )
    ))
  } else if(increase_cross_volume){
    return(list(
      title = "INCREASE CROSS VOLUME",
      flag = "GREEN",
      duration = "Upcoming Week",
      reason = "Well Rested, No Concerning Pain",
      action = c(
        "Add cycling or strength volume",
        "Keep run volume constant"
      )
    ))
  } else if(increase_run_volume){
    return(list(
      title = "INCREASE RUN VOLUME",
      flag = "GREEN",
      duration = "Upcoming Week",
      reason = "Well Rested, No Pain, No Other Concerns",
      action = c(
        "Increase run volume 5–10% max",
        "Do not increase intensity in same week"
      )
    ))
  } else if(baseline_controlled_training){
    
    # WHY NOT INCREASING CROSS VOLUME
      thresholds <- data.frame(
        name = c(
          "Pain During (0-10)",
          "Pain Pre (0-10)",      
          "Pain Post (0-10)",     
          "Tightness Pre (0-10)",            
          "Accumulated fatigue (0-10)",      
          "Motivation before",              
          "Sleep quality last night (0-10)"),
        threshold = c("above 2", "above 2", "above 2", 
                      "above 3", "above 4", "low", "below 4"))
    
      flags <- x %>%
        filter(days_since <= 4) %>%
        mutate(
          `Pain During (0-10)`   = `Pain During (0-10)` > 2,
          `Pain Pre (0-10)`      = `Pain Pre (0-10)` > 2,
          `Pain Post (0-10)`     = `Pain Post (0-10)` > 2,
          `Tightness Pre (0-10)`            = `Tightness Pre (0-10)` >= 4,
          `Accumulated fatigue (0-10)`      = `Accumulated fatigue (0-10)` > 4,
          `Motivation before`               = `Motivation before` == "Low",
          `Sleep quality last night (0-10)` = `Sleep quality last night (0-10)` <= 3
        )
    
      flags <- cbind("date" = flags$date, flags[, sapply(flags, is.logical)]) %>%
        tidyr::pivot_longer(-date) %>%
        group_by(date, name) %>%
        summarize(value = max(value, na.rm = T)) %>%
        ungroup() %>%
        filter(value > 0) %>%
        group_by(name) %>%
        summarize(value = sum(value)) %>%
        left_join(thresholds) %>%
        mutate(name = gsub(" \\(0-10\\)", "", name), 
               comment = paste(value, ifelse(value == 1, "time", "times"), name, "was", threshold))
    
    return(list(
      title = "BASELINE",
      flag = "GRAY",
      duration = "Until State Changes",
      reason = c("Nothing concerning but not optimal",
                 "No increase in cross-volume because", flags$comment),
      action = c(
        "Continue with planned training",
        "If new week should be planned, repeat the previous week until status improves."
      )
    ))
  }
  
}
