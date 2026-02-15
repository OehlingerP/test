# File: R/classes.R

library(R6)

# Factor class: represents a contributing factor to an Indicator
Condition <- R6Class(
  "Condition",
  public = list(
    description = NULL,  # Text description of the factor
    active = TRUE,       # Logical flag to indicate if factor is considered active
    indicators = NULL,   # Names of Indicators this Factor can affect
    recommendation_fun = NULL,  # Function to compute Recommendation based on data
    initialize = function(description, indicators, recommendation_fun, active = TRUE) {
      self$description <- description
      self$indicators <- indicators
      self$recommendation_fun <- recommendation_fun
      self$active <- active
    },
    get_recommendation = function(indicator_name, data) {
      if (!self$active) return(NULL)
      self$recommendation_fun(indicator_name, data)
    }
  )
)

# Indicator class: represents a training status or readiness measure
Alert <- R6Class(
  "Alert",
  public = list(
    name = NULL,         # Name of the indicator
    value = NULL,        # Current numeric or categorical value
    factors = list(),    # List of Factor objects attached to this Indicator
    active_rule = NULL,  # Optional function to determine if Indicator itself is active
    initialize = function(name, value = NA, active_rule = NULL) {
      self$name <- name
      self$value <- value
      self$active_rule <- active_rule
    },
    add_factor = function(factor) {
      self$factors <- c(self$factors, list(factor))
    },
    is_active = function(data) {
      if (is.null(self$active_rule)) return(TRUE)
      self$active_rule(self$factors, data)
    },
    get_recommendations = function(data) {
      if (!self$is_active(data)) return(NULL)
      recs <- lapply(self$factors, function(f) f$get_recommendation(self$name, data))
      recs <- recs[!sapply(recs, is.null)]
      return(recs)
    }
  )
)
