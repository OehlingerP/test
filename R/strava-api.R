#' This function is taken from rStrava. I did not use the full package because
#'   it appears to have many bugs in other functions

strava_oauth <- function (app_name, 
                          app_client_id, 
                          app_secret, 
                          app_scope = "public",
                          cache = FALSE){
  strava_app <- httr::oauth_app(appname = app_name, 
                                key     = app_client_id,
                                secret  = app_secret)
  strava_end <- httr::oauth_endpoint(
    request   = "https://www.strava.com/oauth/authorize?",
    authorize = "https://www.strava.com/oauth/authorize",
    access    = "https://www.strava.com/oauth/token")
  httr::oauth2.0_token(endpoint = strava_end, 
                       app      = strava_app, 
                       scope    = app_scope,
                       cache    = cache)
}





?rStrava::athl_fun

app_name <- 'Training Status' # chosen by user
app_client_id  <- '203917' # an integer, assigned by Strava
app_client_secret <- '098b44bc2468a93aa698a3e92f6466cf3c2e52d7' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(
  token = strava_oauth(app_name,
                       app_client_id,
                       app_secret,
                       app_scope="activity:read_all"))






strava_oauth


myinfo <- get_athlete(stoken, id = '99086278')
head(myinfo)

my_acts <- get_activity_list(stoken, after = as.Date('2020-12-31'))





plot_spdsplits(, stoken, units = 'imperial')

activity <- get_activity(as.character(my_acts[[1111]]$id), stoken)
test <- rStrava::compile_activity(activity)


streams <- get_activity_streams(
  get_activity_list(stoken, after = as.Date('2025-12-31')),
  stoken,
  types = c("time", "heartrate"),
  resolution = "high"
)
