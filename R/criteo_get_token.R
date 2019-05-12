criteo_get_token <- function(client_id, client_secret){
  # Create token object and save it into .RDS file (and return object, if needed).
  # You need to create REST API user in Criteo Marketing account.
  # Args:
  #   client_id: Login (Client ID) of Criteo REST API user;
  #   client_secret: Client Secret of Criteo REST API user.
  #
  # Returns:
  #   By default, into env: object with all parameters, that needed to create Criteo queries;
  #                                                                     into folder with script: "criteo_tokes.RDS" file with same object.

  body <- list(client_id = client_id, client_secret = client_secret, grant_type = 'client_credentials')
  results <- httr::POST(url = 'https://api.criteo.com/marketing/oauth2/token', body = body, encode = 'form')
  token <- httr::content(results)[[1]]

  #  Simple check to valid Criteo responce. All tokens have same length (941 symbol).
  if(nchar(token) != 941){
    cat("Invalid authorization! Criteo returned '", token, "'.", sep = '')
  } else {
    token <- list(token = token, client_id = client_id, client_secret = client_secret, create_time = Sys.time())
    save(token, file = 'criteo_token.RDS')
    return(token)
  }
}
