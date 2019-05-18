criteo_auth <- function(client_id = NULL,
                        client_secret = NULL,
                        save_to_env = F){
  #' Criteo REST API Auth
  #'
  #' Create token object and save it into .RDS file. Return object, if needed.
  #' You need to create REST API user in Criteo Marketing account.
  #'
  #' @param client_id Login (Client ID) of Criteo REST API user.
  #' @param client_secret Client Secret of Criteo REST API user.
  #' @param save_to_env by default, False. If TRUE, saving auth data into global environment.
  #' @return By default, file "criteo_tokes.RDS" into folder with script and success message.
  #'
  #' This is list with 4 parameters:
  #' \itemize{
  #'   \item token: returned token with 300 seconds lifetime.
  #'   \item client_id: your client id from Criteo REST API. It need for future calls.
  #'   \item client_secret: your client secret from Criteo REST API. It need for future calls.
  #'   \item create_time: created time for auto-refresh.
  #' }
  #' @examples
  #' # Authentification in first package use
  #' criteo_auth("mapi-41jf7549-fa1r-90af-pt54-o5jfhan91nty",
  #'             "D1s1by_X)Dgasd;g!*") # it is not real auth data
  #'
  #' # If you want also save all data to global environment.
  #' criteo_auth("mapi-41jf7549-fa1r-90af-pt54-o5jfhan91nty",
  #'             "D1s1by_X)Dgasd;g!*",
  #'             save_to_env = T)

  body <- list(client_id = client_id, client_secret = client_secret, grant_type = "client_credentials")
  results <- httr::POST(url = "https://api.criteo.com/marketing/oauth2/token", body = body, encode = "form")
  token <- httr::content(results)[[1]]

  #  Simple check to valid Criteo responce. All tokens have same length (941 symbol).
  if (nchar(token) != 941){
    if (!is.null(client_secret))
      cat("Invalid authorization! Criteo returned '", token, "'.\n", sep = "")
  } else {
    token <- list(token = token, client_id = client_id, client_secret = client_secret, create_time = Sys.time())
    save(token, file = "criteo_token.RDS")
    if (!file.exists("criteo_token.RDS")){
      cat("Authentication data was saved into project folder successfully!\n")
    } else {
      cat("Access token successfully refreshed!\n")
    }


    if (save_to_env)
      auth_data <<- token
  }

  invisible(token)
}
