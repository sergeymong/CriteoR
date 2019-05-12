criteo_auth <- function(tok = "criteo_token.RDS"){
  #  Getting Criteo valid token object. Updating token if then expired and returning valid for Criteo OAuth token.
  #  Load token to virtual environment in function, that not junking global env.
  #
  # Args:
  #   tok: file or object with authentification data. Object format:
  #      list(
  #           token = <token {class: chr}>,
  #           client_id = <Login (Client ID) Criteo REST API user {class: chr}>,
  #           client_secret = <Client Secret Criteo REST API user {class: chr}>,
  #           create_time = <Creating time {class: POSIXct}>
  #       )
  #
  # Returns:
  #   By default, string "Bearer <token>" for using in other Criteo queries.

  tmp <- new.env()
  if (tok == "criteo_token.RDS") {
    load("criteo_token.RDS", envir = tmp)
    token <- tmp[[ls(tmp)[1]]]
  } else {
    token <- tok
  }

  #  Criteo token live only 300 sec, 5 seconds as buffer. If token expired, creating GET query to refresh it.
  if (as.numeric(Sys.time() - token$create_time, units = "secs") > 295){
    token <- criteo_get_token(token$client_id, token$client_secret)
  }

  return(paste("Bearer", token$token))
}

# TODO  FIRST! подумать, как сделать авторизацию без файла, или компиляцию пакета без ошибки https://stackoverflow.com/questions/28218508/r-check-if-r-object-exists-before-creating-it
