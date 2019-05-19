#'  Getting Criteo Access Token
#'
#'  Getting Criteo valid token object. Updates token if then expired and returning
#'  valid access token.
#'  Loads token to virtual environment in function, that not junking global env.
#'
#' @param tok file or object with authentification data. Object format:
#'
#' You should use this fields (recommended list):
#' \itemize{
#'   \item token = <access token [class: chr]>,
#'   \item client_id = <Login (Client ID) Criteo REST API user [class: chr]>,
#'   \item client_secret = <Client Secret Criteo REST API user [class: chr]>,
#'   \item create_time = <Creating time [class: POSIXct]>
#' }
#' @export
#' @return String "Bearer <access token>" for using in other Criteo queries.
criteo_access_token <- function(tok = "criteo_token.RDS"){
  if (!file.exists("criteo_token.RDS")){
    if (tolower(readline("File with Criteo Auth data not found. Create? (y/n): ")) == tolower("Y")){
      client_id <- readline("Input your Criteo Client ID: ")
      client_secret <- readline("Input your Criteo Client Secret: ")
      token <- criteo_auth(client_id = client_id, client_secret = client_secret)

      return(paste("Bearer", token$token))
    }
  } else {
    tmp <- new.env()
    if (tok == "criteo_token.RDS") {
      load("criteo_token.RDS", envir = tmp)
      token <- tmp[[ls(tmp)[1]]]
    } else {
      token <- tok
    }

    #  Criteo token live only 300 sec, 5 seconds as buffer. If token expired, creating GET query to refresh it.
    if (as.numeric(Sys.time() - token$create_time, units = "secs") > 295){
      token <- criteo_auth(token$client_id, token$client_secret)
    }

    return(paste("Bearer", token$token))
  }

}
