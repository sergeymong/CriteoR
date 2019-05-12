criteo_advertisers <- function(tok = criteo_auth(), to_query_output = T){
  # Get information about all advertisers in Criteo account.
  #
  # Args:
  #   tok: Criteo token. Default is call and refresh token automatically.
  #   to_query_output: If TRUE, return comma separated advertiser id's;
  #                                                     if not, return advertisers data frame.
  #
  # Returns:
  #   By default string of all Criteo advertisers (for using in other queries).

  header <- httr::add_headers(Authorization = tok)
  data <- httr::GET("https://api.criteo.com/marketing/v1/portfolio", header)

  #  Convert raw responce to list of advertisers.
  advertiser <- httr::content(data)

  #  Convert NULL values to NA, because we want to save consistency.
  null_list_to_na <- function(x) lapply(x, function(x) if (is.null(x)) x <- NA else x)
  advertiser <- lapply(advertiser, null_list_to_na)

  #  Unlisting advertiser list. Using sapply, because we want get a vertical matrix.
  #  Then we transpose matrix and convert it to tibble (data frame of dplyr).
  advertiser <- dplyr::as_tibble(t(sapply(advertiser, function(x) unlist(x))))

  if (to_query_output){
    return(paste0(advertiser[[1]], collapse = ", "))
  } else {
    return(advertiser)
  }
}
