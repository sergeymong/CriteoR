criteo_campaigns <- function(tok = criteo_auth(), active_companies_only = T){

  #  Getting meta information about Criteo campaigns.
  #
  # Args:
  #   tok: file or object with authentification data. Object format:
  #      list(
  #           token = <token {class: chr}>,
  #           client_id = <Login (Client ID) Criteo REST API user {class: chr}>,
  #           client_secret = <Client Secret Criteo REST API user {class: chr}>,
  #           create_time = <Creating time {class: POSIXct}>
  #       )
  #   active_companies_only: query parameter. By default TRUE. If TRUE, returns only active companies;
  #                                                             if not, returns all companies in account.
  #
  # Returns:
  #   By default, data frame with active companies.

  status <- ifelse(active_companies_only, 'Running', '')
  query <- list(campaignStatus = status)
  header <- httr::add_headers(Authorization = criteo_auth())
  data <- httr::GET('https://api.criteo.com/marketing/v1/campaigns', header, query = query)
  campaigns <- httr::content(data)

  null_list_to_na <- function(x) lapply(x, function(x) if(is.null(x)) x = NA else x)
  campaigns <- lapply(campaigns, null_list_to_na)
  campaigns <- dplyr::as_tibble(t(sapply(campaigns, function(x) unlist(x))))

  return(campaigns)
}

criteo_campaigns()
