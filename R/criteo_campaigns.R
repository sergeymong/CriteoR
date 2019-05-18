criteo_campaigns <- function(tok = criteo_access_token(), active_campaigns_only = T){

  #'  Getting information about Criteo campaigns
  #
  #' @param tok Criteo access token. Default is call and refresh token automatically.
  #' @param active_campaigns_only: By default TRUE.
  #' @return By default, data frame with active companies.
  #' @examples
  #' # If you want to get all campaigns in account
  #' criteo_campaigns(active_campaigns_only = F)

  status <- ifelse(active_campaigns_only, "Running", "")
  query <- list(campaignStatus = status)
  header <- httr::add_headers(Authorization = tok)
  data <- httr::GET("https://api.criteo.com/marketing/v1/campaigns", header, query = query)
  campaigns <- httr::content(data)

  null_list_to_na <- function(x) lapply(x, function(x) if (is.null(x)) x <- NA else x)
  campaigns <- lapply(campaigns, null_list_to_na)
  campaigns <- dplyr::as_tibble(t(sapply(campaigns, function(x) unlist(x))))

  return(campaigns)
}
