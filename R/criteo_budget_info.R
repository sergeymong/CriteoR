criteo_budget_info <- function(tok = criteo_auth(),
                               columns = c('budgetName', 'type', 'totalAmount', 'remainingBudget'),
                               active_budgets_only = T){
  #  Getting information about Criteo budgets.
  #
  # Args:
  #   tok: file or object with authentification data. Object format:
  #      list(
  #           token = <token {class: chr}>,
  #           client_id = <Login (Client ID) Criteo REST API user {class: chr}>,
  #           client_secret = <Client Secret Criteo REST API user {class: chr}>,
  #           create_time = <Creating time {class: POSIXct}>
  #       )
  #   columns: vector of column names.
  #   active_budgets_only: query parameter. By default TRUE. If TRUE, returns only active budgets;
  #                                                             if not, returns all budgets in account.
  #
  # Returns:
  #   By default, data frame with active budgets and only 4 columns (this is not all columns).

  header <- httr::add_headers(Authorization = tok)
  query <- list(onlyActiveCampaigns = active_budgets_only)
  data <- httr::GET('https://api.criteo.com/marketing/v1/budgets', header, query = query)
  budget <- httr::content(data)

  null_list_to_na <- function(x) lapply(x, function(x) if(is.null(x)) x = NA else x)
  budget <- lapply(budget, null_list_to_na)

  #  Unlisting budgets list. Using sapply, because we want get a vertical matrix.
  #  Then we transpose matrix and convert it to tibble (data frame of dplyr).
  budget <- dplyr::as_tibble(t(sapply(budget, function(x) unlist(x))))

  if (any(columns != 'all')){
    budget <- dplyr::select(budget, columns)
  }
  return(budget)
}

criteo_budget_info(columns = 'all', active_budgets_only = F)
