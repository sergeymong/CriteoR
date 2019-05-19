#' Getting information about Criteo budgets
#'
#' @param tok Criteo access token. By default is call and refresh token automatically.
#' @param return_columns vector of column names, by default:
#' \itemize{
#'   \item budgetName
#'   \item type
#'   \item totalAmount
#'   \item remainingBudget
#' }
#' If you want see all columns, use "all" as parameter. All columns:
#' \itemize{
#'   \item advertiserId
#'   \item budgetId
#'   \item budgetName
#'   \item type
#'   \item totalAmount
#'   \item remainingBudget
#'   \item remainingBudgetUpdated
#'   \item active
#' }
#' @param active_budgets_only by default, TRUE.
#' @export
#' @return By default, data frame with active budgets and only 4 columns (this is not all columns).
#' @examples
#' # Get all active budgets
#' criteo_budget_info()
#'
#' # Get all exist budgets
#' criteo_budget_info(active_budgets_only = F)
#'
#' Get all info about all budgets
#' criteo_budget_info(return_columns = "all", active_budgets_only = F)
criteo_budget_info <- function(tok = criteo_access_token(),
                               return_columns = c("budgetName", "type", "totalAmount", "remainingBudget"),
                               active_budgets_only = T){

  header <- httr::add_headers(Authorization = tok)
  query <- list(onlyActiveCampaigns = active_budgets_only)
  data <- httr::GET("https://api.criteo.com/marketing/v1/budgets", header, query = query)
  budget <- httr::content(data)

  null_list_to_na <- function(x) lapply(x, function(x) if (is.null(x)) x <- NA else x)
  budget <- lapply(budget, null_list_to_na)

  #  Unlisting budgets list. Using sapply, because we want get a vertical matrix.
  #  Then we transpose matrix and convert it to tibble (data frame of dplyr).
  budget <- dplyr::as_tibble(t(sapply(budget, function(x) unlist(x))))

  if (any(return_columns != "all")){
    budget <- dplyr::select(budget, return_columns)
  }
  return(budget)
}
