criteo_stats_report <- function(start_date = Sys.Date() - 1,
                                end_date = Sys.Date() - 1,
                                dimensions = list("Day", "CampaignId"),
                                metrics = list("Audience", "Displays", "Clicks",
                                               "ECpc", "ECpm", "AdvertiserCost"),
                                tok = criteo_access_token(),
                                report_type = "CampaignPerformance",
                                ignore_x_device = F,
                                advertiser_ids = criteo_advertisers(),
                                currency = "RUB",
                                prettify = T){
  #' Get statistics about Criteo Dynamic Remarketing
  #'
  #' @param start_date Start date of your report. By default, yesterday.
  #' @param end_date End date of your report. By default, yesterday.
  #' @param dimensions How group by your data? By default by Day and CampaignId.
  #'
  #' Number of dimensions between one and three. Possible values:
  #' \itemize{
  #'   \item CampaignId
  #'   \item AdvertiserId
  #'   \item Category
  #'   \item Hour
  #'   \item Day
  #'   \item Week
  #'   \item Year
  #' }
  #' @param metrics Which columns will be displayed.
  #'
  #' All possible metrics will below. Default columns:
  #' \itemize{
  #'   \item Audience
  #'   \item Displays
  #'   \item Clicks
  #'   \item ECpc
  #'   \item ECpm
  #'   \item AdvertiserCost
  #' }
  #'
  #' @param tok Criteo token. Default is call and refresh token automatically.
  #' @param report_type By default, CampaignPerformance. Possible values:
  #' \itemize{
  #'   \item FacebookDPA: stats from Facebook Dynamic Ads.
  #'   \item TransactionID: stats about all transactions
  #'   \item CampaignPerformance: stats about Criteo Dynamic Remarketing
  #' }
  #' @param ignore_x_device Ignore cross-device data. By default, FALSE.
  #' @param advertiser_ids By default is call automatically all advertisers. You can set need values.
  #' @param currency By default RUB.
  #' @param prettify By default TRUE. Work only with default params.
  #'
  #' If TRUE returns tibble:
  #' \itemize{
  #'   \item advertiser: <[class: chr]>,
  #'   \item date: <[class: Date]>,
  #'   \item campaign_id: <[class: int]>,
  #'   \item campaign: <[class: chr]>,
  #'   \item audience: <rounded to 0 decimal places [class: int]>,
  #'   \item impressions: <[class: int]>,
  #'   \item clicks: <[class: int]>,
  #'   \item costs: <rounded to 2 decimal places [class: num]>,
  #'   \item cpc: <rrounded to 2 decimal places [class: num]>,
  #'   \item cpm: <rounded to 2 decimal places [class: num]>,
  #'   \item currency: <[class: chr]>
  #' }
  #' If FALSE return data frame with all columns.
  #' @references
  #' \describe{
  #'   \item{Clicks}{Number of times that your Criteo ads have been clicked}
  #'   \item{Displays}{Number of times your Criteo ad is displayed to a user on publishers’ websites}
  #'   \item{AdvertiserCost}{Total cost of your campaigns}
  #'   \item{SalesPc}{The total number of purchases completed via the same device your Criteo ads were clicked. Sales are attributed within 30 days after a user clicks on your Criteo ad.}
  #'   \item{SameSalesPc}{The total number of purchases completed via the same device your Criteo ads were clicked. Sales are attributed within 30 days after a user clicks on your Criteo ad.}
  #'   \item{SalesPcNd}{Post-click sales that are non-deduplicated}
  #'   \item{SalesPv}{All sales that occurred on your website within 24 hours after a user sees a Criteo ad of your campaign. Post-view sales do not include sales from users who clicked an ad, which are counted as sales}
  #'   \item{SalesPvNd}{All non-deduplicated post-view sales that occurred on your website within 24 hours after a user sees a Criteo ad of your campaign. Post-view sales do not include sales from users who clicked an ad, which are counted as sales}
  #'   \item{SalesPcPv}{The total amount of sales generated both post-click and post-view}
  #'   \item{SalesAllPc}{The total number of purchases completed via the same device your Criteo ads were clicked, or a different one. Sales are attributed within 30 days after a user clicks on your Criteo ad.}
  #'   \item{RevenueGeneratedPc}{Total revenue of your campaigns generated from post-click sales}
  #'   \item{RevenueGeneratedPv}{Total revenue of your campaigns generated from post-view sales}
  #'   \item{RevenueGeneratedPcPv}{Post-click + post-view revenue is the total revenue of your campaigns generated from post-click and post-view sales. This number is computed based on the prices provided by your conversion trackers}
  #'   \item{RevenueGeneratedPcNd}{Total revenue of your campaigns generated from post-click sales non-deduplicated}
  #'   \item{RevenueGeneratedPvNd}{Total revenue of your campaigns generated from post-view sales non-deduplicated}
  #'   \item{ExposedUsers}{Number of distinct cookies that have seen at least one Criteo display}
  #'   \item{OverallCompetitionWin}{Percentage of impressions or ad placements won, out of all impressions or ad placements that were bid on. For example, Criteo bid on 10 ad placements and won 2, so the Overall Competition Win is 20 perc. Higher CPC bids result in increased competition win.}
  #'   \item{Audience}{Number of distinct cookies to which Criteo had the opportunity to make a display}
  #'   \item{Reach}{Share of the audience exposed to at least one Criteo display ad (percentage)}
  #'   \item{AverageCart}{Average revenue generated by a campaign’s sales. This number is calculated by dividing the total revenue by the total sales}
  #'   \item{ClickThroughRate}{The number of times your Criteo ads are clicked, divided by the number of times your ad is displayed (impressions). Each campaign has its own CTR}
  #'   \item{ConversionRate}{Number of sales generated by users who clicked on your campaign’s ad, divided by the campaign’s number of clicks}
  #'   \item{ConversionRatePv}{Conversion rate post-view is the number of post-view sales generated by users who viewed on your campaign’s ad, divided by the campaign’s number of impressions}
  #'   \item{ECos}{The ratio between the total cost of the campaign and the sales amount that the campaign generated. COS provides a good indication of your campaign’s ROI}
  #'   \item{ECosPv}{The ratio between the total cost of the campaign and the post-view sales amount that the campaign generated. This number provides a good indication of your campaign’s ROI.}
  #'   \item{ECosPcPv}{Cost of sale post-click + post-view is the ratio between the total cost of the campaign and the post-click and post-view sales amount that the campaign generated. This number provides a good indication of your campaign’s ROI}
  #'   \item{CostPerOrder}{Cost per order is the amount of money (cost) spent to generate post-click sales}
  #'   \item{CostPerOrderPv}{The amount of money (cost) spent to generate post-view sales}
  #'   \item{CostPerOrderPcPv}{Cost per order post-click + post-view is the amount of money (cost) spent to generate post-click and post-view sales}
  #'   \item{ECpc}{The price you pay each time a user clicks your Criteo ad. Depending on your campaign’s business model, you can change the CPC on the Campaigns tab for each campaign}
  #'   \item{ECpm}{Cost per mille is the cost of your campaign for 1,000 impressions}
  #'   \item{ReturnOnAdvertisingSpending}{Corresponds to the ratio between the sales amount the campaign generated and the total cost of the campaign (ROAS = 1/COS)}
  #'   \item{AdvertiserValue}{The total value generated by the campaign. It is equal to the sum of revenues generated by each product multiplied by their corresponding Value coefficient as defined in the advertiser's feed.}
  #'   \item{CostOfAdvertiserValue}{The ratio between the total cost of the campaign and the Advertiser Value of the campaign}
  #'   }
  #' @return Data frame with all needed columns (watch in "prettify") with advertising stats.
  #' @examples
  #' # If you want get stats for yesterday
  #' criteo_stats_report()
  #'
  #' # If you want get stat for exact period
  #' criteo_stats_report(start_date = "2019-01-01", end_date = "2019-05-01")

  if (typeof(dimensions) != "list")
    dimensions <- as.list(dimensions)

  header <- httr::add_headers(Authorization = tok)
  body <- list(ReportType = report_type,
               IgnoreXDevice = ignore_x_device,
               AdvertiserIds = advertiser_ids,
               StartDate = start_date,
               EndDate = end_date,
               Dimensions = dimensions,
               Metrics = metrics,
               Format = "Csv",
               Currency = currency)

  data <- httr::POST("https://api.criteo.com/marketing/v1/statistics", header, body = body, encode = "json")
  if (data$status_code == 400){
    cat(" Oops, error! Criteo says: \n", httr::content(data)$message)
  } else {
    stats <- readr::read_delim(httr::content(data, as = "text", encoding = "UTF-8"), delim = ";")

    if (prettify && (all(unlist(dimensions) %in% c("Day", "CampaignId"))) && report_type == "CampaignPerformance") {
    stats <- dplyr::transmute(
                stats, advertiser = `Advertiser Name`,
                date = lubridate::mdy(Day),
                campaign_id = as.integer(`Campaign ID`),
                campaign = `Campaign Name`,
                audience = round(as.integer(Audience)),
                impressions = as.integer(Impressions),
                clicks = as.integer(Clicks),
                costs = round(Cost, 2),
                cpc = round(CPC, 2),
                cpm = round(ECPM, 2),
                currency = Currency
                )
    }
    return(stats)
  }
}
