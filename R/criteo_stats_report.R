criteo_stats_report <- function(startDate = Sys.Date() - 1,
                                endDate = Sys.Date() - 1,
                                dimensions = list("Day", "CampaignId"),
                                metrics = list("Audience", "Displays", "Clicks",
                                               "ECpc", "ECpm", "AdvertiserCost"),
                                tok = criteo_auth(),
                                reportType = "CampaignPerformance",
                                ignoreXDevice = F,
                                advertiserIds = criteo_advertisers(),
                                currency = 'RUB',
                                prettify = T){
  # Get statistics about your advertising in Criteo.
  #
  # Args:
  #   startDate: Date, start your report starting. By default yesterday.
  #   startDate: Date, end your report starting. By default yesterday.
  #   dimensions: How slice your data? By default by Day and CampaignId. You can set number of dimensions between one and three.
  #                         Possible values: CampaignId, AdvertiserId, Category, Hour, Day, Week, Month, Year.
  #   metrics: Which columns will be displayed. By default Audience, Displays, Clicks, ECpc, ECpm, AdvertiserCost.
  #                         Possible values: #TODO
  #   tok: Criteo token. Default is call and refresh token automatically.
  #   reportType: The type of report to generate. By default, CampaignPerformance
  #                         Possible values: CampaignPerformance, FacebookDPA, TransactionID.
  #   ignoreXDevice: Ignore cross-device data. By default, FALSE.
  #   advertiserIds: Criteo advertiser. Default is call automatically all advertisers. You can set only need value.
  #   currency: By default RUB.
  #   prettify: By default TRUE. If TRUE returns object:
  #     tibble (advertiser: <class: {Character}>,
  #             date: <class: {Date}>,
  #             campaign_id: <class: {Numeric}>,
  #             campaign: <class: {Character}>,
  #             audience: rounded until 0 values after comma <class: {Numeric}>,
  #             impressions: <class: {Numeric}>,
  #             clicks: <class: {Numeric}>,
  #             costs: rounded until 2 values after comma <class: {Numeric}>,
  #             cpc: rounded until 2 values after comma <class: {Numeric}>,
  #             cpm: rounded until 2 values after comma <class: {Numeric}>,
  #             currency: <class: {Character}>). If FALSE return data frame with all data in raw order.
  #
  # Returns:
  #   By default data frame with all needed columns (watch in 'prettify') with advertising stats.

  header <- httr::add_headers(Authorization = tok)
  body <- list(reportType = reportType,
               ignoreXDevice = ignoreXDevice,
               advertiserIds = advertiserIds,
               startDate = startDate,
               endDate = endDate,
               dimensions = dimensions,
               metrics = metrics,
               format = 'Csv',
               currency = currency)

  data <- httr::POST('https://api.criteo.com/marketing/v1/statistics', header, body = body, encode = 'json')

  stats <- readr::read_delim(httr::content(data, as = "text", encoding = 'UTF-8'), delim = ';')
  if (prettify) {
    stats <- stats %>%
      transmute(advertiser = `Advertiser Name`,
                date = lubridate::mdy(Day),
                campaign_id = `Campaign ID`,
                campaign = `Campaign Name`,
                audience = round(Audience),
                impressions = Impressions,
                clicks = Clicks,
                costs = round(Cost, 2),
                cpc = round(CPC, 2),
                cpm = round(ECPM, 2),
                currency = Currency)
    }
  return(stats)
}
