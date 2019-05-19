
<!-- README.md is generated from README.Rmd. Please edit that file -->

The following section helps you to get started straight away to get data
from Criteo REST API.

## Installation

Package available now only from GitHub:

``` r
require(devtools)
devtools::install_github("sergeymong/CriteoR")
```

## Usage

The first one you should have Criteo REST API Client ID and Client
Secret. You can get it in Criteo Dynamic Remarketing Account:

<https://marketing.criteo.com/Home> -\> Management Center -\> Account
Settings -\> Users -\> Create API User (In REST API block)

### Authentication

You just need to log in once. Authorization data are saved to a file and
then authorization will occur automatically.

``` r
library(CriteoR)
criteo_auth("mapi-41jf7549-fa1r-90af-pt54-o5jfhan91nty",
            "D1s1by_X)Dgasd;g!*") # it's example data
```

### Get stats

The simplest way get
stats:

``` r
# This command returns stats by all active campaigns and advertisers in your account for yesterday
criteo_stats_report()
```

If you want more data:

``` r
criteo_stats_report(start_date = "2017-01-01", 
                    end_date = Sys.Date() - 1,
                    metrics = list("Audience", "Displays", "Clicks", 
                                   "ECpc", "ECpm", "AdvertiserCost", 
                                   "SalesPc", "RevenueGeneratedPc", "AverageCart"),
                    prettify = F)
```
