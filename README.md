
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

shopr is a collection of functions that make it easy to interact with
the [Shopify REST Admin API](https://help.shopify.com/en/api/reference)
from R. Currently, shopr only supports *read* operations (i.e. HTTP GET
requests).

Check out [this
tutorial](https://www.gormanalysis.com/blog/pulling-shopify-data-into-r-with-shopr/)
on setting up and using shopr\!

## Installation

``` r
# install.packages("devtools")
devtools::install_github("GormAnalysis/shopr")
```

## Setup

To use the Shopify API, you first need to

1.  [Create a Shopify Partner
    Account](https://help.shopify.com/en/api/getting-started/making-your-first-request#create-a-shopify-partner-account)
2.  [Create a Development
    Store](https://help.shopify.com/en/api/getting-started/making-your-first-request#create-a-development-store)
    (Recommended but not necessary)
3.  [Generate API credentials from the Shopify
    admin](https://help.shopify.com/en/api/getting-started/making-your-first-request#generate-api-credentials-from-the-shopify-admin)

It’d be wise to read Shopify’s [Getting
Started](https://help.shopify.com/en/api/getting-started) article before
tinkering with this package.

## Usage

### API Credentials

The functions in this package require three parameters to connect to a
shop:

1.  shopURL (e.g. “<https://my-test-store.myshopify.com>”)
2.  APIKey (e.g. “a1b2c3”)
3.  APIPassword (e.g. “d4e5g6”)

For many functions, these are the *only* required parameters, although
additional parameters may be given.

### Example Use

Perhaps the most useful function in this package is
`shopr_get_orders()`. Here’s the simplest way to use it.

``` r
library(shopr)

shopr_get_orders(
  shopURL = "https://my-test-store.myshopify.com", 
  APIKey = "abc123", 
  APIPassword = "def456"
)
```

This returns a list of

  - *orders* (a data.table of orders)
  - *discount\_applications* (a data.table of discount applications
    associated with orders)
  - *discount\_codes* (a data.tabe of discount codes associated with
    orders)
  - *note\_attributes* (a data.table of note attributes associated with
    orders)
  - *tax\_lines* (a data.table of tax lines associated with orders)
  - *line\_items* (a data.table of line items associated with orders)
  - *shipping\_lines* (a data.table of shipping lines associated with
    orders)
  - *fulfillments* (a data.table of fulfillment details associated with
    orders)
  - *refunds* (a data.table of refunds associated with orders)

For shops with many orders, this is *a lot* of data. You can filter the
fields or rows using filter criteria like *created\_at\_min*,
*processed\_at\_min*, *fulfillment\_status*, and other parameters. See
`?shopr_get_orders` for more details.

### Pagination

Some resources (e.g. orders and products) require pagination to return a
large result set. As an example, suppose we have a shop with 1001
products and we want to fetch every product. Shopify allows a max return
size of 250 items per query (`limit_per_page = 250`), so we’ll need to
make 5 queries to retrieve all the products. shopr handles pagination
internally so you don’t have to.

See [this article for details about API rate
limits](https://help.shopify.com/en/api/reference/rest-admin-api-rate-limits).
