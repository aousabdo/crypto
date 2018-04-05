library(PoloniexR)
library(data.table)
library(lubridate)
library(Quandl)
library(plyr)
library(stringr)
library(ggplot2)
library(plotly)
library(janitor)
library(quantmod)
library(pryr)
library(corrplot)

# We will be using the public API
poloniex.public <- PoloniexPublicAPI()

#================================================================================#
#================================ Ticker Info ===================================#
#================================================================================#
# get some ticker info
ticker.info      <- ReturnTicker(poloniex.public)
ticker.info$pair <- row.names(ticker.info)
ticker.info      <- as.data.table(ticker.info)
setcolorder(ticker.info, c(1, ncol(ticker.info), 2:(ncol(ticker.info)-1)))

head(ticker.info)

#================================================================================#
#================================ Volume ========================================#
#================================================================================#
# return 24 hour volume
volume.info        <- Return24hVolume(poloniex.public)
volume_pairs       <- volume.info$volume.pairs
volume_pairs$pairs <- row.names(volume_pairs)
volume_pairs       <- as.data.table(volume_pairs)
setcolorder(volume_pairs, c(ncol(volume_pairs), 1:(ncol(volume_pairs)-1)))

head(volume_pairs)

volume_totals <- as.data.table(volume.info$volume.totals)

#================================================================================#
#================================ Order book ====================================#
#================================================================================#
# look at the order books
pair  <- "BTC_ETH"
depth <- 200

btc_usdt_ob <- ReturnOrderBook(theObject = poloniex.public
                               , pair = pair
                               , depth = depth)

head(btc_usdt_ob$bid)
head(btc_usdt_ob$ask)
btc_usdt_ob$frozen
btc_usdt_ob$seq

# get the order book for all pairs
pair       <- "all"
depth      <- 10
order.book <- ReturnOrderBook(poloniex.public,
                              pair  = pair,
                              depth = depth)

#================================================================================#
#================================ Trade history =================================#
#================================================================================#
Sys.setenv(tz="UTC")
pair   <- "USDT_BTC"
from   <- as.POSIXct("2018-02-20 00:00:00 UTC")
to     <- as.POSIXct("2018-03-04 00:00:00 UTC")

btc_usd <- ReturnTradeHistory(theObject = poloniex.public,
                              pair      = pair,
                              from      = from,
                              to        = to)


# get prices for a bunch of coins
altcoins <- c('ETH','LTC','XRP','ETC','STR','DASH','SC','XMR','XEM')
altcoins <- paste0("BTC_", altcoins)
alt_coin_data <- list()

for(i in altcoins){
  alt_coin_data[[i]] <- ReturnTradeHistory(theObject = poloniex.public,
                                           pair      = i,
                                           from      = from,
                                           to        = to)
}

# put them all in one dataframe to plot in ggplot2
alt_coin_data_df <- as.data.table(do.call("rbind", alt_coin_data))

# make sure to add an id which tells us what pair it is
alt_coin_data_df$id <- rep(names(alt_coin_data), sapply(alt_coin_data, nrow))

#================================================================================#
#================================= Chart Data ===================================#
#================================================================================#
Sys.setenv(tz="UTC")
pair    <- "USDT_BTC"

from    <- as.POSIXct("2017-01-01 00:00:00 UTC")
to      <- as.POSIXct("2018-04-09 00:00:00 UTC")
period  <- "D" #"4H"

btc_usd <- ReturnChartData(theObject = poloniex.public,
                           pair      = pair,
                           from      = from,
                           to        = to,
                           period    = period)

tail(btc_usd)

# make a chart using quantmod 
chart_plot <- chart_Series((btc_usd[, "close"])
                           , type   = "line"
                           , name   = pair
                           #, subset = "201711/201803"
)

chart.plot <- add_MACD()
chart.plot <- add_BBands()
chart.plot <- add_RSI()
chart.plot

coin_pairs <- c('ETH','LTC','XRP','ETC','STR','DASH','SC','XMR','XEM')
coin_pairs <- paste0("BTC_", coin_pairs)
coin_pairs <- c("USDT_BTC", coin_pairs)

coin_data <- list()

# pull lots of data
for(i in coin_pairs){
  invisible(cat('\tGetting chart data for ', i, ' pair\n'))
  coin_data[[i]] <- ReturnChartData(theObject = poloniex.public
                                    , pair      = i
                                    , from      = from
                                    , to        = to
                                    , period    = period)}

# put them all in one dataframe to plot in ggplot2
coin_data_df <- (do.call("rbind", coin_data))

# make sure to add an id which tells us what pair it is
coin_data_df$id <- rep(names(coin_data), sapply(coin_data, nrow))

#================================================================================#
#================================= Currencies ===================================#
#================================================================================#
currencies <- ReturnCurrencies(poloniex.public)
head(currencies)


# btc_usd <- as.data.table(btc_usd)
# btc_usd[, exchange := as.factor(str_extract(exchange, "[A-Z]+"))]


################################################################################################################
################################################################################################################
################################################################################################################


#================================================================================#
#================================ Quandl Data ===================================#
#================================================================================#
get_data <- TRUE

get_quandl_data <- function(data_source = "BITFINEX"
                            , pair = 'btcusd'
                            , ...){
  
  # make sure the user supplied the correct data_source
  if(toupper(data_source) != "BITFINEX") stop("data source supplied is wrong...")
  # quandl is case sensitive, all codes have to be upper case
  pair <- toupper(pair)
  tmp <- NA
  try(tmp <- Quandl(code = toupper(paste(data_source, pair, sep = "/")), ...), silent = TRUE)
  return(tmp)
}

if(get_data){
  # btc_usd_kraken <- Quandl('BCHARTS/KRAKENUSD')
  
  exchange_data <- list()
  
  exchanges <- c('KRAKENUSD','COINBASEUSD','BITSTAMPUSD','ITBITUSD')
  
  for (i in exchanges){
    print(paste0('BCHARTS/', i))
    exchange_data[[i]] <- Quandl(paste0('BCHARTS/', i))
  }
}

# put them all in one dataframe to plot in ggplot2
btc_usd <- do.call("rbind", exchange_data)
btc_usd$exchange <- row.names(btc_usd)
btc_usd <- as.data.table(btc_usd)

btc_usd[, exchange := as.factor(str_extract(exchange, "[A-Z]+"))]
btc_usd <- clean_names(btc_usd)
btc_usd <- btc_usd[weighted_price > 0]

btc_usd_average_price <- btc_usd[, mean(weighted_price), by = date]
setnames(btc_usd_average_price, c("date", "averaged_price"))

p <- ggplot(btc_usd, aes(x = date, y = weighted_price, col = exchange)) + geom_line()
print(p)

p2 <- ggplot(btc_usd_average_price, aes(date, y = averaged_price)) + geom_line()
print(p2)

# a treasure of data could be found here
# https://www.quandl.com/data/BITFINEX-Bitfinex

# get btc_xrp data
btc_xrp <- Quandl('BCHARTS/KRAKENXRP')

btc_ltc <- Quandl('BCHARTS/KRAKENLTC')

xmr_usd <- Quandl('BITFINEX/XMRUSD')

xmr_btc <- Quandl('BITFINEX/XMRBTC')

eth_usd <- Quandl('BITFINEX/ETHUSD')

eth_usd <- Quandl('BITSTAMP/USD')

eth_btc <- Quandl('BITFINEX/ETHBTC')

eos_usd <- Quandl('BITFINEX/EOSUSD')

itos_usd <- Quandl('BITFINEX/IOTUSD')

bch_usd <- Quandl('BITFINEX/BCHUSD')

eth_btc_gdax <- Quandl('GDAX/ETH_BTC')


# search quandl
df.search <- Quandl.search('eth', silent = TRUE)
code <- paste(df.search$database_code, df.search$dataset_code, sep = "/")
foo <- list()
for(i in code){
  print(i)
  try(foo[i] <- Quandl(i), silent = T)
}



# Rbitcoin
library(Rbitcoin)

# set mkt, currency pair and type of action
my.mkt <- "kraken"
my.mkt <- "binance"
my.currency <- c("BTC","EUR")
my.action <- 'trades'

# import data
my.l <- market.api.process(market = my.mkt,
                           currency_pair = my.currency,
                           action = my.action)

# print it
print(my.l)
