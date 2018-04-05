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
library(PerformanceAnalytics)
library(tidyr)
library(MLmetrics)

get_alt_data <- function(tz = "UTC"
                         , coin = c("ETH", "LTC")
                         , add_bitcoin = TRUE
                         , return_in_USDT = TRUE
                         , from = "2017-01-01"
                         , to = "2018-04-09"
                         , period = "D"
                         , verbose = FALSE){
  
  # We will be using the public API
  poloniex.public <- PoloniexPublicAPI()
  
  # set the time zone to utc
  Sys.setenv(tz = tz)
  
  # convert from and to into time obj
  from  <- as.POSIXct(paste(from, tz, sep = ""))
  to    <- as.POSIXct(paste(to, tz, sep = ""))
  
  # lists to store data.tables and xts objects
  chart_list <- list()
  dt_list    <- list()
  
  # make sure the coin pair is in upper case
  coin       <- toupper(coin)
  coin_pairs <- paste0("BTC_", coin[coin != "BTC"])
  if(add_bitcoin | return_in_USDT) coin_pairs <- c("USDT_BTC", coin_pairs)
  
  # loop over the coins to get the data
  for(i in coin_pairs){
    if(verbose)
      invisible(cat('\tGetting data for ', i, ' pair\n'))
    
    # this is a list that will contain the chart data for each coin pair
    try(chart_list[[i]] <- ReturnChartData(theObject = poloniex.public
                                       , pair      = i
                                       , from      = from
                                       , to        = to
                                       , period    = period)
        , silent = TRUE)
    
    # list to contain data.tables 
    try(dt_list[[i]] <- as.data.table(chart_list[[i]]), silent = TRUE)
  }
  
  # convert to data.table and make sure to add a column containing the pairs
  coin_dt <- rbindlist(l = dt_list, use.names = TRUE, idcol = "pair")
  
  # return data in usdt prices
  if(return_in_USDT){
    # to get the price of the alt coin in usdt is not that simple but we'll do it
    # get a DT of the btc_usdt pair
    btc_usd <- coin_dt[pair == "USDT_BTC"]
    btc_usd <- btc_usd[, .(index, pair, weightedaverage)]
    setnames(btc_usd, c("Date", "USDT_BTC_pair", "USDT_BTC_price"))
    
    # get DT with only alt coins
    alt_coins <- copy(coin_dt)#[pair != "USDT_BTC"]
    
    # now we need to add an index to the alt_coins table, but first we have to rename the index column
    alt_coins[, Date := index]
    alt_coins[, index := 1:.N]
    setkey(alt_coins, index)
    
    # now merge the data tables
    coin_dt_usdt <- merge(x = alt_coins, y = btc_usd, by = "Date")
    
    # now calcualte the price in usdt
    coin_dt_usdt[, price_usdt := ifelse(pair == "USDT_BTC", USDT_BTC_price, weightedaverage * USDT_BTC_price)]
    
    # now get rid of the extra columns
    coin_dt_usdt[, c("USDT_BTC_price", "USDT_BTC_pair") := NULL]
    
    # we need to change some column names
    col_names_to_change <- c("pair", "high", "low", "open", "close", "volume", "quotevolume", "weightedaverage")
    col_names <- names(coin_dt_usdt)
    col_names[col_names %in% col_names_to_change] <- paste0(col_names_to_change, '_btc')
    
    setnames(coin_dt_usdt, col_names)
    
    # add a column for the usdt pair
    coin_dt_usdt[, pair_usdt := gsub("BTC_", "USDT_", pair_btc)]
    
    # adjust col order
    setcolorder(coin_dt_usdt, c(1:10, 12, 11))
    
    # set key again
    setkey(coin_dt_usdt, index)
    
    # now get rid of the index column since it is not needed anymore
    coin_dt_usdt[, index := NULL]
    
    # now put together the return list  
    return_list <- list(alt_chart_list = chart_list, alt_dt = coin_dt, alt_usdt_dt = coin_dt_usdt)
  }else{
    return_list <- list(alt_chart_list = chart_list, alt_dt = coin_dt)
  }
  
  return(return_list)
}

alt_data <- get_alt_data(return_in_USDT = T
                         , from = "2015-01-01"
                         , coin = c('ETH','XRP', 'BCH', 'LTC', 'NEO', 'XMR', 'DASH', 'XEM')
                         , verbose = T)[['alt_usdt_dt']]

# add daily price change
alt_data[, pct_change := Delt(price_usdt), by = pair_usdt]

# add normalized prices in udst
alt_data[, price_usdt_norm := price_usdt/max(price_usdt), by = pair_usdt]

alt_data_sub <- alt_data[, .(Date, pair_usdt, pct_change)]
# alt_data_sub <- alt_data[, .(Date, pair_usdt, price_usdt)]

alt_data_sub <- spread(data = alt_data_sub, key = "pair_usdt", value = "pct_change")
# alt_data_sub <- spread(data = alt_data_sub, key = "pair_usdt", value = "price_usdt")

alt_data_sub_2017 <- alt_data_sub[year(Date) == "2017"]

alt_data_sub_2017[, Date := NULL]

# plot the percent changes
p <- ggplot(alt_data[Date > ymd("2015-01-01")], aes(x = Date, y =  (100*pct_change), col = pair_usdt)) + geom_line()
p <- p + ggtitle("% Daily Returns over time") + ylab("Daily Return (%)") 
p <- p + annotate("text", x = as.POSIXct("2015-10-05"), y = 75, label = "@aousabdo",fontface="bold")
p <- p + theme_bw() + guides(col=guide_legend(title="Coin Pair"))
ggplotly(p)

p <- ggplot(alt_data[pair_usdt %like% "BTC|LTC"], aes(x = Date, y =  (pct_change), col = pair_usdt)) + geom_line()
ggplotly(p)

p <- ggplot(alt_data[Date > ymd("2015-01-01")], aes(x = Date, y =  (100*pct_change), col = pair_usdt)) + geom_line() + facet_wrap(~ pair_usdt)
p <- p + ggtitle("Percentage Daily Returns over time") + ylab("Daily Return (%)") 
p <- p + theme_bw() + theme(legend.position="none") 
ggplotly(p)

# daily returns of ltc and btc are highly corrleated in some time intervals
start_date <- ymd("2016-02-01")
end_date <- ymd("2016-05-01")
p <- ggplot(alt_data[pair_usdt %like% "BTC|LTC" & Date > start_date & Date < end_date], aes(x = Date, y =  (pct_change), col = pair_usdt)) + geom_line()
ggplotly(p)

# normalized price
p0 <- ggplot(alt_data[pair_usdt %like% "BTC"], aes(x = Date, y =  (price_usdt), col = pair_usdt)) + geom_line()
ggplotly(p0)


p3 <- ggplot(alt_data[year(Date) == 2018], aes(x = Date, y =  price_usdt, col = pair_usdt)) + geom_line()
p3 <- p3 + facet_wrap(~pair_usdt, scales = "free", ncol = 3) + theme_minimal() + theme(legend.position="none") + ylab("Price (USD)")
print(p3)


M <- cor(alt_data_sub_2017, use = "complete.obs")

p2 <- corrplot(corr = M, method = "ellipse", order = "AOE")

corrplot.mixed(M)

chart.Correlation(alt_data_sub_2017, histogram=TRUE, pch=19)

# Get some colors
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = M, col = col, symm = TRUE)

# now let's look at the rolling correlations
# but first we need to convert the data.table to xts object
roll_coll_df <- as.data.frame(alt_data_sub[year(Date) == "2018"])
rownames(roll_coll_df) <- roll_coll_df$Date
roll_coll_df$Date <- NULL

chart.RollingCorrelation(roll_coll_df[, c(1, 3:8), drop = FALSE]
                         , roll_coll_df[, 2, drop = FALSE]
                         , colorset = rich8equal 
                         , legend.loc = "bottomright"
                         , width = 12
                         , main = "Rolling Correlation for Altcoins Against Bitcoin. analyticadss.com")
