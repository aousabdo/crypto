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
library(gtrendsR)

#===========================================================================================#
#====================================== get_alt_data =======================================#
#===========================================================================================#
get_alt_data <- function(tz = "UTC"
                         , coin = c("ETH", "LTC")
                         , add_bitcoin = TRUE
                         , return_in_USDT = TRUE
                         , from = "2017-01-01"
                         , to = "2018-04-09"
                         , period = "D"){
  
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
#===========================================================================================#
#==================================== End: get_alt_data ====================================#
#===========================================================================================#

#===========================================================================================#
#================================= process_gtrends_data ====================================#
#===========================================================================================#
process_gtrends_data <- function(gtrends_output = NULL){
  # function to cleant the date we get from google trends
  # for some reason the data is not always consistent
  
  # make a copy of the list
  tmp <- gtrends_output
  
  # remove NULL list elements:
  tmp[sapply(tmp, is.null)] <- NULL
  
  # in some cases, the hits varialbe is returned as a character
  # we need to convert that into an integer
  try(tmp <- lapply(names(tmp), function(x) modify_var(df = tmp[[x]], var = "hits")))
  
  return(tmp)
}
#===========================================================================================#
#============================== End: process_gtrends_data ==================================#
#===========================================================================================#

#===========================================================================================#
#===================================== modify_var ==========================================#
#===========================================================================================#
modify_var <- function(df = NULL
                       , var = NULL){
  # copy data frame
  tmp <- copy(df)
  
  # only run this function if the dataframe contians a column with the correct name
  if(var %in% names(tmp)){
    # convert to a data.table obj
    tmp <- as.data.table(tmp)
    
    # in some cases we get nothing, set those to 0
    tmp[, eval(var) := gsub("", 0, get(var))]
    
    # in some low-search cases, we get back <1, replace the < with ""
    tmp[, eval(var) := gsub("<", "", get(var))]
    
    # make sure the variable is converted to integer
    tmp[, eval(var) := as.integer(get(var))]
  }
  return(tmp)
}
#===========================================================================================#
#=================================== End: modify_var =======================================#
#===========================================================================================#

#===========================================================================================#
#======================= modified_medium_create_post_from_Rmd ==============================#
#===========================================================================================#
modified_medium_create_post_from_Rmd <-function (Rmd_file = NULL) 
{
  if (is.null(Rmd_file) && rstudioapi::isAvailable()) {
    Rmd_file <- rstudioapi::getActiveDocumentContext()$path
  }
  if (tolower(tools::file_ext(Rmd_file)) != "rmd") {
    stop(sprintf("%s is not .Rmd file!", basename(Rmd_file)))
  }
  file <- try(md_file <- rmarkdown::render(input = Rmd_file, output_format = rmarkdown::md_document(variant = "markdown_github"), 
                                           encoding = "UTF-8"))
  if (class(file) == "try-error") {
    cat("Caught an error during rmarkdown::render, trying to read the md file.\n")
    md_file <- gsub("Rmd", "md", Rmd_file)
    if(!file.exists(md_file)) stop("md file not found. Quitting...")
  }
  front_matter <- rmarkdown::yaml_front_matter(Rmd_file, "UTF-8")
  mediumaddin_upload(md_file = md_file, title = front_matter$title, 
                     tags = front_matter$tags)
}
#===========================================================================================#
#===================== End: modified_medium_create_post_from_Rmd ===========================#
#===========================================================================================#