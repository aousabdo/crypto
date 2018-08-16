crypto_history_2 <- function (coin = NULL, limit = NULL, cpu_cores = NULL, start_date = NULL, 
                              end_date = NULL) 
{
  ifelse(as.character(sys.call()[[1]]) == "getCoins", warning("DEPRECATED: Please use crypto_history() instead of getCoins().", 
                                                              call. = TRUE, immediate. = TRUE), print(" "))
  cat("Retrieves coin market history from CoinMarketCap. ")
  i <- "i"
  options(scipen = 999)
  sys_locale <- Sys.getlocale(category = "LC_TIME")
  replace_encoding(sys_locale)
  coins <- crypto_list(coin, start_date, end_date)
  if (!is.null(limit)) {
    coins <- coins[1:limit, ]
  }
  coinnames <- dplyr::data_frame(symbol = as.character(coins$symbol), 
                                 name = as.character(coins$name), rank = coins$rank, slug = coins$slug)
  length <- as.numeric(length(coins$history_url))
  zrange <- 1:as.numeric(length(coins$history_url))
  if (is.null(cpu_cores)) {
    cpu_cores <- as.numeric(parallel::detectCores(all.tests = FALSE, 
                                                  logical = TRUE))
  }
  ptm <- proc.time()
  cluster <- parallel::makeCluster(cpu_cores, type = "SOCK")
  doSNOW::registerDoSNOW(cluster)
  pb <- txtProgressBar(max = length, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  attributes <- coins$history_url
  slug <- coins$slug
  message("   If this helps you become rich please consider donating", 
          appendLF = TRUE)
  message("ERC-20: 0x375923Bf82F0b728d23A5704261a6e16341fd860", 
          appendLF = TRUE)
  message("XRP: rK59semLsuJZEWftxBFhWuNE6uhznjz2bK", appendLF = TRUE)
  message("LTC: LWpiZMd2cEyqCdrZrs9TjsouTLWbFFxwCj", appendLF = TRUE)
  results_data <- foreach::foreach(i = zrange, .errorhandling = c("remove"), 
                                   .options.snow = opts, .combine = rbind, .verbose = FALSE) %dopar% 
    crypto::scraper(attributes[i], slug[i])
  close(pb)
  parallel::stopCluster(cluster)
  print(proc.time() - ptm)
  results <- merge(results_data, coinnames, by = "slug")
  marketdata <- results %>% as.data.frame()
  namecheck <- as.numeric(ncol(marketdata))
  ifelse(namecheck > 2, colnames(marketdata) <- c("slug", "date", 
                                                  "open", "high", "low", "close", "volume", "market", "symbol", 
                                                  "name", "ranknow"), NULL)
  marketdata <- marketdata[c("slug", "symbol", "name", "date", 
                             "ranknow", "open", "high", "low", "close", "volume", 
                             "market")]
  marketdata$date <- suppressWarnings(lubridate::mdy(unlist(marketdata$date)))
  cols <- c(5:11)
  ccols <- c(7:11)
  marketdata[, cols] <- apply(marketdata[, cols], 2, function(x) gsub(",", 
                                                                      "", x))
  marketdata[, ccols] <- apply(marketdata[, ccols], 2, function(x) gsub("-", 
                                                                        "0", x))
  # marketdata$volume <- marketdata$volume %>% tidyr::replace_na(0) %>% 
  #   as.numeric()
  # marketdata$market <- marketdata$market %>% tidyr::replace_na(0) %>% 
  #   as.numeric()
  marketdata[, cols] <- suppressWarnings(apply(marketdata[, 
                                                          cols], 2, function(x) as.numeric(x)))
  marketdata <- na.omit(marketdata)
  marketdata$close_ratio <- (marketdata$close - marketdata$low)/(marketdata$high - 
                                                                   marketdata$low)
  marketdata$close_ratio <- round(marketdata$close_ratio, 4)
  # marketdata$close_ratio <- marketdata$close_ratio %>% tidyr::replace_na(0) %>% 
  #   as.numeric()
  marketdata$spread <- (marketdata$high - marketdata$low)
  marketdata$spread <- round(marketdata$spread, 2)
  results <- marketdata[order(marketdata$ranknow, marketdata$date, 
                              decreasing = FALSE), ]
  reset_encoding(sys_locale)
  return(results)
}



crypto_return <- function(df = NULL
                          , N_months = 12
                          , coin_name = NULL
                          , save_png = TRUE){
  
  # df: a dataframe object containing daily cyrpto closing prices
  # N_months: the number of months to calcualte the percentage return over
  # get n month bitcoin percentage return
  
  if(is.null(df)) stop("I need a dataframe object containing daily cyrpto closing prices")
  if(sum(c("date", "close") %in% names(bitcoin)) != 2) 
    stop("the dataframe needs to have two columns with the names \"date\" and \"close\" ")
  
  dt <- as.data.table(df)
  
  N    <- N_months
  days <- 30
  l    <- N * days
  
  if(is.null(coin_name)) coin_name <- "crypto_coin"
  # we assusme the data table bitcoin contains the closing price as price and that it has more than l 
  # data points
  
  # calcualte the difference
  return_N_days <- diff(dt$close, lag = l)
  # now calcualte the percentage difference
  return_N_days_new <- 100*return_N_days/dt[1:length(return_N_days), close]
  # now add the missing NAs to avoid recycling when adding new column to the data.table
  return_N_days_new <- c(rep(NA, l), return_N_days_new)
  
  dt[, Return := return_N_days_new]
  
  dt[!is.na(Return)] %>% ggplot(aes(date, Return, col = as.factor(year(date)))) + geom_line(size = 1.5) +
    xlab("Year") + ylab(paste0(N, "-Month % Return")) +
    scale_colour_manual(name = "Year"
                        , values=c("steelblue","blue","red", "orange", "darkgreen", "darkred")
                        , breaks=c(2013:2018)) + 
    geom_abline(slope = 0, intercept = 0) + 
    ggtitle(paste0("Percentage Return of Hodling ", coin_name, " Over a period of ", N, " Months")) -> p

  png(filename = paste0("Percentage_Return_", coin_name,"_", N, "_Months.png"), width = 1060, height = 512)
  print(p)
  dev.off()  
  
  return(p)
}
