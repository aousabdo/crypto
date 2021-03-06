---
title: "Analyzing Cryptocurrency Markets using R - Part 3 Rolling Correlations and More!"
author: "Dr. Aous Abdo @aousabdo"
output:
  html_document:
    df_print: paged
  html_notebook: default
editor_options:
  chunk_output_type: inline
---

In my two previous posts, [Downloading and Processing Crypto Data with R](http://rpubs.com/aousabdo/crypto_1) and [Corrleations in the Crypto World Part 1](http://rpubs.com/aousabdo/crypto_2) I went over how one can download crypto data in R and talked about static correlations for some coins. In this post, we will cover rolling correlatoins, a type of correlations we use with time-series data like crypto data. 

## R Libraries 
In this post we will be using the same libraries from my previous [post](http://rpubs.com/aousabdo/crypto_2). We will also reuse some of the functions introducec in that post. 



## Rolling Correlations

Establishing a relationship, in this case a correlation, between two time series can be very benificial for several reasons, the most important of which is being able to use this relationship to predict market movement, which if you can acheive then you'll be rich. Established correlations between the price of a crypto currency, for example, with other time series data such as market sentiment, news, or the price of another crypto currency, can be used to better forecast the movement of that coin. But as we have seen in the previous [post](http://rpubs.com/aousabdo/crypto_2), one problem exists, these correlations are not static and they could vary greatly overtime.

Rolling correlations are similar to [moving averages](https://www.investopedia.com/university/movingaverage/movingaverages1.asp), they are metrics we apply over a time window to better see potential trends and avoid the noise in the data. Since this might be a new topic for some readers let's elaborate more. 

Suppose we have a dataset which reflects daily sales for a department store for three products, A, B, and C. The sales for these products are shown in the table below. 

![Department store sales](../figures/sales.PNG){width=500px}

One can see from the table that sales for a given product vary over time, with the greatest sales taking place over the weekends, Jan 20-21 and 27-28 in the sample shown. Let's calculate the moving average for product C over a period of 4 days for example. To calculate the moving average, or any rolling function, we first have to select a time window, in this case we selected 4 days. After that we apply the function, in this case calculating the average over the selected time window. Once that is done we shift the window by one unit, one day in this case, and reapply the function and so on. The figure below shows the moving averages for Product C over a period of 4 days.

![Department store sales moving average](../figures/sales2.PNG){width=500px}

One major factor in a rolling function is the time window over which this function is applied. Intuitively, one would expect the variability in the value returned to be lower for larger time windows. We show that in the figure below where we calcualte the moving averages for different time windows.

```r
# sales for product C
prod_c <- c(1030, 1195, 1228, 1178, 1504, 1697,1710 ,1491 ,1240 , 861, 853, 1158, 1265, 1566,1182)

# calculate 3 moving averages for time windows of 2, 4, and 7 days
sma2 <- rollmean(prod_c, k = 2)
sma4 <- rollmean(prod_c, k = 4)
sma7 <- rollmean(prod_c, k = 7)

# plot sales and overlay the moving averages
plot(prod_c, pch = 16, col = "black", xlab = "Index", ylab = "Sales", cex = 1.5, ylim = c(800, 1800))
points(sma2, col = "red", pch=16, cex = 1.5)
points(sma4, col = "blue", pch=16, cex = 1.5)
points(sma7, col = "green", pch=16, cex = 1.5)

# add legend
legend(0.8, 1820, legend=c("Sales", "2-day moving avg.", "4-day moving avg.", "7-day moving avg."),
       col=c("black", "red", "blue", "green"),  pch = rep(16, 4), cex=0.8)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Notice how increasing the time window helps us get rid of the noise in the data. This is evident from the smoother curves for the moving averages with time windows of 4 and 7. It is also worth noting that increasing the time window decreases the number number of retunred moving averages. This implies that a moving average over the whole period will result in just one number, which is simply the average over the whole period. 

The same can be done to any function that calculates a static value, like calculating correlations between two time series. 
Let's now calculate the rolling correlations over a period of 4 days for the sales of Products A and C. 


```r
# let's put the two sales of products A and C in a data.frame
prod_a <- c(250, 241, 251, 286, 365, 412, 415, 362, 301, 280, 250, 234, 215, 198, 300)
prod_c <- c(1030, 1195, 1228, 1178, 1504, 1697,1710 ,1491 ,1240 , 861, 853, 1158, 1265, 1566,1182)

df <- data.frame(prod_a, prod_c)

# now we'll calculate the rolling correlation
rollcorr_a_c <- runCor(df[, 1], df[, 2], n = 4)

# now let's make a plot
plot(rollcorr_a_c, pch = 19, col = "blue", xlab = "Index", ylab = "Correlation", cex = 1.5, type = "b")
abline(h = cor(df[, 1], df[2]), col = "red", lwd = 3)
legend(0.8, -0.5, legend=c("4-day Rolling Correlation", "Static Correlation"),
       col=c("blue", "red"),  lty=c(1, 1),pch=c(19, NA), cex=0.8)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

As one can see from the above figure, for time series the correlation is not static and changes over time. In the figure the rolling correlation, shown in blue, varies greatly overtime, while the static correlation (shown in red) is constant.





