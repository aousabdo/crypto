# code taken from: https://rpubs.com/mengxu/peak_detection

x <- 1:1000 / 100 - 5
y <- exp(abs(x)/20) * sin(2 * x + (x/5)^2) + cos(10*x) / 5 + rnorm(length(x), sd=0.05)

plot(x, y, col = 'Gray', type = 'l')

y.smooth <- loess(y ~ x, span=0.05)$fitted
plot(x, y.smooth, type = 'l')

library(zoo)
w=30
y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
x.max <- rollapply(zoo(x), 2*w+1, median, align="center")
length(y.max)
length(x.max)

plot(x.max, y.max, col = 'Gray', type='l')
lines(x.max, y.max, col = 'SkyBlue', lwd = 2)

plot(x, y, col = 'Gray', type = 'l')
lines(x, y.smooth, col = 'Black')
lines(x.max, y.max, col = 'SkyBlue', lwd = 2)

legend('topleft', c('1', '2', '3'), cex=0.8, col=c('Gray', 'Black', 'SkyBlue'), lty=c(1,1,1));

n <- length(y)
delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
plot(x.max, delta, type='l')
abline(h = 0, lty='dotted', col = 'red')

#####################################################################
argmax <- function(x, y, w=1, FUN = max, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.peak <- rollapply(data = zoo(y.smooth), width = 2*w+1, FUN = FUN, align = "center")
  delta <- y.peak - y.smooth[-c(1:w, n+1-1:w)]
  if(FUN == "max")
    i.max <- which(delta <= 0) + w
  else if(FUN == "min")
    i.max <- which(delta >= 0) + w
  list(x = x[i.max], i = i.max, y.hat = y.smooth)
}

test <- function(x, y, w, span, FUN = max) {
  peaks <- argmax(x, y, w=w, span=span, FUN = FUN)
  
  plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
  lines(x, peaks$y.hat,  lwd=2) #$
  y.min <- min(y)
  sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]), col="Red", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
  print(x[peaks$i])
}

foo <- alt_data_roll_corr %>% 
  select(Date, rolling_corr, Coin) %>%
  filter(Coin == "DASH") 

argmax_1 <- argmax(x = seq_along(foo$rolling_corr), y = foo$rolling_corr, 2, 0.05, FUN = "max")

test(x = seq_along(foo$rolling_corr), y = foo$rolling_corr, 2, 0.05, FUN = "max")
test(x = seq_along(foo$rolling_corr), y = foo$rolling_corr, 2, 0.05, FUN = "min")




findMinMax <- function(x, y, x_axis_time = TRUE, w, span) {
  if(x_axis_time) x2 <- seq_along(x)
  minima <- argmax(x2, y, w=w, span=span, FUN = "min")
  maxima <- argmax(x2, y, w=w, span=span, FUN = "max")
  
  plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
  lines(x, minima$y.hat,  lwd=2) 
  y.min <- min(y)
  
  sapply(minima$i, function(i) lines(c(x[i],x[i]), c(y.min, minima$y.hat[i]), col="Red", lty=2))
  points(x[minima$i], minima$y.hat[minima$i], col="Red", pch=19, cex=1.25)

  sapply(maxima$i, function(i) lines(c(x[i],x[i]), c(y.min, maxima$y.hat[i]), col="blue", lty=2))
  points(x[maxima$i], maxima$y.hat[maxima$i], col="blue", pch=19, cex=1.25)
  
  legend("top", legend=c("Local Minima", "Local Maxima"),
         col=c("red", "blue"),  pch = c(19, 19), cex=0.8)

  df <- data.frame(x_value = c(x[minima$i], x[maxima$i])
                   , y_value = c(minima$y.hat[minima$i], maxima$y.hat[maxima$i])
                   , minmax = as.factor(c(rep("minima", length(x[minima$i])), rep("maxima", length(x[maxima$i])))))
  df <- df %>% arrange(x_value)
  return(df)
}

foo2 <- findMinMax(x = seq_along(foo$rolling_corr), y = foo$rolling_corr, 1, 0.05)

# foo2 <- findMinMax(x = seq_along(foo$rolling_corr[1:(30*3)]), y = foo$rolling_corr[1:(30*3)], 2, 0.05)


ggplot() +
  theme_bw() +
  geom_point(data = foo, aes(x = Date, y = rolling_corr), alpha = 0.5, size = 0.5) +
  geom_smooth(data = foo, aes(x = Date, y = rolling_corr), method="loess",  se=FALSE, size=0.5, span = 0.05) +
  geom_point(data = foo2, aes(x = foo[["Date"]][foo2$x_value], y = foo[["rolling_corr"]][foo2$x_value], col = minmax), lwd = 4) +
  scale_color_tq() + 
  theme(legend.position="none")
