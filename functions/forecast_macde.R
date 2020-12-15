# The function "forecast_macde" is used to add others variables to the answer. 
#The goals is to be like the values of  arima or naive functions (mean, x, fitted...)

#mcd.data - a vector or matrix column
#frequency - the number of data per period i.e frequency for monthly is 12
# growth_forecast - growth forecast 
#base - previous year will be used. #minimum value to use the macde. a-2. 
        #Two years before. The parameter base is used by function "mcd.fitted"
forecast_macde <- function(mcd.data, frequency = 12, growth_forecast = 1, round = T, base = 3){
  ans <- list()
  end.data <-   dmy(paste("01", end(mcd.data)[2], end(mcd.data)[1]))
  start.data <- dmy(paste("01", start(mcd.data)[2], start(mcd.data)[1]))
  
  val <- year(end.data) -  year(start.data)
  #data size invalid
  if(val < base - 1) {
    return(NULL)
  }
  
  ndata <- window(mcd.data, start = c(year(end.data) - base + 1, month(start.data)), 
                  end = c(year(end.data),  month(end.data)),
                  frequency = frequency)
  mcd.mean <- mcd.double.mean(ndata, frequency, growth_forecast, round)
  ans$mean <- ts(mcd.mean, start = c(end(ndata)[1] + 1, start(ndata)[2]), frequency = 12)
  ans$level <- c(95)
  ans$x <- mcd.data
  ans$fitted <- mcd.fitted(mcd.data, frequency, growth_forecast, round, base)
  ans$residuals <- NA
  ans$lambda <- NA
  ans$method <- "Double Mean"
  ans$model <- NA
  ttest <- t.test(ans$mean, conf.level = 0.95)
  n <- length(ans$mean)
  ans$upper <- matrix(NA, nrow = n, ncol = 2)
  ans$lower <- matrix(NA, nrow = n, ncol = 2)
  ans$upper[, 2] <- ans$mean + ttest$statistic *(sd(ans$mean) / sqrt(n))
  ans$lower[, 2] <- ans$mean - ttest$statistic *(sd(ans$mean) / sqrt(n))
 
  return(ans)
}
########################################################################
mcd.fitted <- function(mcd.data, frequency = 12, growth_forecast = 1, round = T, base = 3){
  end.time.data <-   dmy(paste("01", end(mcd.data)[2], end(mcd.data)[1]))
  start.time.data <- dmy(paste("01", start(mcd.data)[2], start(mcd.data)[1]))
  
  val <- year(end.time.data) -  year(start.time.data)
  if(val >= base) {
    ndata <- window(mcd.data, start = c(year(start.time.data), month(start.time.data)), 
                    end = c(year(end.time.data) - 1,  month(end.time.data)),
                    frequency = frequency)
    
    mcd.fit <- mcd.fitted(ndata, frequency, growth_forecast, round, base)
    if(val != base) {
      ndata <- window(mcd.data, start = c(year(end.time.data) - base, month(start.time.data)), 
                      end = c(year(end.time.data) - 1,  month(end.time.data)),
                      frequency = frequency)
    }
    
    mcd.mean <- mcd.double.mean(ndata, frequency, growth_forecast, round)
    mcd.mean.ts <- ts(mcd.mean, start = c(end(ndata)[1] + 1, start(ndata)[2]), frequency = 12)
    
    ans.mcd.fitted <- c(mcd.fit, mcd.mean.ts)
  }
  else{
    numb.na <- (year(end.time.data) - year(start.time.data)) + 1
    ans.mcd.fitted <- rep(NA, numb.na * frequency)
  }
  
  ans.mcd.fitted <- ts(ans.mcd.fitted, 
                       start = c(year(start.time.data), month(start.time.data)),
                       end = c(year(end.time.data), month(end.time.data)),
                       frequency = frequency)
  
  return(ans.mcd.fitted)
}

########################################################################
#The Double Mean Forecast is calculated by the function "mcd.double.mean". 
#This function receive the following parameters:
#mcd.data - is the data used to do forecast.
#frequency - is the quantity of mounth will be forecast
#growth_forecast - if the consumer knows their demand growth or 
#                  if he expects their demand will grow.
#round - if TRUE the final values will be round to the next integer number. 
#        the ceiling function is used.
mcd.double.mean <- function(mcd.data, frequency = 12, growth_forecast = 1, round = F){
  mtx <- matrix(mcd.data, nrow = frequency, byrow = F)
  sz <- rep(1, length(mcd.data) / frequency)
  m.times <- matrix(sz, byrow = F)
  obvalid <- rowSums(!is.na(mtx))
  obvalid[obvalid == 0] <- 1
  mtx[is.na(mtx)] <- 0
  y.mean <- (mtx %*% m.times) / obvalid
  y.max <- apply(mtx, 1, max)
  y.double.mean <- rowMeans(cbind(y.max, y.mean))
  mcd.mean <- y.double.mean * growth_forecast
  
  if(round) {
    mcd.mean <- ceiling(mcd.mean)
  }
  
  return(mcd.mean)
}


