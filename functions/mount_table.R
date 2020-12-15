#create table with all values (MAPE, ERROR...)
#For Naive input growth_forecast doesn't matter
mount_table <- function(myts, typef = 'macde', growth_forecast = 1, base = 3) {
  fcast <- list()
  myts.main.names <- c("pkd", "opd", "pke", "ope")
  tab <- data.frame(stringsAsFactors = F)
  for (i in names(myts$training)) {
    j <- paste("t", floor(max(time(myts$training[[i]])))+ 1, sep = ".")
    resp <- data.frame(stringsAsFactors = F)
    for (k in myts.main.names) {
      if(typef == 'macde'){
        fcast <- forecast_macde(myts$training[[i]][, k], frequency = 12, 
                                       growth_forecast = growth_forecast, round = F, base = base)  
      }
      else {
        fcast <- as.numeric(snaive(myts$training[[i]][, k], h = 12)$mean)
        base = 1
      }
      if(!is.null(fcast)) {
        real <- myts$test[[j]][, k]
        if(typef == 'macde'){
          fcast.both <- fcast$mean
        }
        else {
          fcast.both <- fcast
        }
        
        error <- abs((as.numeric(real) - fcast.both) / as.numeric(real))
        mape <- MAPE(fcast.both, real)
        resp <- cbind("period" = i, "year" = j,"month" = 1:12, growth_forecast,  "base" = base,
                      k,  real, "fcast" = fcast.both, error, mape)
        tab <- rbind(tab, resp)
      }
    }
  }
  return(tab)
}