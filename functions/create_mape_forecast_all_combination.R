#O parâmetro base indica a quantidade de anos anteriores que se deseja utilizar na previsão.
#Como a função create_mape_forecast_all_combination realiza a previsão para todas as possibilidades possíveis,
#caso base=NULL, indica que o valor da base será igual a quantidade de anos passados nos dados.
#Por exemplo, para os anos de 2014-2018, para prever 2016 o valor da base será igual a 5. Já para os anos
# 2015-2017, para prever 2018 o valor da base será base=3.

#Caso o valor da base seja difente de NULL, as combinações serão realizadas apenas para os dados
#que atendam o valor definido da base. Por exemplo, caso o valor base=4 seja passado como parâmetro, 
#a previsão será realizada apenas para os anos em que é possível utilizar 4 anos anteriores. Assim, serão
#realizadas as previsões para 2019 (2018,2017,2016,2015, 2014), 2019(2018,2017,2016,2015) 
#e 2018 (2017,2016,2015, 2014), isso se considerar os dados myts como 2014-2018.
#OBS: apesar de passar como base 5 anos (2018,2017,2016,2015, 2014), como base=4 foi definido, 
#o valor previsto considerará apenas 4 anos anteriores. Assim para as previsões, com base=4, 
#2019 (2018,2017,2016,2015, 2014) e 2019(2018,2017,2016,2015)  o valor previsto será igual.
create_mape_forecast_all_combination <- function(type.fcast = "macde", myts, growth_forecast = 1, base = NULL){
  myts.main.names <- c("pkd", "opd", "pke", "ope")
  fcast <- list()
  frequency <- 12
  fcast$mape.table <- data.frame(stringsAsFactors = FALSE)
  for (i in names(myts$training)) {
    j <- paste("t", floor(max(time(myts$training[[i]])))+ 1, sep = ".") 
    
    for (k in myts.main.names) {
      if(type.fcast == "macde") {
        if(is.null(base)) {
          base.mcd = length(myts$training[[i]][, k]) / frequency
        }else{
          base.mcd = base
        }
        snai <- forecast_macde(myts$training[[i]][, k], frequency = frequency, 
                               growth_forecast = growth_forecast, round = F, base = base.mcd)
      } 
      else if(type.fcast == "naive"){
        snai <- snaive(myts$training[[i]][, k], h = 12)
        base.mcd = 1
      } 
      else{
        return(NULL)
      }
      if(!is.null(snai)){
        #Fitted values (one-step forecasts)
        fcast[[i]][[k]]$fitted <- snai$fitted
        #Residuals from the fitted model. That is x minus fitted values.
        fcast[[i]][[k]]$residuals <- snai$residuals
        #The confidence values associated with the prediction intervals
        fcast[[i]][[k]]$level <- snai$level
        #Upper limits for prediction intervals
        fcast[[i]][[k]]$upper <- snai$upper
        #Lower limits for prediction intervals
        fcast[[i]][[k]]$lower <- snai$lower
        #Point forecasts as a time series
        fcast[[i]][[k]]$mean <- snai$mean
        #A list containing information about the fitted model
        fcast[[i]][[k]]$model <- snai$model
        #The name of the forecasting method as a character string
        fcast[[i]][[k]]$method <- snai$method
        #The original time series (either object itself or the time series used to create 
        #the model stored as object).
        fcast[[i]][[k]]$x <- snai$x
        fcast[[i]][[k]]$lambda <- snai$lambda
        fcast[[i]][[k]]$series <- snai$series
        #Real Values that happened
        fcast[[i]][[k]]$test <- myts$test[[j]][, k]
        #absolute difference between real and forecast
        fcast[[i]][[k]]$diff <- myts$test[[j]][, k] - fcast[[i]][[k]]$mean
        fcast[[i]][[k]]$base <- base.mcd
        #The mean absolute percentage error (MAPE)
        fcast[[i]][[k]]$mape <- MAPE(fcast[[i]][[k]]$mean, myts$test[[j]][, k])
        b <- data.frame("period" = i, "base"= base.mcd,k, "mape" = fcast[[i]][[k]]$mape)
        #table with MAPE for all combination
        fcast$mape.table <- rbind(fcast$mape.table, b)
      }
    }
  }

  return(fcast)
}