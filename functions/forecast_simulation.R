#---------------------------------------------------------------
macdeSimulForecast <- function(myts = myts, growth_forecast = 1.05, base = 3, round = T) {
  #Create Forecast and MAPE for all combination of year
  mcd.fcast <- create_mape_forecast_all_combination(type.fcast = "macde", 
                                                    myts, growth_forecast, base)
  
  if(round) {
    mcd.fcast$mape.table$mape <- round(100 * mcd.fcast$mape.table$mape, 2)
  }
  
  return(mcd.fcast)
}

#---------------------------------------------------------------

arimaSimulForecast <- function(myts, period, year, kvalue, order, 
                               season_order, season_period) {
  nnq <- list()
  nnq$model <- arima(myts$training[[period]][, kvalue], order = order, 
                     seasonal = list(order = season_order, period = season_period))
  nnq$forecast <- forecast(nnq$model, h = season_period)          
  nnq$mape <- MAPE(nnq$forecast$mean, myts$test[[year]][,kvalue])
  return(nnq)
}

#---------------------------------------------------------------

arimaSimulTable <- function(year, kvalue, fcast = nnq, order, season_order)
{
  nnq_real <- myts$test[[year]][,kvalue]
  nnq_forecast <- fcast$forecast$mean
  nnq_acc <- accuracy(nnq_forecast, nnq_real)
  nnq.residuals <- checkresiduals(fcast$model, plot = F)
  
  nnq_table <- list()
  nnq_table$info <- data.frame(modelo = paste("ARIMA(", order[1], ",", order[2], ",", order[3], ")(", 
                                              season_order$order[1], ",", season_order$order[2], ",", season_order$order[3], 
                                              ")", sep = ""), 'MAPE' = round(nnq_acc[1, 'MAPE'], 2), 'AIC' = round(fcast$model$aic, 2),
                               'p.value' = nnq.residuals$p.value)
  error_perc <- round(100 * abs(( nnq_forecast - nnq_real) / nnq_real), 2)
  nnq_table$compare <- cbind("Real" = nnq_real, "Previsto" = round(nnq_forecast, 2), error_perc)
  return(nnq_table)
}

#---------------------------------------------------------------
naiveSimulForecast <- function(myts = myts){
  #Create Forecast and MAPE for all combination of year
  nf <- create_mape_forecast_all_combination(type.fcast = "naive", myts = myts)
  
  return(nf)
}

#---------------------------------------------------------------
arimaSimulGraphs <- function(myts, nnq.fcast, period = "p.2014.2018", 
                 testyear = "t.2019", kvalue = 'pkd') {
  nameg <- paste("nnq_forecast", kvalue, sep = "_")
  g <- plot.ts.forecast.XY(myts$training[[period]][, kvalue], 
                    nnq.fcast, 
                    ymsg = getMsg("msg_demand_kw", lang_activeted), 
                    xmsg = getMsg("msg_year", lang_activeted),
                    save = T,
                    lcolor = c("#808080", "#1C1C1C","#1C1C1C", "#696969","#A9A9A9"),
                    ltype = c("solid","solid","dashed","dashed","dashed"),
                    save.name = nameg,
                    ggwidth = graphics.size.pattern$width,
                    lsize = 0.8,
                    nlegends  = c(as.character(getMsg("msg_observed", lang_activeted)),
                                  as.character(getMsg("msg_fitted", lang_activeted)),
                                  as.character(getMsg("msg_mean", lang_activeted)),
                                  as.character(getMsg("msg_upper_95", lang_activeted)),
                                  as.character(getMsg("msg_lower_95", lang_activeted))))
  return(g)
}
#---------------------------------------------------------------

macdeSimulGraphs <- function(myts, mcd.fcast, period = "p.2014.2018", 
                             testyear = "t.2019", kvalue = 'pkd') {
  mcd.graphs <- list()

  #Create Graph
  nameg <- paste("forecast_mcd", kvalue, sep = "_")
  mcd.graphs$fcastMCD <- plot.ts.forecast.XY(myts$training[[period]][, kvalue], 
                      mcd.fcast[[period]][[kvalue]], 
                      ymsg = getMsg("msg_demand_kw", lang_activeted), 
                      xmsg = getMsg("msg_year", lang_activeted),
                      save = T,
                      lcolor = c("#808080", "#1C1C1C","#1C1C1C", "#696969","#A9A9A9"),
                      ltype = c("solid","solid","dashed","dashed","dashed"),
                      save.name = nameg,
                      ggwidth = graphics.size.pattern$width,
                      lsize = 0.8,
                      nlegends  = c(as.character(getMsg("msg_observed", lang_activeted)),
                                    as.character(getMsg("msg_fitted", lang_activeted)),
                                    as.character(getMsg("msg_mean", lang_activeted)),
                                    as.character(getMsg("msg_upper_95", lang_activeted)),
                                    as.character(getMsg("msg_lower_95", lang_activeted))))
  
  #Create Graph Double Mean
  nameg <- "model_macde_compare"
  mcd.graphs$doubleMean <- plot.macde.compare(main.dts = myts$training[[period]][, kvalue], 
                     fcasted = mcd.fcast[[period]][[kvalue]]$mean,
                     ftest = myts$test[[testyear]][, kvalue],
                     psize = 2.5,
                     lsize = 1.2,
                     ymsg = getMsg("msg_demand_kw", lang_activeted), 
                     xmsg = getMsg("msg_month", lang_activeted),
                     slinetype = c("solid", "dotted"),
                     lynecolor = "#808080",
                     shapecolor = "black",
                     save = T,
                     save.name = nameg, 
                     legendlabels = c(as.character(getMsg("msg_observed", lang_activeted)),
                                      as.character(getMsg("msg_forecast", lang_activeted))))
  return(mcd.graphs)
  
} 

#---------------------------------------------------------------

naiveSimulGraphs <- function(myts, naive.fcast, period = "p.2014.2018", 
                             testyear = "t.2019", kvalue = 'pkd') {
  nameg <- paste("forecast_naive", kvalue, sep = "_")
  mcd.graphs <- plot.ts.forecast.XY(myts$training[[period]][,kvalue], 
                      naive.fcast[[period]][[kvalue]], 
                      ymsg = getMsg("msg_demand_kw", lang_activeted), 
                      xmsg = getMsg("msg_year", lang_activeted),
                      save = T,
                      lcolor = c("#808080", "#1C1C1C","#1C1C1C", "#696969","#A9A9A9"),
                      ltype = c("solid","solid","dashed","dashed","dashed"),
                      save.name = nameg,
                      ggwidth = graphics.size.pattern$width,
                      lsize = 0.8,
                      nlegends  = c(as.character(getMsg("msg_observed", lang_activeted)),
                                    as.character(getMsg("msg_fitted", lang_activeted)),
                                    as.character(getMsg("msg_mean", lang_activeted)),
                                    as.character(getMsg("msg_upper_95", lang_activeted)),
                                    as.character(getMsg("msg_lower_95", lang_activeted))))
  
  return(mcd.graphs)
  
} 

#---------------------------------------------------------------

naiveSimulTable <- function(myts, year = 't.2019', save = F, 
                            kvalue = 'pkd', round = F) {
  table.resul <- mount_table(myts, typef = "naive")
  
  if(save) {
    write.csv(table.resul, file = "output/table_naive_all.csv")
  }
  
  filter_table <- table.resul$year == year & table.resul$k == kvalue
  naive.table <- table.resul[filter_table,]
  
  if(round) {
    naive.table['error'] <- round(100 * as.numeric(as.character(naive.table[['error']])), 2)   
    naive.table['mape'] <- round(100 * as.numeric(as.character(naive.table[['mape']])) , 2)
  }
  return(naive.table)
}

#---------------------------------------------------------------

macdeSimulTable <- function(myts, base, save = F, period = 'p.2016.2018', 
                            kvalue = 'pkd', grw_vct = c(0.95, 0.97, 0.99, 1, 1.01, 1.03, 1.05),
                            round = T) {
  mcd.table <- list()
  
  #table with relative errors belong to growth forecast
  table.error <- matrix(ncol =0, nrow = 12)
  #table with all values forecasted
  table.fcast <- matrix(ncol =0, nrow = 12)
  mp <- matrix()
  for (i in grw_vct) {
    table.resul <- mount_table(myts, typef = "macde", growth_forecast = i, base)
    fpkd <- table.resul[table.resul$period == period & table.resul$k == kvalue,]
    fcast.real <- as.numeric(as.character(fpkd$real))
    fcast.pred <- as.numeric(as.character(fpkd$fcast))
    mp <- cbind(mp, MAPE(fcast.pred, fcast.real))
    error.perc.rel <- 100 * abs((fcast.pred - fcast.real) / fcast.real)
    table.fcast <- cbind(table.fcast, fcast.pred)
    table.error <-cbind(table.error,error.perc.rel)
  }
  colnames(table.fcast) <- grw_vct
  colnames(table.error) <- grw_vct
  
  table.fcast <- cbind(fcast.real, table.fcast)
  table.error <- cbind(fcast.real, table.error)
  
  if(round) {
    table.error <- round(table.error,2)
    table.fcast <- round(table.fcast, 2)
    mp <- round(100 * mp, 2) #MAPE values percentual
  }
  
  mcd.table$compGrowth$fcast <- table.fcast
  mcd.table$compGrowth$error <- table.error
  mcd.table$compGrowth$mape <- mp
  
  if(save) {
    write.csv(table.resul, file = 
                paste("output/table_mcd_all", "growth", growth_forecast, ".csv", sep = "_"))
  }
    
  return(mcd.table)
}
 

