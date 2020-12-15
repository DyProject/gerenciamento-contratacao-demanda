outliers_check <- function(dts, freq = 12, imputation = T){
  new.serie <- dts
  ans <- list()
  for (i in 1:freq) {
    dt_slc_numb <- calcular_sequencia_mes(new.serie, mes =i)
    outl <- boxplot(new.serie[dt_slc_numb, c("pkd", "opd", "pke", "ope")], plot = F)
    
    ans$num_outlier[i] <- length(outl$out)
    if(ans$num_outlier[i]) {
      for (j in 1:ans$num_outlier[i]) {
        group_out <- outl$group[j]
        new.serie[, outl$names[group_out]][ new.serie[, outl$names[group_out]] == outl$out[j]] <- NA
      }
    }
  }
  
  if(imputation) {
    ans$ts <- na_seasplit(new.serie, algorithm ="mean", option = "median")  
  } else {
    ans$ts <- new.serie
  }
  
  
  return(ans)
}


calcular_data <- function(ano, mes) {
  prov <- round(mes/12 - 1/12, 3)
  return(ano + prov)
}

calcular_sequencia_mes <- function(dts, mes, data_instal_solar = c(2100, 12)) {
  freq <- frequency(dts)
  numb_years <- length(time(dts)) / freq
  init_year <- start(dts)[1]
  end_year <- end(dts)[1]
  dif <- data_instal_solar[1] - init_year
  
  seq_val <- c()
  for (i in 0:(numb_years - 1)) {
    if(init_year + i < data_instal_solar[1] | 
       (init_year + i == data_instal_solar[1] & mes < data_instal_solar[2])) {
      seq_val <- append(seq_val, (mes + i * freq))
    }
  }
  
  return(seq_val)
}