load_ts <- function(df, 
                    basic = F,
                    split = T,
                    imputationMV = T,
                    start.ts = c(2014, 1), 
                    end.ts = c(2019, 12), 
                    freq.ts = 12) {
  df <- df[, c("pkd", "opd", "pke", "ope", "ctrd")]
  
  #create times series
  new.ts <- ts(df, start = start.ts, end = end.ts, frequency = freq.ts)
  
  ctrd_prov <- new.ts[,'ctrd']
  
  if(imputationMV) {
    #Basic information - Treating missing values
    new.ts <- na_seasplit(new.ts, algorithm ="mean", option = "median")
  }
  
  if(!basic){
    add_new_no_basic_data_ts(new.ts)
  }
  
  if(split){
    new.ts <- split_data_ts(new.ts, start.ts, end.ts)
  }
  
  return(new.ts)
}

add_new_no_basic_data_ts <- function(dt_ts) {
  #add in dataframe the pu values
  ctrd_prov <- dt_ts[,'ctrd']
  
  dtotal = pmax(dt_ts[,'pkd'], dt_ts[,'opd'])
  etotal = dt_ts[,'pke'] + dt_ts[,'ope']
  dt_ts    <- cbind('pkd' = dt_ts[,'pkd'],  'opd' = dt_ts[,'opd'], 
                     'pke' = dt_ts[,'pke'], 'ope' = dt_ts[,'ope'],
                     'ctrd' = ctrd_prov,
                     dtotal,
                     etotal,
                     'dtotal.pu' = dtotal / max(dtotal),
                     'etotal.pu' = etotal / max(etotal),
                     'pkd.pu' = dt_ts[,'pkd'] / max(dt_ts[,'pkd']),
                     'opd.pu' = dt_ts[,'opd'] / max(dt_ts[,'opd']),
                     'pke.pu' = dt_ts[,'pke'] / max(dt_ts[,'pke']),
                     'ope.pu' = dt_ts[,'ope'] / max(dt_ts[,'ope']))
  return(dt_ts)
}

split_data_ts <- function(dt_ts, start.ts, end.ts) {
  myts <- list()
  myts$x <- dt_ts
  #Create a training subsets of combination from year start to year end. Format: p.START.END
  cmb.date <- combination_date_start_end(start.ts[1], end.ts[1] - 1)
  for (i in 1:nrow(cmb.date)) {
    combname <- paste("p", cmb.date$start[i], cmb.date$end[i], sep=".")
    myts$training[[combname]] <- window(myts$x, start = c(cmb.date$start[i], 1),
                                        end = c(cmb.date$end[i],12), frequency = 12)
  }
  
  #Create a test subsets of combination from year start to year end. Format: t.YEAR
  for (i in start.ts[1]:end.ts[1]) {
    combnam <- paste("t", i, sep=".")
    myts$test[[combnam]] <- window(myts$x, start = c(i, 1), end = c(i,12), frequency = 12)
  }
  return(myts)
}

combination_date_start_end <- function(year.start, year.end ){
  combi <- data.frame()
  sz <- year.end - year.start
  for (i in 0:(sz-1)) {
    for (j in (sz-i):1) {
      combi <- rbind(combi, data.frame(start = year.start + i, end = year.start + i + j))
    }  
  }
  return(combi)
}
