create_forecast_period <- function(main.ts, fcast, set_i, set_j) {
  ans <- list()
  tsss <- list()
  for (j in set_j) {
    a <- data.frame()
    for (i in set_i) {
      set_test <- paste("t", floor(max(trunc(time(main.ts[[i]]))))+ 1, sep = ".") 
      b <- data.frame(as.numeric(fcast[[i]][[j]]))
      d <- cbind(b, rep(i, 12), set_test)
      names(d) <- c("y", "period", "f")
      a <- rbind(a, d)
    }
    names(a) <- c("y", "period", "f")
    ans[[j]] <- a
    
    lvl.ans.f <- levels(ans[[j]][["f"]])
    str.year.lvl <- str_sub(lvl.ans.f, start = 3)
    y.lvl <- as.numeric(str.year.lvl)
  
    tsss[[j]] <- ts(ans[[j]][["y"]], start = c(min(y.lvl), 1), end = c(max(y.lvl), 12), frequency = 12)
   
  }
  
   return(tsss)
}

##############################################################
create_multiple_periods_set_test <- function(myts.test, set_i, set_j) {
  tsss <- list()
  for (j in set_j) {
    a <- data.frame()
    for (i in set_i) {
      b <- data.frame(as.numeric(myts.test[[i]][, j]))
      d <- cbind(b, rep(i, 12))
      a <- rbind(a, d)
    }
    names(a) <- c("y", "f")
    
    lvl.ans.f <- levels(a[["f"]])
    str.year.lvl <- str_sub(lvl.ans.f, start = 3)
    y.lvl <- as.numeric(str.year.lvl)
    tsss[[j]] <- ts(a[["y"]], start = c(min(y.lvl), 1), end = c(max(y.lvl), 12), frequency = 12)
  }
  
  return(tsss)
}



