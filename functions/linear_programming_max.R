linear_programming_max <- function(cfg_opt, demand_measured, v.df, cost_max, dem_cont_before = NULL) {
  #### Objective function ####
  objfn <- list()
  
  dec_var_inf <- v.df[[cfg_opt[["tariff_modality"]]]]
  for (i in 1:length(dec_var_inf$name)) {
    if(dec_var_inf$name[i] == "contracted demand") {
      cost_values <- rep(1, cfg_opt$horizon)
    } else {
      cost_values <- rep(0, cfg_opt$horizon)
    }
    type_values <- rep(dec_var_inf[["type"]][[i]], cfg_opt$horizon)
    objfn$cost <- append(objfn$cost, cost_values)
    objfn$type <- append(objfn$type, type_values)
  }
  
  #make.lp(nrow (número restrições), ncol (variáveis de decisão))
  #inicia sem nenhuma restrição.
  lp <- make.lp(0, length(objfn$cost))
  name.lp(lp, "contracted demand simulation max")
  lp.control(lp, sense="max")
  
  levels_type <- levels(as.factor(dec_var_inf[["type"]]))
  seq_number_type <- 1:length(objfn$type)
  for (i in levels_type) {
    set.type(lp, seq_number_type[objfn$type == i] , i)
  }
  
  set.objfn(lp,  objfn$cost)
  
  #### Restricions ####
  vct_origin <- rep(0, length(objfn$cost))
  names_compare <- v.df[[cfg_opt$tariff_modality]]$name
  seq_number_name <- 1:length(names_compare)
  
  #### Cost #### 
  v <- vct_origin
  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      pos_n <-seq_number_name[names_compare == "faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
      v[pos_init - 1 + i] = cfg_opt$tariffs$pkd

      pos_n <-seq_number_name[names_compare == "overtaked demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
      v[pos_init - 1 + i] = 2 * cfg_opt$tariffs$pkd

      pos_n <-seq_number_name[names_compare == "faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = cfg_opt$tariffs$opd

      pos_n <-seq_number_name[names_compare == "overtaked demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = 2 * cfg_opt$tariffs$opd
    }
  } else {
    for (i in 1:cfg_opt$horizon) {
      pos_n <-seq_number_name[names_compare == "faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
      v[pos_init - 1 + i] = cfg_opt$tariffs$not_aplicable

      pos_n <-seq_number_name[names_compare == "overtaked demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
      v[pos_init - 1 + i] = 2 * cfg_opt$tariffs$not_aplicable
    }
  }
  add.constraint(lp, v, "<=", cost_max * 1.01 )
  
  # #### minimum values contracted demand - limit problem minimization ####
  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
      v[pos_init - 1 + i] = 1

      add.constraint(lp, v, ">=", dem_cont_before[['pkd']][i])
    }

    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = 1

      add.constraint(lp, v, ">=", dem_cont_before[['opd']][i])
    }
  }

  #### bin faturable demand ####
  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = 1

    pos_n <-seq_number_name[names_compare == "bin faturable demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = cfg_opt$bigNumber


    if(cfg_opt$tariff_modality == "green") {
      deam_meas <- demand_measured[["not_aplicable"]][i]
    } else {
      deam_meas <- demand_measured[["pkd"]][i]
    }

    add.constraint(lp, v, ">=", deam_meas)
  }

  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = 1

      pos_n <-seq_number_name[names_compare == "bin faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = cfg_opt$bigNumber

      add.constraint(lp, v, ">=", demand_measured[["opd"]][i])
    }
  }

  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "faturable demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = 1

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = -1

    pos_n <-seq_number_name[names_compare == "bin faturable demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = -cfg_opt$bigNumber

    add.constraint(lp, v, "<=", 0)
  }

  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = 1

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = -1

      pos_n <-seq_number_name[names_compare == "bin faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = -cfg_opt$bigNumber

      add.constraint(lp, v, "<=", 0)
    }
  }

  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "faturable demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = 1

    pos_n <-seq_number_name[names_compare == "bin faturable demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = cfg_opt$bigNumber

    if(cfg_opt$tariff_modality == "green") {
      deam_meas <- demand_measured[["not_aplicable"]][i]
    } else {
      deam_meas <- demand_measured[["pkd"]][i]
    }

    add.constraint(lp, v, "<=", deam_meas + cfg_opt$bigNumber)
  }

  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = 1

      pos_n <-seq_number_name[names_compare == "bin faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = cfg_opt$bigNumber

      add.constraint(lp, v, "<=", demand_measured[["opd"]][i] + cfg_opt$bigNumber)
    }
  }

  #### Faturable demand - contracted demand ####
  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "faturable demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = 1

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init - 1 + i] = -1

    add.constraint(lp, v, ">=", 0)
  }

  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = 1

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = -1

      add.constraint(lp, v, ">=", 0)
    }
  }

  #### Faturable demand - demand measured ####
  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "faturable demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i] = 1

    if(cfg_opt$tariff_modality == "green") {
      deam_meas <- demand_measured[["not_aplicable"]][i]
    } else {
      deam_meas <- demand_measured[["pkd"]][i]
    }

    add.constraint(lp, v, ">=", deam_meas)
  }

  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "faturable demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i] = 1

      add.constraint(lp, v, ">=", demand_measured[["opd"]][i])
    }
  }

#### overtaking restriction ####
for (i in 1:cfg_opt$horizon) {
  v <- vct_origin

  pos_n <-seq_number_name[names_compare == "contracted demand"]
  pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
  v[pos_init -1 + i] = 1 + cfg_opt$overtaking_tolerance

  pos_n <-seq_number_name[names_compare == "overtaked demand"]
  pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
  v[pos_init -1 + i] = 1

  if(cfg_opt$tariff_modality == "green") {
    deam_meas <- demand_measured[["not_aplicable"]][i]
  } else {
    deam_meas <- demand_measured[["pkd"]][i]
  }
  add.constraint(lp, v, ">=", deam_meas)
}

if(cfg_opt$tariff_modality == "blue") {
  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
    v[pos_init -1 + i] = 1 + cfg_opt$overtaking_tolerance

    pos_n <-seq_number_name[names_compare == "overtaked demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
    v[pos_init -1 + i] = 1

    add.constraint(lp, v, ">=", demand_measured[["opd"]][i])
  }
}

for (i in 1:cfg_opt$horizon) {
  v <- vct_origin

  pos_n <-seq_number_name[names_compare == "contracted demand"]
  pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
  v[pos_init -1 + i] = 1

  pos_n <-seq_number_name[names_compare == "overtaked demand"]
  pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
  v[pos_init -1 + i] = 1

  pos_n <-seq_number_name[names_compare == "bin overtaking"]
  pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
  v[pos_init -1 + i] = -cfg_opt$bigNumber

  if(cfg_opt$tariff_modality == "green") {
    deam_meas <- demand_measured[["not_aplicable"]][i]
  } else {
    deam_meas <- demand_measured[["pkd"]][i]
  }

  add.constraint(lp, v, ">=",deam_meas - cfg_opt$bigNumber)
}

if(cfg_opt$tariff_modality == "blue") {
  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
    v[pos_init -1 + i] = 1

    pos_n <-seq_number_name[names_compare == "overtaked demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
    v[pos_init -1 + i] = 1

    pos_n <-seq_number_name[names_compare == "bin overtaking"]
    pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
    v[pos_init -1 + i] = -cfg_opt$bigNumber

    add.constraint(lp, v, ">=", demand_measured[["opd"]][i] - cfg_opt$bigNumber)
  }
}

for (i in 1:cfg_opt$horizon) {
  v <- vct_origin

  pos_n <-seq_number_name[names_compare == "overtaked demand"]
  pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
  v[pos_init -1 + i] = 1

  pos_n <-seq_number_name[names_compare == "bin overtaking"]
  pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
  v[pos_init -1 + i] = -cfg_opt$bigNumber

  add.constraint(lp, v, "<=", 0)
}

if(cfg_opt$tariff_modality == "blue") {
  for (i in 1:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "overtaked demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
    v[pos_init -1 + i] = 1

    pos_n <-seq_number_name[names_compare == "bin overtaking"]
    pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
    v[pos_init -1 + i] = -cfg_opt$bigNumber

    add.constraint(lp, v, "<=", 0)
  }
}

  # #### Reduction number ####
  for (i in 2:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i -1] = 1

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i] = -1

    pos_n <-seq_number_name[names_compare == "bin reduction"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i] = -cfg_opt$bigNumber

    add.constraint(lp, v, "<=", 0)
  }

  if(cfg_opt$tariff_modality == "blue") {
    for (i in 2:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i -1] = 1

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i] = -1

      pos_n <-seq_number_name[names_compare == "bin reduction"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i] = -cfg_opt$bigNumber

      add.constraint(lp, v, "<=", 0)
    }
  }

  v <- vct_origin
  for (i in 2:cfg_opt$horizon) {
    pos_n <-seq_number_name[names_compare == "bin reduction"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i] = 1
  }
  add.constraint(lp, v, "<=", cfg_opt$maximum.number.reduction)


  if(cfg_opt$tariff_modality == "blue") {
    v <- vct_origin
    for (i in 2:cfg_opt$horizon) {
      pos_n <-seq_number_name[names_compare == "bin reduction"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i] = 1
    }
    add.constraint(lp, v, "<=", cfg_opt$maximum.number.reduction)
  }
  #### Increased number ####
  for (i in 2:cfg_opt$horizon) {
    v <- vct_origin

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i -1] = 1

    pos_n <-seq_number_name[names_compare == "contracted demand"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i] = -1

    pos_n <-seq_number_name[names_compare == "bin increased"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i] = cfg_opt$bigNumber

    add.constraint(lp, v, ">=", 0)
  }

  if(cfg_opt$tariff_modality == "blue") {
    for (i in 2:cfg_opt$horizon) {
      v <- vct_origin

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i -1] = 1

      pos_n <-seq_number_name[names_compare == "contracted demand"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i] = -1

      pos_n <-seq_number_name[names_compare == "bin increased"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i] = cfg_opt$bigNumber

      add.constraint(lp, v, ">=", 0)
    }
  }

  v <- vct_origin
  for (i in 2:cfg_opt$horizon) {
    pos_n <-seq_number_name[names_compare == "bin increased"]
    pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
    v[pos_init -1 + i] = 1
  }
  add.constraint(lp, v, "<=", cfg_opt$maximum.number.increase)

  if(cfg_opt$tariff_modality == "blue") {
    v <- vct_origin
    for (i in 2:cfg_opt$horizon) {
      pos_n <-seq_number_name[names_compare == "bin increased"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init -1 + i] = 1
    }
    add.constraint(lp, v, "<=", cfg_opt$maximum.number.increase)
  }

  #bin pkd e bin opd devem ser zero
  if(cfg_opt$tariff_modality == "blue") {
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin
      
      pos_n <-seq_number_name[names_compare == "bin reduction"]
      pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
      v[pos_init - 1 + i] = 1
      
      pos_n <-seq_number_name[names_compare == "bin reduction"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = -1
      
      add.constraint(lp, v, "=", 0)
    }
    
    for (i in 1:cfg_opt$horizon) {
      v <- vct_origin
      
      pos_n <-seq_number_name[names_compare == "bin increased"]
      pos_init <- (cfg_opt$horizon * (pos_n[1] - 1)) + 1
      v[pos_init - 1 + i] = 1
      
      pos_n <-seq_number_name[names_compare == "bin increased"]
      pos_init <- (cfg_opt$horizon * (pos_n[2] - 1)) + 1
      v[pos_init - 1 + i] = -1
      
      add.constraint(lp, v, "=", 0)
    }
  }
  
  #### SOLVE #### 
  code_status <- solve(lp)
  
  list(code_status, lp)
  
  return(list("code_status" = code_status, "lp" = lp))
}