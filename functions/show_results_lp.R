show_results_lp <- function(cfg_opt, lp, dem_meas) {
  names_compare <- v.df[[cfg_opt$tariff_modality]]$name
  seq_number_name <- 1:length(names_compare)
  
  pos_n_faturable <-seq_number_name[names_compare == "faturable demand"]
  pos_n_overtaked <-seq_number_name[names_compare == "overtaked demand"]
  pos_n_contracted <-seq_number_name[names_compare == "contracted demand"]
  pos_n_bin_overtaking <-seq_number_name[names_compare == "bin overtaking"]
  pos_n_bin_reduction <-seq_number_name[names_compare == "bin reduction"]
  pos_n_bin_increased <-seq_number_name[names_compare == "bin increased"]
  pos_n_bin_tariff_post_bigger_minimum <-seq_number_name[names_compare == "bin tariff post bigger minimum"]
  
  print(paste("Code Status:", status_code_lp_solve(lp$code_status), sep = ""))
  
  if(cfg_opt$tariff_modality == "blue") {
    print("Custo:")
    print(get.objective(lp$lp))
    print("Variáveis de decisão:")
    print(get.variables(lp$lp))
    print("Restrições:")
    print(get.constraints(lp$lp))
    print("Demanda Medida PKD:")
    print(dem_meas$pkd)
    print("Demanda Medida OPD:")
    print(dem_meas$opd)
    
    faturable_pkd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_faturable - 1)) + 1)[1]:(pos_n_faturable*cfg_opt$horizon)[1]]
    faturable_opd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_faturable - 1)) + 1)[2]:(pos_n_faturable*cfg_opt$horizon)[2]]
    print("faturable_pkd")
    print(faturable_pkd)
    print("faturable_opd")
    print(faturable_opd)
    
    overtaked_pkd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_overtaked - 1)) + 1)[1]:(pos_n_overtaked*cfg_opt$horizon)[1]]
    overtaked_opd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_overtaked - 1)) + 1)[2]:(pos_n_overtaked*cfg_opt$horizon)[2]]
    print("overtaked_pkd")
    print(overtaked_pkd)
    print("overtaked_opd")
    print(overtaked_opd)
    
    contracted_pkd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_contracted - 1)) + 1)[1]:(pos_n_contracted*cfg_opt$horizon)[1]]
    contracted_opd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_contracted - 1)) + 1)[2]:(pos_n_contracted*cfg_opt$horizon)[2]]
    print("contracted_pkd")
    print(contracted_pkd)
    print("contracted_opd")
    print(contracted_opd)
    
    
    bin_overtaking_pkd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_overtaking - 1)) + 1)[1]:(pos_n_bin_overtaking*cfg_opt$horizon)[1]]
    bin_overtaking_opd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_overtaking - 1)) + 1)[2]:(pos_n_bin_overtaking*cfg_opt$horizon)[2]]
    print("bin_overtaking_pkd")
    print(bin_overtaking_pkd)
    print("bin_overtaking_opd")
    print(bin_overtaking_opd)
    
    bin_reduction_pkd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_reduction - 1)) + 1)[1]:(pos_n_bin_reduction*cfg_opt$horizon)[1]]
    bin_reduction_opd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_reduction - 1)) + 1)[2]:(pos_n_bin_reduction*cfg_opt$horizon)[2]]
    print("bin_reduction_pkd")
    print(bin_reduction_pkd)
    print("bin_reduction_opd")
    print(bin_reduction_opd)
    
    bin_increased_pkd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_increased - 1)) + 1)[1]:(pos_n_bin_increased*cfg_opt$horizon)[1]]
    bin_increased_opd <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_increased - 1)) + 1)[2]:(pos_n_bin_increased*cfg_opt$horizon)[2]]
    print("bin_increased_pkd")
    print(bin_increased_pkd)
    print("bin_increased_opd")
    print(bin_increased_opd)
    
    bin_tariff_post_bigger_minimum <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_tariff_post_bigger_minimum - 1)) + 1)[1]:(pos_n_bin_tariff_post_bigger_minimum*cfg_opt$horizon)[1]]
    print("bin_tariff_post_bigger_minimum")
    print(bin_tariff_post_bigger_minimum)
    
    print("________________________________________________________________")
    print("PKD")
    print("________________________________________________________________")
    tem_ultrapassagem <- dem_meas[["pkd"]] > 
      ((1 + cfg_opt$overtaking_tolerance) * contracted_pkd)
    
    ultrapassagem <- dem_meas[["pkd"]] - contracted_pkd
    
    faturado <- pmax(dem_meas[["pkd"]], contracted_pkd)
    
    custo_final <- cfg_opt$tariffs[["pkd"]] * faturado + 
      2 * cfg_opt$tariffs[["pkd"]] * ultrapassagem * tem_ultrapassagem
    
    resp_final_pkd <- rbind("medido_pkd" = dem_meas[["pkd"]], "contratada_pkd"=contracted_pkd,
                        "faturada_pkd"=faturado, tem_ultrapassagem, ultrapassagem, custo_final)
    print(paste("Custo Total PKD : ", sum(custo_final), sep = ""))
    print(resp_final_pkd)
    
    print("________________________________________________________________")
    print("OPD")
    print("________________________________________________________________")
    tem_ultrapassagem <- dem_meas[["opd"]] > 
      ((1 + cfg_opt$overtaking_tolerance) * contracted_opd)
    
    ultrapassagem <- dem_meas[["opd"]] - contracted_opd
    
    faturado <- pmax(dem_meas[["opd"]], contracted_opd)
    
    custo_final <- cfg_opt$tariffs[["opd"]] * faturado + 
      2 * cfg_opt$tariffs[["opd"]] * ultrapassagem * tem_ultrapassagem
    
    resp_final_opd <- rbind("medido_opd" = dem_meas[["opd"]], "contratada_opd"=contracted_opd,
                        "faturada_opd"=faturado, tem_ultrapassagem, ultrapassagem, custo_final)
    print(paste("Custo Total OPD: ", sum(custo_final), sep = ""))
    print(resp_final_opd)
    
  } else {
    print("Custo:")
    print(get.objective(lp$lp))
    print("Variáveis de decisão:")
    print(get.variables(lp$lp))
    print("Restrições:")
    print(get.constraints(lp$lp))
    print("Demanda Medida:")
    print(dem_meas$not_aplicable)
    print("faturable")
    faturable <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_faturable - 1)) + 1):(pos_n_faturable*cfg_opt$horizon)]
    print(faturable)
    
    print("overtaked")
    overtaked <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_overtaked - 1)) + 1):(pos_n_overtaked*cfg_opt$horizon)]
    print(overtaked)
    
    print("contracted")
    contracted <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_contracted - 1)) + 1):(pos_n_contracted*cfg_opt$horizon)]
    print(contracted)
    
    print("bin_overtaking")
    bin_overtaking <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_overtaking - 1)) + 1):(pos_n_bin_overtaking*cfg_opt$horizon)]
    print(bin_overtaking)
    
    print("bin_reduction")
    bin_reduction <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_reduction - 1)) + 1):(pos_n_bin_reduction*cfg_opt$horizon)]
    print(bin_reduction)
    
    print("bin_increased")
    bin_increased <- get.variables(lp$lp)[((cfg_opt$horizon * (pos_n_bin_increased - 1)) + 1):(pos_n_bin_increased*cfg_opt$horizon)]
    print(bin_increased)
    
    print("________________________________________________________________")
    print("VERDE")
    print("________________________________________________________________")
    tem_ultrapassagem <- dem_meas[["not_aplicable"]] > 
      ((1 + cfg_opt$overtaking_tolerance) * contracted)
    
    ultrapassagem <- dem_meas[["not_aplicable"]] - contracted
    
    faturado <- pmax(dem_meas[["not_aplicable"]], contracted)
    
    custo_final <- cfg_opt$tariffs[["not_aplicable"]] * faturado + 
      2 * cfg_opt$tariffs[["not_aplicable"]] * ultrapassagem * tem_ultrapassagem
    
    resp_final <- rbind("medido_verde" = dem_meas[["not_aplicable"]], "contratada_verde"=contracted,
                        "faturada_verde"=faturado, tem_ultrapassagem, ultrapassagem, custo_final)
    print(paste("Custo Total VERDE: ", sum(custo_final), sep = ""))
    print(resp_final)
  }
}