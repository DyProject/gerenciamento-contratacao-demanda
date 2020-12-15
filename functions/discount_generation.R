discount_generation <- function(dts, freq = 12, energies_values_generation, demands_values_generation, data_instal_solar) {
  for (i in 1:freq) {
    dt_slc_numb <- calcular_sequencia_mes(dts, mes = i, data_instal_solar)
    
    dts[dt_slc_numb, "ope"] <- dts[dt_slc_numb, "ope"] - energies_values_generation[i]
    dts[dt_slc_numb, "opd"] <- dts[dt_slc_numb, "opd"] - demands_values_generation[i]
  }
  
  return(dts)
}