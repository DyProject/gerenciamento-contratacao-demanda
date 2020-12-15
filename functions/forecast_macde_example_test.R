#Example of application and test of function "forecast_macde"
rm(list=ls(all=T))
source("functions/init_config.R")
source("functions/load_ts.R")
source("functions/mount_table.R")
#Load and separate Time Series trainning and test
myts <- load_ts("dataframes/1419_en_fln_demand_energy.csv")

#No exemplo abaixo o resultado previsto deve ser igual. A função forecast_macde utiliza 
  #o parâmetro base = 5. Isso significa que no caso dos dados fornecidos 2014-2018,
  #o ano de 2019 será previsto utilizando 5 anos anteriores (2014 a 2018).
# Por outro lado a função mcd.double.mean realiza a previsão com base nos dados fornecidos
  #como os dados fornecidos são referente a 2014 até 2018, o resultado da previsão
  #para 2019 utilizará também 5 anos anteriores ao ano previsto.
# a diferença entre as duas funções é que a função mcd.double.mean calcula a previsão
  # baseada nos dados fornecidos. Apenas isso. Já a função forecast_macde, calcula
  # outros valores para a previsão como (fitted, x, upper, lower...) e utiliza a 
  # função mcd.double.mean para calcular o valor previsto (mean).
# O outro exemplo, utiliza base = 3. Assim, para os valores previstos serem iguais,
  #a função mcd.double.mean deve receber como parâmetro os dados de 2014 até 2018.
#OBS: os outros parâmetros também devem ser iguais: frequência, previsão decrescimento
  #e arredondamento.

ans <- forecast_macde(myts$training[["p.2014.2018"]][, "PKD"], frequency = 12, 
               growth_forecast = 1, round = F, base = 5)
mcd.dmean <- mcd.double.mean(myts$training[["p.2014.2018"]][, "PKD"], 
                            frequency = 12, growth_forecast = 1, round = F)
ans$mean == mcd.dmean # resposta exemplo 1

ans <- forecast_macde(myts$training[["p.2014.2018"]][, "PKD"], frequency = 12, 
                      growth_forecast = 1, round = F, base = 3)
mcd.dmean <- mcd.double.mean(myts$training[["p.2016.2018"]][, "PKD"], 
                             frequency = 12, growth_forecast = 1, round = F)
ans$mean == mcd.dmean # resposta exemplo 2


# Um dos valores retornados pela função forecast_macde é o fitted. Esse valor
#é o valor ajustado. Isso significa que para a previsão para 2019, utilizando 
#base = 3, passando os dados de 2014-2018, siginifica que o fitted terá os valores 
#previstos para: 2018 baseado em 2015 até 2017; 2017 baseado de 2014 até 2016. 
# Os anos de 2014 e 2015 não serão previstos, pois como foi selecionado base=3, 
#não há dados anteriores suficientes. 
#Assim para os demais anos o valor de fitted será NA.
# No exemplo abaixo a variável mcd.dmean_1 calcula a previsão para 2017 e mcd.dmean_2
#para 2018.
ans <- forecast_macde(myts$training[["p.2014.2018"]][, "PKD"], frequency = 12, 
                      growth_forecast = 1, round = F, base = 3)
mcd.dmean_1 <- mcd.double.mean(myts$training[["p.2014.2016"]][, "PKD"], 
                               frequency = 12, growth_forecast = 1, round = F)
mcd.dmean_2 <- mcd.double.mean(myts$training[["p.2015.2017"]][, "PKD"], 
                             frequency = 12, growth_forecast = 1, round = F)

ans$fitted
mcd.dmean_1
mcd.dmean_2

#A função create_mape_forecast_all_combination, cria todas as combinações possíveis, se parâmetro base=NULL.
# Ela é utilizada para criar as combinações tanto para o macde quanto para o naive.
#Para o macde, ao realizar a previsão, ela utiliza como base o tamanho de anos anteriores informados.
#No exemplo abaixo, quando a função passar internamente a combinação 2014-2015, será 
#utilizado o valor de base=2, para prever 2016.
allcomb <- create_mape_forecast_all_combination(type.fcast = "macde", myts, growth_forecast = 1)
a <- allcomb$p.2014.2015$PKD$mean
b <- mcd.double.mean(myts$training[["p.2014.2015"]][, "PKD"], 
                frequency = 12, growth_forecast = 1, round = F)
a == b

#Caso o valor da base seja difente de NULL, as combinações serão realizadas apenas para os dados
#que atendam o valor definido da base. Por exemplo, caso o valor base=4 seja passado como parâmetro, 
#a previsão será realizada apenas para os anos em que é possível utilizar 4 anos anteriores. Assim, serão
#realizadas as previsões para 2019 (2018,2017,2016,2015, 2014), 2019(2018,2017,2016,2015) 
#e 2018 (2017,2016,2015, 2014), isso se considerar os dados myts como 2014-2018.
#OBS: apesar de passar como base 5 anos (2018,2017,2016,2015, 2014), como base=4 foi definido, 
#o valor previsto considerará apenas 4 anos anteriores. Assim para as previsões, com base=4, 
#2019 (2018,2017,2016,2015, 2014) e 2019(2018,2017,2016,2015)  o valor previsto será igual.
allcomb <- create_mape_forecast_all_combination(type.fcast = "macde", myts, growth_forecast = 1, base=4)
a <- allcomb$p.2014.2018$PKD$mean
b <- allcomb$p.2015.2018$PKD$mean
c <- mcd.double.mean(myts$training[["p.2015.2018"]][, "PKD"], 
                     frequency = 12, growth_forecast = 1, round = F)
a==b
a==c

#Por exemplo, se for escolhido o valor da base=3. a função create_mape_forecast_all_combination, irá criar
#valores de previsão para todos os anos que permitam no mínimo 3 anos anteriores, como: 2019(2018,2017,2016),
#2019(2018,2017,2016,2015), etc. Porém, os dois exemplos de previsão para 2019 terão o mesmo resultado,
#já que a base=3.

#Quando base=3, dados de 2014-2018, a função create_mape_forecast_all_combination irá calcular
#várias previsões para o ano de 2019, considerando: 2019(2018,2017,2016,2015,2014), 2019(2018,2017,2016,2015)
#$2019(2018,2017,2016). Como base=3 as previsões para 2019 serão iguais.

#O mesmo vale para prever o ano de 2018(2017,2016,2015,2014) e 2018(2017,2016,2015)
allcomb <- create_mape_forecast_all_combination(type.fcast = "macde", myts, growth_forecast = 1, base=3)
a <- allcomb$p.2014.2018$PKD$mean
b <- allcomb$p.2015.2018$PKD$mean
c <- allcomb$p.2016.2018$PKD$mean
d <- allcomb$p.2014.2017$PKD$mean
e <- allcomb$p.2015.2017$PKD$mean
a==b
a==c
d==e

#A função mount_table retorna um dataframe com todas as informações das previsões 
#com a finalidade de comparação de valores.
mcd.base <- 3
growth_forecast = 0.97
table.resul <- mount_table(myts, typef = "macde", growth_forecast, base = mcd.base)

ans <- forecast_macde(myts$training[["p.2014.2018"]][, "PKD"], frequency = 12, 
                      growth_forecast = 0.97, round = F, base = 3)
mcd.subset <- subset(table.resul, period == "p.2016.2018")
mcd.subset <- subset(mcd.subset, k == "PKD")
a <- as.numeric(ans$mean) 
b <- as.numeric(as.character(mcd.subset$fcast))
round(a, 2) == round(b, 2)
