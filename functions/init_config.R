library(ggplot2)
library(forecast)
library(urca)
library(astsa)
library(MLmetrics)
library(stringr)
library(lubridate)
library(imputeTS)
library(lpSolveAPI)

####Graphics config####
#Language activated options at graphs.
#Available language {pt-br - Portuguese Brazil, en - english (default)}
lang_activeted = "pt-br"

source("functions/graphicsmessages.R")
source("functions/graphicsConfigPattern.R")
source("functions/basicgraphics.R")
source("functions/forecast_macde.R")
source("functions/create_mape_forecast_all_combination.R")
source("functions/create_multiple_forecast_periods.R")
source("functions/macde.R")
source("functions/mount_table.R")
source("functions/load_ts.R")
source("functions/forecast_simulation.R")
source("functions/outliers_check.R")
source("functions/discount_generation.R")
source("functions/linear_programming_max.R")
source("functions/linear_programming_min.R")
source("functions/show_results_lp.R")
library("xlsx")