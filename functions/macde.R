calculateEnergyCost <- function(measured, tariff) {
  cost <- list()
  cost$monthly <- measured * tariff
  cost$yearly <- sum(cost$monthly)
  
  return(cost)
}

#Input:
  #measured - data series of demand measured along the months
  #reference - contracted demand
  #tariff - The tariff value can be a constant (the same tariff for all months) or a vector (when different value).
  #The default value is 1 considering the objective is find the best demand, i.e sweeping loop to find the
  # optimal value of demand.
  #limit - limit of tolerance for demand overtaked. The default value is 1.05 according RN414 ANEEL.
#Output:
  #cost - is a list with the follow values:
    #overtaked - the overtaked value in each month considering the tolerance limit. 
    #monthly - the cost monthly for each demands
    #yearly - is the sum of cost of all months
calculateDemandCost <- function(measured = list(), reference = NULL, tariff = 1, limit = 1.05) {
  cost <- list()
  cost$overtaked <- (measured > (reference * limit)) * (measured - reference)
  cost$monthly = (pmax(measured, reference) + 2 * cost$overtaked) * tariff
  cost$yearly = sum(cost$monthly)
  
  return(cost)
}

#This functions calculate de optimal demand considering or not the seasonality. In this version the function
#calculate a maximum of two demand values.
#Input:
#measured - data series of demand measured along the months
#clearance - clearance limit to choose the optminal demand. Its represent a increase of cost to 
#give a margin of demand variation in real measure.
#birt - is the initial value for a value of demand. birt means birthday. brit needs to be a value 
#between 1 and 12. for test all months birt = 1:12. To start in the first month, birt = 1. Value 
#can be a vector with values between 1 and 12.
#case - is the form that a vector measured goint to divid. case = 6, means the vector going to divid in 6x6 
#two equals part. case=8 means a a vector going to divid in 8x4 months. Default value is 0, means 
#the vector wont be divide, only a optminal demand value going to be calculate
#if  testMode = T, return all values of cost simulated
#Output
#ans - ans is a dataframe with the valu of cost and demand for two part and the total demand for the optiminal
#demand. If case equal zero, the cost and demand partA is 0.
#Example:
#To calculate two optminal demands, considering a division de 6x6, 7x5, 8x4, 9x3 of the vector.
#sazonalityExploratoryOptimization(demand, clearance = 1.01, birt = 1:12, case = c(6, 7, 8, 9))
#Green demand = c(392, 821, 787, 793, 653, 434, 287, 324, 566, 678, 729, 697)
#The best demand is 789 and 420.
sazonalityExploratoryOptimization <- function(measured, clearance = 1.01, birt = 1, weighting, case = 0, testMode = F) {
  i <- 1:12
  
  is_blue_modality <- !is.null(dim(measured))
  has_sazonality <- !(length(case) == 1 && case == 0)
  
  optimal <- data.frame()
  for (nCase in 1:length(case)) {
    for (nBirt in 1:length(birt)) {
      slc <- split_vector(birt[nBirt], case[nCase])
      serie_part_a <- data.frame()
      serie_part_b <- data.frame()
      
      if(has_sazonality){
        if(is_blue_modality) {
          serie_part_a <- rbind(serie_part_a, exploratoryOptimization(measured[, 1][slc], clearance))
          serie_part_a <- rbind(serie_part_a, exploratoryOptimization(measured[, 2][slc], clearance))
        } else {
          serie_part_a <- rbind(serie_part_a, exploratoryOptimization(measured[slc], clearance))
        }
      } else {
        serie_part_a <- rbind(serie_part_a, data.frame(cost = 0, demand = 0))
        if(is_blue_modality) {
          serie_part_a <- rbind(serie_part_a, data.frame(cost = 0, demand = 0))
        }
      }
      
      if(is_blue_modality) {
        serie_part_b <- rbind(serie_part_b, exploratoryOptimization(measured[, 1][!slc], clearance))
        serie_part_b <- rbind(serie_part_b, exploratoryOptimization(measured[, 2][!slc], clearance))
        
        sum_pkd <- (serie_part_b[, 'cost'][1] + serie_part_a[, 'cost'][1])
        sum_opd <- serie_part_a[, 'cost'][2] + serie_part_b[, 'cost'][2]
        total_cost <- sum_pkd * weighting + sum_opd
      } else {
        serie_part_b <- rbind(serie_part_b, exploratoryOptimization(measured[!slc], clearance))
        total_cost <- serie_part_a[, 'cost'] + serie_part_b[, 'cost']
      }
      
      df <- cbind(total_cost, "birt" = birt[nBirt], "case" = case[nCase], serie_part_a[, 'cost'],
                  serie_part_b[, 'cost'], serie_part_a[, 'demand'],
                  serie_part_b[, 'demand'])
      if(is_blue_modality) {
        df <- cbind(df, "s1_part_a_cost" = serie_part_a$cost[1], "s1_part_b_cost" = serie_part_b$cost[1],
                    "s1_part_a_demand" =serie_part_a$demand[1], "s1_part_b_demand" = serie_part_b$demand[1],
                    "s2_part_a_cost" = serie_part_a$cost[2], "s2_part_b_cost" = serie_part_b$cost[2],
                    "s2_part_a_demand" =serie_part_a$demand[2], "s2_part_b_demand" = serie_part_b$demand[2])  
      } else {
        df <- cbind(df, "s1_part_a_cost" = serie_part_a$cost, "s1_part_b_cost" = serie_part_b$cost,
                    "s1_part_a_demand" =serie_part_a$demand, "s1_part_b_demand" = serie_part_b$demand)
      }
      
      optimal <- rbind(optimal, df)
    }
  }
  
  if(testMode) {
    return(optimal)
  }
  
  z <- min(optimal$total_cost)
  ans <- optimal[optimal$total_cost == z,]
  return(ans)
}


split_vector <- function(nBirt, nCase) {
  i <- 1:12
  
  if((nBirt + nCase) > 12) {
    slc <- i >= nBirt | (nBirt + nCase) %% 13 - i >= 0
  }else {
    slc <-i >= nBirt & i < (nBirt + nCase)
  }
  
  return(slc)
}

#input
  #measured - data series of demand measured along the months
  #clearance - clearance limit to choose the optminal demand. Its represent a increase of cost to 
               #give a margin of demand variation in real measure.
#Output
  #optimal - the optmal demand value if testMode equal to F
  #relative values - the values considering tariff equal1 for all values between max and min measured data if testMode equal to V.
exploratoryOptimization <- function(measured, clearance = 1.01, testMode = F) {
  m <- list()
  m$x <- measured
  m$min <- min(m$x)
  m$max <- max(m$x)
  
  relativeValues <-data.frame()
  for (i in m$min:(m$max + 1)) {
    cost <- calculateDemandCost(m$x, i)
    df <- data.frame(cost$yearly, i)
    relativeValues <- rbind(relativeValues, df)
  }
  names(relativeValues) <- c("cost", "demand")
  minCost <- min(relativeValues$cost)
  minCostWithClearance <- clearance * minCost
  lowerValues <- relativeValues[relativeValues$cost <= minCostWithClearance,]
  optimal_val <- lowerValues[which.max(lowerValues$demand),]
  
  if(testMode) {
    return(relativeValues)
  }
  
  return(optimal_val)
}
