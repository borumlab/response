#' Med calculate
#'
#' This function will perform the calculations of the values for percent med free (on baseline days
#' and therapy days), med response (daily and for every 30 days), percent med
#' free response, med score, med number response (daily and for every 30 days),
#' and med number score
#' @param x Med load table
#' @param n Number that signifies which column number corresponse with the MED_NUMBER_DAY column
#' @param l Number that signifies which column number corresponds with the MED_LOAD_DAY column
#' @param baseline Subset of load table corresponding with baseline days
#' @param therapy Subset of load table corresponding with therapy days
#' @param patient Four-letter patient initials
#' @param mrnumber Medical record number object
#' @param type DAY_TYPE column object
#' @param number MED_NUMBER_DAY/MED_LOAD_DAY data frame object
#' @return med response/score calculations
#' @keywords med response

# Purpose - This script will perform the calculations of the values for percent med free (on baseline days
#           and therapy days), med response (daily and for every 30 days), percent med
#           free response, med score, med number response (daily and for every 30 days),
#           and med number score
# Parameters - x = med load table
#              n = number that signifies which column number corresponds with the MED_NUMBER_DAY column
#              l = number that signifies which column number corresponds with the MED_LOAD_DAY column
#              baseline = subset of load table corresponding with baseline days
#              therapy = subset of load table corresponding with therapy days
#              patient = four-letter patient initials
#              mrnumber = medical record number object
#              type = DAY_TYPE column object
#              number = MED_NUMBER_DAY/MED_LOAD_DAY data frame object
med_calculate <- function(x,n,l,baseline,therapy,patient,mrnumber,type,number) {

  ## percent free days during baseline
  base.free <- dim(baseline[baseline[,l]==0,])[1]
  free.base <- (base.free/dim(baseline)[1])*100
  
  
  #For putting data frame together
  t <- data.frame(type)
  ml <- data.frame(number[,1])
  mn <- data.frame(number[,2])
  
  
  
  ## daily response calculation
  baseline.day <- subset(baseline,baseline[,l]!=0)
  daily.response <- c((therapy[,l])/((sum(baseline.day$MED_LOAD_DAY)/length(baseline.day$MED_LOAD_DAY)))*100)
  daily.response <- data.frame(therapy$DATE,daily.response)
  colnames(daily.response)[1] <- "DATE"
  colnames(daily.response)[2] <- "MED_RESPONSE"
  
  
  #daily number response calculation
  response <- c(((therapy[,n]/((sum(baseline.day$MED_NUMBER_DAY)/length(baseline.day$MED_NUMBER_DAY)))))*100)
  response <- data.frame(daily.response,response)
  response <- data.frame(rep(mrnumber,dim(response)[1]),response)
  response <- data.frame(response[,1:2],t,ml,mn,response[,3:4])
  colnames(response)[1] <- "MRNUMBER"
  colnames(response)[2] <- "DATE"
  colnames(response)[3] <- "DAY_TYPE"
  colnames(response)[4] <- "MED_LOAD_DAY"
  colnames(response)[5] <- "MED_NUMBER_DAY"
  colnames(response)[6] <- "MED_RESPONSE_DAY"
  colnames(response)[7] <- "MED_NUMBER_RESPONSE_DAY"
  
  
  #30 day Med Score
  therapy.30.days <- split(therapy[,l],ceiling(seq_along(therapy[,l])/30))
  therapy.number.30.days <- split(therapy[,n],ceiling(seq_along(therapy[,n])/30))
  
  #Candice's edit#
  y2 <- (sum(baseline$MED_LOAD_DAY)/length(baseline$MED_LOAD_DAY))*30
  MED_SCORE_30 <- as.numeric(lapply(therapy.30.days, function(x) (((sum(x)/30)*30)/y2)*100))
  
  # 30 day Med Number Score
  
  #Candice's edit#
  y3 <- (sum(baseline$MED_NUMBER_DAY)/length(baseline$MED_NUMBER_DAY))*30
  MED_NUMBER_SCORE_30 <- as.numeric(lapply(therapy.number.30.days, function(x) (((sum(x)/30)*30)/y3)*100))
  
  
  #For purposes of making complete data frame
  c <- c(1:dim(therapy)[1])
  r <- c[c[]/30 > 0 & c[]/30 <= 1]
  free.30.days <- data.frame(therapy$DATE[min(r)],
                             therapy$DATE[max(r)],
                             (length(therapy.30.days[[1]][therapy.30.days[[1]]==0])/(length(therapy.30.days[[1]])))*100)
  colnames(free.30.days)[1:2] <- c("FIRST_DATE","LAST_DATE")
  colnames(free.30.days)[3] <- "%_MED_FREE_DAYS"
  if (ceiling(dim(therapy)[1]/30) >= 2) {
    for (i in 2:(ceiling(dim(therapy)[1]/30))) {
      therapy.free <- (length(therapy.30.days[[i]][therapy.30.days[[i]]==0])/(length(therapy.30.days[[i]])))*100
      r <- c[c[]/30 > i-1 & c[]/30 <= i]
      newrow <- data.frame(therapy$DATE[min(r)],therapy$DATE[max(r)],therapy.free)
      colnames(newrow) <- colnames(free.30.days)
      free.30.days <- data.frame(rbind(free.30.days,newrow))
      colnames(free.30.days) <- colnames(newrow)
    }
  }
  
  
  results <- data.frame(free.30.days[,1],
                        free.30.days[,2],
                        cbind(#free.30.days[,3],
                          rep(NA, dim(free.30.days)[1]),
                          rep(NA, dim(free.30.days)[1]),
                          rep(NA, dim(free.30.days)[1]),
                          rep(NA, dim(free.30.days)[1]),
                          #period.response[,3],
                          #period.response[,4],
                          #percent.free.response[,3],
                          #overall.score[,3],
                          MED_SCORE_30,
                          #overall.number.score[,3]))
                          MED_NUMBER_SCORE_30))
  
  results <- data.frame(rep(mrnumber,dim(results)[1]),results)
  colnames(results)[1] <- "MRNUMBER"
  colnames(results)[2:3] <- c("FIRST_DATE","LAST_DATE")
  colnames(results)[4] <- "%_MED_FREE"
  colnames(results)[5] <- "MED_RESPONSE"
  colnames(results)[6] <- "MED_NUMBER_RESPONSE"
  colnames(results)[7] <- "% MED_FREE_RESPONSE"
  colnames(results)[8] <- "MED_SCORE_30"
  colnames(results)[9] <- "MED_NUMBER_SCORE_30"
  
  na <- rep(NA,dim(response)[1])
  outcome <- data.frame(response,a=na,b=na,c=na,d=na,e=na)
  colnames(outcome)[8] <- "%_MED_FREE_30_DAYS"
  colnames(outcome)[9] <- "MED_RESPONSE_30_DAYS"
  colnames(outcome)[10] <- "%_MED_FREE_RESPONSE_30_DAYS"
  colnames(outcome)[11] <- "MED_SCORE_30"
  colnames(outcome)[12] <- "MED_NUMBER_SCORE_30" 
  
  outcome[outcome$DATE %in% results$LAST_DATE,c(8:12)] <- results[,c(4,5,7,8,9)]
  return(outcome)
}






