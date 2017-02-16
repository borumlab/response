#' Seizure calculate
#'
#' This function will perform the calculations of the values for percent seizure free (on baseline days
#' and therapy days), seizure response (daily and for every 30 days), percent seizure
#' free response, seizure score, seizure number response (daily and for every 30 days),
#' and seizure number score
#' @param x Seizure load table
#' @param n Number that signifies which column number corresponse with the SEIZURE_NUMBER_DAY column
#' @param l Number that signifies which column number corresponds with the SEIZURE_LOAD_DAY column
#' @param baseline Subset of load table corresponding with baseline days
#' @param therapy Subset of load table corresponding with therapy days
#' @param patient Four-letter patient initials
#' @param mrnumber Medical record number object
#' @param type DAY_TYPE column object
#' @param quality DATA_QUALITY_S column object
#' @param number SEIZURE_NUMBER_DAY/SEIZURE_LOAD_DAY data frame object
#' @return Seizure response/score calculations
#' @keywords seizure response

# Purpose - This script will perform the calculations of the values for percent seizure free (on baseline days
#           and therapy days), seizure response (daily and for every 30 days), percent seizure
#           free response, seizure score, seizure number response (daily and for every 30 days),
#           and seizure number score
# Parameters - x = seizure load table
#              n = number that signifies which column number corresponds with the SEIZURE_NUMBER_DAY column
#              l = number that signifies which column number corresponds with the SEIZURE_LOAD_DAY column
#              baseline = subset of load table corresponding with baseline days
#              therapy = subset of load table corresponding with therapy days
#              patient = four-letter patient initials
#              mrnumber = medical record number object
#              type = DAY_TYPE column object
#              quality = DATA_QUALITY_S column object
#              number = SEIZURE_NUMBER_DAY/SEIZURE_LOAD_DAY data frame object
seizure_calculate <- function(x,n,l,baseline,therapy,patient,mrnumber,type,quality,number) {

  
  base.free <- dim(baseline[baseline$SEIZURE_NUMBER_DAY==0,])[1]
  free.base <- (base.free/dim(baseline)[1])*100
  
  #for putting data frame together
  t <- data.frame(type)
  q <- data.frame(quality)
  q <- q[,-1]
  sn <- data.frame(number[,1])
  sl <- data.frame(number[,2])
  response <- data.frame(rep(NA,dim(therapy)[1]),rep(NA,dim(therapy)[1]),rep(NA,dim(therapy)[1]))
  
  if (free.base!=100) { 
    ## daily response calculation
    baseline.day <- subset(baseline,baseline[,l]!=0)#What is this line doing? Is it getting a subset of baseline where the Seizure Load does not equal 0?
    daily.response <- c((therapy[,l])/((sum(baseline.day$SEIZURE_LOAD_DAY)/length(baseline.day$SEIZURE_LOAD_DAY)))*100) #Candice's edit
    daily.response <- data.frame(therapy$DATE,daily.response)
    colnames(daily.response)[1] <- "DATE"
    colnames(daily.response)[2] <- "SEIZURE_RESPONSE"
    
    ## daily number response calculation
    response <- c(((therapy[,n]/((sum(baseline.day$SEIZURE_NUMBER_DAY)/length(baseline.day$SEIZURE_NUMBER_DAY)))))*100) #Candice's edit
    response <- data.frame(daily.response,response)
  }
  response <- data.frame(rep(mrnumber,dim(response)[1]),response)
  response[,2] <- therapy$DATE
  colnames(response)[1] <- "MRNUMBER"
  colnames(response)[2] <- "DATE"
  colnames(response)[3] <- "SEIZURE_RESPONSE"
  colnames(response)[4] <- "SEIZURE_NUMBER_RESPONSE"
  
  #Put the daily together in a datafram
  
  response <- data.frame(response[,1:2],t,q,sl,sn,response[,3:4])
  colnames(response)[c(3,4,5,6,7,8)] <- c("DAY_TYPE","DAY_QUALITY_S","SEIZURE_LOAD_DAY","SEIZURE_NUMBER_DAY",
                                          "SEIZURE_RESPONSE_DAY","SEIZURE_NUMBER_RESPONSE_DAY")
  
  #30 day Seizure Score
  therapy.30.days <- split(therapy[,l],ceiling(seq_along(therapy[,l])/30))
  therapy.number.30.days <- split(therapy[,n],ceiling(seq_along(therapy[,n])/30))
  
  #Candice's edit#
  y2 <- (sum(baseline$SEIZURE_LOAD_DAY)/length(baseline$SEIZURE_LOAD_DAY))*30
  SEIZURE_SCORE_30 <- as.numeric(lapply(therapy.30.days, function(x) (((sum(x)/30)*30)/y2)*100))
  
  # 30 day Seizure Number Score
  
  #Candice's edit#
  y3 <- (sum(baseline$SEIZURE_NUMBER_DAY)/length(baseline$SEIZURE_NUMBER_DAY))*30
  SEIZURE_NUMBER_SCORE_30 <- as.numeric(lapply(therapy.number.30.days, function(x) (((sum(x)/30)*30)/y3)*100))
  
  # for purposes of making complete data frame
  c <- c(1:dim(therapy)[1])
  r <- c[c[]/30 > 0 & c[]/30 <= 1]
  free.30.days <- data.frame(therapy$DATE[min(r)],
                             therapy$DATE[max(r)],
                             (length(therapy.30.days[[1]][therapy.30.days[[1]]==0])/(length(therapy.30.days[[1]])))*100)
  colnames(free.30.days)[1:2] <- c("FIRST_DATE","LAST_DATE")
  colnames(free.30.days)[3] <- "%_SEIZURE_FREE_DAYS"
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
  
  #what is this doing?
  results <- data.frame(free.30.days[,1],#do we need to get rid of this? what is the 1, I see [,3]
                        free.30.days[,2],#do we need to get rid of this? what is the 2, I see [,3]
                        cbind(#free.30.days[,3],#can delete?
                          rep(NA, dim(free.30.days)[1]),
                          rep(NA, dim(free.30.days)[1]),
                          rep(NA, dim(free.30.days)[1]),
                          rep(NA, dim(free.30.days)[1]),
                          #     period.response[,3],#can delete?
                          #    period.response[,4],#can delete?
                          #   percent.free.response[,3],#can delete?
                          #  overall.score[,3],#can change to y2
                          SEIZURE_SCORE_30,
                          # overall.number.score[,3]))#can change to y3
                          SEIZURE_NUMBER_SCORE_30))
  
  results <- data.frame(rep(mrnumber,dim(results)[1]),results)
  colnames(results)[1] <- "MRNUMBER"#this
  colnames(results)[2:3] <- c("FIRST_DATE","LAST_DATE")#this
  colnames(results)[4] <- "%_SEIZURE_FREE"
  colnames(results)[5] <- "SEIZURE_RESPONSE" #this
  colnames(results)[6] <- "SEIZURE_NUMBER_RESPONSE" #this
  colnames(results)[7] <- "% SEIZURE_FREE_RESPONSE"
  colnames(results)[8] <- "SEIZURE_SCORE_30" #this
  colnames(results)[9] <- "SEIZURE_NUMBER_SCORE_30" #this
  #need column for SEIZURE SCORE 7 DAYS, SEIZURE NUMBER SCORE 7 DAYS 
  
  #WHAT IS THIS?
  na <- rep(NA,dim(response)[1])
  outcome <- data.frame(response,a=na,b=na,c=na,d=na,e=na)
  colnames(outcome)[9] <- "%_SEIZURE_FREE_30_DAYS"
  colnames(outcome)[10] <- "SEIZURE_RESPONSE_30_DAYS"
  colnames(outcome)[11] <- "%_SEIZURE_FREE_RESPONSE_30_DAYS"
  colnames(outcome)[12] <- "SEIZURE_SCORE_30"
  colnames(outcome)[13] <- "SEIZURE_NUMBER_SCORE_30"
  
  outcome[outcome$DATE %in% results$LAST_DATE,c(9:13)] <- results[,c(4,5,7,8,9)]#what is this
  
  
  return(outcome)
}