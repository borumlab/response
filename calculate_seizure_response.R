seizure_calculate <- function(x,n,l,baseline,therapy,patient,mrnumber,type,quality,number) {
  ## percent free days during baseline
  base.free <- dim(baseline[baseline$SEIZURE_NUMBER_DAY==0,])[1]
  free.base <- (base.free/dim(baseline)[1])*100
  print(paste("The percentage of baseline days with no seizures is:",free.base,"%"))
  
  ## percent free days during therapy
  therapy.30.days <- split(therapy[,l],ceiling(seq_along(therapy[,l])/30))
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
  
  baseline <- subset(baseline,baseline[,l]!=0)
  base.median <- median(baseline[,l])
  base.number.median <- median(baseline[,n])
  
  ## daily response calculation
  daily.response <- c((therapy[,l]/base.median)*100)
  daily.response <- data.frame(therapy$DATE,daily.response)
  colnames(daily.response)[1] <- "DATE"
  colnames(daily.response)[2] <- "SEIZURE_RESPONSE"
  
  ## daily number response calculation
  response <- c((therapy[,n]/base.number.median)*100)
  response <- data.frame(daily.response,response)
  
  response <- data.frame(rep(mrnumber,dim(response)[1]),response)
  colnames(response)[1] <- "MRNUMBER"
  colnames(response)[2] <- "DATE"
  colnames(response)[3] <- "SEIZURE_RESPONSE"
  colnames(response)[4] <- "SEIZURE_NUMBER_RESPONSE"
  
  t <- data.frame(type)
  q <- data.frame(quality)
  q <- q[,-1]
  sn <- data.frame(number[,1])
  sl <- data.frame(number[,2])
    
  response <- data.frame(response[,1:2],t,q,sl,sn,response[,3:4])
  colnames(response)[c(3,4,5,6,7,8)] <- c("DAY_TYPE","DAY_QUALITY_S","SEIZURE_LOAD_DAY","SEIZURE_NUMBER_DAY",
                                                         "SEIZURE_RESPONSE_DAY","SEIZURE_NUMBER_RESPONSE_DAY")
  

  ## 30 day response calculation
  period.response <- c()
  for (i in 1:(ceiling(dim(therapy)[1]/30))) {
    r <- c()
    period.median <- median(therapy.30.days[[i]][therapy.30.days[[i]]!=0])
    if (is.na(period.median) || period.median == 0) {
      r <- 0
    } else {
      r <- (period.median/base.median)*100
    }
    period.response <- c(period.response,r)
  }
  
  period.response <- data.frame(free.30.days[,1:2],period.response)
  colnames(period.response)[3] <- "SEIZURE_RESPONSE"
  
  ## 30 day number response calculation
  therapy.number.30.days <- split(therapy[,n],ceiling(seq_along(therapy[,n])/30))
  period.number.response <- c()
  for (i in 1:(ceiling(dim(therapy)[1]/30))) {
    period.number.median <- median(therapy.number.30.days[[i]][therapy.number.30.days[[i]]!=0])
    if (is.na(period.number.median) || period.number.median == 0) {
      r <- 0
    } else {
      r <- (period.number.median/base.number.median)*100
    }
    period.number.response <- c(period.number.response,r)
  }
  
  period.response <- data.frame(period.response,period.number.response)
  colnames(period.response)[4] <- "SEIZURE_NUMBER_RESPONSE"
  
  ## percent free response
  
  percent.free.response <- period.response
  percent.free.response[,3] <- 100 - (free.30.days[,3] - free.base)
  colnames(percent.free.response)[3] <- "%_SEIZURE_FREE_RESPONSE"
  
  ## score
  score.1 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
  score.2 <- (((100-free.30.days[,3])/100) * ((period.response[,3])/100)) * 100
  score <- score.1 + score.2
  overall.score <- percent.free.response
  overall.score[,3] <- score
  colnames(overall.score)[3] <- "SEIZURE_SCORE_30_DAYS"
  
  ## number score
  score.3 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
  score.4 <- (((100-free.30.days[,3])/100) * ((period.response[,4])/100)) * 100
  score2 <- score.3 + score.4
  overall.number.score <- percent.free.response
  overall.number.score[,3] <- score2
  colnames(overall.number.score)[3] <- "SEIZURE_NUMBER_SCORE"
  
  results <- data.frame(overall.score[,1],
                        overall.score[,2],
                        cbind(free.30.days[,3],
                              period.response[,3],
                              period.response[,4],
                              percent.free.response[,3],
                              overall.score[,3],
                              overall.number.score[,3]))
  
  results <- data.frame(rep(mrnumber,dim(results)[1]),results)
  colnames(results)[1] <- "MRNUMBER"
  colnames(results)[2:3] <- c("FIRST_DATE","LAST_DATE")
  colnames(results)[4] <- "%_SEIZURE_FREE"
  colnames(results)[5] <- "SEIZURE_RESPONSE"
  colnames(results)[6] <- "SEIZURE_NUMBER_RESPONSE"
  colnames(results)[7] <- "% SEIZURE_FREE_RESPONSE"
  colnames(results)[8] <- "SEIZURE_SCORE"
  colnames(results)[9] <- "SEIZURE_NUMBER_SCORE"
  
  na <- rep(NA,dim(response)[1])
  outcome <- data.frame(response,a=na,b=na,c=na,d=na)
  colnames(outcome)[9] <- "%_SEIZURE_FREE_30_DAYS"
  colnames(outcome)[10] <- "SEIZURE_RESPONSE_30_DAYS"
  colnames(outcome)[11] <- "%_SEIZURE_FREE_RESPONSE_30_DAYS"
  colnames(outcome)[12] <- "SEIZURE_SCORE_30_DAYS"
  
  outcome[outcome$DATE %in% results$LAST_DATE,c(9:12)] <- results[,c(4,5,7,8)]
  return(outcome)
}

calculate_seizure_response <- function(patient,data,x,sourcedata,ranking) {
  
  x <- subset(x,!is.na(x$SEIZURE_NUMBER_DAY))
  x <- x[,colnames(x)=="MRNUMBER" | colnames(x)=="DATE" | colnames(x)=="DAY_TYPE" | colnames(x)=="SEIZURE_SEVERITY" | colnames(x)=="SEIZURE_LENGTH" | colnames(x)=="SEIZURE_TYPE" | colnames(x)=="SEIZURE_VARIABLE" | colnames(x)=="SEIZURE_CLUSTER" | colnames(x)=="SEIZURE_NUMBER" | colnames(x)=="SEIZURE_LOAD" | colnames(x)=="SEIZURE_NUMBER_DAY" | colnames(x)=="SEIZURE_LOAD_DAY"]
  x$DATE <- as.Date(x$DATE)
  
  seizure.baseline <- subset(x,x$DAY_TYPE==1)
  seizure.therapy <- subset(x,x$DAY_TYPE!=1)
  
  mrnumber <- unique(x$MRNUMBER)
  daytype <- seizure.therapy$DAY_TYPE
  seizurequality <- unique(data[as.Date(data$DATE) %in% as.Date(seizure.therapy$DATE),colnames(data)=="DATE" | colnames(data)=="S_DATA_QUALITY"])
  seizurenumber <- x[!is.na(x$SEIZURE_NUMBER_DAY) & x$DATE %in% seizure.therapy$DATE,colnames(x)=="SEIZURE_NUMBER_DAY" | colnames(x)=="SEIZURE_LOAD_DAY"]
  x <- x[,-(colnames(x)=="MRNUMBER")]
  
  results <- seizure_calculate(x,which(colnames(seizure.therapy)=="SEIZURE_NUMBER_DAY"),which(colnames(seizure.therapy)=="SEIZURE_LOAD_DAY"),seizure.baseline,seizure.therapy,patient,mrnumber,daytype,seizurequality,seizurenumber)
  print("Outcome has been calculated")
  
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  results$DATE <- as.Date(results$DATE)
  string <- paste(patient,"Daily Seizure Response")
  len <- ceiling((results$DATE[length(results$DATE)] - results$DATE[1])/4)
  base <- ggplot(results,aes(results$DATE,SEIZURE_RESPONSE_DAY)) + geom_point(colour="#FF8C00",size=0.5) + geom_line(aes(DATE,SEIZURE_RESPONSE_DAY),colour="#FF8C00",size=0.1) 
  base <- base + geom_hline(yintercept=100) 
  base <- base + scale_x_date(date_breaks=paste(len,"days"),date_labels="%m/%d/%Y") + ggtitle(string) + xlab("Date on PKT") + ylab("Daily Seizure Response")
  base <- base + theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  base <- base + theme(axis.line.x=element_line(size=0.5,colour="black"),axis.line.y=element_line(size=0.5,colour="black"))
  file <- gsub(" ","",paste(patient,"_SEIZURE_DAILY_GRAPH.jpeg"))
  ggsave(base,filename=file,width=6,height=4)
  
  print(paste(gsub(" ","",paste(patient,"_SEIZURE_DAILY_GRAPH.jpeg")),"created and saved in the patient folder"))
  
  print(paste("Saving seizure clinical outcome table as",gsub(" ","",paste(patient,"_SEIZURE_DATA_CLINICAL.xlsx")),"in directory",getwd()))
  xlsx <- "SEIZURE_DATA_CLINICAL.xlsx"
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx2(results,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  print("Would you like to upload seizure research data into a MySQL database?")
  print("Type 'YES' to do so, else type 'NO'")
  rl <- " "
  while (tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if (tolower(rl)=="yes") {
    uploadtodatabase(results,"SEIZURE",sourcedata,ranking)
  }
}