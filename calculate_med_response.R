med_calculate <- function(x,n,l,baseline,therapy,patient,mrnumber,type,quality,number) {
  ## percent free days during baseline
  base.free <- dim(baseline[baseline[,l]==0,])[1]
  free.base <- (base.free/dim(baseline)[1])*100
  print(paste("The percentage of baseline days with no meds is:",free.base,"%"))
  
  ## percent free days during therapy
  therapy.30.days <- split(therapy[,l],ceiling(seq_along(therapy[,l])/30))
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
  
  baseline <- subset(baseline,baseline[,l]!=0)
  base.median <- median(baseline[,l])
  base.number.median <- median(baseline[,n])
  
  ## daily response calculation
  daily.response <- c((therapy[,l]/base.median)*100)
  daily.response <- data.frame(therapy$DATE,daily.response)
  colnames(daily.response)[1] <- "DATE"
  colnames(daily.response)[2] <- "MED_RESPONSE"
  
  ## daily number response calculation
  response <- c((therapy[,n]/base.number.median)*100)
  response <- data.frame(daily.response,response)
  
  t <- data.frame(type)
  ml <- data.frame(number[,1])
  mn <- data.frame(number[,2])
  response <- data.frame(rep(mrnumber,dim(response)[1]),response)
  response <- data.frame(response[,1:2],t,ml,mn,response[,3:4])
  colnames(response)[1] <- "MRNUMBER"
  colnames(response)[2] <- "DATE"
  colnames(response)[3] <- "DAY_TYPE"
  colnames(response)[4] <- "MED_LOAD_DAY"
  colnames(response)[5] <- "MED_NUMBER_DAY"
  colnames(response)[6] <- "MED_RESPONSE_DAY"
  colnames(response)[7] <- "MED_NUMBER_RESPONSE_DAY"
  
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
  colnames(period.response)[3] <- "MED_RESPONSE"
  
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
  colnames(period.response)[4] <- "MED_NUMBER_RESPONSE"
  
  ## percent free response
  
  percent.free.response <- period.response
  percent.free.response[,3] <- 100 - (free.30.days[,3] - free.base)
  colnames(percent.free.response)[3] <- "%_MED_FREE_RESPONSE"
  
  ## score
  score.1 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
  score.2 <- (((100-free.30.days[,3])/100) * ((period.response[,3])/100)) * 100
  score <- score.1 + score.2
  overall.score <- percent.free.response
  overall.score[,3] <- score
  colnames(overall.score)[3] <- "MED_SCORE"
  
  ## number score
  score.3 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
  score.4 <- (((100-free.30.days[,3])/100) * ((period.response[,4])/100)) * 100
  score2 <- score.3 + score.4
  overall.number.score <- percent.free.response
  overall.number.score[,3] <- score2
  colnames(overall.number.score)[3] <- "MED_NUMBER_SCORE"
  
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
  colnames(results)[4] <- "%_MED_FREE"
  colnames(results)[5] <- "MED_RESPONSE"
  colnames(results)[6] <- "MED_NUMBER_RESPONSE"
  colnames(results)[7] <- "% MED_FREE_RESPONSE"
  colnames(results)[8] <- "MED_SCORE"
  colnames(results)[9] <- "MED_NUMBER_SCORE"
  
  na <- rep(NA,dim(response)[1])
  outcome <- data.frame(response,a=na,b=na,c=na,d=na)
  colnames(outcome)[8] <- "%_MED_FREE_30_DAYS"
  colnames(outcome)[9] <- "MED_RESPONSE_30_DAYS"
  colnames(outcome)[10] <- "%_MED_FREE_RESPONSE_30_DAYS"
  colnames(outcome)[11] <- "MED_SCORE_30_DAYS"
  
  outcome[outcome$DATE %in% results$LAST_DATE,c(8:11)] <- results[,c(4,5,7,8)]
  return(outcome)
}

calculate_med_response <- function(patient,data,y,calculated,ranking) {
  
  string <- gsub(" ","",paste(patient,"_","SEIZURE_DATA_SOURCE.xlsx"))
  datecheck <- readWorksheetFromFile(string,sheet=1)
  datecheck <- subset(datecheck,!is.na(datecheck$DATE))
  datecheck$DATE <- as.Date(datecheck$DATE,format="%m/%d/%Y")
  
  y <- subset(y,!is.na(y$MED_LOAD_DAY))
  y$DATE <- as.Date(y$DATE,format="%m/%d/%Y")
  data <- subset(data,!is.na(data$DATE))
  data$DATE <- as.Date(data$DATE,format="%m/%d/%Y")
  
  if ((max(unique(datecheck$DATE))) > (max(unique(y$DATE)))) {
    temp <- data.frame(y[y$DATE==max(unique(y$DATE)),])
    for (i in 1:((max(unique(datecheck$DATE))) - (max(unique(y$DATE))) - 1)) {
      if (((max(unique(datecheck$DATE))) - (max(unique(y$DATE))) - 1) == 0) {
        break
      }
      t <- data.frame(y[y$DATE==max(unique(y$DATE)),])
      t$DATE <- max(unique(y$DATE))+i
      temp <- rbind.data.frame(temp,t)
    }
  } else if ((max(unique(datecheck$DATE))) < (max(unique(y$DATE)))) {
    temp <- y[y$DATE <= max(unique(datecheck$DATE)),]
    rm(y)
    y <- temp
  }
  
  med.baseline <- subset(y,y$DAY_TYPE==1)
  med.therapy <- subset(y,y$DAY_TYPE!=1)
  
  daytype <- med.therapy$DAY_TYPE
  mednumber <- y[!is.na(y$MED_NUMBER_DAY) & (y$DATE %in% med.therapy$DATE),colnames(y)=="MED_NUMBER_DAY" | colnames(y)=="MED_LOAD_DAY"]

  mrnumber <- unique(data$MRNUMBER)
  y <- y[,-(colnames(y)=="MRNUMBER")]
  results <- med_calculate(y,which(colnames(med.therapy)=="MED_NUMBER_DAY"),which(colnames(med.therapy)=="MED_LOAD_DAY"),med.baseline,med.therapy,patient,mrnumber,daytype,NA,mednumber)
  print("Outcome has been calculated")
  
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  results$DATE <- as.Date(results$DATE)
  string <- paste(patient,"Daily Med Response")
  len <- ceiling((results$DATE[length(results$DATE)] - results$DATE[1])/4)
  base <- ggplot(results,aes(results$DATE,MED_RESPONSE_DAY)) + geom_point(colour="#FF8C00",size=0.5) + geom_line(aes(DATE,MED_RESPONSE_DAY),colour="#FF8C00",size=0.1) 
  base <- base + geom_hline(yintercept=100) 
  base <- base + scale_x_date(date_breaks=paste(len,"days"),date_labels="%m/%d/%Y") + ggtitle(string) + xlab("Date on PKT") + ylab("Daily Med Response")
  base <- base + theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  base <- base + theme(axis.line.x=element_line(size=0.5,colour="black"),axis.line.y=element_line(size=0.5,colour="black"))
  file <- gsub(" ","",paste(patient,"_MED_DAILY_GRAPH.jpeg"))
  ggsave(base,filename=file,width=6,height=4)
  
  print(paste(gsub(" ","",paste(patient,"_MED_DAILY_GRAPH.jpeg")),"created and saved in the patient folder"))
  
  print(paste("Saving med clinical outcome table as",gsub(" ","",paste(patient,"_MED_DATA_CLINICAL.xlsx")),"in directory",getwd()))
  xlsx <- "MED_DATA_CLINICAL.xlsx"
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx2(results,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  print("Would you like to upload med research data into a MySQL database?")
  print("Type 'YES' to do so, else type 'NO'")
  rl <- " "
  while (tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if (tolower(rl)=="yes") {
    uploadtodatabase(results,"MED",calculated,NA)
  }
}