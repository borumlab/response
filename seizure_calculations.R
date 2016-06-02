seizure_calculations <- function() {
  
  print("Input the directory in which the seizure load file you would like to use is located")
  print("Example: C:/Folder_Name/")
  dir <- readline(prompt="Enter here: ")
  print("Input the name of the file in which the daily seizure loads can be found (leave off the .csv)")
  print("Example: filename")
  loads <- readline(prompt="Enter here: ")
  loads <- gsub(" ","",paste(loads,".csv"))
  x <- gsub("/ ","/",paste(dir,loads))
  x <- read.csv(x,header=TRUE)
  colnames(x)[11] <- "load"
  
  x <- subset(x,!is.na(x$load))
  
  ## Seizure free calculation
  x.base <- subset(x,x$day.type==1)
  x.therapy <- subset(x,x$day.type!=1)
  
  ## percent seizure free days during baseline
  base.free <- dim(x.base[x.base$number==0,])[1]
  seizure.free.base <- (base.free/dim(x.base)[1])*100
  cat("The percentage of baseline days with no seizures is:",seizure.free.base,"%",'\n')
  
  ## percent seizure free days during therapy
  x.therapy.30.days <- split(x.therapy$load,ceiling(seq_along(x.therapy$load)/30))
  c <- c(1:dim(x.therapy)[1])
  r <- c[c[]/30 > 0 & c[]/30 <= 1]
  seizure.free.30.days <- data.frame(x.therapy$date[min(r)],
                                           x.therapy$date[max(r)],
                                           (length(x.therapy.30.days[[1]][x.therapy.30.days[[1]]==0])/(length(x.therapy.30.days[[1]])))*100)
  colnames(seizure.free.30.days) <- c("First date","Last date","Percent seizure free days")
  if (ceiling(dim(x.therapy)[1]/30) >= 2) {
    for (i in 2:(ceiling(dim(x.therapy)[1]/30))) {
      therapy.free <- (length(x.therapy.30.days[[i]][x.therapy.30.days[[i]]==0])/(length(x.therapy.30.days[[i]])))*100
      r <- c[c[]/30 > i-1 & c[]/30 <= i]
      newrow <- data.frame(x.therapy$date[min(r)],x.therapy$date[max(r)],therapy.free)
      colnames(newrow) <- c("First date","Last date","Percent seizure free days")
      seizure.free.30.days <- data.frame(rbind(seizure.free.30.days,newrow))
      colnames(seizure.free.30.days) <- c("First date","Last date","Percent seizure free days")
    }
  }

  print("Seizure free percentage calculated.")
  
  x.base.seizure <- subset(x.base,x.base$load!=0)
  base.median <- median(x.base.seizure$load)
  base.number.median <- median(x.base.seizure[,10])
  
  ## daily seizure response calculation
  daily.response <- c((x.therapy$load/base.median)*100)
  daily.response <- data.frame(x.therapy$date,daily.response)
  colnames(daily.response) <- c("Date","Seizure Response")
  print("Daily seizure response calculated.")
  
  ## daily seizure number response calculation
  daily.number.response <- c((x.therapy[,10]/base.number.median)*100)
  daily.number.response <- data.frame(daily.response,daily.number.response)
  colnames(daily.number.response) <- c("Date","Seizure Response","Seizure Number Response")
  print("Daily seizure number response calculated")
  
  print("If you would like to save a csv file of the daily responses, type YES. Otherwise, type anything else")
  reply <- readline(prompt="Enter here: ")
  if (tolower(reply) == "yes") {
    print("Set the work directory in which you would like to save this file")
    wd <- readline(prompt="Enter here: ")
    setwd(wd)
    print("Give the name you would like to give to the file (leaving off .csv)")
    csv <- readline(prompt="Enter here: ")
    csv <- gsub(" ","",paste(csv,".csv"))
    write.csv(daily.number.response,file=csv,na="",row.names=FALSE)
  }
  
  ## 30 day seizure response calculation
  period.response <- c()
  for (i in 1:(ceiling(dim(x.therapy)[1]/30))) {
    period.median <- median(x.therapy.30.days[[i]][x.therapy.30.days[[i]]!=0])
    if (is.na(period.median) || period.median == 0) {
      response <- 0
    } else {
      response <- (period.median/base.median)*100
    }
    period.response <- c(period.response,response)
  }
  
  period.response <- data.frame(seizure.free.30.days[,1:2],period.response)
  colnames(period.response)[3] <- "Seizure response"
  
  print("30 day response calculated.")
  
  ## 30 day seizure number response calculation
  x.therapy.number.30.days <- split(x.therapy[,10],ceiling(seq_along(x.therapy[,10])/30))
  period.number.response <- c()
  for (i in 1:(ceiling(dim(x.therapy)[1]/30))) {
    period.number.median <- median(x.therapy.number.30.days[[i]][x.therapy.number.30.days[[i]]!=0])
    if (is.na(period.number.median) || period.number.median == 0) {
      response <- 0
    } else {
      response <- (period.number.median/base.number.median)*100
    }
    period.number.response <- c(period.number.response,response)
  }
  
  period.response <- data.frame(period.response,period.number.response)
  colnames(period.response)[4] <- "Seizure number response"
  
  print("30 day number response calculated.")
  
  ## percent seizure free response
  
  percent.seizure.free.response <- period.response
  percent.seizure.free.response[,3] <- 100 - (seizure.free.30.days[,3] - seizure.free.base)
  colnames(percent.seizure.free.response)[3] <- "Percent seizure free response"
  
  print("Seizure free percentage response calculated.")
  
  ## seizure score
  score.1 <- (((seizure.free.30.days[,3])/100) * ((percent.seizure.free.response[,3])/100)) * 100
  score.2 <- (((100-seizure.free.30.days[,3])/100) * ((period.response[,3])/100)) * 100
  score <- score.1 + score.2
  seizure.score <- percent.seizure.free.response
  seizure.score[,3] <- score
  colnames(seizure.score)[3] <- "Seizure score"
  
  print("Seizure score calculated.")
  
  ## seizure number score
  score.3 <- (((seizure.free.30.days[,3])/100) * ((percent.seizure.free.response[,3])/100)) * 100
  score.4 <- (((100-seizure.free.30.days[,3])/100) * ((period.response[,4])/100)) * 100
  score2 <- score.3 + score.4
  seizure.number.score <- percent.seizure.free.response
  seizure.number.score[,3] <- score2
  colnames(seizure.number.score)[3] <- "Seizure number score"
  
  print("Seizure number score calculated.")
  
  results <- data.frame(seizure.score[,1],
             seizure.score[,2],
             cbind(seizure.free.30.days[,3],
                   period.response[,3],
                   period.response[,4],
                   percent.seizure.free.response[,3],
                   seizure.score[,3],
                   seizure.number.score[,3]))
  colnames(results) <- c("First date","Last date","% Seizure Free","Seizure Response","Seizure Number Response",
                         "% Seizure Free Response","Seizure Score","Seizure Number Score")

  cat("If you would like to save a csv file of all of the 30 day results combined, type YES. Otherwise, type anything else")
  reply <- readline(prompt="Enter here: ")
  if (tolower(reply) == "yes") {
    print("Set the work directory in which you would like to save this file")
    wd <- readline(prompt="Enter here: ")
    setwd(wd)
    print("Give the name you would like to give to the file (leaving off .csv)")
    csv <- readline(prompt="Enter here: ")
    csv <- gsub(" ","",paste(csv,".csv"))
    write.csv(results,file=csv,na="",row.names=FALSE)
  }
  
  cat("Your results have been calculated",'\n','\n')
  return(results)
}