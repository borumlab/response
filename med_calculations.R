med_calculations <- function() {
  
  print("Input the directory in which the med load file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir1 <- readline(prompt="Enter here: ")
  print("Input the name of the file in which the daily med loads can be found (leave off the .csv)")
  print("Example: filename")
  x <- readline(prompt="Enter here: ")
  
  x <- gsub(" .csv",".csv",paste(x,".csv"))
  x <- gsub("/ ","/",paste(dir1,x))
  x <- read.csv(x,header=TRUE)
  
  print("Input the directory in which the patient raw data file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir2 <- readline(prompt="Enter here: ")
  print("Input the name of the patient raw data file you would like to use (leave off the .csv)")
  print("Example: filename")
  data <- readline(prompt="Enter here: ")
  
  data <- gsub(" .csv",".csv",paste(data,".csv"))
  data <- gsub("/ ","/",paste(dir2,data))
  data <- read.csv(data,header=TRUE)
  
  colnames(x)[9] <- "load"
  colnames(x)[10] <- "number"
  x <- subset(x,!is.na(x$load))
  x[,1] <- as.Date(x[,1],format="%m/%d/%Y")
  data <- subset(data,!is.na(data[,1]))
  data[,2] <- as.Date(data[,2],format="%m/%d/%Y")
  
  ## Med free calculation
  baseline <- unique(data[data[,3]==1,2,2])
  therapy <- unique(data[data[,3]==2,2])
  x.base <- x[x$date<therapy[1],]
  x.therapy <- x[x$date>=therapy[1],]
  
  ## percent med free days during baseline
  base.free <- dim(x.base[x.base$load==0,])[1]
  med.free.base <- (base.free/dim(x.base)[1])*100
  cat("The percentage of baseline days with no meds is:",med.free.base,"%",'\n')
  
  ## percent med free days during therapy
  x.therapy.30.days <- split(x.therapy$load,ceiling(seq_along(x.therapy$load)/30))
  c <- c(1:dim(x.therapy)[1])
  r <- c[c[]/30 > 0 & c[]/30 <= 1]
  med.free.30.days <- data.frame(x.therapy$date[min(r)],
                                     x.therapy$date[max(r)],
                                     (length(x.therapy.30.days[[1]][x.therapy.30.days[[1]]==0])/(length(x.therapy.30.days[[1]])))*100)
  colnames(med.free.30.days) <- c("First date","Last date","Percent med free days")
  if (ceiling(dim(x.therapy)[1]/30) >= 2) {
    for (i in 2:(ceiling(dim(x.therapy)[1]/30))) {
      therapy.free <- (length(x.therapy.30.days[[i]][x.therapy.30.days[[i]]==0])/(length(x.therapy.30.days[[i]])))*100
      r <- c[c[]/30 > i-1 & c[]/30 <= i]
      newrow <- data.frame(x.therapy$date[min(r)],x.therapy$date[max(r)],therapy.free)
      colnames(newrow) <- c("First date","Last date","Percent med free days")
      med.free.30.days <- data.frame(rbind(med.free.30.days,newrow))
      colnames(med.free.30.days) <- c("First date","Last date","Percent med free days")
    }
  }
  
  print("med free percentage calculated.")
  
  x.base.med <- subset(x.base,x.base$load!=0)
  base.median <- median(x.base.med$load)
  base.median.number <- median(x.base.med$number)
  
  ## daily med response calculation
  daily.response <- c((x.therapy$load/base.median)*100)
  daily.response <- data.frame(x.therapy$date,daily.response)
  colnames(daily.response) <- c("Date","Response")
  
  ## daily med number response calculation
  daily.number.response <- c((x.therapy$number/base.number.median)*100)
  daily.number.response <- data.frame(daily.response,daily.number.response)
  colnames(daily.number.response) <- c("Date","med Response","med Number Response")
  print("Daily med number response calculated")
  
  print("Daily response calculated.")
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
  
  ## 30 day med response calculation
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
  
  period.response <- data.frame(med.free.30.days[,1:2],period.response)
  colnames(period.response)[3] <- "med response"
  
  print("30 day response calculated.")
  
  ## 30 day med number response calculation
  x.therapy.number.30.days <- split(x.therapy$number,ceiling(seq_along(x.therapy$number)/30))
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
  colnames(period.response)[4] <- "Med number response"
  
  print("30 day number response calculated.")
  
  ## percent med free response
  
  percent.med.free.response <- period.response
  percent.med.free.response[,3] <- 100 - (med.free.30.days[,3] - med.free.base)
  colnames(percent.med.free.response)[3] <- "Percent med free response"
  
  print("med free percentage response calculated.")
  
  ## med score
  score.1 <- (((med.free.30.days[,3])/100) * ((percent.med.free.response[,3])/100)) * 100
  score.2 <- (((100-med.free.30.days[,3])/100) * ((period.response[,3])/100)) * 100
  score <- score.1 + score.2
  med.score <- percent.med.free.response
  med.score[,3] <- score
  colnames(med.score)[3] <- "med score"
  
  print("med score calculated.")
  
  ## med number score
  score.3 <- (((med.free.30.days[,3])/100) * ((percent.med.free.response[,3])/100)) * 100
  score.4 <- (((100-med.free.30.days[,3])/100) * ((period.response[,4])/100)) * 100
  score2 <- score.3 + score.4
  med.number.score <- percent.med.free.response
  med.number.score[,3] <- score2
  colnames(med.number.score)[3] <- "Med number score"
  
  print("Med number score calculated.")
  
  results <- data.frame(med.score[,1],
                        med.score[,2],
                        cbind(med.free.30.days[,3],
                              period.response[,3],
                              period.response[,4],
                              percent.med.free.response[,3],
                              med.score[,3],
                              med.number.score[,3]))
  colnames(results) <- c("First date","Last date","% Med Free","Med Response","Med Number Response",
                         "% Med Free Response","Med Score","Med Number Score")
  
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