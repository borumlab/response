calculate <- function(x,n,l,baseline,therapy,patient,string,mrnumber) {
  # x,10,11,seizure.baseline,seizure.therapy,AlRo,"Seizure"
  # y,9,10,med.baseline,med.therapy,AlRo,"Med"
  
  ## percent free days during baseline
  base.free <- c()
  if (string == "Seizure") {
    base.free <- dim(baseline[baseline[,n]==0,])[1]
  } else if (string == "Med") {
    base.free <- dim(baseline[baseline[,l]==0,])[1]
  }
  free.base <- (base.free/dim(baseline)[1])*100
  print(paste("The percentage of baseline days with no",string,"is:",free.base,"%"))
  
  ## percent free days during therapy
  therapy.30.days <- split(therapy[,l],ceiling(seq_along(therapy[,l])/30))
  c <- c(1:dim(therapy)[1])
  r <- c[c[]/30 > 0 & c[]/30 <= 1]
  free.30.days <- data.frame(therapy$date[min(r)],
                             therapy$date[max(r)],
                             (length(therapy.30.days[[1]][therapy.30.days[[1]]==0])/(length(therapy.30.days[[1]])))*100)
  colnames(free.30.days)[1:2] <- c("First date","Last date")
  colnames(free.30.days)[3] <- paste("Percent",string,"Free Days")
  if (ceiling(dim(therapy)[1]/30) >= 2) {
    for (i in 2:(ceiling(dim(therapy)[1]/30))) {
      therapy.free <- (length(therapy.30.days[[i]][therapy.30.days[[i]]==0])/(length(therapy.30.days[[i]])))*100
      r <- c[c[]/30 > i-1 & c[]/30 <= i]
      newrow <- data.frame(therapy$date[min(r)],therapy$date[max(r)],therapy.free)
      colnames(newrow) <- colnames(free.30.days)
      free.30.days <- data.frame(rbind(free.30.days,newrow))
      colnames(free.30.days) <- colnames(newrow)
    }
  }
  
  print(paste(string,"free percentage calculated."))
  
  baseline <- subset(baseline,baseline[,l]!=0)
  base.median <- median(baseline[,l])
  base.number.median <- median(baseline[,n])
  
  ## daily response calculation
  daily.response <- c((therapy[,l]/base.median)*100)
  daily.response <- data.frame(therapy$date,daily.response)
  colnames(daily.response)[1] <- c("Date")
  colnames(daily.response)[2] <- paste(string,"Response")
  print(paste("Daily",string,"response calculated."))
  
  ## daily number response calculation
  daily.number.response <- c((therapy[,n]/base.number.median)*100)
  daily.number.response <- data.frame(daily.response,daily.number.response)
  print(paste("Daily",string,"number response calculated"))
  
  daily.number.response <- data.frame(rep(mrnumber,dim(daily.number.response)[1]),daily.number.response)
  colnames(daily.number.response)[1] <- "MRNUMBER"
  colnames(daily.number.response)[2] <- c("Date")
  colnames(daily.number.response)[3] <- paste(string,"Response")
  colnames(daily.number.response)[4] <- paste(string,"Number Response")
  
  print("Give the name you would like to give to the response file (leave off the .xlsx and the patient letters)")
  xlsx <- readline(prompt="Enter here: ")
  xlsx <- gsub(" ","",paste(xlsx,".xlsx"))
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx2(daily.number.response,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  ## 30 day response calculation
  period.response <- c()
  for (i in 1:(ceiling(dim(therapy)[1]/30))) {
    response <- c()
    period.median <- median(therapy.30.days[[i]][therapy.30.days[[i]]!=0])
    if (is.na(period.median) || period.median == 0) {
      response <- 0
    } else {
      response <- (period.median/base.median)*100
    }
    period.response <- c(period.response,response)
  }
  
  period.response <- data.frame(free.30.days[,1:2],period.response)
  colnames(period.response)[3] <- paste(string,"Response")
  
  print("30 day response calculated.")
  
  ## 30 day number response calculation
  therapy.number.30.days <- split(therapy[,n],ceiling(seq_along(therapy[,n])/30))
  period.number.response <- c()
  for (i in 1:(ceiling(dim(therapy)[1]/30))) {
    period.number.median <- median(therapy.number.30.days[[i]][therapy.number.30.days[[i]]!=0])
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
  
  ## percent free response
  
  percent.free.response <- period.response
  percent.free.response[,3] <- 100 - (free.30.days[,3] - free.base)
  colnames(percent.free.response)[3] <- paste("Percent",string,"free response")
  
  print(paste(string,"free percentage response calculated."))
  
  ## score
  score.1 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
  score.2 <- (((100-free.30.days[,3])/100) * ((period.response[,3])/100)) * 100
  score <- score.1 + score.2
  overall.score <- percent.free.response
  overall.score[,3] <- score
  colnames(overall.score)[3] <- paste(string,"score")
  
  print(paste(string,"score calculated."))
  
  ## number score
  score.3 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
  score.4 <- (((100-free.30.days[,3])/100) * ((period.response[,4])/100)) * 100
  score2 <- score.3 + score.4
  overall.number.score <- percent.free.response
  overall.number.score[,3] <- score2
  colnames(overall.number.score)[3] <- paste(string,"number score")
  
  print(paste(string,"number score calculated."))
  
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
  colnames(results)[2:3] <- c("First date","Last date")
  colnames(results)[4] <- paste("%",string,"Free")
  colnames(results)[5] <- paste(string,"Response")
  colnames(results)[6] <- paste(string,"Number Response")
  colnames(results)[7] <- paste("%",string,"Free Response")
  colnames(results)[8] <- paste(string,"Score")
  colnames(results)[9] <- paste(string,"Number Score")
  
  print("Give the name you would like to give to the score file (leave off the .xlsx and the patient letters)")
  xlsx <- readline(prompt="Enter here: ")
  xlsx <- gsub(" ","",paste(xlsx,".xlsx"))
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx2(results,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  cat("Your results have been calculated",'\n','\n')
}

score_calculation <- function() {
  
  if (!require("rJava")) {
    install.packages("rJava")
  }
  library(rJava)
  if (!require("XLConnectJars")) {
    install.packages("XLConnectJars")
  }
  library(XLConnectJars)
  if (!require("XLConnect")) {
    install.packages("XLConnect")
  }
  library(XLConnect)
  if (!require("xlsxjars")) {
    install.packages("xlsxjars")
  }
  library(xlsxjars)
  if (!require("xlsx")) {
    install.packages("xlsx")
  }
  library(xlsx)
  
  print("Input the directory that you wish to draw this patient's data from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  print("Input the four letters that signify the patient we are doing calculations for")
  print("Example: FiLa")
  patient <- readline(prompt="Enter here: ")
  
  print("Input the name of the file in which the daily seizure loads can be found (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  x <- readline(prompt="Enter here: ")
  
  x <- gsub(" .xlsx",".xlsx",paste(x,".xlsx"))
  x <- gsub(" ","",paste(patient,"_",x))
  x <- readWorksheetFromFile(x,sheet=1)
  x <- subset(x,!is.na(x[,11]))
  ## Seizure number per day found in column 10 of this file
  ## Seizure load per day found in column 11 of this file
  
  seizure.baseline <- subset(x,x$day.type==1)
  seizure.therapy <- subset(x,x$day.type!=1)
  
  mrnumber <- unique(x[,1])
  x <- x[,-1]
  
  calculate(x,10,11,seizure.baseline,seizure.therapy,patient,"Seizure",mrnumber)
  
  return(1)
  
  ###################################################################
  
  print("Input the name of the file in which the daily med loads can be found (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  y <- readline(prompt="Enter here: ")
  
  y <- gsub(" .xlsx",".xlsx",paste(y,".xlsx"))
  y <- gsub(" ","",paste(patient,"_",y))
  y <- readWorksheetFromFile(y,sheet=1)
  
  print("Input the name of the med raw data file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  data <- readline(prompt="Enter here: ")
  
  data <- gsub(" .xlsx",".xlsx",paste(data,".xlsx"))
  data <- gsub(" ","",paste(patient,"_",data))
  data <- readWorksheetFromFile(data,sheet=1)
  
  ## Med load per day found in column 9 of this file
  ## Med number per day found in column 10 of this file
  y <- y[,-1]
  y <- subset(y,!is.na(y[,10]))
  y[,1] <- as.Date(y[,1],format="%m/%d/%Y")
  data <- subset(data,!is.na(data[,1]))
  data[,2] <- as.Date(data[,2],format="%m/%d/%Y")
  
  ## Med free calculation
  therapy <- unique(data[data[,3]==2,2])
  med.baseline <- y[y[,1]<therapy[1],]
  med.therapy <- y[y[,1]>=therapy[1],]
  
  calculate(y,9,10,med.baseline,med.therapy,patient,"Med",mrnumber)
}