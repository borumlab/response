med_load_calculation <- function() {
  
  ## Read in all relevant data from csv files
  print("Input the directory in which the patient raw data file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir1 <- readline(prompt="Enter here: ")
  print("Input the name of the patient raw data file you would like to use (leave off the .csv)")
  print("Example: filename")
  data <- readline(prompt="Enter here: ")
  
  data <- gsub(" .csv",".csv",paste(data,".csv"))
  data <- gsub("/ ","/",paste(dir1,data))
  data <- read.csv(data,header=TRUE)
  
  print("Input the directory in which the anthropometrics file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir2 <- readline(prompt="Enter here: ")
  print("Input the name of the anthropometrics file you would like to use (leave off the .csv)")
  print("Example: filename")
  anthro <- readline(prompt="Enter here: ")
  
  anthro <- gsub(" .csv",".csv",paste(anthro,".csv"))
  anthro <- gsub("/ ","/",paste(dir2,anthro))
  anthro <- read.csv(anthro,header=TRUE)
  
  print("Input the directory in which the demographics file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir3 <- readline(prompt="Enter here: ")
  print("Input the name of the demographics file you would like to use (leave off the .csv)")
  print("Example: filename")
  demo <- readline(prompt="Enter here: ")
  
  demo <- gsub(" .csv",".csv",paste(demo,".csv"))
  demo <- gsub("/ ","/",paste(dir3,demo))
  demo <- read.csv(demo,header=TRUE)
  
  print("Input the directory in which the med ranking table file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir4 <- readline(prompt="Enter here: ")
  print("Input the name of the med ranking table file you would like to use (leave off the .csv)")
  print("Example: filename")
  ranking <- readline(prompt="Enter here: ")
  
  ranking <- gsub(" .csv",".csv",paste(ranking,".csv"))
  ranking <- gsub("/ ","/",paste(dir4,ranking))
  ranking <- read.csv(ranking,header=TRUE)
  
  data <- data[!is.na(data[,1]),]
  anthro <- anthro[!is.na(anthro[,1]),]
  demo <- demo[!is.na(demo[,1]),]
  ranking <- ranking[!is.na(ranking[,5]),]
  
  print("Calculating med intake in mg/kg/day, please wait...")
  
  ## 
  mrnumber <- unique(anthro[,1])
  
  med_dose <- data[,c(2,5,9)]
  med_dose[,1] <- gsub(" ","",med_dose[,1],fixed=TRUE)
  med_dose[,1] <- as.Date(med_dose[,1],format="%m/%d/%Y")
  
  anthro <- anthro[,c(1,2,4,6,17)]
  anthro[,2] <- gsub(" ","",anthro[,2],fixed=TRUE)
  anthro[,2] <- as.Date(anthro[,2],format="%m/%d/%Y")
  anthro[,5] <- gsub(" ","",anthro[,5],fixed=TRUE)
  anthro[,5] <- as.Date(anthro[,5],format="%m/%d/%Y")
  
  birthdate <- unique(anthro[,5])
  last_date <- max(max(unique(anthro[,2])),max(unique(med_dose[,1])))
  
  ## Create table with start and end dates for each period of specific med usage and dosage
  END_DATE <- data.frame(rep(unique(med_dose[,1])[2]-1,length(med_dose[med_dose[,1]==unique(med_dose[,1])[2-1],2])))
  colnames(END_DATE) <- "END_DATE"
  for (i in 3:(length(unique(med_dose[,1])))) {
    end_date <- data.frame(rep(unique(med_dose[,1])[i]-1,length(med_dose[med_dose[,1]==unique(med_dose[,1])[i-1],1])))
    colnames(end_date) <- "END_DATE"
    END_DATE <- data.frame(rbind(END_DATE,end_date))
    colnames(END_DATE) <- "END_DATE"
  }
  na <- cbind(rep(NA,length(med_dose[med_dose[,1]==unique(med_dose[,1])[length(unique(med_dose[,1]))],1])))
  colnames(na) <- "END_DATE"
  END_DATE <- data.frame(rbind(END_DATE,na))
  med_dose <- cbind(med_dose[,1],END_DATE,med_dose[,2:3])
  
  ## Create table with start and end dates for each period of specific weight, which will be used for linear interpolation
  naive <- TRUE
  if (demo[demo[,3]==mrnumber,4] == "E") {
    naive <- FALSE
  }
  if (naive == TRUE) {
    anthro <- anthro[anthro[,3]==1,]
  } else if (naive == FALSE) {
    anthro <- anthro[(anthro[,3]==1 & anthro[,3]==2),]
  }
  
  END_DATE <- data.frame(mrnumber,anthro[,2],birthdate,1,anthro[,4],birthdate)
  colnames(END_DATE)[2:5] <- c("start.date","end.date","source","weight")
  for (i in 2:(length(anthro[,1]))) {
    END_DATE[(i-1),3] <- END_DATE[i,2]-1
    colnames(END_DATE) <- c("mrnumber","start.date","end.date","source","weight","birthdate")
  }
  END_DATE[length(END_DATE[,1]),3] <- NA
  anthro <- END_DATE
  
  
  ##
  med_intake <- data.frame(start.date=as.Date(character()),med.id=character(),
                           dosage=integer(),weight=integer(),med.intake=integer())
  temp_date <- med_dose[1,1]
  
  for (i in 1:((as.integer(last_date-med_dose[1,1]))+1)) {
    compare <- max(unique(med_dose[,1])[unique(med_dose[,1])<=temp_date])
    temp_med_intake <- data.frame(temp_date,med_dose[med_dose[,1]==compare,c(3:4)],NA,NA)
    colnames(temp_med_intake) <- c("date","med.id","dosage","weight","med.intake")
    med_intake <- data.frame(rbind(med_intake,temp_med_intake))
    
    for (j in 1:(length(anthro[,1]))) {
      if (j == length(anthro[,1])) {
        med_intake[med_intake[,1]==temp_date,4] <- anthro[j,5]
        break
      }
      if (temp_date <= anthro[j,3]) {
        weight.1 <- anthro[j,5]
        weight.2 <- anthro[(j+1),5]
        diffweight <- (weight.2-weight.1)
        difftotal <- as.integer(anthro[j,3]-anthro[j,2])
        diffday <- as.integer(temp_date-anthro[j,2])
        newweight <- round(weight.1+(diffweight*(diffday/difftotal)),1)
        med_intake[med_intake[,1]==temp_date,4] <- newweight
        break
      }
    }
    med_intake[med_intake[,1]==temp_date,5] <- med_intake[med_intake[,1]==temp_date,3]/med_intake[med_intake[,1]==temp_date,4] 
    temp_date <- temp_date + 1 
  }

  print("Calculating minimum dose in mg/kg/day, please wait...")
  
  ## Denominator: Minimum dose in mg/kg/day

  if (!require("lubridate")) {
    install.packages("lubridate")
  }
  library(lubridate)

  med_min_dose <- data.frame(med_intake[,1:4],age=NA,min.dose=NA)

  temp_date <- med_dose[1,1]
  for (i in 1:(as.integer(last_date-med_dose[1,1])+1)) {
    duration <- interval(birthdate,temp_date)
    med_min_dose[med_min_dose[,1]==temp_date,5] <- round(duration/dyears(1),2)
    
    for (j in med_min_dose[med_min_dose[,1]==temp_date,2]) {
      compare <- unique(med_min_dose[med_min_dose[,1]==temp_date,5])
      if (j == "MID0003") {
        compare <- unique(med_min_dose[med_min_dose[,1]==temp_date,4])
      }
      for (k in 1:(length(ranking[ranking[,3]==j,3]))) {
        if ((compare >= ranking[ranking[,3]==j,][k,5]) && (compare <= ranking[ranking[,3]==j,][k,6])) {
          med_min_dose[med_min_dose[,1]==temp_date & med_min_dose[,2]==j,6] <- ranking[ranking[,3]==j,][k,7]
          break
        }
      }
    }
    
    temp_date <- temp_date+1
  }
  
  print("Calculating med load per day, please wait...")
  
  ## Use numerator and denominator to calculate the med load
  med_load <- data.frame(med_min_dose[,1:5],med_intake[,5],med_min_dose[,6],med.load.per.med=NA,med.load.per.day=NA)
  colnames(med_load)[6:7] <- c("med.intake","min.dose")
  med_load[,8] <- round(med_load[,6]/med_load[,7],4)
  med_load[,6:7] <- round(med_load[,6:7],4)
  for (i in unique(med_load[,1])) {
    med_load[med_load[,1]==i,9][1] <- sum(med_load[med_load[,1]==i,8])  
  }
  
  med.number.per.day <- rep(NA,dim(med_load)[1])
  med_load <- data.frame(med_load,med.number.per.day)
  colnames(med_load)[10] <- "med.number.per.day"
  for (i in unique(med_load[,1])) {
    med_load[med_load[,1]==i,10][1] <- length(med_load[med_load[,1]==i & med_load[,6]!=0,6])
  }
  
  print("Set the work directory in which you would like to save this file")
  wd <- readline(prompt="Enter here: ")
  setwd(wd)
  print("Give the name you would like to give to the file. Make sure to add the .csv at the end")
  csv <- readline(prompt="Enter here: ")
  write.csv(med_load,file=csv,na="",row.names=FALSE)
  
  if (!require("astsa")) {
    install.packages("astsa")
  }
  library(astsa)
  
  unique_med_load <- med_load[!is.na(med_load[,9]),9]
  print("Here is a plot of the daily med loads (see right): ")
  plot(as.Date(unique(med_load[,1])),unique_med_load,type='l',xlab="Date",ylab="Daily Med Load")
  
  return(med_load)
}

# data <- read.csv("G:/Notebooks_E/e10/Jonathan Lee/Med load/AlRo_Med_Raw_Table.csv")