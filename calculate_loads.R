rankwithvalues <- function(x,a) {
  if ((is.null(x)) || (is.na(x)) || (x == "")) {
    return(0)
  }
  for (i in 1:((dim(a)[1])-1)) {
    if (tolower(as.character(x)) == as.character(a[i,1])) {
      return(as.numeric(a[i,2]))
    }
  }
  return(as.numeric(a[dim(a)[1],2]))
}

rankwithranges <- function(x,b) {
  if ((is.null(x)) || (is.na(x)) || (x == "")) {
    return(0)
  } else if (tolower(as.character(x)) == "u") {
    for (i in 1:(dim(b)[1])) {
      if (b[i,1] == "u") {
        return(as.numeric(b[i,2]))
      }
    }
  }
  for (i in 1:(dim(b)[1])) {
    if (b[i,1] == "u") {
      b <- b[-i,]
      break
    }
  }
  for (i in 1:((dim(b)[1])-1)) {
    if ((as.numeric(as.character(x))) >= as.numeric(as.character((b[i,1])))
        && (as.numeric(as.character((x))) < as.numeric(as.character(b[(i+1),1])))) {
      return(as.numeric(b[i,2]))
    }
  }
  return(as.numeric(b[dim(b)[1],2]))
}

makelowercase <- function(x) {
  for (i in length(x)) {
    if (is.character(x[i])) {
      x[i] <- tolower(x[i])
    }
  }
  return(x)
}

seizure_sum <- function(x,a,b,c,d,e) {
  colnames(x)[5] <- "Seizure_Severity"
  colnames(x)[6] <- "Seizure_Length"
  colnames(x)[7] <- "Seizure_Type"
  colnames(x)[8] <- "Seizure_Variables"
  colnames(x)[10] <- "Seizure_Cluster"
  srank <- rep(0,dim(x)[1]); lrank <- srank; trank <- srank; vrank <- srank; crank <- srank; seizureloadvalues <- srank
  ranktable <- cbind(srank,lrank,trank,vrank,crank)
  seizureloadvalues <- rep(0,dim(x)[1])
  for (i in 1:(dim(x)[1])) {
    if (x[i,4] == 3) {
      ranktable[i,c(1,2,3,4,5)] <- c(NA,NA,NA,NA,NA)
      seizureloadvalues[i] <- NA
    } else {
      ranktable[i,1] <- rankwithvalues(x$Seizure_Severity[i],a[,c(1,2)])
      ranktable[i,2] <- rankwithranges(x$Seizure_Length[i],b[,c(1,2)])
      ranktable[i,3] <- rankwithvalues(x$Seizure_Type[i],c[,c(1,2)])
      ranktable[i,4] <- rankwithvalues(x$Seizure_Variables[i],d[,c(1,2)])
      ranktable[i,5] <- rankwithranges(x$Seizure_Cluster[i],e[,c(1,2)])
      for (j in 1:5) {
        seizureloadvalues[i] <- seizureloadvalues[i] + ranktable[i,j]
      }
    }
  }
  ranktable <- cbind(ranktable,seizureloadvalues)
  colnames(ranktable) <- c("severity","length","type","variables","cluster","load")
  return(ranktable)
}

calculate_ranks <- function(n,m) {
  ranks <- c()
  j <- 0
  while (j < (m+1)) {
    for (i in 1:(dim(n)[1])) {
      if (n[i,4] == j) {
        ranks <- rbind(ranks,n[i,c(3,4)])
      }
    }
    j <- j + 1
  }
  return(ranks)
}

missing_sums <- function(x,y,q) {
  
  colnames(x)[1] <- "dates"
  
  z <- as.numeric(x[,1])
  y <- y
  
  a <- 1
  while (a <= (dim(x)[1])) {
    
    if ((as.numeric(as.character(y[a])) == 3) && (is.na(x[a,9]))) {
      
      k <- 1
      rownums <- c(a)
      if (a < dim(x)[1]) {
        for (j in (a+1):((dim(x)[1]))) {
          if (as.numeric(as.character(y[j])) == 3) {
            k <- k + 1
            rownums <- c(rownums,j)
          } else {
            break
          }
        }
      }
      
      upper <- 0
      lower <- 0
      if (1 %in% rownums) {
        lower <- k
      } else if (dim(x)[1] %in% rownums) {
        upper <- k
      } else {
        if (k%%2 != 0) {
          upper <- ceiling(k/2)
          lower <- floor(k/2)
        } else {
          upper <- k/2
          lower <- k/2
        }
      }
      
      lowsums <- c()
      upsums <- c()
      lownumbers <- c()
      upnumbers <- c() 
      
      lo <- lower; up <- upper
      
      uppernadates <- data.frame()
      lowernadates <- data.frame()
      
      if (upper != 0) {
        uppernadates <- data.frame(x[(a:(a+upper-1)),1])
        colnames(uppernadates) <- "dates"
      }
      if (lower != 0) {
        lowernadates <- data.frame(x[(a+upper):(a+k-1),1])
        colnames(lowernadates) <- "dates"
      }
      
      upperday <- 0
      lowerday <- 0
      upday <- c()
      lowday <- c()
      upcount <- c()
      lowcount <- c()
      
      if (upper != 0) {
        
        for (u in (a-1):1) {
          if (as.numeric(as.character(y[u])) != 3) {
            upsums <- c(x[u,9],upsums)
            upnumbers <- c(x[u,8],upnumbers)
            upday <- c(upperday,upday)
            upcount <- c(upperday,upcount)
            temp <- data.frame(x[u,1])
            colnames(temp) <- "dates"
            if (u == 1) {
              up <- up - 1
            } else {
              if ((as.numeric(as.character(z[u]))) != (as.numeric(as.character(z[1])))) {
                if ((as.numeric(as.character(z[u]))) != (as.numeric(as.character(z[u-1])))) {
                  up <- up - 1
                  upperday <- upperday + 1
                } 
              }
            }
          }
          if (up == 0) {
            break
          }
          
        } 
        
        upday[] <- max(upday) - upday[]
        upsumday <- rbind(upsums,upnumbers,upday,upcount)
        
        if (up != 0) {
          c <- upsums
          d <- upnumbers
          e <- upday
          upsums <- c()
          upday <- c()
          upcount <- c()
          for (s in 0:(upper-1)) {
            upsums <- c(upsums,c[e[]==(s%%(max(e)+1))])
            upnumbers <- c(upnumbers,d[e[]==(s%%(max(e)+1))])
            upday <- c(upday,e[e[]==(s%%(max(e)+1))])
            upcount <- c(upcount,rep(s,length(upsums)-length(upcount)))
          }
          upsumday <- rbind(upsums,upnumbers,upday,upcount)
        }
        
        
      } else {
        upsumday <- c()
      }
      
      if (lower != 0) {
        
        for (l in (a+k):(dim(x)[1])) {
          
          if (as.numeric(as.character(y[l])) != 3) {
            lowsums <- c(lowsums,x[l,9])
            lownumbers <- c(lownumbers,x[l,8])
            lowday <- c(lowday,lowerday)
            lowcount <- c(lowcount,lowerday)
            temp <- data.frame(x[l,1])
            colnames(temp) <- "dates"
            
            if (l == dim(x)[1]) {
              lo <- lo - 1
            } else {
              if ((as.numeric(as.character(z[l]))) != as.numeric(as.character(z[length(z)]))) {
                if ((as.numeric(as.character(z[l]))) != (as.numeric(as.character(z[l+1])))) {
                  lo <- lo - 1
                  lowerday <- lowerday + 1
                }
              }
            }
          }
          if (lo == 0) {
            break
          }
        }
        
        lowday[] <- max(lowday) - lowday[]
        lowsumday <- rbind(lowsums,lownumbers,lowday,lowcount)
        
        if (lo != 0) {
          c <- lowsums
          d <- lownumbers
          e <- lowday
          lowsums <- c()
          lowday <- c()
          lowcount <- c()
          for (s in 0:(lower-1)) {
            lowsums <- c(c[e[]==(s%%(max(e)+1))],lowsums)
            lownumbers <- c(d[e[]==(s%%(max(e)+1))],lownumbers)
            lowday <- c(e[e[]==(s%%(max(e)+1))],lowday)
            lowcount <- c(rep(s,length(lowsums)-length(lowcount)),lowcount)
          }
          lowsumday <- rbind(lowsums,lownumbers,lowday,lowcount)
        }
        
      } 
      
      temp <- data.frame(x[-(a:(a+k-1)),])
      colnames(temp) <- colnames(x)
      
      if ((dim(lowernadates)[1] > 0)) {
        new <- c()
        for (n in 1:(dim(lowernadates)[1])) {
          newrows <- data.frame(lowernadates[n,1],unique(x[x[,1]==lowernadates[n,1],2]),NA,NA,NA,NA,NA,lowsumday[2,lowsumday[4,]==n-1],lowsumday[1,lowsumday[4,]==n-1])
          colnames(newrows) <- colnames(temp)
          if (is.null(new)) {
            new <- newrows
            colnames(new) <- colnames(newrows)
          } else {
            new <- data.frame(rbind(new,newrows))
          }
        }
        if (a == 1) {
          temp <- data.frame(rbind(new,temp))
        } else if (a == ((dim(temp)[1])+1)) {
          temp <- data.frame(rbind(temp,new))
        } else {
          temp <- data.frame(rbind(temp[1:(a-1),],new,temp[a:(dim(temp)[1]),]))
        }
        colnames(temp) <- colnames(x)
      } 
      if (dim(uppernadates)[1] > 0) {
        new <- c()
        for (n in 1:(dim(uppernadates)[1])) {
          t <- (dim(uppernadates)[1])-n
          newrows <- data.frame(uppernadates[n,1],unique(x[x[,1]==uppernadates[n,1],2]),NA,NA,NA,NA,NA,upsumday[2,upsumday[4,]==t],upsumday[1,upsumday[4,]==t])
          colnames(newrows) <- colnames(x)
          if (is.null(new)) {
            new <- newrows
            colnames(new) <- colnames(newrows)
          } else {
            new <- data.frame(rbind(new,newrows))
          }
        }
        if (a == 1) {
          temp <- data.frame(rbind(new,temp))
        } else if (a == ((dim(temp)[1])+lower+1)) {
          temp <- data.frame(rbind(temp,new))
        } else {
          temp <- data.frame(rbind(temp[1:(a-1),],new,temp[a:(dim(temp)[1]),]))
        }
        colnames(temp) <- colnames(x)
      }
      
      y <- c(y[1:(a-1)],rep(3,(dim(temp)[1]-length(y)+k)),y[((a+k):length(y))])
      zz <- c()
      for (i in 1:k) {
        zz <- c(zz,rep(z[a+i-1],dim(temp[as.numeric(temp[,1])==z[a+i-1],])[1]))
      }
      if (a == 1) {
        z <- c(zz,z)
      } else if (a == (dim(x)[1] - upper - lower + 1)) {
        z <- c(z,zz)
      } else {
        z <- c(z[1:(a-1)],zz,z[(a+k):(length(z))])
      }
      
      add <- (dim(temp)[1])-(dim(x)[1])
      x <- temp
      
      a <- a + lower + upper + add
      
    } else {
      a <- a + 1
    }
  }
  
  rownames(x) <- rep(NULL,dim(x)[1])
  return(x)
}

load_calculation <- function() {
  
  if (!require("xlsx")) {
    install.packages("xlsx")
  }
  library(xlsx)
  
  print("Input the directory that you wish to draw this patient's data from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  print("Input the four letters that signify the patient we are doing calculations for")
  print("Example: JoLe")
  patient <- readline(prompt="Enter here: ")
  
  print("Input the name of the patient data file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  data <- readline(prompt="Enter here: ")
  
  data <- gsub(" .xlsx",".xlsx",paste(data,".xlsx"))
  data <- gsub(" ","",paste(patient,"_",data))
  data <- read.xlsx(data,sheetIndex=1)
  
  print("Input the name of the ranking table file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  ranking <- readline(prompt="Enter here: ")
  
  ranking <- gsub(" .xlsx",".xlsx",paste(ranking,".xlsx"))
  ranking <- gsub(" ","",paste(patient,"_",ranking))
  ranking <- read.xlsx(ranking,sheetIndex=1)
  
  mrnumber <- unique(data[,1])
  data <- data[!is.na(data[,1]),]
  for (i in 1:length(data[,1])) {
    if (nchar(as.character(data[i,2]))>10) {
      data[,2] <- substr(data[,2],1,nchar(as.character(data[,2]))-5)
    }
  }
  data[,2] <- as.Date(data[,2],format="%m/%d/%Y")
  
  print("Calculating ranks. Please wait...")
  
  q <- 0
  for (i in 1:(dim(data)[1])) {
    if (!is.na(data[i,1])) {
      q <- q + 1
    } else {
      break
    }
  }
  
  data <- data[1:q,]
  
  n1 <- c(); n2 <- c(); n3 <- c(); n4 <- c(); n5 <- c()
  for (i in 1:(dim(ranking)[1])) {
    if (grepl("severity",tolower(ranking[i,2]))) {
      n1 <- rbind(n1,ranking[i,])
    } else if (grepl("length",tolower(ranking[i,2]))) {
      n2 <- rbind(n2,ranking[i,])
    } else if (grepl("type",tolower(ranking[i,2]))) {
      n3 <- rbind(n3,ranking[i,])
    } else if (grepl("variable",tolower(ranking[i,2]))) {
      n4 <- rbind(n4,ranking[i,])
    } else if (grepl("cluster",tolower(ranking[i,2]))) {
      n5 <- rbind(n5,ranking[i,])
    }
  }
  
  n1max <- max(n1[,4])
  n2max <- max(n2[,4])
  n3max <- max(n3[,4])
  n4max <- max(n4[,4])
  n5max <- max(n5[,4])
  
  ranks1 <- c(); ranks2 <- c(); ranks3 <- c(); ranks4 <- c(); ranks5 <- c()
  
  ranks1 <- calculate_ranks(n1,n1max)
  ranks2 <- calculate_ranks(n2,n2max)
  ranks3 <- calculate_ranks(n3,n3max)
  ranks4 <- calculate_ranks(n4,n4max)
  ranks5 <- calculate_ranks(n5,n5max)
  
  data[,5] <- makelowercase(data[,5])
  data[,6] <- makelowercase(data[,6])
  data[,7] <- makelowercase(data[,7])
  data[,8] <- makelowercase(data[,8])
  data[,10] <- makelowercase(data[,10])
  
  values <- seizure_sum(data,ranks1,ranks2,ranks3,ranks4,ranks5)
  values <- cbind(data[,3],values[,1:5],data[,9],values[,6])
  values[,8] <- (values[,7]*values[,8])
  colnames(values)[7:8] <- c("number","sum")
  
  values <- data.frame(data[,2],values)
  colnames(values)[1] <- "date"
  
  print("Calculating sums for missing days. Please wait...")
  
  values <- missing_sums(values,data[,4])
  colnames(values)[2] <- "day type"
  
  seizure.load.per.day <- rep(NA,dim(values)[1])
  seizure.number.per.day <- rep(NA,dim(values)[1])
  
  print("Now calculating daily seizure loads. Please wait...")
  
  k <- 0
  for (i in 1:(dim(values)[1])) {
    if (k > 0) {
      k <- k - 1
    } else {
      k <- 0
      seizure.load.per.day[i] <- values[i,9]
      seizure.number.per.day[i] <- values[i,8]
      if (i != dim(values)[1]) {
        for (j in 1:(dim(values)[1]-i)) {
          if (values[(i+j),1] == values[i,1]) {
            k <- k + 1
          } else if (values[(i+j),1] != values[i,9]) {
            break
          }
        }
      }
      if (k > 0) {
        for (m in 1:k) {
          seizure.load.per.day[i] <- as.numeric(seizure.load.per.day[i]) + as.numeric(values[(i+m),9])
          seizure.number.per.day[i] <- as.numeric(seizure.number.per.day[i]) + as.numeric(values[(i+m),8])
        }
      }
    }
  }
  
  seizure_load <- data.frame(cbind(rep(mrnumber,length(seizure.load.per.day))),values,seizure.number.per.day,seizure.load.per.day)
  colnames(seizure_load)[1] <- "MRNUMBER"
  colnames(seizure_load)[2] <- "date"

  print("Give the name you would like to give to the file (leave off the .xlsx and the patient letters)")
  xlsx <- readline(prompt="Enter here: ")
  xlsx <- gsub(" .xlsx",".xlsx",paste(xlsx,".xlsx"))
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx(seizure_load,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  return(1)
  
  ## Read in all relevant data from xlsx files
  print("Input the name of the patient raw data file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  data <- readline(prompt="Enter here: ")
  
  data <- gsub(" .xlsx",".xlsx",paste(data,".xlsx"))
  data <- gsub(" ","",paste(patient,"_",data))
  data <- read.xlsx(data,sheetIndex=1)

  print("Input the name of the anthropometrics file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  anthro <- readline(prompt="Enter here: ")
  
  anthro <- gsub(" .xlsx",".xlsx",paste(anthro,".xlsx"))
  anthro <- read.xlsx(anthro,sheetIndex=1)
  
  print("Input the name of the demographics file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  demo <- readline(prompt="Enter here: ")
  
  demo <- gsub(" .xlsx",".xlsx",paste(demo,".xlsx"))
  demo <- read.xlsx(demo,sheetIndex=1)

  print("Input the name of the med ranking table file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  ranking <- readline(prompt="Enter here: ")
  
  ranking <- gsub(" .xlsx",".xlsx",paste(ranking,".xlsx"))
  ranking <- read.xlsx(ranking,sheetIndex=1)
  
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
  last_date <- max(max(unique(anthro[,2])),max(unique(med_dose[,1])))-1
  
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

  print("Give the name you would like to give to the file (leave off the .xlsx and the patient letters)")
  xlsx <- readline(prompt="Enter here: ")
  xlsx <- gsub(" .xlsx",".xlsx",paste(xlsx,".xlsx"))
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx(med_load,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  if (!require("astsa")) {
    install.packages("astsa")
  }
  library(astsa)
  
  unique_med_load <- med_load[!is.na(med_load[,9]),9]
  print("Here is a plot of the daily med loads (see right): ")
  plot(as.Date(unique(med_load[,1])),unique_med_load,type='l',xlab="Date",ylab="Daily Med Load")
}