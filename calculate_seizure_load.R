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
  srank <- rep(0,dim(x)[1]); lrank <- srank; trank <- srank; vrank <- srank; crank <- srank; seizureloadvalues <- srank
  ranktable <- cbind(srank,lrank,trank,vrank,crank)
  seizureloadvalues <- rep(0,dim(x)[1])
  for (i in 1:(dim(x)[1])) {
    if (x$S_DATA_QUALITY[i] == 3) {
      ranktable[i,c(1,2,3,4,5)] <- c(NA,NA,NA,NA,NA)
      seizureloadvalues[i] <- NA
    } else {
      ranktable[i,1] <- rankwithvalues(x$SEIZURE_SEVERITY[i],a[,c(1,2)])
      ranktable[i,2] <- rankwithranges(x$SEIZURE_LENGTH[i],b[,c(1,2)])
      ranktable[i,3] <- rankwithvalues(x$SEIZURE_TYPE[i],c[,c(1,2)])
      ranktable[i,4] <- rankwithvalues(x$SEIZURE_VARIABLES[i],d[,c(1,2)])
      ranktable[i,5] <- rankwithranges(x$SEIZURE_CLUSTER[i],e[,c(1,2)])
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
      if (n$SEIZURE_RANKING[i] == j) {
        ranks <- rbind(ranks,n[i,colnames(n)=="SEIZURE_ENTRY" | colnames(n)=="SEIZURE_RANKING"])
      }
    }
    j <- j + 1
  }
  return(ranks)
}

missing_sums <- function(x,y) {
  
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
      
      tempupper <- data.frame()
      templower <- data.frame()
      
      if (upper != 0) {
        
        tempupper <- x[x$date==x$date[a]-1,]
        while (length(unique(tempupper$date)) < upper & !(x$date[1] %in% unique(tempupper$date))) {
          tempupper <- rbind.data.frame(x[!is.na(x$severity) & x$date<min(tempupper$date) & x$date==max(x$date[!is.na(x$severity) & x$date<min(unique(tempupper$date))]),],tempupper)
        }
        if (length(unique(tempupper$date)) < upper) {
          n <- ceiling(upper/length(unique(tempupper$date)))
          tempupper <- do.call("rbind",replicate(n,tempupper,simplify=FALSE))
          if (length(unique(tempupper$date)) > upper) {
            len <- length(unique(templower$date))-upper
            index <- 1
            while (index <= len) {
              tempdate <- tempupper$date[1]
              tempupper <- tempupper[-1,]
              if (tempdate!=tempupper$date[1]) {
                index <- index + 1
              }
            }
          }
        }
        
        # values[3450:3500,]
        
        tempupper[,3:7] <- NA
        newdate <- x$date[a]
        if (dim(tempupper)[1] > 1) {
          for (i in 1:(dim(tempupper)[1]-1)) {
            storeolddate <- tempupper$date[i]
            tempupper$date[i] <- newdate
            if (storeolddate != tempupper$date[i+1]) {
              newdate <- newdate + 1
            }
          }
        }
        tempupper$date[dim(tempupper)[1]] <- newdate
      }
      
      if (lower != 0) {
        
        templower <- x[x$date==x$date[a+k],]
        while (length(unique(templower$date)) < lower & !(x$date[dim(x)[1]] %in% unique(templower$date))) {
          templower <- rbind.data.frame(templower,x[!is.na(x$severity) & x$date>max(unique(templower$date)) & x$date==min(x$date[!is.na(x$severity) & x$date>max(unique(templower$date))]),])
        }
        if (length(unique(templower$date)) < lower) {
          n <- ceiling(lower/length(unique(templower$date)))
          templower <- do.call("rbind",replicate(n,templower,simplify=FALSE))
          if (length(unique(templower$date)) > lower) {
            len <- length(unique(templower$date))-lower
            index <- 1
            while (index <= len) {
              tempdate <- templower$date[length(templower$date)]
              templower <- templower[-(length(templower$date)),]
              if (tempdate!=templower$date[length(templower$date)]) {
                index <- index + 1
              }
            }
          }
        }
        
        templower[,3:7] <- NA
        newdate <- x$date[a+upper]
        if (dim(templower)[1] > 1) {
          for (i in 1:(dim(templower)[1]-1)) {
            storeolddate <- templower$date[i]
            templower$date[i] <- newdate
            if (storeolddate != templower$date[i+1]) {
              newdate <- newdate + 1
            }
          }
        }
        templower$date[dim(templower)[1]] <- newdate
        
      } 
      
      temp1 <- data.frame(x[-(a:(dim(x)[1])),])
      temp2 <- data.frame(x[-(1:(a+k-1)),])
      colnames(temp1) <- colnames(x)
      colnames(temp2) <- colnames(x)
      temp <- rbind.data.frame(temp1,tempupper,templower,temp2)
      colnames(temp) <- colnames(x)
      
      if (a == 1) {
        y <- c(rep(3,(dim(temp)[1]-length(y)+k)),y[((a+k):length(y))])
      } else if (a == (dim(x)[1] - upper - lower + 1)) {
        y <- c(y[1:(a-1)],rep(3,(dim(temp)[1]-length(y)+k)))
      } else {
        y <- c(y[1:(a-1)],rep(3,(dim(temp)[1]-length(y)+k)),y[((a+k):length(y))])
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

calculate_seizure_load <- function() {
  
  if (!require("rJava")) {
    install.packages("rJava")
  }
  library(rJava)
  if (!require("xlsxjars")) {
    install.packages("xlsxjars")
  }
  library(xlsxjars)
  if (!require("xlsx")) {
    install.packages("xlsx")
  }
  library(xlsx)
  if (!require("XLConnectJars")) {
    install.packages("XLConnectJars")
  }
  library(XLConnectJars)
  if (!require("XLConnect")) {
    install.packages("XLConnect")
  }
  library(XLConnect)
  if (!require("openxlsx")) {
    install.packages("openxlsx")
  }
  library(openxlsx)
  
  print("Input the four letters that signify the patient we are doing calculations for")
  print("Example: FILA")
  patient <- readline(prompt="Enter here: ")
  
  print("Input the directory that you wish to draw this patient's SEIZURE_DATA_SOURCE file and SEIZURE_RANKING_SOURCE file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  data <- "SEIZURE_DATA_SOURCE.xlsx"
  data <- gsub(" ","",paste(patient,"_",data))
  data <- read.xlsx(data,sheet=1,detectDates=TRUE)

  ranking <- "SEIZURE_RANKING_SOURCE.xlsx"
  ranking <- gsub(" ","",paste(patient,"_",ranking))
  ranking <- read.xlsx(ranking,sheet=1,detectDates=TRUE)
  
  data <- data[!is.na(data$MRNUMBER),]
  for (i in 1:length(data$DATE)) {
    if (nchar(as.character(data$DATE[i]))>10) {
      data$DATE <- substr(data$DATE,1,nchar(as.character(data$DATE))-5)
    }
  }
  data$DATE <- as.Date(data$DATE,format="%m/%d/%Y")
  
  savedata <- data[,!is.na(colnames(data)) & colnames(data)!="ENTERED" & colnames(data)!="AUDITED" & colnames(data)!="COMMENTS"] ## Save a copy of the FILA_SEIZURE_DATA_SOURCE file for later use
  
  mrnumber <- unique(data$MRNUMBER)
  
  print("Calculating ranks. Please wait...")
  
  n1 <- ranking[grepl("severity",tolower(ranking$SEIZURE_PARAMETER)),]
  n2 <- ranking[grepl("length",tolower(ranking$SEIZURE_PARAMETER)),]
  n3 <- ranking[grepl("type",tolower(ranking$SEIZURE_PARAMETER)),]
  n4 <- ranking[grepl("variable",tolower(ranking$SEIZURE_PARAMETER)),]
  n5 <- ranking[grepl("cluster",tolower(ranking$SEIZURE_PARAMETER)),]
  
  n1max <- max(n1$SEIZURE_RANKING)
  n2max <- max(n2$SEIZURE_RANKING)
  n3max <- max(n3$SEIZURE_RANKING)
  n4max <- max(n4$SEIZURE_RANKING)
  n5max <- max(n5$SEIZURE_RANKING)
  
  ranks1 <- calculate_ranks(n1,n1max)
  ranks2 <- calculate_ranks(n2,n2max)
  ranks3 <- calculate_ranks(n3,n3max)
  ranks4 <- calculate_ranks(n4,n4max)
  ranks5 <- calculate_ranks(n5,n5max)
  
  data$SEIZURE_SEVERITY <- makelowercase(data$SEIZURE_SEVERITY)
  data$SEIZURE_LENGTH <- makelowercase(data$SEIZURE_LENGTH)
  data$SEIZURE_TYPE <- makelowercase(data$SEIZURE_TYPE)
  data$SEIZURE_VARIABLE <- makelowercase(data$SEIZURE_VARIABLE)
  data$SEIZURE_CLUSTER <- makelowercase(data$SEIZURE_CLUSTER)
  
  values <- seizure_sum(data,ranks1,ranks2,ranks3,ranks4,ranks5)
  values <- cbind(as.numeric(data$DAY_TYPE),values[,1:5],as.numeric(data$SEIZURE_NUMBER),values[,6])
  
  values[,8] <- (values[,7]*values[,8])
  colnames(values)[7:8] <- c("number","sum")
  
  values <- data.frame(data$DATE,values)
  colnames(values)[c(1,2)] <- c("date","day_type")
  
  print("Calculating sums for missing days. Please wait...")
  
  values1 <- missing_sums(values[values$day_type==1,],data[data$DAY_TYPE==1,colnames(data)=="S_DATA_QUALITY"])
  values2 <- missing_sums(values[values$day_type!=1,],data[data$DAY_TYPE!=1,colnames(data)=="S_DATA_QUALITY"])
  values1 <- data.frame(values1)
  values2 <- data.frame(values2)
  colnames(values1) <- colnames(values)
  colnames(values2) <- colnames(values1)
  values <- rbind.data.frame(values1,values2)
  
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
          } else if (values[(i+j),1] != values[i,1]) {
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
  colnames(seizure_load)[2] <- "DATE"
  colnames(seizure_load)[3] <- "DAY_TYPE"
  colnames(seizure_load)[4] <- "SEIZURE_SEVERITY"
  colnames(seizure_load)[5] <- "SEIZURE_LENGTH"
  colnames(seizure_load)[6] <- "SEIZURE_TYPE"
  colnames(seizure_load)[7] <- "SEIZURE_VARIABLES"
  colnames(seizure_load)[8] <- "SEIZURE_CLUSTER"
  colnames(seizure_load)[9] <- "SEIZURE_NUMBER"
  colnames(seizure_load)[10] <- "SEIZURE_LOAD"
  colnames(seizure_load)[11] <- "SEIZURE_NUMBER_DAY"
  colnames(seizure_load)[12] <- "SEIZURE_LOAD_DAY"
  
  ## Check to see if number of rows for missing days in the source table differs from number of such rows in load table
  ## If so, pad the source table with the appropriate number of rows in the appropriate locations so that the two
  ## Tables can be combined
  if (dim(savedata[savedata$S_DATA_QUALITY==3,])[1] != dim(seizure_load[seizure_load$DATE %in% savedata[savedata$S_DATA_QUALITY==3,colnames(savedata)=="DATE"],])[1]) {
    for (j in unique(savedata[savedata$S_DATA_QUALITY==3,colnames(savedata)=="DATE"])) {
      if (dim(seizure_load[seizure_load$DATE==j,])[1] > 1) {
        numrowsload <- nrow(seizure_load[seizure_load$DATE==j,])
        newrows <- data.frame(mrnumber,savedata[savedata$DATE==j,colnames(savedata)=="DATE"],savedata[savedata$DATE==j,colnames(savedata)=="DAY_TYPE"],3,NA,NA,NA,NA,NA,NA)
        newrows <- as.data.frame(lapply(newrows,rep,numrowsload-1))
        colnames(newrows) <- colnames(savedata)
        savedata <- rbind.data.frame(savedata[savedata$DATE<=j,],newrows,savedata[savedata$DATE>j,])
      }
    }
  }
  
  load <- data.frame(savedata,seizure_load[,c(4,5,6,7,8,9,10)])
  colnames(load)[4] <- "S_DAY_QUALITY"
  colnames(load)[11] <- "SEIZURE_SEVERITY_RANK"
  colnames(load)[12] <- "SEIZURE_LENGTH_RANK"
  colnames(load)[13] <- "SEIZURE_TYPE_RANK"
  colnames(load)[14] <- "SEIZURE_VARIABLE_RANK"
  colnames(load)[15] <- "SEIZURE_CLUSTER_RANK"
  colnames(load)[16] <- "SEIZURE_NUMBER_SEIZURE"
  colnames(load)[17] <- "SEIZURE_LOAD_SEIZURE"
  
  observe_load <- FALSE
  print("Would you like to save a temporary file to look at the seizure loads?")
  print("Type 'YES' to save a file to look at, type 'NO' to move onto next step")
  rl <- " "
  while (tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if (tolower(rl)=="yes") {
    observe_load <- TRUE
  }
  if (observe_load == TRUE) {
    print(paste("Saving seizure load table as",gsub(" ","",paste(patient,"_SEIZURE_LOAD.xlsx")),"in directory",directory))
    xlsx <- "SEIZURE_LOAD.xlsx"
    xlsx <- gsub(" ","",paste(patient,"_",xlsx))
    write.xlsx2(load,file=xlsx,showNA=FALSE,row.names=FALSE)
    print("Type 'OKAY' whenever you are ready to move on to the next step")
    print("Or type 'QUIT' if you would like to exit")
    while (tolower(rl)!="okay" && tolower(rl)!="quit") {
      rl <- readline(prompt="Enter here: ")
    }
  } else {
    rl <- "okay"
  }
  if (tolower(rl)=="okay") {
    calculate_seizure_response(patient,data,seizure_load,load,ranking)
  }
}