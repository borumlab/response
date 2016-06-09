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

update_seizure_loads <- function() {
  
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
  
  print("Input the name of the patient data file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  data <- readline(prompt="Enter here: ")
  
  data <- gsub(" .xlsx",".xlsx",paste(data,".xlsx"))
  data <- gsub(" ","",paste(patient,"_",data))
  data <- readWorksheetFromFile(data,sheet=1)
  
  print("Input the name of the ranking table file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  ranking <- readline(prompt="Enter here: ")
  
  ranking <- gsub(" .xlsx",".xlsx",paste(ranking,".xlsx"))
  ranking <- gsub(" ","",paste(patient,"_",ranking))
  ranking <- readWorksheetFromFile(ranking,sheet=1)
  
  print("Input the name of the seizure load table file you would like to use (leave off the .xlsx and the patient letters)")
  print("Example: filename")
  load <- readline(prompt="Enter here: ")
  
  load <- gsub(" .xlsx",".xlsx",paste(load,".xlsx"))
  load <- gsub(" ","",paste(patient,"_",load))
  load <- readWorksheetFromFile(load,sheet=1)  
  
  print("Input the first new date that you would like to have added or updated in the seizure load file")
  print("Format date in this manner: year, then month, then day (all numeric), seperating each with either all '/' or all '-'")
  print("Example: 2016/1/5 or 2016-1-5 (January 5th, 2016)")
  start.date <- readline(prompt="Enter here: ")
  
  print("Input the first new date that you would like to have added or updated in the seizure load file")
  print("Format date in this manner: year, then month, then day (all numeric), seperating each with either all '/' or all '-'")
  print("Example: 2016/1/5 or 2016-1-5 (January 5th, 2016)")
  end.date <- readline(prompt="Enter here: ")
  
  print("Determining days to add/update. Please wait...")
  
  mrnumber <- unique(data[,1])
  data <- data[!is.na(data[,1]),]
  for (i in 1:length(data[,1])) {
    if (nchar(as.character(data[i,2]))>10) {
      data[,2] <- substr(data[,2],1,nchar(as.character(data[,2]))-5)
    }
  }
  data[,2] <- as.Date(data[,2],format="%m/%d/%Y")
  
  load[,2] <- as.Date(load[,2],format="%m/%d/%Y")
  
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  
  store.data <- data
  data <- unique(data[,c(2,4)])
  rownames(data) <- c(1:(dim(data)[1]))
  
  
  ## Look at the subset of the load sheet being updated/added and determine the start and end dates of
  ## the range required to do calculations for any missing dates
  index <- cbind(rep(NA,(dim(data[data[,1]>=start.date & data[,1]<=end.date,])[1])),NA)
  for (i in dim(data[data[,1]<=end.date,])[1]:(dim(data[data[,1]<start.date,])[1]+1)) {
    if (data[i,2] == 3) {
      if (length(index[is.na(index[,1]),1])==length(index[is.na(index[,2]),2])) {
        index[i-(dim(data[data[,1]<start.date,])[1]+1)+1,1] <- i
      }
    } else {
      index[!is.na(index[,1]) & is.na(index[,2]),2] <- i+1
    }
  }
  index[!is.na(index[,1]) & is.na(index[,2]),2] <- dim(data[data[,1]<start.date,])[1]+1
  
  redo <- c(data[,1][length(data[,1])]+1,data[,1][1]-1)
  
  index.min <- min(index[!is.na(index)])
  index.max <- max(index[!is.na(index)])
  if (max(as.numeric(rownames(data[data[,1]>=start.date & data[,1]<=end.date,])))==index.max) {
    temp <- min(as.numeric(rownames(data)[data[,1]>end.date & data[,2]==1]))
    len <- temp - index.max
    upper <- ceiling(len/2)
    lower <- floor(len/2)
    redo[2] <- data[as.numeric(rownames(data))>index.max & data[,2]!=3,][lower,1]
  }
  if (min(as.numeric(rownames(data[data[,1]>=start.date & data[,1]<=end.date,])))==index.min) {
    temp <- max(as.numeric(rownames(data)[data[,1]<start.date & data[,2]==1]))
    len <- index.min - temp
    upper <- ceiling(len/2)
    lower <- floor(len/2)
    redo[2] <- data[as.numeric(rownames(data))<index.min & data[,2]!=3,][dim(data[as.numeric(rownames(data))<index.min & data[,2]!=3,])[1]-upper+1,1]
  }
  
  if (dim(index[is.na(index[,1]),])[1]!=dim(index)[1]) {
    for (j in index[!is.na(index[,1]),1]) {
      len <- j-index[!is.na(index[,1]) & index[,1]==j,2]+1
      lower <- floor(len/2)
      upper <- ceiling(len/2)
      if (index[!is.na(index[,1]) & index[,1]==j,2]-upper < dim(data[data[,1]<start.date,])[1]+1) {
        if (dim(data[as.numeric(rownames(data))<(j+1-len) & data[,2]!=3,])[1] < upper) {
          redo[1] <- data[1,1]
        } else if (data[as.numeric(rownames(data))<(j+1-len) & data[,2]!=3,][dim(data[as.numeric(rownames(data))<(j+1-len) & data[,2]!=3,])[1]-upper+1,1] < redo[1]) {
          redo[1] <- data[as.numeric(rownames(data))<(j+1-len) & data[,2]!=3,][dim(data[as.numeric(rownames(data))<(j+1-len) & data[,2]!=3,])[1]-upper+1,1]
        }
      }
      if (j+lower > dim(data[data[,1]<=end.date,])[1]) {
        if (dim(data[as.numeric(rownames(data))>j & data[,2]!=3,])[1] < lower) {
          redo[2] <- data[dim(data)[1],1]
        } else if (data[as.numeric(rownames(data))>j & data[,2]!=3,][lower,1] > redo[2]) {
          redo[2] <- data[as.numeric(rownames(data))>j & data[,2]!=3,][lower,1]
        }
      }
    }
  }
  if (redo[1] == data[,1][length(data[,1])]+1) {
    redo[1] <- data[dim(data[data[,1]<start.date,])[1]+1,1]
  }
  if (redo[2] == data[,1][1]-1) {
    redo[2] <- data[dim(data[data[,1]<=end.date,])[1],1]
  }
  
  
  ## Look at all dates preceding the range of dates being added/updated, and determine if the 
  ## calculation of any missing dates will need to be redone due to the addition of the new calculations
  index <- data.frame(last=numeric(),first=numeric())
  if (redo[1] != data[1,1] && start.date > data[1,1]) {
    subset <- as.numeric(rownames(data[data[,2]==3 & data[,1]<start.date,]))
    if (length(subset) > 0) {
      first <- subset[1]
      last <- 0
      for (i in subset) {
        if (first %in% index[,2]) {
          first <- i
        }
        if (!((i+1) %in% subset)) {
          last <- i
          newrow <- data.frame(last,first)
          colnames(newrow) <- c("last","first")
          index <- data.frame(rbind(index,newrow))
        } 
      }
      for (j in (dim(index)[1]):1) {
        len <- index[j,1]-index[j,2]+1
        upper <- ceiling(len/2)
        lower <- floor(len/2)
        if (lower > dim(data[data[,1]<start.date 
                      & data[,1]>data[index[j,1],1]
                      & data[,2]!= 3,])[1]) {
          if (dim(data[data[,1]<data[index[j,2],1] & data[,2]!=3,])[1] < upper) {
            redo[1] <- data[1,1]
          } else if (data[data[,1]<data[index[j,2],1] & data[,2]!= 3,][dim(data[data[,1]<data[index[j,2],1] & data[,2]!= 3,])[1]-upper+1,1] < redo[1]) {
            redo[1] <- data[data[,1]<data[index[j,2],1] & data[,2]!= 3,][dim(data[data[,1]<data[index[j,2],1] & data[,2]!= 3,])[1]-upper+1,1]
          }
        }
      }
    }
  }
  
  
  ## Look at all dates following the range of dates being added/updated, and determine if the 
  ## calculation of any missing dates will need to be redone due to the addition of the new calculations
  index <- data.frame(last=numeric(),first=numeric())
  if (redo[2]!=data[length(data[,1]),1] && end.date > data[length(data[,1]),1]) {
    subset <- as.numeric(rownames(data[data[,2]==3 & data[,1]>end.date,]))
    if (length(subset) > 0) {
      first <- subset[1]
      last <- 0
      for (i in subset) {
        if (first %in% index[,2]) {
          first <- i
        }
        if (!((i+1) %in% subset)) {
          last <- i
          newrow <- data.frame(last,first)
          colnames(newrow) <- c("last","first")
          index <- data.frame(rbind(index,newrow))
        } 
      }
      for (j in 1:(dim(index)[1])) {
        len <- index[j,1]-index[j,2]+1
        upper <- ceiling(len/2)
        lower <- floor(len/2)
        if (upper > dim(data[data[,1]>end.date 
                             & data[,1]<data[index[j,2],1]
                             & data[,2]!= 3,])[1]) {
          if (dim(data[data[,1]>data[index[j,1],1] & data[,2]!=3,])[1] < lower) {
            redo[2] <- data[length(data[,1]),1]
          } else if (data[data[,1]>data[index[j,1],1] & data[,2]!= 3,][lower,1] > redo[2]) {
            redo[2] <- data[data[,1]>data[index[j,1],1] & data[,2]!= 3,][lower,1]
          }
        }
      }
    }
  }
  
  
  ## Calculate seizure sums and number sums for this subset
  print("Calculating sums for missing days. Please wait...")
  
  subset <- load[load[,2]>=redo[1] & load[,2]<=redo[2],-c(dim(load)[2],dim(load)[2]-1)]
  if (dim(subset[is.na(subset[,5]),])[1] > 0) {
    subset[is.na(subset[,5]),c(9,10)] <- NA
    rows <- rownames(unique(subset[is.na(subset[,5]),]))
    s <- subset[is.na(subset[,5]) & as.numeric(rownames(subset)) %in% rows,]
    subset <- subset[!(is.na(subset[,5])),]
    colnames(s) <- colnames(subset)
    subset <- data.frame(rbind(subset,s))
    subset <- subset[order(subset[,2],decreasing=FALSE),]
  }
  #return(subset)
  day.quality <- data.frame(subset[,2],NA)
  seq <- seq(1:length(day.quality[,1]))
  for (i in seq) {
    day.quality[i,2] <- data[data[,1]==day.quality[i,1],2]
  }
  day.quality <- as.numeric(day.quality[,2])
  
  subset <- subset[,-1]
  values <- missing_sums(subset,day.quality)
  colnames(values)[c(1,2)] <- c("date","day type")
  MRNUMBER <- data.frame(rep(mrnumber,dim(values)[1]))
  colnames(MRNUMBER) <- "MRNUMBER"
  values <- data.frame(MRNUMBER,values)
  colnames(values)[1] <- "MRNUMBER"
  
  seizure.load.per.day <- rep(NA,dim(values)[1])
  seizure.number.per.day <- rep(NA,dim(values)[1])
  
  
  ## Calculate daily seizure and number loads for this subset
  print("Now calculating daily seizure loads. Please wait...")
  
  k <- 0
  for (i in 1:(dim(values)[1])) {
    if (k > 0) {
      k <- k - 1
    } else {
      k <- 0
      seizure.load.per.day[i] <- values[i,10]
      seizure.number.per.day[i] <- values[i,9]
      if (i != dim(values)[1]) {
        for (j in 1:(dim(values)[1]-i)) {
          if (values[(i+j),2] == values[i,2]) {
            k <- k + 1
          } else if (values[(i+j),2] != values[i,2]) {
            break
          }
        }
      }
      if (k > 0) {
        for (m in 1:k) {
          seizure.load.per.day[i] <- as.numeric(seizure.load.per.day[i]) + as.numeric(values[(i+m),10])
          seizure.number.per.day[i] <- as.numeric(seizure.number.per.day[i]) + as.numeric(values[(i+m),9])
        }
      }
    }
  }
  
  seizure_load_replace <- data.frame(cbind(values,seizure.number.per.day,seizure.load.per.day))
  colnames(seizure_load_replace)[1] <- "MRNUMBER"
  colnames(seizure_load_replace)[2] <- "date"
  
  
  ## Place this table of new/updated loads into the proper section of the existing load table (removing 
  ## the old values of any days that were already in the table but updated)
  load <- load[load[,2]<redo[1] | load[,2]>redo[2],]
  if (redo[2] < load[1,2]) {
    load <- data.frame(rbind(seizure_load_replace,load))
  } else if (redo[1] > load[length(load[,2]),2]) {
    load <- data.frame(rbind(load,seizure_load_replace))
  } else {
    load <- data.frame(rbind(load[load[,2]<redo[1],],seizure_load_replace,load[load[,2]>redo[2],]))
  }
  rownames(load) <- c(1:length(load[,1]))
  
  print("Give the name you would like to give to the file (leave off the .xlsx and the patient letters)")
  xlsx <- readline(prompt="Enter here: ")
  xlsx <- gsub(" .xlsx",".xlsx",paste(xlsx,".xlsx"))
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx2(load,file=xlsx,showNA=FALSE,row.names=FALSE)
}