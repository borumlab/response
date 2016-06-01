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
        upsumday <- rbind(upsums,upday,upcount)
        
        if (up != 0) {
          c <- upsums
          d <- upday
          upsums <- c()
          upday <- c()
          upcount <- c()
          for (s in 0:(upper-1)) {
            upsums <- c(upsums,c[d[]==(s%%(max(d)+1))])
            upday <- c(upday,d[d[]==(s%%(max(d)+1))])
            upcount <- c(upcount,rep(s,length(upsums)-length(upcount)))
          }
          upsumday <- rbind(upsums,upday,upcount)
        }
        
        
      } else {
        upsumday <- c()
      }
      
      if (lower != 0) {
        
        for (l in (a+k):(dim(x)[1])) {
          
          if (as.numeric(as.character(y[l])) != 3) {
            lowsums <- c(lowsums,x[l,9])
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
        lowsumday <- rbind(lowsums,lowday,lowcount)
        
        if (lo != 0) {
          c <- lowsums
          d <- lowday
          lowsums <- c()
          lowday <- c()
          lowcount <- c()
          for (s in 0:(lower-1)) {
            lowsums <- c(c[d[]==(s%%(max(d)+1))],lowsums)
            lowday <- c(d[d[]==(s%%(max(d)+1))],lowday)
            lowcount <- c(rep(s,length(lowsums)-length(lowcount)),lowcount)
          }
          lowsumday <- rbind(lowsums,lowday,lowcount)
        }
        
      } 
      
      temp <- data.frame(x[-(a:(a+k-1)),])
      colnames(temp) <- colnames(x)
      
      if ((dim(lowernadates)[1] > 0)) {
        new <- c()
        for (n in 1:(dim(lowernadates)[1])) {
          newrows <- data.frame(lowernadates[n,1],unique(x[x[,1]==lowernadates[n,1],2]),NA,NA,NA,NA,NA,NA,lowsumday[1,lowsumday[3,]==n-1])
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
          newrows <- data.frame(uppernadates[n,1],unique(x[x[,1]==uppernadates[n,1],2]),NA,NA,NA,NA,NA,NA,upsumday[1,upsumday[3,]==t])
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

seizure_load_calculation <- function() {
  
  print("Input the directory in which the patient data file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir1 <- readline(prompt="Enter here: ")
  print("Input the name of the patient data file you would like to use (leave off the .csv)")
  print("Example: filename")
  data <- readline(prompt="Enter here: ")
  
  print("Input the directory in which the ranking table file you would like to use can be found")
  print("Example: C:/Folder_Name/")
  dir2 <- readline(prompt="Enter here: ")
  print("Input the name of the ranking table file you would like to use (leave off the .csv)")
  print("Example: filename")
  ranking <- readline(prompt="Enter here: ")
  
  data <- gsub(" ","",paste(data,".csv"))
  data <- gsub("/ ","/",paste(dir1,data))
  data <- read.csv(data,header=TRUE)
  ranking <- gsub(" ","",paste(ranking,".csv"))
  ranking <- gsub("/ ","/",paste(dir2,ranking))
  ranking <- read.csv(ranking,header=TRUE)
  
  data[,2] <- substr(data[,2],1,nchar(as.character(data[,2]))-5)
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
  
  load <- rep(0,dim(values)[1])
  
  print("Now calculating daily seizure loads. Please wait...")
  
  k <- 0
  for (i in 1:(dim(values)[1])) {
    if (k > 0) {
      k <- k - 1
    } else {
      k <- 0
      load[i] <- values[i,9]
      if (i != dim(values)[1]) {
        for (j in 1:(dim(values)[1]-i)) {
          if (values[(i+j),1] == values[i,1]) {
            load[(i+j)] <- NA
            k <- k + 1
          } else if (values[(i+j),1] != values[i,9]) {
            break
          }
        }
      }
      if (k > 0) {
        for (m in 1:k) {
          load[i] <- as.numeric(load[i]) + as.numeric(values[(i+m),9])
        }
      }
    }
  }
  
  values <- data.frame(values,load)
  colnames(values)[1] <- "date"
  
  print("Set the work directory in which you would like to save this file")
  wd <- readline(prompt="Enter here: ")
  setwd(wd)
  print("Give the name you would like to give to the file. Make sure to add the .csv at the end")
  csv <- readline(prompt="Enter here: ")
  write.csv(values,file=csv,na="")
}