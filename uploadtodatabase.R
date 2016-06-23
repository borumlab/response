uploadtodatabase <- function(data,str,sourcedata,table) {
  
  if (!require("RMySQL")) {
    install.packages("RMySQL")
  }
  library(RMySQL)
  
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons) {
    dbDisconnect(con)
  }
  
  str <- str
  data <- data
  
  print("When prompted, please input the MySQL username, password, database name, and host that you wish to use")
  print("To leave any of these fields blank, press the enter key without typing anything else")
  u <- readline(prompt="User: ")
  p <- readline(prompt="Password: ")
  d <- readline(prompt="Database Name: ")
  h <- readline(prompt="Host: ")
  connect <- dbConnect(MySQL(),user=u,password=p,dbname=d,host=h)
  
  if (str=="SEIZURE") {
    MySQLstatement <- paste("SELECT COUNT(MRNUMBER) AS VALUE FROM seizure_ranking_id_source WHERE MRNUMBER =",unique(data$MRNUMBER),";")
    is.empty <- data.frame(dbGetQuery(connect,MySQLstatement))
    if (!is.na(is.empty$VALUE[1])) {
      MySQLstatement <- paste("DELETE FROM seizure_ranking_id_source WHERE MRNUMBER =",unique(data$MRNUMBER),";")
    } 
    subset <- table[grepl("SEIZURE",table$SEIZURE_PARAMETER),c(1,2,3,4)]
    dbWriteTable(connect,value=subset,name="seizure_ranking_id_source",append=TRUE)
  }
  
  if (str=="SEIZURE") {
    MySQLstatement <- paste("SELECT MAX(DATE) AS LAST_DATE FROM seizure_data_id_research WHERE MRNUMBER =",unique(data$MRNUMBER),";")
    getlastdate <- data.frame(dbGetQuery(connect,MySQLstatement))
    if (is.na(getlastdate$LAST_DATE[1])) {
      print(paste("There is no data in the source table for this patient with mrnumber",unique(data$MRNUMBER)))
    } else {
      print(paste("The last date found in the source table for this patient with mrnumber",unique(data$MRNUMBER),"is:",getlastdate$LAST_DATE[1]))
    }
    
    MySQLstatement <- paste("SELECT MAX(DATE) AS LAST_DATE FROM seizure_data_id_calculated WHERE MRNUMBER =",unique(data$MRNUMBER),";")
    getlastdate <- data.frame(dbGetQuery(connect,MySQLstatement))
    if (is.na(getlastdate$LAST_DATE[1])) {
      print(paste("There is no data in the research table for this patient with mrnumber",unique(data$MRNUMBER)))
    } else {
      print(paste("The last date found in the research table for this patient with mrnumber",unique(data$MRNUMBER),"is:",getlastdate$LAST_DATE[1]))
    }
  } else if (str=="MED") {
    MySQLstatement <- paste("SELECT MAX(DATE) AS LAST_DATE FROM med_data_id_research WHERE MRNUMBER =",unique(data$MRNUMBER),";")
    getlastdate <- data.frame(dbGetQuery(connect,MySQLstatement))
    if (is.na(getlastdate$LAST_DATE[1])) {
      print(paste("There is no data in the source table for this patient with mrnumber",unique(data$MRNUMBER)))
    } else {
      print(paste("The last date found in the source table for this patient with mrnumber",unique(data$MRNUMBER),"is:",getlastdate$LAST_DATE[1]))
    }
    
    MySQLstatement <- paste("SELECT MAX(DATE) AS LAST_DATE FROM med_data_id_calculated WHERE MRNUMBER =",unique(data$MRNUMBER),";")
    getlastdate <- data.frame(dbGetQuery(connect,MySQLstatement))
    if (is.na(getlastdate$LAST_DATE[1])) {
      print(paste("There is no data in the research table for this patient with mrnumber",unique(data$MRNUMBER)))
    } else {
      print(paste("The last date found in the research table for this patient with mrnumber",unique(data$MRNUMBER),"is:",getlastdate$LAST_DATE[1]))
    }
  }
  
  print("Please specify the range of dates that you would like to have added to the table")
  print("Format date in this manner: year, then month, then day (all numeric), seperating each with either all '/' or all '-'")
  print("Example: 2016/1/5 or 2016-1-5 (January 5th, 2016)")
  first <- readline(prompt="First date: ")
  last <- readline(prompt="Last date: ")
  first <- as.Date(first)
  last <- as.Date(last)
  
  #MySQLstatement <- paste("SELECT COUNT(MRNUMBER) AS COUNT FROM",t,"WHERE MRNUMBER =",unique(data$MRNUMBER),";")
  #query <- data.frame(dbGetQuery(connect,MySQLstatement))
  if (str=="SEIZURE") {
    subset1 <- sourcedata[sourcedata$DATE>=first & sourcedata$DATE<=last,]
    subset2 <- data[data$DATE>=first & data$DATE<=last,c(1:8)]
    dbWriteTable(connect,value=subset1,name="seizure_data_id_calculated",append=TRUE)
    dbWriteTable(connect,value=subset2,name="seizure_data_id_research",append=TRUE)
  } else if (str=="MED") {
    subset1 <- sourcedata[sourcedata$DATE>=first & sourcedata$DATE<=last,]
    subset2 <- data[data$DATE>=first & data$DATE<=last,c(1:7)]
    dbWriteTable(connect,value=subset1,name="med_data_id_calculated",append=TRUE)
    dbWriteTable(connect,value=subset2,name="med_data_id_research",append=TRUE)
  }
  
  print("Your tables in MySQL have been updated")
}