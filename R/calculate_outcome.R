#' Calculate outcome
#'
#' This function allow you to calculate a patient's outcome (i.e. it creates the OUTCOME_DATA_CLINICAL
#' file and the MED_SEIZURE_OUTCOME_BAR_TABLE files, as well as creates bar graphs for seizure score,
#' med score, and outcome for each period of 30 days)
#' @keywords outcome
#' @export
#' @import ggplot2
#' @import openxlsx
#' @examples
#' calculate_outcome()

calculate_outcome <- function() {
  
  print("Input the identifier that signify the patient we are doing calculations for")
  print("Example: FILA")
  patient <- readline(prompt="Enter here: ")
  
  # Directory for the clinic visit source file: G:\MySQL Database\Demographics
  print("Input the directory that you wish to draw the CLINIC_VISIT_SOURCE file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  cvs <- "CLINIC_VISIT_SOURCE.xlsx"
  cvs <- read.xlsx(cvs,sheet=1,detectDates=TRUE)
  
  print("Type 'yes' if the DEMOGRAPHICS_SOURCE file can be found in the same folder as the CLINIC_VISIT_SOURCE table. Type 'no' if it is in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to draw the DEMOGRAPHICS_SOURCE file from")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  
  demo <- "DEMOGRAPHICS_SOURCE.xlsx"
  demo <- read.xlsx(demo,sheet=1,detectDates=TRUE)
  
  print("Type 'yes' if this patient's SEIZURE_DATA_CLINICAL file can be found in the same folder as the DEMOGRAPHICS_SOURCE table. Type 'no' if it is in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to draw this patient's SEIZURE_DATA_CLINICAL file from")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  
  s.clinical <- "SEIZURE_DATA_CLINICAL.xlsx"
  s.clinical <- gsub(" ","",paste(patient,"_",s.clinical))
  s.clinical <- read.xlsx(s.clinical,sheet=1,detectDates=TRUE)
  
  print("Type 'yes' if this patient's MED_DATA_CLINICAL file can be found in the same folder as this patient's SEIZURE_DATA_CLINICAL table. Type 'no' if it is in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to draw this patient's MED_DATA_CLINICAL file from")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  
  m.clinical <- "MED_DATA_CLINICAL.xlsx"
  m.clinical <- gsub(" ","",paste(patient,"_",m.clinical))
  m.clinical <- read.xlsx(m.clinical,sheet=1,detectDates=TRUE)
  
  mrnumber <- unique(s.clinical$MRNUMBER)
  
  name <- unique(demo[demo$MRNUMBER==mrnumber,colnames(demo)=="FIRST"])
  cvs <- cvs[!is.na(cvs$MRNUMBER),]
  cvs <- unique(cvs[cvs$MRNUMBER==mrnumber,colnames(cvs)=="MRNUMBER" | colnames(cvs)=="DATE"])
  cvs$DATE <- as.Date(cvs$DATE)
  
  ## Create and save FILA_OUTCOME_DATA_CLINICAL.xlsx
  outcome <- data.frame(MRNUMBER=integer(),DATE=as.Date(as.character()),DAY_TYPE=integer(),OUTCOME_DAY=integer(),OUTCOME_30_DAYS=integer())
  outcome[1:dim(s.clinical)[1],] <- c(s.clinical[,colnames(s.clinical)=="MRNUMBER" | colnames(s.clinical)=="DATE" | colnames(s.clinical)=="DAY_TYPE"],NA,NA)
  outcome[,4] <- (s.clinical$SEIZURE_RESPONSE_DAY+m.clinical$MED_RESPONSE_DAY)/2
  outcome[,5] <- (s.clinical$SEIZURE_SCORE_30_DAYS+m.clinical$MED_SCORE_30_DAYS)/2
  
  print("Type 'yes' if you wish to save the OUTCOME_DATA_CLINICAL file in the same folder as this patient's MED_DATA_CLINICAL table. Type 'no' if you would like for it to be in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to save this patient's OUTCOME_DATA_CLINICAL file in")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  print(paste("Saving data clinical outcome table as",gsub(" ","",paste(patient,"_OUTCOME_DATA_CLINICAL.xlsx")),"in directory",getwd()))
  xlsx <- "OUTCOME_DATA_CLINICAL.xlsx"
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  xlsx::write.xlsx2(outcome,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  bar_table_dates <- cvs$DATE[which(cvs$DATE %in% outcome$DATE)]
  mrn <- cvs$MRNUMBER[1:length(bar_table_dates)]
  bar_table_dates <- cbind.data.frame(mrn,bar_table_dates)
  seizure_bar_table <- data.frame(MRNUMBER=integer(),START_DATE=as.Date(as.character()),END_DATE=as.Date(as.character()),SEIZURE_0_TO_10=integer(),SEIZURE_10_TO_80=integer(),SEIZURE_GREATER_THAN_80=integer())
  seizure_bar_table[1:dim(bar_table_dates)[1],] <- NA
  seizure_bar_table$MRNUMBER <- mrn
  seizure_bar_table$START_DATE <- bar_table_dates$bar_table_dates
  seizure_bar_table$END_DATE[1:dim(seizure_bar_table)[1]-1] <- bar_table_dates$bar_table_dates[2:dim(seizure_bar_table)[1]]-1
  seizure_bar_table$END_DATE[dim(seizure_bar_table)[1]] <- max(outcome$DATE)
  
  med_bar_table <- data.frame(MED_0_TO_10=integer(),MED_10_TO_80=integer(),MED_GREATER_THAN_80=integer())
  med_bar_table[1:dim(bar_table_dates)[1],] <- NA
  
  outcome_bar_table <- data.frame(OUTCOME_0_TO_10=integer(),OUTCOME_10_TO_80=integer(),OUTCOME_GREATER_THAN_80=integer())
  outcome_bar_table[1:dim(bar_table_dates)[1],] <- NA
  
  dates <- data.frame(TABLEDATES=as.Date(as.character()))
  dates[1:(length(seizure_bar_table$START_DATE)+1),] <- NA
  dates[1:length(seizure_bar_table$START_DATE),] <- seizure_bar_table$START_DATE
  dates[dim(dates)[1],] <- seizure_bar_table$END_DATE[length(seizure_bar_table$END_DATE)]+1
  for (j in 2:length(dates$TABLEDATES)) {
    
    ## Create table of values used to create FILA_SEIZURE_BAR_GRAPH.jpeg
    
    s.totalnumdays <- nrow(s.clinical[s.clinical$DATE>=dates$TABLEDATES[j-1] & s.clinical$DATE<dates$TABLEDATES[j],])
   
    s.total0to10days <- nrow(s.clinical[s.clinical$DATE>=dates$TABLEDATES[j-1] & s.clinical$DATE<dates$TABLEDATES[j] & s.clinical$SEIZURE_RESPONSE_DAY>=0 & s.clinical$SEIZURE_RESPONSE_DAY<=10,])/s.totalnumdays
    s.total10to80days <- nrow(s.clinical[s.clinical$DATE>=dates$TABLEDATES[j-1] & s.clinical$DATE<dates$TABLEDATES[j] & s.clinical$SEIZURE_RESPONSE_DAY>10 & s.clinical$SEIZURE_RESPONSE_DAY<=80,])/s.totalnumdays
    s.totalgreater80days <- nrow(s.clinical[s.clinical$DATE>=dates$TABLEDATES[j-1] & s.clinical$DATE<dates$TABLEDATES[j] & s.clinical$SEIZURE_RESPONSE_DAY>80,])/s.totalnumdays
  
    seizure_bar_table$SEIZURE_0_TO_10[j-1] <- round(s.total0to10days*100,2)
    seizure_bar_table$SEIZURE_10_TO_80[j-1] <- round(s.total10to80days*100,2)
    seizure_bar_table$SEIZURE_GREATER_THAN_80[j-1] <- round(s.totalgreater80days*100,2)
    
    ## Create table of values used to create FILA_MED_BAR_GRAPH.jpeg
    
    m.totalnumdays <- nrow(m.clinical[m.clinical$DATE>=dates$TABLEDATES[j-1] & m.clinical$DATE<dates$TABLEDATES[j],])
   
    m.total0to10days <- nrow(m.clinical[m.clinical$DATE>=dates$TABLEDATES[j-1] & m.clinical$DATE<dates$TABLEDATES[j] & m.clinical$MED_RESPONSE_DAY>=0 & m.clinical$MED_RESPONSE_DAY<=10,])/m.totalnumdays
    m.total10to80days <- nrow(m.clinical[m.clinical$DATE>=dates$TABLEDATES[j-1] & m.clinical$DATE<dates$TABLEDATES[j] & m.clinical$MED_RESPONSE_DAY>10 & m.clinical$MED_RESPONSE_DAY<=80,])/m.totalnumdays
    m.totalgreater80days <- nrow(m.clinical[m.clinical$DATE>=dates$TABLEDATES[j-1] & m.clinical$DATE<dates$TABLEDATES[j] & m.clinical$MED_RESPONSE_DAY>80,])/m.totalnumdays
    
    med_bar_table$MED_0_TO_10[j-1] <- round(m.total0to10days*100,2)
    med_bar_table$MED_10_TO_80[j-1] <- round(m.total10to80days*100,2)
    med_bar_table$MED_GREATER_THAN_80[j-1] <- round(m.totalgreater80days*100,2)
    
    ## Create table of values used to create FILA_OUTCOME_BAR_GRAPH.jpeg
    
    o.totalnumdays <- nrow(outcome[outcome$DATE>=dates$TABLEDATES[j-1] & outcome$DATE<dates$TABLEDATES[j],])
    
    o.total0to10days <- nrow(outcome[outcome$DATE>=dates$TABLEDATES[j-1] & outcome$DATE<dates$TABLEDATES[j] & outcome$OUTCOME_DAY>=0 & outcome$OUTCOME_DAY<=10,])/o.totalnumdays
    o.total10to80days <- nrow(outcome[outcome$DATE>=dates$TABLEDATES[j-1] & outcome$DATE<dates$TABLEDATES[j] & outcome$OUTCOME_DAY>10 & outcome$OUTCOME_DAY<=80,])/o.totalnumdays
    o.totalgreater80days <- nrow(outcome[outcome$DATE>=dates$TABLEDATES[j-1] & outcome$DATE<dates$TABLEDATES[j] & outcome$OUTCOME_DAY>80,])/o.totalnumdays
    
    outcome_bar_table$OUTCOME_0_TO_10[j-1] <- round(o.total0to10days*100,2)
    outcome_bar_table$OUTCOME_10_TO_80[j-1] <- round(o.total10to80days*100,2)
    outcome_bar_table$OUTCOME_GREATER_THAN_80[j-1] <- round(o.totalgreater80days*100,2)
  }
  
  s.graph <- function(s.bar_graph,t,di) {
    if (di==TRUE) {
      s.bar_graph$PERIOD <- seq(1:(length(s.bar_graph$PERIOD)))
      s.bar_graph$PERIOD <- paste("Month",s.bar_graph$PERIOD)
    }
    seizure_bar_graph <- ggplot(data=s.bar_graph,aes(x=PERIOD,y=PERCENT,fill=SECTION,colour=SECTION))
    seizure_bar_graph <- seizure_bar_graph + geom_bar(stat="identity")
    seizure_bar_graph <- seizure_bar_graph + scale_colour_manual(name="",
                                                                 breaks=c("Seizures 0-10%",
                                                                          "Seizures 10-80%",
                                                                          "Seizures >80%"),
                                                                 values=c(
                                                                   "Seizures 0-10%"="black",
                                                                   "Seizures 10-80%"="black",
                                                                   "Seizures >80%"="black"))
    seizure_bar_graph <- seizure_bar_graph + scale_fill_manual(name="", 
                                                               breaks=c("Seizures 0-10%",
                                                                        "Seizures 10-80%",
                                                                        "Seizures >80%"),
                                                               values=c("Seizures 0-10%" = "green",
                                                                        "Seizures 10-80%"= "yellow",
                                                                        "Seizures >80%" = "red"))
    seizure_bar_graph <- seizure_bar_graph + theme(axis.text.x=element_text(angle=45,hjust=1,size = 5, face="bold", color="black"),
                                                   axis.text.y = element_text(color="black"))#vjust=0.5,siz=4))
    seizure_bar_graph <- seizure_bar_graph + labs(title=paste(gsub(" ","",paste(t,"'s:")),"Seizure Score Response"),
                                                  x="Time on Therapy",
                                                  y="% of Time Period on Therapy") +
      theme(plot.title=element_text(hjust=0.5)) 
    seizure_bar_graph <- seizure_bar_graph + theme(axis.title.y=element_text(angle=90,size=9))
    return(seizure_bar_graph)
  }
  
  m.graph <- function(m.bar_graph,t,di) {
    med_bar_graph <- ggplot(data=m.bar_graph,aes(x=PERIOD,y=PERCENT,fill=SECTION,colour=SECTION))
    med_bar_graph <- med_bar_graph + geom_bar(stat="identity")
    med_bar_graph <- med_bar_graph + scale_colour_manual(name="",
                                                         breaks=c("Meds 0-10%",
                                                                  "Meds 10-80%",
                                                                  "Meds >80%"),
                                                         values=c(
                                                           "Meds 0-10%"="black",
                                                           "Meds 10-80%"="black",
                                                           "Meds >80%"="black"))
    med_bar_graph <- med_bar_graph + scale_fill_manual(name="",
                                                       breaks=c("Meds 0-10%",
                                                                "Meds 10-80%",
                                                                "Meds >80%"),
                                                       values=c("Meds 0-10%" = "green",
                                                                "Meds 10-80%"= "yellow",
                                                                "Meds >80%" = "red"))
    med_bar_graph <- med_bar_graph + theme(axis.text.x=element_text(angle=45,hjust=1,size=5, face="bold", color="black"),
                                           axis.text.y = element_text(color="black"))
    med_bar_graph <- med_bar_graph + labs(title=paste(gsub(" ","",paste(t,"'s:")),"Med Score Response"),
                                          x="Time on Therapy",
                                          y="% of Time Period on Therapy") +
      theme(plot.title=element_text(hjust=0.5))
    med_bar_graph <- med_bar_graph + theme(axis.title.y=element_text(angle=90,size=9))
    return(med_bar_graph)
  }
  
  o.graph <- function(o.bar_graph,t,di) {
    outcome_bar_graph <- ggplot(data=o.bar_graph,aes(x=PERIOD,y=PERCENT,fill=SECTION,colour=SECTION))
    outcome_bar_graph <- outcome_bar_graph + geom_bar(stat="identity")
    outcome_bar_graph <- outcome_bar_graph + scale_colour_manual(name="",
                                                                 breaks=c("Outcome 0-10%",
                                                                          "Outcome 10-80%",
                                                                          "Outcome >80%"),
                                                                 values=c(
                                                                   "Outcome 0-10%"="black",
                                                                   "Outcome 10-80%"="black",
                                                                   "Outcome >80%"="black"))
    outcome_bar_graph <- outcome_bar_graph + scale_fill_manual(name="",
                                                               breaks=c("Outcome 0-10%",
                                                                        "Outcome 10-80%",
                                                                        "Outcome >80%"),
                                                               values=c("Outcome 0-10%" = "green",
                                                                        "Outcome 10-80%"= "yellow",
                                                                        "Outcome >80%" = "red"))
    outcome_bar_graph <- outcome_bar_graph + theme(axis.text.x=element_text(angle=45,hjust=1,size=5, face="bold", color="black"),
                                                   axis.text.y = element_text(color="black"))
    outcome_bar_graph <- outcome_bar_graph + labs(title=paste(gsub(" ","",paste(t,"'s:")),"Outcome Score Response"),
                                                  x="Time on Therapy",
                                                  y="% of Time Period on Therapy") +
      theme(plot.title=element_text(hjust=0.5)) 
    outcome_bar_graph <- outcome_bar_graph + theme(axis.title.y=element_text(angle=90,size=9))
    return(outcome_bar_graph)
  }
  
  ## Create and save FILA_SEIZURE_BAR_GRAPH.jpeg
  
  s.bar_graph <- data.frame(PERIOD=character(),PERCENT=integer(),SECTION=character(),stringsAsFactors=FALSE)
  s.bar_graph[1:((dim(seizure_bar_table)[1])*3),c("PERIOD")] <- rep(as.character(seizure_bar_table$START_DATE[1:length(seizure_bar_table$START_DATE)]),each=3)
  s.bar_graph[,c("PERIOD")] <- paste(as.character(seizure_bar_table$START_DATE[ceiling(as.numeric(rownames(s.bar_graph))/3)]),"-",as.character(seizure_bar_table$END_DATE[ceiling(as.numeric(rownames(s.bar_graph))/3)]))
  
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%3==1,c("PERCENT")] <- seizure_bar_table$SEIZURE_0_TO_10
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%3==1,c("SECTION")] <- "Seizures 0-10%"
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%3==2,c("PERCENT")] <- seizure_bar_table$SEIZURE_10_TO_80
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%3==2,c("SECTION")] <- "Seizures 10-80%"
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%3==0,c("PERCENT")] <- seizure_bar_table$SEIZURE_GREATER_THAN_80
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%3==0,c("SECTION")] <- "Seizures >80%"
  s.bar_graph$SECTION <- factor(s.bar_graph$SECTION, levels=c("Seizures >80%", "Seizures 10-80%", "Seizures 0-10%")) #edit so that the bars in the bar graph are stacked in order 
  
  seizure_bar_graph <- s.graph(s.bar_graph,name,FALSE)
  
  ## Create and save FILA_MED_BAR_GRAPH.jpeg
  
  m.bar_graph <- data.frame(PERIOD=character(),PERCENT=integer(),SECTION=character(),stringsAsFactors=FALSE)
  m.bar_graph[1:((dim(seizure_bar_table)[1])*3),c("PERIOD")] <- rep(as.character(seizure_bar_table$START_DATE[1:length(seizure_bar_table$START_DATE)]),each=3)
  m.bar_graph[,c("PERIOD")] <- paste(as.character(seizure_bar_table$START_DATE[ceiling(as.numeric(rownames(m.bar_graph))/3)]),"-",as.character(seizure_bar_table$END_DATE[ceiling(as.numeric(rownames(m.bar_graph))/3)]))
  
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%3==1,c("PERCENT")] <- med_bar_table$MED_0_TO_10
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%3==1,c("SECTION")] <- "Meds 0-10%"
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%3==2,c("PERCENT")] <- med_bar_table$MED_10_TO_80
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%3==2,c("SECTION")] <- "Meds 10-80%"
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%3==0,c("PERCENT")] <- med_bar_table$MED_GREATER_THAN_80
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%3==0,c("SECTION")] <- "Meds >80%"
  m.bar_graph$SECTION <- factor(m.bar_graph$SECTION, levels=c("Meds >80%", "Meds 10-80%", "Meds 0-10%")) #edit so that the bars in the bar graph are stacked in order 
  med_bar_graph <- m.graph(m.bar_graph,name,FALSE)
  
  ## Create and save FILA_OUTCOME_BAR_GRAPH.jpeg
  
  o.bar_graph <- data.frame(PERIOD=character(),PERCENT=integer(),SECTION=character(),stringsAsFactors=FALSE)
  o.bar_graph[1:((dim(seizure_bar_table)[1])*3),c("PERIOD")] <- rep(as.character(seizure_bar_table$START_DATE[1:length(seizure_bar_table$START_DATE)]),each=3)
  o.bar_graph[,c("PERIOD")] <- paste(as.character(seizure_bar_table$START_DATE[ceiling(as.numeric(rownames(o.bar_graph))/3)]),"-",as.character(seizure_bar_table$END_DATE[ceiling(as.numeric(rownames(o.bar_graph))/3)]))
  
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%3==1,c("PERCENT")] <- outcome_bar_table$OUTCOME_0_TO_10
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%3==1,c("SECTION")] <- "Outcome 0-10%"
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%3==2,c("PERCENT")] <- outcome_bar_table$OUTCOME_10_TO_80
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%3==2,c("SECTION")] <- "Outcome 10-80%"
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%3==0,c("PERCENT")] <- outcome_bar_table$OUTCOME_GREATER_THAN_80
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%3==0,c("SECTION")] <- "Outcome >80%"
  o.bar_graph$SECTION <- factor(o.bar_graph$SECTION, levels=c("Outcome >80%", "Outcome 10-80%", "Outcome 0-10%")) #edit so that the bars in the bar graph are stacked in order 
  outcome_bar_graph <- o.graph(o.bar_graph,name,FALSE)
  
  print("Type 'yes' if you wish to save all graphs in the same folder as this patient's OUTCOME_DATA_CLINICAL file. Type 'no' if you would like for it to be in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to save this patient's graphs in")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  
  file <- gsub(" ","",paste(patient,"_SEIZURE_BAR_GRAPH.png"))
  ggsave(seizure_bar_graph,file=file,width=6,height=4, unit='in', dpi=600)
  print("Seizure bar graph created and saved")
  
  file <- gsub(" ","",paste(patient,"_MED_BAR_GRAPH.png"))
  ggsave(med_bar_graph,filename=file,width=6,height=4, unit='in', dpi=600)
  print("Med bar graph created and saved")
  
  file <- gsub(" ","",paste(patient,"_OUTCOME_BAR_GRAPH.png"))
  ggsave(outcome_bar_graph,filename=file,width=6,height=4, unit='in', dpi=600)
  print("Outcome bar graph created and saved")
  
  print("Type 'yes' if you wish to save this patient's MED_SEIZURE_OUTCOME_BAR_TABLE file in the same folder as this patient's OUTCOME_DATA_CLINICAL file. Type 'no' if you would like for it to be in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to save this patient's MED_SEIZURE_OUTCOME_BAR_TABLE file in")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  
  ## Create and save FILA_MED_SEIZURE_OUTCOME_BAR_TABLE.xlsx
  
  print(paste("Saving med/seizure/outcome bar table as",gsub(" ","",paste(patient,"_MED_SEIZURE_OUTCOME_BAR_TABLE.xlsx")),"in directory",getwd()))
  bar_table <- cbind.data.frame(seizure_bar_table,med_bar_table,outcome_bar_table)
  xlsx <- "MED_SEIZURE_OUTCOME_BAR_TABLE.xlsx"
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  xlsx::write.xlsx2(bar_table,file=xlsx,showNA=FALSE,row.names=FALSE)
}

