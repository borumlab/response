calculate_outcome <- function() {
  
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
  if (!require("reshape2")) {
    install.packages("respahe2")
  }
  library(reshape2)
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  print("Input the four letters that signify the patient we are doing calculations for")
  print("Example: FILA")
  patient <- readline(prompt="Enter here: ")
  
  print("Input the directory that you wish to draw the CLINIC_VISIT_SOURCE file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  cvs <- "CLINIC_VISIT_SOURCE.xlsx"
  cvs <- read.xlsx(cvs,sheet=1,detectDates=TRUE)
  
  print("Input the directory that you wish to draw this patient's SEIZURE_DATA_CLINICAL file and MED_DATA_CLINICAL file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  s.clinical <- "SEIZURE_DATA_CLINICAL.xlsx"
  s.clinical <- gsub(" ","",paste(patient,"_",s.clinical))
  s.clinical <- read.xlsx(s.clinical,sheet=1,detectDates=TRUE)
  
  m.clinical <- "MED_DATA_CLINICAL.xlsx"
  m.clinical <- gsub(" ","",paste(patient,"_",m.clinical))
  m.clinical <- read.xlsx(m.clinical,sheet=1,detectDates=TRUE)
  
  mrnumber <- unique(s.clinical$MRNUMBER)
  
  name <- unique(cvs[cvs$MRNUMBER==mrnumber,c("FIRST")])
  cvs <- unique(cvs[cvs$MRNUMBER==mrnumber,colnames(cvs)=="MRNUMBER" | colnames(cvs)=="DATE"])
  cvs$DATE <- as.Date(cvs$DATE)
  
  ## Create and save FILA_OUTCOME_DATA_CLINICAL.xlsx
  outcome <- data.frame(MRNUMBER=integer(),DATE=as.Date(as.character()),DAY_TYPE=integer(),OUTCOME_DAY=integer(),OUTCOME_30_DAYS=integer())
  outcome[1:dim(s.clinical)[1],] <- c(s.clinical[,colnames(s.clinical)=="MRNUMBER" | colnames(s.clinical)=="DATE" | colnames(s.clinical)=="DAY_TYPE"],NA,NA)
  outcome[,4] <- (s.clinical$SEIZURE_RESPONSE_DAY+m.clinical$MED_RESPONSE_DAY)/2
  outcome[,5] <- (s.clinical$SEIZURE_RESPONSE_30_DAYS+m.clinical$MED_RESPONSE_30_DAYS)/2
  
  print(paste("Saving data clinical outcome table as",gsub(" ","",paste(patient,"_OUTCOME_DATA_CLINICAL.xlsx")),"in directory",getwd()))
  xlsx <- "OUTCOME_DATA_CLINICAL.xlsx"
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx2(outcome,file=xlsx,showNA=FALSE,row.names=FALSE)
  
  seizure_bar_table <- data.frame(MRNUMBER=integer(),START_DATE=as.Date(as.character()),END_DATE=as.Date(as.character()),SEIZURE_FREE=integer(),SEIZURE_0_TO_50=integer(),SEIZURE_50_TO_100=integer(),SEIZURE_GREATER_THAN_100=integer())
  seizure_bar_table[1:dim(cvs)[1]-1,] <- NA
  seizure_bar_table$MRNUMBER <- rep(mrnumber,dim(cvs)[1]-1)
  seizure_bar_table$START_DATE <- cvs$DATE[1:dim(cvs)[1]-1]
  seizure_bar_table$END_DATE[1:dim(seizure_bar_table)[1]-1] <- cvs$DATE[2:(length(cvs$DATE)-1)]-1
  seizure_bar_table$END_DATE[dim(seizure_bar_table)[1]] <- cvs$DATE[length(cvs$DATE)]
  
  med_bar_table <- data.frame(MED_FREE=integer(),MED_0_TO_50=integer(),MED_50_TO_100=integer(),MED_GREATER_THAN_100=integer())
  med_bar_table[1:dim(cvs)[1]-1,] <- NA
  
  outcome_bar_table <- data.frame(OUTCOME_FREE=integer(),OUTCOME_0_TO_50=integer(),OUTCOME_50_TO_100=integer(),OUTCOME_GREATER_THAN_100=integer())
  outcome_bar_table[1:dim(cvs)[1]-1,] <- NA
  
  for (j in 2:length(cvs$DATE)) {
    
    ## Create table of values used to create FILA_SEIZURE_BAR_GRAPH.jpeg
    
    s.totalnumdays <- nrow(s.clinical[s.clinical$DATE>=cvs$DATE[j-1] & s.clinical$DATE<cvs$DATE[j],])
    s.totalfreedays <- nrow(s.clinical[s.clinical$DATE>=cvs$DATE[j-1] & s.clinical$DATE<cvs$DATE[j] & s.clinical$SEIZURE_RESPONSE_DAY==0,])/s.totalnumdays
    s.total0to50days <- nrow(s.clinical[s.clinical$DATE>=cvs$DATE[j-1] & s.clinical$DATE<cvs$DATE[j] & s.clinical$SEIZURE_RESPONSE_DAY>0 & s.clinical$SEIZURE_RESPONSE_DAY<=50,])/s.totalnumdays
    s.total50to100days <- nrow(s.clinical[s.clinical$DATE>=cvs$DATE[j-1] & s.clinical$DATE<cvs$DATE[j] & s.clinical$SEIZURE_RESPONSE_DAY>50 & s.clinical$SEIZURE_RESPONSE_DAY<=100,])/s.totalnumdays
    s.totalgreater100days <- nrow(s.clinical[s.clinical$DATE>=cvs$DATE[j-1] & s.clinical$DATE<cvs$DATE[j] & s.clinical$SEIZURE_RESPONSE_DAY>100,])/s.totalnumdays
    seizure_bar_table$SEIZURE_FREE[j-1] <- round(s.totalfreedays*100,2)
    seizure_bar_table$SEIZURE_0_TO_50[j-1] <- round(s.total0to50days*100,2)
    seizure_bar_table$SEIZURE_50_TO_100[j-1] <- round(s.total50to100days*100,2)
    seizure_bar_table$SEIZURE_GREATER_THAN_100[j-1] <- round(s.totalgreater100days*100,2)
    
    ## Create table of values used to create FILA_MED_BAR_GRAPH.jpeg
    
    m.totalnumdays <- nrow(m.clinical[m.clinical$DATE>=cvs$DATE[j-1] & m.clinical$DATE<cvs$DATE[j],])
    m.totalfreedays <- nrow(m.clinical[m.clinical$DATE>=cvs$DATE[j-1] & m.clinical$DATE<cvs$DATE[j] & m.clinical$MED_RESPONSE_DAY==0,])/m.totalnumdays
    m.total0to50days <- nrow(m.clinical[m.clinical$DATE>=cvs$DATE[j-1] & m.clinical$DATE<cvs$DATE[j] & m.clinical$MED_RESPONSE_DAY>0 & m.clinical$MED_RESPONSE_DAY<=50,])/m.totalnumdays
    m.total50to100days <- nrow(m.clinical[m.clinical$DATE>=cvs$DATE[j-1] & m.clinical$DATE<cvs$DATE[j] & m.clinical$MED_RESPONSE_DAY>50 & m.clinical$MED_RESPONSE_DAY<=100,])/m.totalnumdays
    m.totalgreater100days <- nrow(m.clinical[m.clinical$DATE>=cvs$DATE[j-1] & m.clinical$DATE<cvs$DATE[j] & m.clinical$MED_RESPONSE_DAY>100,])/m.totalnumdays
    med_bar_table$MED_FREE[j-1] <- round(m.totalfreedays*100,2)
    med_bar_table$MED_0_TO_50[j-1] <- round(m.total0to50days*100,2)
    med_bar_table$MED_50_TO_100[j-1] <- round(m.total50to100days*100,2)
    med_bar_table$MED_GREATER_THAN_100[j-1] <- round(m.totalgreater100days*100,2)
    
    ## Create table of values used to create FILA_OUTCOME_BAR_GRAPH.jpeg
    
    o.totalnumdays <- nrow(outcome[outcome$DATE>=cvs$DATE[j-1] & outcome$DATE<cvs$DATE[j],])
    o.totalfreedays <- nrow(outcome[outcome$DATE>=cvs$DATE[j-1] & outcome$DATE<cvs$DATE[j] & outcome$OUTCOME_DAY==0,])/o.totalnumdays
    o.total0to50days <- nrow(outcome[outcome$DATE>=cvs$DATE[j-1] & outcome$DATE<cvs$DATE[j] & outcome$OUTCOME_DAY>0 & outcome$OUTCOME_DAY<=50,])/o.totalnumdays
    o.total50to100days <- nrow(outcome[outcome$DATE>=cvs$DATE[j-1] & outcome$DATE<cvs$DATE[j] & outcome$OUTCOME_DAY>50 & outcome$OUTCOME_DAY<=100,])/o.totalnumdays
    o.totalgreater100days <- nrow(outcome[outcome$DATE>=cvs$DATE[j-1] & outcome$DATE<cvs$DATE[j] & outcome$OUTCOME_DAY>100,])/o.totalnumdays
    outcome_bar_table$OUTCOME_FREE[j-1] <- round(o.totalfreedays*100,2)
    outcome_bar_table$OUTCOME_0_TO_50[j-1] <- round(o.total0to50days*100,2)
    outcome_bar_table$OUTCOME_50_TO_100[j-1] <- round(o.total50to100days*100,2)
    outcome_bar_table$OUTCOME_GREATER_THAN_100[j-1] <- round(o.totalgreater100days*100,2)
  }

  ## Create and save FILA_SEIZURE_BAR_GRAPH.jpeg
  
  s.bar_graph <- data.frame(PERIOD=character(),PERCENT=integer(),SECTION=character(),stringsAsFactors=FALSE)
  s.bar_graph[1:((dim(cvs)[1]-1)*4),c("PERIOD")] <- rep(as.character(cvs$DATE[2:length(cvs$DATE)]),each=4)
  s.bar_graph[,c("PERIOD")] <- paste("Period:",as.character(cvs$DATE[ceiling(as.numeric(rownames(s.bar_graph))/4)]),"-",as.character(cvs$DATE[ceiling(as.numeric(rownames(s.bar_graph))/4)+1]-1))
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==1,c("PERCENT")] <- seizure_bar_table$SEIZURE_FREE
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==1,c("SECTION")] <- "Seizure Free"
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==2,c("PERCENT")] <- seizure_bar_table$SEIZURE_0_TO_50
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==2,c("SECTION")] <- "Seizures 0-50%"
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==3,c("PERCENT")] <- seizure_bar_table$SEIZURE_50_TO_100
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==3,c("SECTION")] <- "Seizures 50-100%"
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==0,c("PERCENT")] <- seizure_bar_table$SEIZURE_GREATER_THAN_100
  s.bar_graph[as.numeric(rownames(s.bar_graph))%%4==0,c("SECTION")] <- "Seizures worse"
  seizure_bar_graph <- ggplot(data=s.bar_graph,aes(x=PERIOD,y=PERCENT,fill=SECTION,colour=SECTION))
  seizure_bar_graph <- seizure_bar_graph + geom_bar(stat="identity")
  seizure_bar_graph <- seizure_bar_graph + scale_colour_manual(breaks=c("Seizure Free","Seizures 0-50%","Seizures 50-100%","Seizures worse"),
                                                               values=c("Seizure Free"="black",
                                                                        "Seizures 0-50%"="black",
                                                                        "Seizures 50-100%"="black",
                                                                        "Seizures worse"="black"))
  seizure_bar_graph <- seizure_bar_graph + scale_fill_manual(breaks=c("Seizure Free","Seizures 0-50%","Seizures 50-100%","Seizures worse"),
                                                             values=c("white","green","yellow","red"))
  seizure_bar_graph <- seizure_bar_graph + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,siz=3))
  seizure_bar_graph <- seizure_bar_graph + labs(title=paste(gsub(" ","",paste(name,"'s")),"Seizure Response"),
                                                x="Time Period on Ketogenic Therapy",
                                                y="% of Time Period on Ketogenic Therapy Seizures")
  seizure_bar_graph <- seizure_bar_graph + theme(axis.title.y=element_text(angle=90,size=6))
  file <- gsub(" ","",paste(patient,"_SEIZURE_BAR_GRAPH.jpeg"))
  ggsave(seizure_bar_graph,filename=file,width=6,height=4)
  print("Seizure bar graph saved to the patient folder")
  
  ## Create and save FILA_MED_BAR_GRAPH.jpeg
  
  m.bar_graph <- data.frame(PERIOD=character(),PERCENT=integer(),SECTION=character(),stringsAsFactors=FALSE)
  m.bar_graph[1:((dim(cvs)[1]-1)*4),c("PERIOD")] <- rep(as.character(cvs$DATE[2:length(cvs$DATE)]),each=4)
  m.bar_graph[,c("PERIOD")] <- paste("Period:",as.character(cvs$DATE[ceiling(as.numeric(rownames(m.bar_graph))/4)]),"-",as.character(cvs$DATE[ceiling(as.numeric(rownames(m.bar_graph))/4)+1]-1))
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==1,c("PERCENT")] <- med_bar_table$MED_FREE
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==1,c("SECTION")] <- "Med Free"
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==2,c("PERCENT")] <- med_bar_table$MED_0_TO_50
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==2,c("SECTION")] <- "Meds 0-50%"
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==3,c("PERCENT")] <- med_bar_table$MED_50_TO_100
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==3,c("SECTION")] <- "Meds 50-100%"
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==0,c("PERCENT")] <- med_bar_table$MED_GREATER_THAN_100
  m.bar_graph[as.numeric(rownames(m.bar_graph))%%4==0,c("SECTION")] <- "Meds worse"
  med_bar_graph <- ggplot(data=m.bar_graph,aes(x=PERIOD,y=PERCENT,fill=SECTION,colour=SECTION))
  med_bar_graph <- med_bar_graph + geom_bar(stat="identity")
  med_bar_graph <- med_bar_graph + scale_colour_manual(breaks=c("Med Free","Meds 0-50%","Meds 50-100%","Meds worse"),
                                                       values=c("Med Free"="black",
                                                                "Meds 0-50%"="black",
                                                                "Meds 50-100%"="black",
                                                                "Meds worse"="black"))
  med_bar_graph <- med_bar_graph + scale_fill_manual(breaks=c("Med Free","Meds 0-50%","Meds 50-100%","Meds worse"),
                                                     values=c("white","green","yellow","red"))
  med_bar_graph <- med_bar_graph + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=4))
  med_bar_graph <- med_bar_graph + labs(title=paste(gsub(" ","",paste(name,"'s")),"Med Response"),
                                        x="Time Period on Ketogenic Therapy",
                                        y="% of Time Period on Ketogenic Therapy Meds")
  med_bar_graph <- med_bar_graph + theme(axis.title.y=element_text(angle=90,size=6))
  file <- gsub(" ","",paste(patient,"_MED_BAR_GRAPH.jpeg"))
  ggsave(med_bar_graph,filename=file,width=6,height=4)
  print("Med bar graph saved to the patient folder")
  
  ## Create and save FILA_OUTCOME_BAR_GRAPH.jpeg
  
  o.bar_graph <- data.frame(PERIOD=character(),PERCENT=integer(),SECTION=character(),stringsAsFactors=FALSE)
  o.bar_graph[1:((dim(cvs)[1]-1)*4),c("PERIOD")] <- rep(as.character(cvs$DATE[2:length(cvs$DATE)]),each=4)
  o.bar_graph[,c("PERIOD")] <- paste("Period:",as.character(cvs$DATE[ceiling(as.numeric(rownames(o.bar_graph))/4)]),"-",as.character(cvs$DATE[ceiling(as.numeric(rownames(o.bar_graph))/4)+1]-1))
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==1,c("PERCENT")] <- outcome_bar_table$OUTCOME_FREE
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==1,c("SECTION")] <- "Outcome Free"
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==2,c("PERCENT")] <- outcome_bar_table$OUTCOME_0_TO_50
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==2,c("SECTION")] <- "Outcome 0-50%"
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==3,c("PERCENT")] <- outcome_bar_table$OUTCOME_50_TO_100
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==3,c("SECTION")] <- "Outcome 50-100%"
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==0,c("PERCENT")] <- outcome_bar_table$OUTCOME_GREATER_THAN_100
  o.bar_graph[as.numeric(rownames(o.bar_graph))%%4==0,c("SECTION")] <- "Outcome Worse"
  outcome_bar_graph <- ggplot(data=o.bar_graph,aes(x=PERIOD,y=PERCENT,fill=SECTION,colour=SECTION))
  outcome_bar_graph <- outcome_bar_graph + geom_bar(stat="identity")
  outcome_bar_graph <- outcome_bar_graph + scale_colour_manual(breaks=c("Outcome Free","Outcome 0-50%","Outcome 50-100%","Outcome Worse"),
                                                               values=c("Outcome Free"="black",
                                                                        "Outcome 0-50%"="black",
                                                                        "Outcome 50-100%"="black",
                                                                        "Outcome Worse"="black"))
  outcome_bar_graph <- outcome_bar_graph + scale_fill_manual(breaks=c("Outcome Free","Outcome 0-50%","Outcome 50-100%","Outcome Worse"),
                                                             values=c("green","yellow","white","red"))
  outcome_bar_graph <- outcome_bar_graph + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=4))
  outcome_bar_graph <- outcome_bar_graph + labs(title=paste(gsub(" ","",paste(name,"'s")),"Outcome Response"),
                                                x="Time Period on Ketogenic Therapy",
                                                y="% of Time Period on Ketogenic Therapy Outcome")
  outcome_bar_graph <- outcome_bar_graph + theme(axis.title.y=element_text(angle=90,size=6))
  file <- gsub(" ","",paste(patient,"_OUTCOME_BAR_GRAPH.jpeg"))
  ggsave(outcome_bar_graph,filename=file,width=6,height=4)
  print("Outcome bar graph saved to the patient folder")
  
  ## Create and save FILA_MED_SEIZURE_OUTCOME_BAR_TABLE.xlsx
  
  print(paste("Saving med/seizure/outcome bar table as",gsub(" ","",paste(patient,"_MED_SEIZURE_OUTCOME_BAR_TABLE.xlsx")),"in directory",getwd()))
  bar_table <- cbind.data.frame(seizure_bar_table,med_bar_table,outcome_bar_table)
  xlsx <- "MED_SEIZURE_OUTCOME_BAR_TABLE.xlsx"
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  write.xlsx2(bar_table,file=xlsx,showNA=FALSE,row.names=FALSE)
}