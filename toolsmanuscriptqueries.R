## To run this script:
## Type test_normality(u,p,d,h,string) into the R console, where
## u = MySQL user name
## p = password
## d = database name
## h = host name

## Medical record numbers are hard coded into this script. These numbers can be changed as necessary to
## Graph the data for any other patient, given that their seizure and med data has been uploaded
## into the MySQL database

## SQL queries will be required to draw the data from the database and store it as an R frame for graphing
## Templates of what queries should be used in which situations are as follows:

## 1) Pull out med data:
## SELECT *
## FROM med_data_id_research
## WHERE MRNUMBER=[medical record number]
## AND DAY_TYPE NOT IN ('1','4')
## AND row_names < ([number of days wanted + 1] + (SELECT COUNT(1)
##                                                 FROM med_data_id_research 
##                                                 WHERE MRNUMBER=[medical record number]
##                                                 AND DAY_TYPE=1));
## (The last four rows finds the first x number of days that are not baseline days)
## (If you wish to pull out all of this patient's data, then you only need the first four lines of this query)

## 2) Pull out seizure data:
## SELECT *
## FROM seizure_data_id_research
## WHERE MRNUMBER=[medical record number]
## AND DAY_TYPE NOT IN ('1','4')
## AND row_names < ([number of days wanted + 1] + (SELECT COUNT(1)
##                                                 FROM seizure_data_id_research 
##                                                 WHERE MRNUMBER=[medical record number]
##                                                 AND DAY_TYPE=1));
## (The last four rows finds the first x number of days that are not baseline days)
## (If you wish to pull out all of this patient's data, then you only need the first four lines of this query)

## 3) Pull out med data for baseline days excluding med free days:
## SELECT *
## FROM med_data_id_research
## WHERE MRNUMBER=[medical record number]
## AND DAY_TYPE=1
## AND MED_NUMBER>0;

## 4) Pull out seizure data for baseline days excluding seizure free days:
## SELECT *
## FROM seizure_data_id_research
## WHERE MRNUMBER=[medical record number]
## AND DAY_TYPE=1
## AND SEIZURE_NUMBER>0;

## 5) Pull out med data excluding med free days:
## SELECT *
## FROM med_data_id_research
## WHERE MRNUMBER=[medical record number]
## AND DAY_TYPE NOT IN ('1','4')
## AND MED_NUMBER_DAY>0
## AND row_names < ([number of days wanted + 1] + (SELECT COUNT(1)
##                                                 FROM med_data_id_research 
##                                                 WHERE MRNUMBER=[medical record number]
##                                                 AND DAY_TYPE=1));
## (The last four rows finds the first x number of days that are not baseline days)
## (If you wish to pull out all of this patient's data, then you only need the first four lines of this query)



tools_manuscript <- function(u,p,d,h) {
  
  if (!require("RMySQL")) {
    install.packages("RMySQL")
  }
  library(RMySQL)
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  if (!require("reshape")) {
    install.packages("reshape")
  }
  library(reshape)
  if (!require("lubridate")) {
    install.packages("lubridate")
  }
  library(lubridate)
  if (!require("plyr")) {
    install.packages("plyr")
  }
  library(plyr)
  
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons) {
    dbDisconnect(con)
  }
  
  connect <- dbConnect(MySQL(),user=u,password=p,dbname=d,host=h)
  
  send_query <- function(query) {
    dbSendQuery(connect,query)
  }
  
  get_query <- function(query) {
    output <- data.frame(dbGetQuery(connect,query))
    return(output)
  }
  
  setwd("G:/Notebooks_E/e10/Jonathan Lee/ToolsManuscript/")
  
  send_query("USE patient_pkt;")
  
  p1med <- get_query("SELECT *
                      FROM med_data_id_research
                      WHERE MRNUMBER=[MRNUMBER1]
                      AND DAY_TYPE NOT IN ('1','4')
                      AND row_names < (3601 + (SELECT COUNT(1) 
                                               FROM med_data_id_research
                                               WHERE MRNUMBER=[MRNUMBER1]
                                               AND DAY_TYPE=1));")
  
  p2med <- get_query("SELECT *
                      FROM med_data_id_research
                      WHERE MRNUMBER=[MRNUMBER2]
                      AND DAY_TYPE NOT IN ('1','4')
                      AND row_names < (3601 + (SELECT COUNT(1) 
                                               FROM med_data_id_research
                                               WHERE MRNUMBER=[MRNUMBER2]
                                               AND DAY_TYPE=1));")
  
  p3med <- get_query("SELECT *
                      FROM med_data_id_research
                      WHERE MRNUMBER=[MRNUMBER3]
                      AND DAY_TYPE NOT IN ('1','4')
                      AND row_names < (3601 + (SELECT COUNT(1) 
                                               FROM med_data_id_research
                                               WHERE MRNUMBER=[MRNUMBER3]
                                               AND DAY_TYPE=1));")
  
  p1blmed <- get_query("SELECT *
                        FROM med_data_id_research
                        WHERE MRNUMBER=[MRNUMBER1]
                        AND DAY_TYPE=1
                        AND MED_NUMBER_DAY>0;")
  
  p2blmed <- get_query("SELECT *
                        FROM med_data_id_research
                        WHERE MRNUMBER=[MRNUMBER2]
                        AND DAY_TYPE=1
                        AND MED_NUMBER_DAY>0;")
  
  p3blmed <- get_query("SELECT *
                        FROM med_data_id_research
                        WHERE MRNUMBER=[MRNUMBER3]
                        AND DAY_TYPE=1
                        AND MED_NUMBER_DAY>0;")
  
  print("The median med load at baseline (not including med free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1med$MRNUMBER),":")),median(p1blmed$MED_LOAD_DAY)))
  print(paste(gsub(" ","",paste(unique(p2med$MRNUMBER),":")),median(p2blmed$MED_LOAD_DAY)))
  print(paste(gsub(" ","",paste(unique(p3med$MRNUMBER),":")),median(p3blmed$MED_LOAD_DAY)))
  
  print("The median med number at baseline (not including med free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1med$MRNUMBER),":")),median(p1blmed$MED_NUMBER_DAY)))
  print(paste(gsub(" ","",paste(unique(p2med$MRNUMBER),":")),median(p2blmed$MED_NUMBER_DAY)))
  print(paste(gsub(" ","",paste(unique(p3med$MRNUMBER),":")),median(p3blmed$MED_NUMBER_DAY)))
  
  print("The mean med responses per day (not including med free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1med$MRNUMBER),":")),round(mean(p1med$MED_RESPONSE_DAY[p1med$MED_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p2med$MRNUMBER),":")),round(mean(p2med$MED_RESPONSE_DAY[p2med$MED_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p3med$MRNUMBER),":")),round(mean(p3med$MED_RESPONSE_DAY[p3med$MED_RESPONSE_DAY!=0]),2)))
  
  print("The mean med number responses per day (not including med free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1med$MRNUMBER),":")),round(mean(p1med$MED_NUMBER_RESPONSE_DAY[p1med$MED_NUMBER_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p2med$MRNUMBER),":")),round(mean(p2med$MED_NUMBER_RESPONSE_DAY[p2med$MED_NUMBER_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p3med$MRNUMBER),":")),round(mean(p3med$MED_NUMBER_RESPONSE_DAY[p3med$MED_NUMBER_RESPONSE_DAY!=0]),2)))
  
  
  print(paste("Wilcoxon Rank Sum Test for patient with MRNUMBER",unique(p1med$MRNUMBER)))
  print(wilcox.test(p1med$MED_RESPONSE_DAY,p1med$MED_NUMBER_RESPONSE_DAY))
  print(paste("Wilcoxon Rank Sum Test for patient with MRNUMBER",unique(p2med$MRNUMBER)))
  print(wilcox.test(p2med$MED_RESPONSE_DAY,p2med$MED_NUMBER_RESPONSE_DAY))
  print(paste("Wilcoxon Rank Sum Test for patient with MRNUMBER",unique(p3med$MRNUMBER)))
  print(wilcox.test(p3med$MED_RESPONSE_DAY,p3med$MED_NUMBER_RESPONSE_DAY))
  
  p1seizure <- get_query("SELECT *
                          FROM seizure_data_id_research
                          WHERE MRNUMBER=[MRNUMBER4]
                          AND DAY_TYPE!=1;")
  
  p2seizure <- get_query("SELECT *
                          FROM seizure_data_id_research
                          WHERE MRNUMBER=[MRNUMBER5]
                          AND DAY_TYPE!=1;")
  
  p3seizure <- get_query("SELECT *
                          FROM seizure_data_id_research
                          WHERE MRNUMBER=[MRNUMBER6]
                          AND DAY_TYPE!=1
                          AND row_names < (3601 + (SELECT COUNT(1) 
                                                   FROM seizure_data_id_research
                                                   WHERE MRNUMBER=[MRNUMBER6]
                                                   AND DAY_TYPE=1));")
  
  p1blseizure <- get_query("SELECT *
                            FROM seizure_data_id_research
                            WHERE MRNUMBER=[MRNUMBER4]
                            AND DAY_TYPE=1
                            AND SEIZURE_NUMBER_DAY>0;")
  
  p2blseizure <- get_query("SELECT *
                            FROM seizure_data_id_research
                            WHERE MRNUMBER=[MRNUMBER5]
                            AND DAY_TYPE=1
                            AND SEIZURE_NUMBER_DAY>0;")
  
  p3blseizure <- get_query("SELECT *
                            FROM seizure_data_id_research
                            WHERE MRNUMBER=[MRNUMBER6]
                            AND DAY_TYPE=1
                            AND SEIZURE_NUMBER_DAY>0;")
  
  print("The median seizure load at baseline (not including seizure free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1seizure$MRNUMBER),":")),median(p1blseizure$SEIZURE_LOAD_DAY)))
  print(paste(gsub(" ","",paste(unique(p2seizure$MRNUMBER),":")),median(p2blseizure$SEIZURE_LOAD_DAY)))
  print(paste(gsub(" ","",paste(unique(p3seizure$MRNUMBER),":")),median(p3blseizure$SEIZURE_LOAD_DAY)))
  
  print("The median seizure number at baseline (not including seizure free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1seizure$MRNUMBER),":")),median(p1blseizure$SEIZURE_NUMBER_DAY)))
  print(paste(gsub(" ","",paste(unique(p2seizure$MRNUMBER),":")),median(p2blseizure$SEIZURE_NUMBER_DAY)))
  print(paste(gsub(" ","",paste(unique(p3seizure$MRNUMBER),":")),median(p3blseizure$SEIZURE_NUMBER_DAY)))
  
  print("The mean seizure responses per day (not including seizure free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1seizure$MRNUMBER),":")),round(mean(p1seizure$SEIZURE_RESPONSE_DAY[p1seizure$SEIZURE_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p2seizure$MRNUMBER),":")),round(mean(p2seizure$SEIZURE_RESPONSE_DAY[p2seizure$SEIZURE_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p3seizure$MRNUMBER),":")),round(mean(p3seizure$SEIZURE_RESPONSE_DAY[p3seizure$SEIZURE_RESPONSE_DAY!=0]),2)))
  
  print("The mean seizure number responses per day (not including seizure free days) for each patient are as follows: ")
  print(paste(gsub(" ","",paste(unique(p1seizure$MRNUMBER),":")),round(mean(p1seizure$SEIZURE_NUMBER_RESPONSE_DAY[p1seizure$SEIZURE_NUMBER_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p2seizure$MRNUMBER),":")),round(mean(p2seizure$SEIZURE_NUMBER_RESPONSE_DAY[p2seizure$SEIZURE_NUMBER_RESPONSE_DAY!=0]),2)))
  print(paste(gsub(" ","",paste(unique(p3seizure$MRNUMBER),":")),round(mean(p3seizure$SEIZURE_NUMBER_RESPONSE_DAY[p3seizure$SEIZURE_NUMBER_RESPONSE_DAY!=0]),2)))
  
  
  print(paste("Wilcoxon Rank Sum Test for patient with MRNUMBER",unique(p1seizure$MRNUMBER)))
  print(wilcox.test(p1seizure$SEIZURE_RESPONSE_DAY,p1seizure$SEIZURE_NUMBER_RESPONSE_DAY))
  print(paste("Wilcoxon Rank Sum Test for patient with MRNUMBER",unique(p2seizure$MRNUMBER)))
  print(wilcox.test(p2seizure$SEIZURE_RESPONSE_DAY,p2seizure$SEIZURE_NUMBER_RESPONSE_DAY))
  print(paste("Wilcoxon Rank Sum Test for patient with MRNUMBER",unique(p3seizure$MRNUMBER)))
  print(wilcox.test(p3seizure$SEIZURE_RESPONSE_DAY,p3seizure$SEIZURE_NUMBER_RESPONSE_DAY))
  
  p1splot <- p1seizure[p1seizure$SEIZURE_NUMBER_DAY>0,colnames(p1seizure)%in%c("MRNUMBER","DATE","SEIZURE_RESPONSE_DAY","SEIZURE_NUMBER_RESPONSE_DAY")]
  p2splot <- p2seizure[p2seizure$SEIZURE_NUMBER_DAY>0,colnames(p2seizure)%in%c("MRNUMBER","DATE","SEIZURE_RESPONSE_DAY","SEIZURE_NUMBER_RESPONSE_DAY")]
  p3splot <- p3seizure[p3seizure$SEIZURE_NUMBER_DAY>0,colnames(p3seizure)%in%c("MRNUMBER","DATE","SEIZURE_RESPONSE_DAY","SEIZURE_NUMBER_RESPONSE_DAY")]
  splot <- rbind.data.frame(p1splot,p2splot,p3splot)
  splot$MRNUMBER <- factor(splot$MRNUMBER,levels=c("[MRNUMBER4]","[MRNUMBER5]","[MRNUMBER6]"))
  
  splot <- ggplot(splot,aes(SEIZURE_NUMBER_RESPONSE_DAY,SEIZURE_RESPONSE_DAY,color=MRNUMBER,fill=MRNUMBER)) 
  splot <- splot + geom_hline(yintercept=100) + geom_abline(intercept=0,slope=1) + geom_point(shape=21)
  splot <- splot + xlab("Seizure Number Response per day (%)") + ylab("Seizure Response per Day (%)") + ggtitle("Seizure Number Response vs Seizure Response per day")
  splot <- splot + theme(axis.title=element_text(angle=0,size=10))
  splot <- splot + theme(plot.title=element_text(angle=0,size=10))
  splot <- splot + theme(legend.title=element_text(size=14,face="bold"))
  splot <- splot + scale_color_manual(name="",labels=c("KG0202","KG0214","KG0194"),values=c("red","green","yellow"))
  splot <- splot + scale_fill_manual(name="",labels=c("KG0202","KG0214","KG0194"),values=c("white","gray","black"))
  splot <- splot + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  splot <- splot + theme(panel.border=element_rect(colour="black",fill=NA,size=1.5))
  splot <- splot + scale_x_continuous(limits=c(0,400))
  splot <- splot + scale_y_continuous(limits=c(0,400))
  print("Seizure response vs. seizure number response scatter plot created")
  splotfile <- gsub(" ","",paste("TOOLS_MANUSCRIPT_SEIZURE_RESPONSE_SCATTER_PLOT.jpeg"))
  ggsave(splot,filename=splotfile,width=6,height=4)
  
  p1med30 <- get_query("SELECT *
                        FROM med_data_id_research
                        WHERE MRNUMBER=[MRNUMBER1]
                        AND DAY_TYPE!=1
                        AND MED_NUMBER_DAY>0
                        AND row_names < (3601 + (SELECT COUNT(MRNUMBER) 
						                                     FROM med_data_id_research
                                                 WHERE MRNUMBER=[MRNUMBER2]
                                                 AND DAY_TYPE=1));")
  
  p2med30 <- get_query("SELECT *
                        FROM med_data_id_research
                        WHERE MRNUMBER=[MRNUMBER2]
                        AND DAY_TYPE!=1 
                        AND MED_NUMBER_DAY>0
                        AND row_names < (3601 + (SELECT COUNT(MRNUMBER) 
						                                    FROM med_data_id_research
                                                WHERE MRNUMBER=[MRNUMBER2]
                                                AND DAY_TYPE=1));")
  
  med30 <- rbind.data.frame(p1med30[as.numeric(rownames(p1med30))%%30==1,], 
                            p2med30[as.numeric(rownames(p2med30))%%30==1,])
  med30 <- melt(med30,id=c("row_names","MRNUMBER","DATE","DAY_TYPE","MED_LOAD_DAY","MED_NUMBER_DAY"))
  med30$DATE <- as.Date(med30$DATE)
  med30 <- cbind.data.frame(MONTHNUM=rep(NA,dim(med30)[1]),med30)
  for (i in unique(med30$MRNUMBER)) {
    k <- 1
    for (j in unique(med30[med30$MRNUMBER==i,colnames(med30)=="DATE"])) {
      med30[med30$MRNUMBER==i & med30$DATE==j,colnames(med30)=="MONTHNUM"] <- k
      k <- k + 1
    }
  }
  med30$MRNUMBER <- factor(med30$MRNUMBER,levels=c("[MRNUMBER1]","[MRNUMBER2]"))
  med30$variable <- factor(med30$variable,levels=c("MED_RESPONSE_DAY","MED_NUMBER_RESPONSE_DAY"))
  NUM <- rep(NA,dim(med30)[1])
  med30 <- cbind.data.frame(med30,NUM)
  med30[med30$variable=="MED_RESPONSE_DAY" & med30$MRNUMBER=="[MRNUMBER1]",colnames(med30)=="NUM"] <- "1"
  med30[med30$variable=="MED_RESPONSE_DAY" & med30$MRNUMBER=="[MRNUMBER2]",colnames(med30)=="NUM"] <- "2"
  med30[med30$variable=="MED_NUMBER_RESPONSE_DAY" & med30$MRNUMBER=="[MRNUMBER1]",colnames(med30)=="NUM"] <- "3"
  med30[med30$variable=="MED_NUMBER_RESPONSE_DAY" & med30$MRNUMBER=="[MRNUMBER2]",colnames(med30)=="NUM"] <- "4"
  med30$NUM <- factor(med30$NUM,levels=c("1","2","3","4"))
  med30.1 <- med30[med30$MRNUMBER=="[MRNUMBER1]",]
  med30.2 <- med30[med30$MRNUMBER=="[MRNUMBER2]",]

  m30plot <- ggplot(med30,aes(MONTHNUM,value,color=factor(NUM),shape=factor(NUM),fill=factor(NUM)))
  m30plot <- m30plot + geom_hline(yintercept=100)
  m30plot <- m30plot + geom_line(data=med30.1,aes(group=variable),color="black")
  m30plot <- m30plot + geom_line(data=med30.2,aes(group=variable),color="black")
  m30plot <- m30plot + geom_point(size=1.5)
  m30plot <- m30plot + xlab("Month on Therapy") + ylab("Response (%)") + ggtitle("Med Response and Med Number Response per 30 days")
  m30plot <- m30plot + theme(axis.title=element_text(angle=0,size=10))
  m30plot <- m30plot + theme(plot.title=element_text(angle=0,size=10))
  m30plot <- m30plot + theme(legend.title=element_text(size=14,face="bold"))
  m30plot <- m30plot + scale_color_manual(name="",labels=c("KG0129 MNR","KG0020 MNR","KG0129 MR","KG0020 MR"),values=c("red","green","red","green"))
  m30plot <- m30plot + scale_shape_manual(name="",labels=c("KG0129 MNR","KG0020 MNR","KG0129 MR","KG0020 MR"),values=c(21,22,21,22))
  m30plot <- m30plot + scale_fill_manual(name="",labels=c("KG0129 MNR","KG0020 MNR","KG0129 MR","KG0020 MR"),values=c("black","black","white","white"))
  m30plot <- m30plot + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5,size=8))
  m30plot <- m30plot + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  m30plot <- m30plot + theme(panel.border=element_rect(colour="black",fill=NA,size=1.5))
  m30plot <- m30plot + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=6))
  m30plot <- m30plot + scale_x_continuous(limits=c(0,120),breaks=scales::pretty_breaks(24))
  m30plot <- m30plot + scale_y_continuous(limits=c(0,600),breaks=scales::pretty_breaks(12))
  print("Med response and med number response per 30 days line plot created")
  m30plotfile <- gsub(" ","",paste("TOOLS_MANUSCRIPT_MED_RESPONSE_30_DAYS_LINE_PLOT.jpeg"))
  ggsave(m30plot,filename=m30plotfile,width=6,height=4)
  
  p1score.s <- get_query("SELECT *
                          FROM seizure_data_id_research
                          WHERE MRNUMBER=[MRNUMBER1]
                          AND DAY_TYPE!=1 
                          AND row_names < (3601 + (SELECT COUNT(1) 
                                                   FROM seizure_data_id_research
                                                   WHERE MRNUMBER=[MRNUMBER1]
                                                   AND DAY_TYPE=1));")
  p1score.m <- get_query("SELECT *
                          FROM med_data_id_research
                          WHERE MRNUMBER=[MRNUMBER1]
                          AND DAY_TYPE!=1 
                          AND row_names < (3601 + (SELECT COUNT(1) 
                                                   FROM med_data_id_research
                                                   WHERE MRNUMBER=[MRNUMBER1]
                                                   AND DAY_TYPE=1));")
  p1baseline.s <- get_query("SELECT *
                             FROM seizure_data_id_research
                             WHERE MRNUMBER=[MRNUMBER1]
                             AND DAY_TYPE=1;")
  p1baseline.m <- get_query("SELECT *
                             FROM med_data_id_research
                             WHERE MRNUMBER=[MRNUMBER1]
                             AND DAY_TYPE=1;")
  p2score.s <- get_query("SELECT *
                          FROM seizure_data_id_research
                          WHERE MRNUMBER=[MRNUMBER2]
                          AND DAY_TYPE!=1 
                          AND row_names < (3601 + (SELECT COUNT(1) 
                                                   FROM seizure_data_id_research
                                                   WHERE MRNUMBER=[MRNUMBER2]
                                                   AND DAY_TYPE=1));")
  p2score.m <- get_query("SELECT *
                          FROM med_data_id_research
                          WHERE MRNUMBER=[MRNUMBER2]
                          AND DAY_TYPE!=1 
                          AND row_names < (3601 + (SELECT COUNT(1) 
                                                   FROM med_data_id_research
                                                   WHERE MRNUMBER=[MRNUMBER2]
                                                   AND DAY_TYPE=1));")
  p2baseline.s <- get_query("SELECT *
                             FROM seizure_data_id_research
                             WHERE MRNUMBER=[MRNUMBER2]
                             AND DAY_TYPE=1;")
  p2baseline.m <- get_query("SELECT *
                             FROM med_data_id_research
                             WHERE MRNUMBER=[MRNUMBER2]
                             AND DAY_TYPE=1;")
  p3score.s <- get_query("SELECT *
                          FROM seizure_data_id_research
                          WHERE MRNUMBER=[MRNUMBER3]
                          AND DAY_TYPE!=1 
                          AND row_names < (3601 + (SELECT COUNT(1) 
                                                   FROM seizure_data_id_research
                                                   WHERE MRNUMBER=[MRNUMBER3]
                                                   AND DAY_TYPE=1));")
  p3score.m <- get_query("SELECT *
                          FROM med_data_id_research
                          WHERE MRNUMBER=[MRNUMBER3]
                          AND DAY_TYPE!=1 
                          AND row_names < (3601 + (SELECT COUNT(1) 
                                                   FROM med_data_id_research
                                                   WHERE MRNUMBER=[MRNUMBER3]
                                                   AND DAY_TYPE=1));")
  p3baseline.s <- get_query("SELECT *
                             FROM seizure_data_id_research
                             WHERE MRNUMBER=[MRNUMBER3]
                             AND DAY_TYPE=1;")
  p3baseline.m <- get_query("SELECT *
                             FROM med_data_id_research
                             WHERE MRNUMBER=[MRNUMBER3]
                             AND DAY_TYPE=1;")
  
  p4score.s <- get_query("SELECT *
                          FROM seizure_data_id_research
                          WHERE MRNUMBER=[MRNUMBER6]
                          AND DAY_TYPE!=1;")
  p4score.m <- get_query("SELECT *
                          FROM med_data_id_research
                          WHERE MRNUMBER=[MRNUMBER6]
                          AND DAY_TYPE!=1;")
  p4baseline.s <- get_query("SELECT *
                             FROM seizure_data_id_research
                             WHERE MRNUMBER=[MRNUMBER6]
                             AND DAY_TYPE=1;")
  p4baseline.m <- get_query("SELECT *
                             FROM med_data_id_research
                             WHERE MRNUMBER=[MRNUMBER6]
                             AND DAY_TYPE=1;")
  
  formattable <- function(x) {
    rownames(x) <- seq(1:dim(x)[1])
    x$DATE <- as.Date(x$DATE)
    return(x)
  }
  
  p1score.s <- formattable(p1score.s)
  p1score.m <- formattable(p1score.m)
  p1baseline.s <- formattable(p1baseline.s) 
  p1baseline.m <- formattable(p1baseline.m)
  p2score.s <- formattable(p2score.s)
  p2score.m <- formattable(p2score.m)
  p2baseline.s <- formattable(p2baseline.s) 
  p2baseline.m <- formattable(p2baseline.m)
  p3score.s <- formattable(p3score.s)
  p3score.m <- formattable(p3score.m)
  p3baseline.s <- formattable(p3baseline.s) 
  p3baseline.m <- formattable(p3baseline.m)
  p4score.s <- formattable(p4score.s)
  p4score.m <- formattable(p4score.m)
  p4baseline.s <- formattable(p4baseline.s) 
  p4baseline.m <- formattable(p4baseline.m)
  
  calculatescore <- function(table,baseline) {
    table <- cbind.data.frame(table,NA,NA,NA,NA)
    colnames(table)[(dim(table)[2]-3):(dim(table)[2])] <- c("%_FREE","%_FREE_RESPONSE","RESPONSE","SCORE")
    object <- table[as.numeric(rownames(table))%%30==0 | as.numeric(rownames(table))==dim(table)[1],]
    object$'%_FREE'[1] <- 100*dim(table[table$DATE<=object$DATE[1] & table[,grepl("NUMBER_DAY",colnames(table))]==0,])[1]/dim(table[table$DATE<=object$DATE[1],])[1]
    medianbaseline <- ifelse(dim(baseline[baseline[,grepl("LOAD_DAY",colnames(baseline))]!=0,])[1]>0,median(baseline[baseline[,grepl("LOAD_DAY",colnames(baseline))]!=0,grepl("LOAD_DAY",colnames(baseline))]),0)
    mediantherapy <- ifelse(dim(table[table$DATE<=object$DATE[1] & table[,grepl("LOAD_DAY",colnames(table))]!=0,])[1]>0,median(table[table$DATE<=object$DATE[1] & table[,grepl("LOAD_DAY",colnames(table))]!=0,grepl("LOAD_DAY",colnames(table))]),0)
    object$RESPONSE[1] <- 100*mediantherapy/medianbaseline
    for (i in 2:(dim(object)[1])) {
      object$'%_FREE'[i] <- 100*dim(table[table$DATE<=object$DATE[i] & table$DATE>object$DATE[i-1] & table[,grepl("NUMBER_DAY",colnames(table))]==0,])[1]/dim(table[table$DATE<=object$DATE[i] & table$DATE>object$DATE[i-1],])[1]
      mediantherapy <- ifelse(dim(table[table$DATE<=object$DATE[i] & table$DATE>object$DATE[i-1] & table[,grepl("LOAD_DAY",colnames(table))]!=0,])[1]>0,median(table[table$DATE<=object$DATE[i] & table$DATE>object$DATE[i-1] & table[,grepl("LOAD_DAY",colnames(table))]!=0,grepl("LOAD_DAY",colnames(table))]),0)
      object$RESPONSE[i] <- 100*mediantherapy/medianbaseline
    }
    basepercent <- dim(baseline[baseline[,grepl("NUMBER_DAY",colnames(baseline))]==0,])[1]/dim(baseline)[1]
    object$'%_FREE_RESPONSE' <- 100-((object$'%_FREE')-basepercent)
    #print(object)
    object$SCORE <- 100*(((object$'%_FREE'/100)*(object$'%_FREE_RESPONSE'/100))+(((100-object$'%_FREE')/100)*(object$RESPONSE/100)))
    object[,colnames(object)%in%c("%_FREE","%_FREE_RESPONSE","RESPONSE","SCORE")] <- round(object[,colnames(object)%in%c("%_FREE","%_FREE_RESPONSE","RESPONSE","SCORE")],2)
    return(object)
  }
  
  #p1score.s 
  p1score.s <- calculatescore(p1score.s,p1baseline.s)
  p1score.m <- calculatescore(p1score.m,p1baseline.m)
  p2score.s <- calculatescore(p2score.s,p2baseline.s)
  p2score.m <- calculatescore(p2score.m,p2baseline.m)
  p3score.s <- calculatescore(p3score.s,p3baseline.s)
  p3score.m <- calculatescore(p3score.m,p3baseline.m)
  p4score.s <- calculatescore(p4score.s,p4baseline.s)
  p4score.m <- calculatescore(p4score.m,p4baseline.m)
  
  outcomescore <- function(seizure,med) {
    outcome <- data.frame(DATE=seizure$DATE,SEIZURESCORE=seizure$SCORE,MEDSCORE=med$SCORE,OUTCOMESCORE=rep(NA,length(seizure$DATE)))
    outcome$OUTCOMESCORE <- (seizure$SCORE+med$SCORE)/2
    return(outcome)
  }
  
  p1score.o <- outcomescore(p1score.s,p1score.m)
  p2score.o <- outcomescore(p2score.s,p2score.m)
  p3score.o <- outcomescore(p3score.s,p3score.m)
  p4score.o <- outcomescore(p4score.s,p4score.m)
  #print(p4score.o)
  
  scoreplot <- function(score,kgnumber,sh,col,x.axis,y.axis) {
    score <- melt(score,id=c("DATE"))
    score$variable <- factor(score$variable,levels=c("SEIZURESCORE","MEDSCORE","OUTCOMESCORE"))
    score <- cbind.data.frame(MONTHNUM=rep(NA,dim(score)[1]),score)
    for (i in unique(score$variable)) {
      k <- 1
      for (j in unique(score$DATE)) {
        score[score$variable==i & score$DATE==j,colnames(score)=="MONTHNUM"] <- k
        k <- k + 1
      }
    }
    plotscore <- ggplot(score,aes(MONTHNUM,value,fill=factor(variable)))
    plotscore <- plotscore + geom_hline(yintercept=100)
    plotscore <- plotscore + geom_line(data=score[score$variable=="SEIZURESCORE",],color="black") 
    plotscore <- plotscore + geom_line(data=score[score$variable=="MEDSCORE",],color="black") 
    plotscore <- plotscore + geom_line(data=score[score$variable=="OUTCOMESCORE",],color="black") 
    plotscore <- plotscore + geom_point(shape=sh,color=col,size=2)
    plotscore <- plotscore + xlab("Month on Therapy") + ylab("Score and Outcome (%)") + ggtitle(paste("Seizure Score, Med Score, and Outcome per 30 days:",kgnumber))
    plotscore <- plotscore + theme(axis.title=element_text(angle=0,size=10))
    plotscore <- plotscore + theme(plot.title=element_text(angle=0,size=10))
    plotscore <- plotscore + theme(legend.title=element_text(size=14,face="bold"))
    plotscore <- plotscore + scale_fill_manual(name="",labels=c("Med Score","Outcome","Seizure Score"),values=c("white","gray","black"))
    plotscore <- plotscore + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5,size=8))
    plotscore <- plotscore + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
    plotscore <- plotscore + theme(panel.border=element_rect(colour="black",fill=NA,size=1.5))
    plotscore <- plotscore + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=6))
    plotscore <- plotscore + scale_x_continuous(limits=c(0,x.axis),breaks=scales::pretty_breaks(24))
    plotscore <- plotscore + scale_y_continuous(limits=c(0,y.axis),breaks=scales::pretty_breaks(10))
    print(paste(kgnumber,"seizure, med, outcome score per 30 days line plot created"))
    plotscorefile <- gsub(" ","",paste(kgnumber,"_TOOLS_MANUSCRIPT_SCORE_30_DAYS_LINE_PLOT.jpeg"))
    ggsave(plotscore,filename=plotscorefile,width=6,height=4)
    return(plotscore)
  }

  scoreplot(p1score.o,"KG0129",21,"red",121,110)
  scoreplot(p2score.o,"KG0020",22,"green",121,600)
  scoreplot(p3score.o,"KG0174",23,"yellow",121,150)
  scoreplot(p4score.o,"KG0194",24,"blue",101,100)
}