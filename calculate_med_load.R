calculate_med_load <- function() {
  
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
  
  ## Read in all relevant data from xlsx files
  print("Input the four letters that signify the patient we are doing calculations for")
  print("Example: FILA")
  patient <- readline(prompt="Enter here: ")
  
  print("Input the directory that you wish to draw this patient's DEMOGRAPHICS_SOURCE file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  demo <- "DEMOGRAPHICS_SOURCE.xlsx"
  demo <- read.xlsx(demo,sheet=1,detectDates=TRUE)
  
  print("Input the directory that you wish to draw this patient's MED_RANKING_SOURCE file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  ranking <- "MED_RANKING_SOURCE.xlsx"
  ranking <- read.xlsx(ranking,sheet=1,detectDates=TRUE)
  
  print("Input the directory that you wish to draw this patient's MED_DATA_SOURCE file and ANTHROPOMETRICS_CLINICAL file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  data <- "MED_DATA_SOURCE.xlsx"
  data <- gsub(" ","",paste(patient,"_",data))
  data <- read.xlsx(data,sheet=1,detectDates=TRUE)
  
  anthro <- "ANTHROPOMETRICS_CLINICAL.xlsx"
  anthro <- gsub(" ","",paste(patient,"_",anthro))
  anthro <- read.xlsx(anthro,sheet=1,cols=c(1,2,3,4,5,6,81),detectDates=TRUE)
  
  data <- data[!is.na(data$MRNUMBER),]
  anthro <- anthro[!is.na(anthro$MRNUMBER),]
  demo <- demo[!is.na(demo$LAST),]
  ranking <- ranking[!is.na(ranking$MED_GENERIC_NAME),]
  
  print("Calculating med intake in mg/kg/day, please wait...")
  
  mrnumber <- unique(anthro$MRNUMBER)
  birthdate <- demo$BIRTH_DATE[demo$MEDICAL_RECORD_NUMBER==mrnumber]
  birthdate <- as.Date(birthdate,format="%m/%d/%Y")
  
  for (i in 1:length(data$DATE)) {
    if (nchar(as.character(data$DATE[i]))>10) {
      data$DATE <- substr(data$DATE,1,nchar(as.character(data$DATE))-5)
    }
  }
  data$DATE <- as.Date(data$DATE,format="%m/%d/%Y")
  
  med_dose <- data[,colnames(data)=="DATE" | colnames(data)=="MED_ID" | colnames(data)=="DAILY_MED_DOSAGE_MG"]
  
  anthro <- anthro[,colnames(anthro)=="MRNUMBER" | colnames(anthro)=="DATE" | colnames(anthro)=="WT_DAY"]
  colnames(anthro)[3] <- "WT_DAY"
  anthro$DATE <- as.Date(anthro$DATE,format="%m/%d/%Y")
  
  last_date <- as.Date(max(max(unique(anthro[,2])),max(unique(med_dose[,1]))))
  
  ## Create table with start and end dates for each period of specific med usage and dosage
  END_DATE <- data.frame(rep(unique(med_dose$DATE)[2]-1,length(med_dose[med_dose$DATE==unique(med_dose$DATE)[2-1],2])))
  colnames(END_DATE) <- "END_DATE"
  for (i in 3:(length(unique(med_dose$DATE)))) {
    end_date <- data.frame(rep(unique(med_dose$DATE)[i]-1,length(med_dose[med_dose$DATE==unique(med_dose$DATE)[i-1],1])))
    colnames(end_date) <- "END_DATE"
    END_DATE <- data.frame(rbind(END_DATE,end_date))
    colnames(END_DATE) <- "END_DATE"
  }
  na <- cbind(rep(NA,length(med_dose[med_dose$DATE==unique(med_dose$DATE)[length(unique(med_dose$DATE))],colnames(med_dose)=="DATE"])))
  colnames(na) <- "END_DATE"
  END_DATE <- data.frame(rbind(END_DATE,na))
  med_dose <- cbind(med_dose$DATE,END_DATE,med_dose[,colnames(med_dose)!="DATE"])
  colnames(med_dose)[1] <- "START_DATE"
  
  ## Create table with start and end dates for each period of specific weight, which will be used for linear interpolation
  anthro <- data.frame(anthro,birthdate)
  colnames(anthro)[4] <- "BIRTHDATE"
  
  ##
  med_intake <- data.frame(start.date=as.Date(character()),med.id=character(),
                           dosage=integer(),weight=integer(),med.intake=integer())
  temp_date <- med_dose$START_DATE[1]
  
  for (i in 1:((as.integer(last_date-med_dose$START_DATE[1]))+1)) {
    compare <- max(unique(med_dose$START_DATE)[unique(med_dose$START_DATE)<=temp_date])
    temp_med_intake <- data.frame(temp_date,med_dose[med_dose$START_DATE==compare,colnames(med_dose)=="MED_ID" | colnames(med_dose)=="DAILY_MED_DOSAGE_MG"],NA,NA)
    colnames(temp_med_intake) <- c("DATE","MED_ID","DOSAGE","WEIGHT","MED_INTAKE")
    med_intake <- data.frame(rbind(med_intake,temp_med_intake))
    
    weight <- anthro[anthro$DATE==temp_date,colnames(anthro)=="WT_DAY"]
    med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="WEIGHT"] <- weight
    med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="MED_INTAKE"] <- med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="DOSAGE"]/med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="WEIGHT"] 
    temp_date <- med_dose$START_DATE[1]+i
  }
  
  print("Calculating minimum dose in mg/kg/day, please wait...")
  
  ## Denominator: Minimum dose in mg/kg/day
  
  if (!require("lubridate")) {
    install.packages("lubridate")
  }
  library(lubridate)
  
  med_min_dose <- data.frame(med_intake[,colnames(med_intake)!="MED_INTAKE"],AGE=NA,MIN_DOSE=NA)
  
  temp_date <- med_dose$START_DATE[1]
  for (i in 1:(as.integer(last_date-med_dose[1,1])+1)) {
    duration <- interval(birthdate,temp_date)
    med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="AGE"] <- round(duration/dyears(1),2)
    
    for (j in med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="MED_ID"]) {
      compare <- unique(med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="AGE"])
      if (j == "MID0003") {
        compare <- unique(med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="WEIGHT"])
      }
      for (k in 1:(length(ranking[ranking$MED_ID==j,colnames(ranking)=="MED_ID"]))) {
        if ((compare >= ranking[ranking$MED_ID==j,][k,colnames(ranking)=="MED_LIMIT_LOW"]) && (compare <= ranking[ranking$MED_ID==j,][k,colnames(ranking)=="MED_LIMIT_HIGH"])) {
          med_min_dose[med_min_dose$DATE==temp_date & med_min_dose$MED_ID==j,colnames(med_min_dose)=="MIN_DOSE"] <- ranking[ranking$MED_ID==j,][k,colnames(ranking)=="MED_MIN_DOSE_KG"]
          break
        }
      }
    }
    
    temp_date <- temp_date+1
  }
  
  print("Calculating med load per day, please wait...")
  
  med_min_dose[,colnames(med_min_dose)=="WEIGHT"] <- round(med_min_dose$WEIGHT,2)
  
  ## Use numerator and denominator to calculate the med load
  med_load <- data.frame(med_min_dose[,colnames(med_min_dose)!="MIN_DOSE"],med_intake[,colnames(med_intake)=="MED_INTAKE"],med_min_dose[,colnames(med_min_dose)=="MIN_DOSE"],MED_LOAD_PER_MED=NA,MED_LOAD_DAY=NA)
  colnames(med_load)[c(6,7)] <- c("MED_INTAKE","MIN_DOSE")
  med_load$MED_LOAD_PER_MED <- round(med_load$MED_INTAKE/med_load$MIN_DOSE,4)
  med_load[,colnames(med_load)=="MED_INTAKE"] <- round(med_load$MED_INTAKE,4)
  for (i in unique(med_load$DATE)) {
    med_load[med_load$DATE==i,colnames(med_load)=="MED_LOAD_DAY"][1] <- sum(med_load[med_load$DATE==i,colnames(med_load)=="MED_LOAD_PER_MED"])  
  }
  
  med.number.per.day <- rep(NA,dim(med_load)[1])
  med_load <- data.frame(med_load,med.number.per.day)
  colnames(med_load)[10] <- "MED_NUMBER_DAY"
  for (i in unique(med_load[,1])) {
    med_load[med_load$DATE==i,colnames(med_load)=="MED_NUMBER_DAY"][1] <- length(med_load[med_load$DATE==i & med_load[,colnames(med_load)=="MED_INTAKE"]!=0,colnames(med_load)=="MED_INTAKE"])
  }
  
  med_load <- data.frame(mrnumber,med_load)
  colnames(med_load)[1] <- "MRNUMBER"
  
  med_load$DATE <- as.Date(med_load$DATE,format="%m/%d/%Y")
  na <- rep(NA,dim(med_load)[1])
  med_load <- data.frame(med_load[,1:2],na,med_load[,3:11])
  colnames(med_load)[3] <- "DAY_TYPE"
  for (i in 1:dim(med_load)[1]) {
    med_load$DAY_TYPE[i] <- unique(data[data$DATE==max(unique(data$DATE[data$DATE<=med_load$DATE[i]])),colnames(data)=="DAY_TYPE"])
  }
  
  daily.dosage <- data.frame(DATE=as.Date(as.character()),DAILY_MED_DOSAGE_MG=integer(),DAILY_MED_DOSAGE_MG_KG=integer())
  daily.dosage[1:dim(med_load)[1],c("DATE")] <- med_load$DATE
  daily.dosage$DAILY_MED_DOSAGE_MG <- med_load$DOSAGE[med_load$DATE==daily.dosage$DATE]
  daily.dosage$DAILY_MED_DOSAGE_MG_KG <- round(daily.dosage$DAILY_MED_DOSAGE_MG/(med_load$WEIGHT[med_load$DATE==daily.dosage$DATE]),4)
  load <- data.frame(med_load[,1:3],med_load[,4],daily.dosage[,2:3],med_load[,9:10])
  colnames(load) <- c("MRNUMBER","DATE","DAY_TYPE","MED_ID","DAILY_MED_DOSAGE_MG","DAILY_MED_DOSAGE_MG_KG","MED_MIN_DOSE","MED_LOAD_MED")
  
  colnames(med_load) <- c("MRNUMBER","DATE","DAY_TYPE","MED_ID","DAILY_MED_DOSAGE_MG","WT","AGE","DAILY_MED_DOSAGE_MG_KG","MED_MIN_DOSE","MED_LOAD_MED","MED_LOAD_DAY","MED_NUMBER_DAY")
  
  observe_load <- FALSE
  print("Would you like to save a temporary file to look at the med loads?")
  print("Type 'YES' to save a file to look at, type 'NO' to move onto next step")
  rl <- " "
  while (tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if (tolower(rl)=="yes") {
    observe_load <- TRUE
  }
  if (observe_load == TRUE) {
    print(paste("Saving med load table as",gsub(" ","",paste(patient,"_MED_LOAD.xlsx")),"in directory",directory))
    xlsx <- "MED_LOAD.xlsx"
    xlsx <- gsub(" ","",paste(patient,"_",xlsx))
    write.xlsx2(med_load,file=xlsx,showNA=FALSE,row.names=FALSE)
    print("Type 'OKAY' whenever you are ready to move on to the next step")
    print("Or type 'QUIT' if you would like to exit")
    while (tolower(rl)!="okay" && tolower(rl)!="quit") {
      rl <- readline(prompt="Enter here: ")
    }
  } else {
    rl <- "okay"
  }
  if (tolower(rl)=="okay") {
    calculate_med_response(patient,data,med_load,load,NA)
  }
}