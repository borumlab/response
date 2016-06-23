connect <- dbConnect(MySQL(),user="jlee101",password="D@4@b@se!",dbname="patient_pkt",host="IF-SRVV-BORUM")

MySQLstatement <- paste("SELECT COUNT(MRNUMBER) AS VALUE FROM seizure_ranking_id_source WHERE MRNUMBER =",unique(data$MRNUMBER),";")

MySQLstatement <- paste("SELECT MRNUMBER, DATE, SEIZURE_LOAD_DAY, SEIZURE_NUMBER_DAY, SEIZURE_RESPONSE_DAY, SEIZURE_NUMBER_RESPONSE_DAY
                        FROM patient_pkt.seizure_data_id_research
                        WHERE MRNUMBER IN('1449813','1338587');")
is.empty <- data.frame(dbGetQuery(connect,MySQLstatement))

