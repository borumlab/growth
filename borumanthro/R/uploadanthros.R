#' Upload to database
#'
#' This function allow you to upload seizure or med data into the clinical and research tables of the MySQL database.
#' It will be necessary to have access to the lab's database in order to do this
#' @param data Data to be input into the research table
#' @param str String defined as either "MED" or "SEIZURE"
#' @param sourcedata Data to be input to the calculated table
#' @param table Data to be added to the ranking table (only used if str=="SEIZURE")
#' @keywords upload database
#' @import RMySQL
#function to upload anthros into MySQL
uploadanthros <- function(finaltable){

  library(RMySQL)

  all_cons <- dbListConnections(MySQL())
  for (con in all_cons) {
    dbDisconnect(con)
  }

  # Input MySQL user name, password, database name, and host
  # Then establish a connection to the database
  print("When prompted, please input the MySQL username, password, database name, and host that you wish to use")
  print("To leave any of these fields blank, press the enter key without typing anything else")
  u <- readline(prompt="User: ")
  p <- readline(prompt="Password: ")
  d <- readline(prompt="Database Name: ")
  h <- readline(prompt="Host: ")
  connect <- dbConnect(MySQL(),user=u,password=p,dbname=d,host=h)

  dbSendQuery(connect,paste("USE",d))

  mrnumber <- unique(finaltable$MRNUMBER)
  table <- "anthropometrics_id_research"

  exists <- dbGetQuery(connect,paste("SHOW TABLES FROM",d,"LIKE",gsub(" ","",paste("'",table,"'")),";"))
  if (length(exists[,1])==1) {
    exists.this <- dbGetQuery(connect,paste("SELECT * FROM",table,"WHERE MRNUMBER=",mrnumber,";"))
    if (dim(exists.this)[1]>0) {
      print(paste("Data for patient with mrnumber",mrnumber,"already exists in table",
                  gsub(" ","",paste(table,".")),"Would you like to update it?"))
      r <- " "
      while (tolower(r)!="yes" & tolower(r)!="no") {
        r <- readline(prompt="Type 'yes' or 'no': ")
      }
      if (tolower(r)=="yes") {
        dbSendQuery(connect,paste("DELETE FROM",table,"WHERE MRNUMBER=",mrnumber,";"))
        dbWriteTable(connect,name=table,value=finaltable,append=TRUE)
      }
    } else {
      dbWriteTable(connect,name=table,value=finaltable,append=TRUE)
    }
    print(paste("Table",table,"has been updated with data for this patient"))
  } else if (length(exists[,1])==0) {
    dbWriteTable(connect,name=table,value=finaltable,append=TRUE)
    print(paste("Table",table,"has been created and can be found in the database"))
  }
}
