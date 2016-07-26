#' Upload to database
#'
#' This function allow you to upload anthropometric data into the MySQL database.
#' It will be necessary to have access to the lab's database in order to do this
#' @param data Data to be input into the research table
#' @param str String defined as "ANTHROS"
#' @param sourcedata Data to be input to the calculated table
#' @keywords upload database
#' @import RMySQL

# Purpose - This script is used to upload data into the following table to the patient_pkt database:
#           ANTHROPOMETRICS_ID_RESEARCH
# Parameters - data = data input to the research table
#              str = string defined as "ANTHROS"
#              sourcedata = data input to the calculated table
            
uploadtodatabase <- function(data,str,sourcedata) {
  
  if (!require("RMySQL")) {
    install.packages("RMySQL")
  }
  library(RMySQL)
  
  # Delete any previously existing connections creating using the RMySQL package
  # (If too many are running at once, the script will not be able to establish a new connection)
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons) {
    dbDisconnect(con)
  }
  
  str <- str
  data <- data
  
  mrnumber <- unique(data$MRNUMBER)
  
  # Input MySQL user name, password, database name, and host
  # Then establish a connection to the database
  print("When prompted, please input the MySQL username, password, database name, and host that you wish to use")
  print("To leave any of these fields blank, press the enter key without typing anything else")
  u <- readline(prompt="User: ")
  p <- readline(prompt="Password: ")
  d <- readline(prompt="Database Name: ")
  h <- readline(prompt="Host: ")
  connect <- dbConnect(dbDriver("MySQL"),user=u,password=p,dbname=d,host=h)
  
  dbSendQuery(connect,"USE patient_pkt;")
  
  # Query the last data found in both the ANTHROPOMETRICS_ID_RESEARCH table for
  # If there is no data in a table for that patient, let the user know. 
  # If there is data, print the first and last date found in each table for
  # that patient on the console.
  if (str=="ANTHROS") {
    MySQLstatement <- paste("SELECT DATE FROM anthropometrics_id_research WHERE MRNUMBER =",mrnumber,";")
    getdate <- data.frame(dbGetQuery(connect,MySQLstatement))
    if (is.na(getdate$DATE[1])) {
      print(paste("There is no data in the table for this patient with mrnumber",mrnumber))
    } else {
      print(paste("The first date found in the table for this patient with mrnumber",mrnumber,"is:",getdate$DATE[1]))
      print(paste("The last date found in the table for this patient with mrnumber",mrnumber,"is:",getdate$DATE[length(getdate$DATE)]))
    }
    
  }
  
  # Input the first and last date of the range of dates to upload data into the calculated table
  # If there is any overlap in dates, you will be informed of this, and you will additionally be 
  # asked to confirm whether you wish to replace the data for these dates in the database with the
  # new data or not
  print(paste("Type 'yes' to input data into the",tolower(str),"calculated table in the database"))
  r <- readline(prompt="Enter here: ")
  if (r=="yes") {
    print(paste("Please specify the range of dates that you would like to have added to the",tolower(str),"calculated table"))
    print("Format date in this manner: year, then month, then day (all numeric), seperating each with either all '/' or all '-'")
    print("Example: 2016/1/5 or 2016-1-5 (January 5th, 2016)")
    first <- readline(prompt="First date: ")
    last <- readline(prompt="Last date: ")
    first <- as.Date(first)
    last <- as.Date(last)
    n <- ""
    if (str=="ANTHROS") {
      n <- "anthropometrics_id_research"
    } 
    
    subset <- sourcedata[sourcedata$DATE>=first & sourcedata$DATE<=last,]
    return(subset)
    MySQLstatement <- paste("SELECT DATE FROM",n,"WHERE MRNUMBER=",mrnumber,";")
    exist.dates <- data.frame(dbGetQuery(connect,MySQLstatement))
    exist.dates$DATE <- as.Date(exist.dates$DATE)
    print(exist.dates)
    if (!is.na(exist.dates$DATE[1])) {
      overlap <- exist.dates[exist.dates$DATE %in% subset$DATE,colnames(exist.dates)=="DATE"]
      colnames(overlap) <- "DATE"
      if (!is.na(overlap$DATE[1])) {
        print(paste("Dates ranging from",as.character(overlap$DATE[1]),"to",as.character(overlap$DATE[length(overlap$DATE)]), 
                    "already exist in the database"))
        print("Would you like to replace these dates in the database with this new data?")
        print("To do so, type 'yes'")
        print("Otherwise, type anything else and no changes will be made to this table")
        r <- readline(prompt="Enter here: ")
        if (r=="yes") {
          MySQLstatement <- paste("DELETE FROM",n,"WHERE MRNUMBER=",mrnumber, 
                                  "AND DATE >=",as.character(overlap$DATE[1]), 
                                  "AND DATE <=",as.character(overlap$DATE[length(overlap$DATE)]),";")
          dbSendQuery(connect,MySQLstatement)
          dbWriteTable(connect,value=subset,name=n,append=TRUE)
        }
      }
    } else {
      dbWriteTable(connect,value=subset,name=n,append=TRUE)
    }
  }
  
  print("Your tables in MySQL have been updated")
}
