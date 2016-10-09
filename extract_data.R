
library(RODBC)
channel = odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/Keguo/Dropbox/Research/3RD PROJECT/HomeHospital/database/April2004/April2004.mdb")
Table1Dat <- sqlFetch(channel, "visits")

file <- "C:/Users/Keguo/Dropbox/Research/3RD PROJECT/HomeHospital/database/April2004/visits.txt"
apr2014_visits <- read.delim(file = file, header = T, sep =",")


months = c("January", "February", "March", "April", "May", "June", "July", 
           "August", "September", "October", "November", "December")
years = 2004:2007
tables = c("physical_details", "visit_details",
           "visits", "ward_first_procedure", "xrays_visits")

# "prev_physical_details", "prev_visit_details",
# "prev_visits", "pre_ward_first_procedure", 

for (i in years[1:2]){
  for (j in months[1:2]){
    for (k in tables[1:2]){
      file <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/",
                    "Users/Keguo/Dropbox/Research/3RD PROJECT/HomeHospital/",
                    "database/",j, i, "/", j, i, ".mdb", sep = "")
      channel = odbcDriverConnect(file)
      assign(paste(i, j, k, sep = "_"), sqlFetch(channel, k))
    }
  }
}