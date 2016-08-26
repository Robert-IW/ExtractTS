## To match-up in situ stations with nearest satellite data for each day.
## The output is a list for each sensor (e.g. 'lst_MODISA') of data frames for each day

## Requires 'getInsituRecords.R' to be run first (or load 'OutPut01...Rdata')
## Requires 'list_dates', 'row_stations' 'col_time' from 'getInsituRecords.R'
## Calls 'callSatDataList.R'
## Calls the set of 'callSENSORSatVal.R' scripts

#################################################################################
# WARNING: Check that rounding 23:46 doesn't end up as 00:00
# anything beyond 11:45 should be rounded to 11:30
#################################################################################

require("Imap")
require("ncdf4")
require("tools")

base_source <- "/home/robert/Dropbox/PostDoc/R/SatVSInsitu/"

type_data <- list("AATSR","AVHRR","MODISA","MODIST","SEVIRIlo","SEVIRIhi")
num_st <- length(row_stations)

# create a list to store the data frames of each sensor and for each daily file
lst_AATSR     <- setNames(vector("list", length(list_dates)), list_dates)
lst_AVHRR     <- setNames(vector("list", length(list_dates)), list_dates)
lst_MODISA    <- setNames(vector("list", length(list_dates)), list_dates)
lst_MODIST    <- setNames(vector("list", length(list_dates)), list_dates)
lst_SEVIRI_lo <- setNames(vector("list", length(list_dates)), list_dates)
lst_SEVIRI_hi <- setNames(vector("list", length(list_dates)), list_dates)

# create a list to store the regional subset data frames of each sensor granule
gran_AATSR     <- setNames(vector("list", length(list_dates)), list_dates)
gran_AVHRR     <- setNames(vector("list", length(list_dates)), list_dates)
gran_MODISA    <- setNames(vector("list", length(list_dates)), list_dates)
gran_MODIST    <- setNames(vector("list", length(list_dates)), list_dates)
gran_SEVIRI_lo <- setNames(vector("list", length(list_dates)), list_dates)

################################################################################
# because of size, SEVIRI hi is done on a monthly sequence
# initialize day counter, it is reset in 'callSEVERIhiSatVal.R'
ii <- 0               # count the days in each month to index the monthly granule list

########## FOR EACH DAY 'i'
for (i in 1:length(list_dates)){
  
  # Get a list of files for this date for each sensor
  print(paste("Starting on day", i))
  
  # get the dateas seperate year month day for the file search pattern
  temp_date <- as.Date(list_dates[i])
  val_year <<-  format(temp_date,"%Y")
  val_month <<- format(temp_date, "%m")
  val_day <- format(temp_date,"%d")
  
  # run the function to get a list of all the sensor files for the date
  source(paste(base_source,"callSatDataList.R", sep = ""))

  ########################################## WARNING ######################################
  # This section produces very large Rdata file. It may be better to run 1 sensor at a time
  
  ########## FOR EACH SENSOR 'j'
  for (j in 6){ #1:length(type_data)){
    type_sensor <- type_data[[j]]
    
    if (type_sensor == "AATSR"){
      source(paste(base_source,"callAATSRSatVal.R", sep = ""))
    } else if (type_sensor == "AVHRR"){
      source(paste(base_source,"callAVHRRSatVal.R", sep = ""))
    } else if (type_sensor == "MODISA"){
      source(paste(base_source,"callMODISASatVal.R", sep = ""))
    } else if (type_sensor == "MODIST"){
      source(paste(base_source,"callMODISTSatVal.R", sep = ""))
    } else if (type_sensor == "SEVIRIlo"){
      source(paste(base_source,"callSEVIRIloSatVal.R", sep = ""))
    } else if (type_sensor == "SEVIRIhi"){
      source(paste(base_source,"callSEVIRIhiSatVal.R", sep = ""))
    }
    
  } # for each sensor 'j'
} # for each day 'i'
rm(i, j, k, x, y, base_source, type_sensor, type_data, val_day, val_month, val_year, temp_date)
rm(type_data, AATSRList,AVHRRList,MODISAList,MODISTList,SEVIRIhiList,SEVIRIloList)
