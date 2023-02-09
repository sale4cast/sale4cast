library(lubridate)

findDateTimeFormat <- function(dateTimeVec){
  #daily date format 
  if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y", quiet = TRUE))) == 0) format <- "dmy"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y", quiet = TRUE))) == 0) format <- "mdy"   
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d", quiet = TRUE))) == 0) format <- "ymd"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m", quiet = TRUE))) == 0) format <- "ydm"
  #day-month-year hour AM PM     
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H %p", quiet = TRUE))) == 0) format <- "dmy_h_p" 
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H", quiet = TRUE))) == 0)   format <- "dmy_h"  
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M %p", quiet = TRUE))) == 0) format <- "dmy_hm_p"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M", quiet = TRUE))) == 0)  format <- "dmy_hm"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S %p", quiet = TRUE))) == 0) format <- "dmy_hms_p"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S", quiet = TRUE))) == 0) format <- "dmy_hms"
  
  #month-day-year hour AM PM
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H %p", quiet = TRUE))) == 0) format <- "mdy_h_p" 
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H", quiet = TRUE))) == 0)   format <- "mdy_h"    
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M %p", quiet = TRUE))) == 0) format <- "mdy_hm_p" 
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M", quiet = TRUE))) == 0)  format <- "mdy_hm"  
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S %p", quiet = TRUE))) == 0) format <- "mdy_hms_p" 
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S", quiet = TRUE))) == 0) format <- "mdy_hms"
  
  #year-month-day hour AM PM
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H %p", quiet = TRUE))) == 0) format <- "ymd_h_p" 
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H", quiet = TRUE))) == 0)   format <- "ymd_h"  
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M %p", quiet = TRUE))) == 0) format <- "ymd_hm_p"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M", quiet = TRUE))) == 0)  format <- "ymd_hm"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S %p", quiet = TRUE))) == 0) format <- "ymd_hms_p"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S", quiet = TRUE))) == 0) format <- "ymd_hms"
  
  #year-day-month hour AM PM
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H %p", quiet = TRUE))) == 0) format <- "ydm_h_p"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H", quiet = TRUE))) == 0)   format <- "ydm_h"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M %p", quiet = TRUE))) == 0) format <- "ydm_hm_p"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M", quiet = TRUE))) == 0)  format <- "ydm_hm"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S %p", quiet = TRUE))) == 0) format <- "ydm_hms_p"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S", quiet = TRUE))) == 0) format <- "ydm_hms" 
  
  #With UTC offset
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S %z",quiet = TRUE))) == 0) format <- "dmy_hms_z"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S %z",quiet = TRUE))) == 0) format <- "mdy_hms_z"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S %z",quiet = TRUE))) == 0) format <- "ydm_hms_z"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S %z",quiet = TRUE))) == 0) format <- "ymd_hms_z"
  
  
  #Truncated date time format
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S",truncated = 3, quiet = TRUE))) == 0) format <- "ydm_hms_tc"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S",truncated = 3, quiet = TRUE))) == 0)   format <- "dmy_hms_tc"    
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S",truncated = 3, quiet = TRUE))) == 0) format <- "ymd_hms_tc"
  else if (sum(is.na(dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S",truncated = 3, quiet = TRUE))) == 0)  format <- "mdy_hms_tc"
  
  else format <- "0"
  return(format)
} 

convertToISOFormat <- function(dateTimeFormat, dateTimeVec){
  #daily date format 
  if (dateTimeFormat == "dmy") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y")
  else if (dateTimeFormat == "mdy") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y")
  else if (dateTimeFormat == "ymd") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d")
  else if (dateTimeFormat == "ydm") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m")
  
  #day-month-year hour AM PM 
  else if (dateTimeFormat == "dmy_h_p") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H %p")
  else if (dateTimeFormat == "dmy_h") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H")
  else if (dateTimeFormat == "dmy_hm_p") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M %p")
  else if (dateTimeFormat == "dmy_hm") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M")
  else if (dateTimeFormat == "dmy_hms_p") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S %p")
  else if (dateTimeFormat == "dmy_hms") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S")
  
  #month-day-year hour AM PM
  else if (dateTimeFormat == "mdy_h_p") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H %p")
  else if (dateTimeFormat == "mdy_h") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H")
  else if (dateTimeFormat == "mdy_hm_p") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M %p")
  else if (dateTimeFormat == "mdy_hm") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M")
  else if (dateTimeFormat == "mdy_hms_p") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S %p")
  else if (dateTimeFormat == "mdy_hms") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S")
  
  #year-month-day hour AM PM
  else if (dateTimeFormat == "ymd_h_p") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H %p")
  else if (dateTimeFormat == "ymd_h") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H")
  else if (dateTimeFormat == "ymd_hm_p") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M %p")
  else if (dateTimeFormat == "ymd_hm") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M")
  else if (dateTimeFormat == "ymd_hms_p") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S %p")
  else if (dateTimeFormat == "ymd_hms") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S")
  
  #year-day-month hour AM PM
  else if (dateTimeFormat == "ydm_h_p") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H %p")
  else if (dateTimeFormat == "ydm_h") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H")
  else if (dateTimeFormat == "ydm_hm_p") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M %p")
  else if (dateTimeFormat == "ydm_hm") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M")
  else if (dateTimeFormat == "ydm_hms_p") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S %p")
  else if (dateTimeFormat == "ydm_hms") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S")
  
  #with UTC offset
  else if (dateTimeFormat == "dmy_hms_z") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S %z")
  else if (dateTimeFormat == "mdy_hms_z") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S %z")
  else if (dateTimeFormat == "ydm_hms_z") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S %z")
  else if (dateTimeFormat == "ymd_hms_z") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S %z")
  
  #Exceptional or unfinished file handling
  else if (dateTimeFormat == "ydm_hms_tc") dateTime <- parse_date_time(dateTimeVec, orders = "%y%d%m %H%M%S",truncated = 3)
  else if (dateTimeFormat == "dmy_hms_tc") dateTime <- parse_date_time(dateTimeVec, orders = "%d%m%y %H%M%S",truncated = 3)
  else if (dateTimeFormat == "ymd_hms_tc") dateTime <- parse_date_time(dateTimeVec, orders = "%y%m%d %H%M%S",truncated = 3)
  else if (dateTimeFormat == "mdy_hms_tc") dateTime <- parse_date_time(dateTimeVec, orders = "%m%d%y %H%M%S",truncated = 3)
  
  return(dateTime)
}   
