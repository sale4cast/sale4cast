library(data.table)
findDateTimeColumnIndex <- function(datf)
{
  aggMinToHourDatf <- aggMinToHourTS <- aggMinToHourFlag <- aggHourToHalfDayDatf <- 0
  aggHourToHalfDayTS <- aggHourToHalfDayFlag <- aggHalfToFullDayDatf <- aggHalfToFullDayTS <- aggHalfToFullDayFlag <- 0
  dateTimeColIndex <- dateColIndex <- timeColIndex <- valueColIndex <- 0
  dateTimeFormat <- dateFormat <- timeFormat <- "0"
  nCol <- NCOL(datf)
  if(NROW(datf) > 60 && nCol > 0) {
    datf10Row <- head(datf,10)
    a <- 10
    for(colNo in 1:nCol) {
      if((dateTimeFormat = findDateTimeFormat(datf10Row[[colNo]])) != "0") {
        dateTimeColIndex = colNo
        if(dateTimeFormat == "ymd" || dateTimeFormat == "ydm" || dateTimeFormat == "mdy" || dateTimeFormat == "dmy") {
          dateColIndex = dateTimeColIndex
          nextCol = dateColIndex + 1
          prevCol = dateColIndex - 1
          dateTimeColIndex = 0
          if(timeColIndex == 0 && nextCol <= nCol) {
            for(tColIndex in nextCol:nCol) {
              if(suppressWarnings(sum(is.na(parse_time(datf10Row[[tColIndex]])) == 0))) {
                timeColIndex = tColIndex
                break
              }
            }
          }
          if(timeColIndex == 0 && prevCol > 0) {
            for(tColIndex in 1:prevCol) {
              if(suppressWarnings(sum(is.na(parse_time(datf10Row[[tColIndex]])) == 0))) {
                timeColIndex = tColIndex
                break
              }
            }
          }
        }
        break
      }
    }
  }
  a <- 10
  if(dateTimeColIndex == 0 && dateColIndex == 0 && timeColIndex == 0) {
    stop("There is no time stamp")
  } 
  else if(dateTimeColIndex != 0 && dateColIndex == 0 && timeColIndex == 0) {
    #column1: A76, 12/17/2022T23:12:32Z, 27 
    if(nCol > dateTimeColIndex) 
      valueColIndex = dateTimeColIndex + 1
  }
  else if(dateTimeColIndex == 0 && dateColIndex != 0 && timeColIndex != 0) {
    #column1: 12/17/2022, A76, 23:12:32, 27  
    #column1: A76, 12/17/2022, 23:12:32, 27  
    if(timeColIndex > dateColIndex && nCol > timeColIndex) 
      valueColIndex = timeColIndex + 1
    #column1: A76, 23:12:32, 12/17/2022, 27  
    else if(timeColIndex < dateColIndex && (dateColIndex - timeColIndex) == 1 && nCol > dateColIndex) 
      valueColIndex = dateColIndex + 1
    #column1: 23:12:32, 27, 12/17/2022, A76  
    else if(dateColIndex > timeColIndex && (dateColIndex - timeColIndex) > 1 && nCol > dateColIndex) 
      valueColIndex = timeColIndex + 1
  }
  else if(dateTimeColIndex == 0 && dateColIndex != 0 && timeColIndex == 0) {
    #column1: A76, 12/17/2022, 27 
    if(nCol > dateColIndex) 
      valueColIndex = dateColIndex + 1
  }
  else if(dateTimeColIndex == 0 && dateColIndex == 0 && timeColIndex != 0) {
    #column1: A76, 10:29:59, 27 
    if(nCol > timeColIndex) 
      valueColIndex = timeColIndex + 1
  }
  
#  if(valueColIndex != 0 && parseNumber(head(datf[[valueColIndex]], 50) == "0"))
#    valueColIndex = 0
  
  return(list(dateTimeColIndex, dateColIndex, timeColIndex, valueColIndex))
}

datfWithSelectedCol <- function(datf, findColIndex) {
  dateTimeColIndex <- dateColIndex <- timeColIndex <- valueColIndex <- 0
  datfSplitAggByHour <- 0
  dateTimeISO <- timeISO <- salesQty <- 0  
  dateTimeColIndex <- findColIndex[[1]]
  dateColIndex     <- findColIndex[[2]]
  timeColIndex     <- findColIndex[[3]] 
  valueColIndex    <- findColIndex[[4]]   

  if(dateTimeColIndex != 0 && dateColIndex == 0 && timeColIndex == 0) {
    dateTimeFormat <- findDateTimeFormat(datf[[dateTimeColIndex]])
    dateTimeISO <- convertToISOFormat(dateTimeFormat, datf[[dateTimeColIndex]])    
    if(valueColIndex == 0){
      datfNew <- tibble(dateTimeISO)     
#      setnames(datfNew, c(colnames(datf)[dateTimeColIndex]))      
    } 
    else if(valueColIndex != 0) {
      qty <- parseNumber(datf[[valueColIndex]])      
      datfNew <- tibble(dateTimeISO, qty)      
#      setnames(datfNew, c(colnames(datf)[dateTimeColIndex], colnames(datf)[valueColIndex]))
    } 
    datfNew %>% mutate(Year = year(datfNew[[1]]), Month = month(datfNew[[1]]), Day = day(datfNew[[1]]), Hour = hour((datfNew[[1]])), 
                       WeekDay = format(datfNew[[1]], "%A"), WeekNr = format(datfNew[[1]], "W-%V-%b")) -> datfSplit
    datfSplit %>% select(Year:WeekDay, everything(), -colnames(datfNew)[1]) %>% arrange(Year, Month, Day, Hour) -> datfSplit
    if(valueColIndex == 0) datfSplit %>% group_by(Year, Month, Day, Hour, WeekDay, WeekNr) %>% summarise(NumberOfCustomer = n()) -> datfSplitAggByHour
    else if(valueColIndex != 0) datfSplit %>% group_by(Year, Month, Day, Hour, WeekDay, WeekNr) %>% summarise(NumberOfCustomer = n(), salesQty = sum(qty)) -> datfSplitAggByHour
    a <- 10
  }
  else if(dateTimeColIndex == 0 && dateColIndex != 0 && timeColIndex != 0) {
    dateFormat <- findDateTimeFormat(head(datf[[dateColIndex]], nRow))
    dateISO <- convertToISOFormat(dateFormat, datf[[dateColIndex]]) 
    timeISO <- parse_time(datf[[timeColIndex]])
    if(valueColIndex == 0) {
      datfNew <- tibble(dateISO, timeISO)          
    }
    else if(valueColIndex != 0) {
      qty <- parseNumber(datf[[valueColIndex]]) 
      datfNew <- tibble(dateISO, timeISO, qty)    
    }
    datfNew %>% mutate(Year = year(datfNew[[1]]), Month = month(datfNew[[1]]), Day = day(datfNew[[1]]), Hour = hour((datfNew[[2]])), 
                       WeekDay = format(datfNew[[1]], "%A"), WeekNr = format(datfNew[[1]], "W%V-%b")) -> datfSplit 
    datfSplit %>% select(Year:WeekDay, everything(), -colnames(datfNew)[1], -colnames(datfNew)[2]) %>% arrange(Year, Month, Day, Hour) -> datfSplit
    if(valueColIndex == 0) datfSplit %>% group_by(Year, Month, Day, Hour, WeekDay, WeekNr) %>% summarise(NumberOfCustomer = n()) -> datfSplitAggByHour
    else if(valueColIndex != 0) datfSplit %>% group_by(Year, Month, Day, Hour, WeekDay, WeekNr) %>% summarise(NumberOfCustomer = n(), salesQty = sum(qty)) -> datfSplitAggByHour
    a <- 10
  }
  return(datfSplitAggByHour)
}

formatQYMYDMY <- function(timeFormat) {
  if(dateFormat[[2]] == "ym" || dateFormat[[2]] == "my" ||
     dateFormat[[2]] == "qy" || dateFormat[[2]] == "yq" ||
     dateFormat[[2]] == "ymd" || dateFormat[[2]] == "ydm" || 
     dateFormat[[2]] == "mdy" || dateFormat[[2]] == "dmy")
    return(TRUE)
  else
    return(FALSE)
}

formatDMYHMS <- function(timeFormat) {
  if(timeFormat == "ymd_hms" || timeFormat == "ymd_hm" || timeFormat == "ymd_h" ||
     timeFormat == "dmy_hms" || timeFormat == "dmy_hm" || timeFormat == "dmy_h" ||
     timeFormat == "mdy_hms" || timeFormat == "mdy_hm" || timeFormat == "mdy_h" ||
     timeFormat == "ydm_hms" || timeFormat == "ydm_hm" || timeFormat == "ydm_h") 
    return(TRUE)
  else
    return(FALSE)
}

minMaxLim <- function(vec) {
  maxVal <- max(vec)
  minVal <- min(vec)
  offVal <- (maxVal - minVal)/50
  maxVal = maxVal + offVal
  if(minVal != 0 && (minVal - offVal) >= 0) minVal = minVal - offVal
  return(c(minVal, maxVal))
}


