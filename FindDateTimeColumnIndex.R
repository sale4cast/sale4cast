nRowToFindDateTimeFormat <- 27000
    nRowToCheckTimeSpace <- 2880
   nRowToGetNumberLocale <- 25
   salesVsHourFactor <- 1.4

guessInitialSalesOrderColIndex <- function(numOfCol, dateTimeColIndex, dateColIndex, timeColIndex) {
  if(dateTimeColIndex != 0 && dateColIndex == 0 && timeColIndex == 0 && numOfCol > dateTimeColIndex) return(dateTimeColIndex + 1) #Ex: A76, 12/17/2022T23:12:32Z, 27 

  else if(dateTimeColIndex == 0 && dateColIndex != 0 && timeColIndex != 0) {
         if(timeColIndex > dateColIndex && numOfCol > timeColIndex) return(timeColIndex + 1) #Ex1: 12/17/2022, A76, 23:12:32, 27 #Ex2: A76, 12/17/2022, 23:12:32, 27    
    else if(timeColIndex < dateColIndex && (dateColIndex - timeColIndex) == 1 && numOfCol > dateColIndex) return(dateColIndex + 1) #Ex: A76, 23:12:32, 12/17/2022, 27
    else if(dateColIndex > timeColIndex && (dateColIndex - timeColIndex) > 1 && numOfCol > dateColIndex) return(timeColIndex + 1) #Ex: 23:12:32, 27, 12/17/2022, A76  
  }
  
  else if(dateTimeColIndex == 0 && dateColIndex != 0 && timeColIndex == 0 && numOfCol > dateColIndex) return(dateColIndex + 1) #Ex: A76, 12/17/2022, 27 
  return(0)
}

findTimeColIndex <- function(numOfCol,datf10Row, dateColIndex) {
  nextCol = dateColIndex + 1
  prevCol = dateColIndex - 1
  timeColIndex = 0
  if(nextCol <= numOfCol) {
    for(tColIndex in nextCol:numOfCol) {
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
  return(timeColIndex)
}

findDateTimeColumnIndex <- function(datf)
{
  dateTimeColIndex <- dateColIndex <- timeColIndex <- 0
  dateTimeFormat <- dateFormat <- timeFormat <- "0"
  numOfCol <- NCOL(datf)
  
    datf10Row <- head(datf,10)
    for(colNo in 1:numOfCol) {
      if((dateTimeFormat = findDateTimeFormat(datf10Row[[colNo]])) != "0") {
        dateTimeColIndex = colNo
        if(dateTimeFormat == "ymd" || dateTimeFormat == "ydm" || dateTimeFormat == "mdy" || dateTimeFormat == "dmy") {
          dateColIndex = dateTimeColIndex
          dateTimeColIndex = 0
          timeColIndex = findTimeColIndex(numOfCol, datf10Row, dateColIndex)
        }
        break
      }
    }
    if(dateTimeColIndex == 0 && dateColIndex == 0 && timeColIndex == 0) stop("There is no time stamp")
  
  return(list(dateTimeColIndex, dateColIndex, timeColIndex))
}

#Story:
#1.) From a DateTime column, find Avg Number of Sales over an Hour. If DateTime column is spaced hourly, avgNumOfSalesOverHour should be 1.
#2.) If DateTime column is spaced minutely, avgNumOfSalesOverHour should be more than 1.
#3.) To check whether a data has been spaced as hourly or minutely, an avgNumOfSalesOverHour is compared with a value of salesVsHourFactor.
#4.) The value of a salesVsHourFactor is defined around 1.4.
addSalesOrderAndQty <- function(datf, datfSplitAggByHour, salesOrderColIndex, numOfCol) {
             datfHead <- head(datf, nRowToGetNumberLocale)
  datfSplitAggByHourM <- datfSplitAggByHour %>% group_by(Year, Month, Day, Hour) %>% summarise(NumOfSalesOrder = n())
   avgNumOfSalesOverHour <- mean(datfSplitAggByHourM$NumOfSalesOrder)
     salesQtyColIndex <- 0
         
  if(avgNumOfSalesOverHour >= salesVsHourFactor && salesOrderColIndex == 0) 
     datfSplitAggByHour <- datfSplitAggByHourM
         
  else if(avgNumOfSalesOverHour >= salesVsHourFactor && salesOrderColIndex != 0) {
    while(salesOrderColIndex <= numOfCol && (!is.null(numberLocale <- getNumberLocale(datfHead[[salesOrderColIndex]])))) {
        SalesQty <- parseNumber(numberLocale, datf[[salesOrderColIndex]])
        if(sum(SalesQty)/NROW(SalesQty) >= 1 && sum(SalesQty == 0)/NROW(SalesQty) < 0.21) {
          datfSplitAggByHour <- datfSplitAggByHour %>% add_column(SalesQty)
          break
        } 
      salesOrderColIndex = salesOrderColIndex + 1
    }
    if("SalesQty" %in% colnames(datfSplitAggByHour)) datfSplitAggByHour <- datfSplitAggByHour %>% group_by(Year, Month, Day, Hour) %>% summarise(NumOfSalesOrder = n(), SalesQty = sum(SalesQty))
    else datfSplitAggByHour <- datfSplitAggByHourM
  }
         
  else if(avgNumOfSalesOverHour < salesVsHourFactor && salesOrderColIndex != 0) {
    
    while(salesOrderColIndex <= numOfCol && (!is.null(numberLocale <- getNumberLocale(datfHead[[salesOrderColIndex]]))) && (salesQtyColIndex = salesOrderColIndex + 1)) {
        datfSplitAggByHour <- datfSplitAggByHour %>% add_column(NumOfSalesOrder = parseNumber(numberLocale, datf[[salesOrderColIndex]]))
        if("NumOfSalesOrder" %in% colnames(datfSplitAggByHour)) break
        salesOrderColIndex = salesOrderColIndex + 1
    }
    
    while(salesQtyColIndex != 0 && salesQtyColIndex <= numOfCol && (!is.null(numberLocale <- getNumberLocale(datfHead[[salesOrderColIndex]])))) {
      SalesQty <- parseNumber(numberLocale, datf[[salesQtyColIndex]])
      if(mean(datfSplitAggByHour$NumOfSalesOrder) <= mean(SalesQty)) { 
        datfSplitAggByHour <- datfSplitAggByHour %>% add_column(SalesQty)
        break
      }
      salesQtyColIndex = salesQtyColIndex + 1
    }
    datfSplitAggByHour <- datfSplitAggByHour %>% group_by(Year, Month, Day, Hour) %>% summarise_all(list(sum))
  }
  
  datfSplitAggByHour <- na.omit(datfSplitAggByHour)
  return(datfSplitAggByHour)
}
#Story:
#1.) Identify a DateTime or Date and Time column from a datf.
#2.) Extract the identified column and parse it into ISO format.
#3.) Split the ISO formatted DateTime column into Year, Month, Day, Hour column and make a new Tibble 'datfSplitAggByHour'.
splitDatfForSelectedCol <- function(datf, dateTimeColIndex, dateColIndex, timeColIndex) {
  salesOrderColIndex <- qtyColIndex <- dateTimeISO <- timeISO <- SalesQty <- 0
  datfSplitAggByHour <- NumOfSalesOrder <- NULL
            numOfCol <- NCOL(datf)
          
  if(dateTimeColIndex != 0 && dateColIndex == 0 && timeColIndex == 0) {
        dateTimeFormat <- findDateTimeFormat(head(datf[[dateTimeColIndex]], nRowToFindDateTimeFormat))
           dateTimeISO <- convertToISOFormat(dateTimeFormat, datf[[dateTimeColIndex]])    
    salesOrderColIndex <- guessInitialSalesOrderColIndex(numOfCol, dateTimeColIndex, dateColIndex, timeColIndex)
    datfSplitAggByHour <- tibble(Year = year(dateTimeISO), Month = month(dateTimeISO), Day = day(dateTimeISO), Hour = hour(dateTimeISO))
  }
  
  else if(dateTimeColIndex == 0 && dateColIndex != 0 && timeColIndex != 0) {
    dateFormat <- findDateTimeFormat(head(datf[[dateColIndex]], nRowToFindDateTimeFormat))
    dateISO <- convertToISOFormat(dateFormat, datf[[dateColIndex]]) 
    timeISO <- parse_time(datf[[timeColIndex]])
    salesOrderColIndex <- guessInitialSalesOrderColIndex(numOfCol, dateTimeColIndex, dateColIndex, timeColIndex)
    datfSplitAggByHour <- tibble(Year = year(dateISO), Month = month(dateISO), Day = day(dateISO), Hour = hour(timeISO))
  }
          
  return(addSalesOrderAndQty(datf, datfSplitAggByHour, salesOrderColIndex, numOfCol))
}

