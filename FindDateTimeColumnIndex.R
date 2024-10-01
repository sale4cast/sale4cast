nRowToFindDateTimeFormat <- 27000
    nRowToCheckTimeSpace <- 2880
   nRowToGetNumberLocale <- 25

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
  dateTimeColIndex <- dateColIndex <- timeColIndex <- onlyOneNumberCol <- 0
  dateTimeFormat <- dateFormat <- timeFormat <- "0"
  numOfCol <- NCOL(datf)
  
    datf10Row <- head(datf,20)
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
    if(dateTimeColIndex == 0 && dateColIndex == 0 && timeColIndex == 0) 
      if(!is.null(getNumberLocale(datf10Row[[1]]))) onlyOneNumberCol <- 1
      else  stop("There is no time stamp or not a single numeric column")
  
  return(list(dateTimeColIndex, dateColIndex, timeColIndex, onlyOneNumberCol))
}

addSalesOrderAndQty <- function(datf, datfSplitAggByHour, salesOrderColIndex, timeSpace, numOfCol) {
             datfHead <- head(datf, nRowToGetNumberLocale)
  datfSplitAggByHourM <- datfSplitAggByHour %>% group_by(Year, Month, Day, Hour) %>% summarise(NumberOfCustomer = n())
     salesQtyColIndex <- 0
  if(timeSpace != "Equal" && salesOrderColIndex == 0) {
    datfSplitAggByHour <- datfSplitAggByHourM
    #colName <- c('Number of Customer')
  }
  else if(timeSpace != "Equal" && salesOrderColIndex != 0) {
    while(salesOrderColIndex <= numOfCol) {
      if(!is.null(numberLocale <- getNumberLocale(datfHead[[salesOrderColIndex]]))) {
        SalesQty <- parseNumber(numberLocale, datf[[salesOrderColIndex]])
        #colName <- c('Number of Customer', colnames(datf)[salesOrderColIndex])
        sumSalesQty <- sum(na.omit(SalesQty))
        nRowSalesQty <- NROW(na.omit(SalesQty))
        if(sumSalesQty / nRowSalesQty >= 1 && sum(na.omit(SalesQty) == 0)/nRowSalesQty < 0.21) {
          datfSplitAggByHour$SalesQty <- SalesQty
          salesQtyColIndex = salesOrderColIndex     
          break
        } 
      }
      salesOrderColIndex = salesOrderColIndex + 1
    }
    if("SalesQty" %in% colnames(datfSplitAggByHour)) {
      datfSplitAggByHour <- datfSplitAggByHour %>% group_by(Year, Month, Day, Hour) %>% summarise(NumberOfCustomer = n(), SalesQty = sum(na.omit(SalesQty))) 
      names(datfSplitAggByHour)[6] <- names(datf)[salesQtyColIndex]
    }
    else datfSplitAggByHour <- datfSplitAggByHourM
  }
  else if(timeSpace == "Equal" && salesOrderColIndex != 0) {
    while(salesOrderColIndex <= numOfCol) {
      if(!is.null(numberLocale <- getNumberLocale(datfHead[[salesOrderColIndex]]))) {
          salesQtyColIndex = salesOrderColIndex + 1        
          datfSplitAggByHour$NumberOfCustomer <- parseNumber(numberLocale, datf[[salesOrderColIndex]])
          names(datfSplitAggByHour)[5] <- names(datf)[salesOrderColIndex]
          break
      } 
      salesOrderColIndex = salesOrderColIndex + 1
    }
    while(salesQtyColIndex != 0 && salesQtyColIndex <= numOfCol) {
      if(!is.null(numberLocale <- getNumberLocale(datfHead[[salesQtyColIndex]]))) {
        SalesQty <- parseNumber(numberLocale, datf[[salesQtyColIndex]])
        #colName <- c(colName, colnames(datf)[salesQtyColIndex])
        if(mean(datfSplitAggByHour$NumberOfCustomer) <= mean(SalesQty)) { 
          datfSplitAggByHour$SalesQty <- SalesQty
          names(datfSplitAggByHour)[6] <- names(datf)[salesQtyColIndex]
          break
        }
      }
      salesQtyColIndex = salesQtyColIndex + 1
    }
    datfSplitAggByHour <- datfSplitAggByHour %>% group_by(Year, Month, Day, Hour) %>% summarise_all(list(sum))
  }
  datfSplitAggByHour <- na.omit(datfSplitAggByHour)
  #browser()
  return(datfSplitAggByHour)
}

splitDatfForDateTimeCol <- function(datf, dateTimeColIndex, dateColIndex, timeColIndex) {
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

  timeSpace <- timeSpaceFromDiff <- timeSpaceFromMinCount <- "Inequal"
  if(NROW(dateTimeISO) > 2)
  {
    diffDT <- tibble(diffDateTime = diff(dateTimeISO))
    count <- diffDT %>% group_by(diffDateTime) %>% summarise(freqOfDiffDateTime = n())
    if(100 * max(count$freqOfDiffDateTime) / NROW(dateTimeISO) >= 40) 
      timeSpaceFromDiff <- "Equal"
    
    data <- datfSplitAggByHour %>% group_by(Year, Month, Day, Hour) %>% summarise(freqOfMinuteWithinHour = n())     
    count <- data %>% group_by(freqOfMinuteWithinHour) %>% summarise(fregOfFreq = n())
    if(100 * max(count$fregOfFreq) / NROW(data$freqOfMinuteWithinHour) >= 40)  
      timeSpaceFromMinCount <- "Equal"  
  }
  if(timeSpaceFromDiff == "Equal" || timeSpaceFromMinCount == "Equal")
    timeSpace = "Equal"
  #print(paste("timeSpace = ",timeSpace))
  splitedDatf <- addSalesOrderAndQty(datf, datfSplitAggByHour, salesOrderColIndex, timeSpace, numOfCol)
  return(splitedDatf)
}

splitDatfForDateColWithQty <- function(datf, dateTimeColIndex, dateColIndex, timeColIndex) {
  salesOrderColIndex  <- timeISO <- SalesQty <- 0
  splitedDatf <- NumOfSalesOrder <- NULL
  numOfCol <- NCOL(datf)
  
    dateFormat <- findDateTimeFormat(head(datf[[dateColIndex]], nRowToFindDateTimeFormat))
    dateISO <- convertToISOFormat(dateFormat, datf[[dateColIndex]]) 
    salesOrderColIndex <- guessInitialSalesOrderColIndex(numOfCol, dateTimeColIndex, dateColIndex, timeColIndex)
    splitedDatf <- tibble(Year = year(dateISO), Month = month(dateISO), Day = day(dateISO))
    datfHead <- head(datf, nRowToGetNumberLocale)
    
    while(salesOrderColIndex <= numOfCol && (!is.null(numberLocale <- getNumberLocale(datfHead[[salesOrderColIndex]]))) && (salesQtyColIndex = salesOrderColIndex + 1)) {
      splitedDatf <- splitedDatf %>% add_column(NumOfSalesOrder = parseNumber(numberLocale, datf[[salesOrderColIndex]]))
      #colName <- c(colnames(datf)[salesOrderColIndex])
      if("NumOfSalesOrder" %in% colnames(splitedDatf)) break
      salesOrderColIndex = salesOrderColIndex + 1
    }
    
    while(salesQtyColIndex != 0 && salesQtyColIndex <= numOfCol && (!is.null(numberLocale <- getNumberLocale(datfHead[[salesQtyColIndex]])))) {
      SalesQty <- parseNumber(numberLocale, datf[[salesQtyColIndex]])
      #colName <- c(colName, colnames(datf)[salesQtyColIndex])
      if(mean(splitedDatf$NumOfSalesOrder) <= mean(SalesQty)) { 
        splitedDatf <- splitedDatf %>% add_column(SalesQty)
        break
      }
      salesQtyColIndex = salesQtyColIndex + 1
    }
  
  return(splitedDatf)
}

splitDatfForDateCol <- function(datf, dateTimeColIndex, dateColIndex, timeColIndex) {
  salesOrderColIndex  <- timeISO <- SalesQty <- 0
  splitedDatf <- NumOfSalesOrder <- NULL
  numOfCol <- NCOL(datf)
  dateFormat <- findDateTimeFormat(head(datf[[dateColIndex]], nRowToFindDateTimeFormat))
  dateISO <- convertToISOFormat(dateFormat, datf[[dateColIndex]]) 
  salesOrderColIndex <- guessInitialSalesOrderColIndex(numOfCol, dateTimeColIndex, dateColIndex, timeColIndex)
  splitedDatf <- tibble(Year = year(dateISO), Month = month(dateISO), Day = day(dateISO))
  datfHead <- head(datf, nRowToGetNumberLocale)
  colIndex = 4
  
  while(salesOrderColIndex <= numOfCol) {
    if(NROW(na.omit(datfHead[[salesOrderColIndex]])) > 1 && !is.null(numberLocale <- getNumberLocale(na.omit(datfHead[[salesOrderColIndex]])))) {
      splitedDatf <- splitedDatf %>% add_column(parseNumber(numberLocale, datf[[salesOrderColIndex]]))
      names(splitedDatf)[colIndex] <- names(datf)[salesOrderColIndex]
      colIndex = colIndex + 1        
    }
    salesOrderColIndex = salesOrderColIndex + 1
  }
  return(splitedDatf)
}

splitDatfForNumericCol <- function(datf) {
  splitedDatf <- tibble(index = 1:NROW(datf))
  numOfCol <- NCOL(datf)
  datfHead <- head(datf, nRowToGetNumberLocale)
  colIndex <-  salesOrderColIndex <- 1
  while(salesOrderColIndex <= numOfCol) {
    if(NROW(na.omit(datfHead[[salesOrderColIndex]])) > 1 && !is.null(numberLocale <- getNumberLocale(na.omit(datfHead[[salesOrderColIndex]])))) {
      splitedDatf <- splitedDatf %>% add_column(parseNumber(numberLocale, datf[[salesOrderColIndex]]))
      names(splitedDatf)[colIndex + 1] <- names(datf)[salesOrderColIndex]
      colIndex = colIndex + 1        
    }
    salesOrderColIndex = salesOrderColIndex + 1
  }
  return(splitedDatf %>% select(-index))
}
