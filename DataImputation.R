dataImputation <- function(datf)
{
  numOfHourWithinDay <- datf %>% select(Year, Month, Day, Hour) %>% group_by(Year, Month, Day) %>% summarise(FreqHourPerDay = n())
  nDays <- NROW(numOfHourWithinDay)  
  #Count appearance of nth Hour in entire time series. 
  freqOfAnHourOvernDays <- datf %>% group_by(Hour) %>% summarise(freqOfAnHour = n()) %>% select(Hour, freqOfAnHour)
  #Drop an hour of a day whose frequency is less than 50% in the time series and record the identity of effective hours into regularHour for forecast. 
  freqOfAnHourOvernDays <- freqOfAnHourOvernDays %>% mutate(FreqRateOfAnHourOvernDays = 100*freqOfAnHour/nDays)
  regularHourList <- freqOfAnHourOvernDays %>% filter(FreqRateOfAnHourOvernDays > 49) %>% select(Hour)
  dataForAlgo <- datf %>% filter(Hour %in% regularHourList[[1]])
  regularHourFreq <- freqOfAnHourOvernDays %>% filter(FreqRateOfAnHourOvernDays > 49, FreqRateOfAnHourOvernDays < 100)
  
  #It is important to check whether the day begin with 00.00h at midnight. In this code, we assume, the begin with 00.00h. 
  hourList <- c(0:23)
  mapHour <- hourList %in% regularHourList[[1]]
  firstTimeStamp <- make_datetime(datf[[1,1]], datf[[1,2]], datf[[1,3]], datf[[1,4]])
  lastRowIndex <- NROW(datf)
  lastTimeStamp <- make_datetime(datf[[lastRowIndex ,1]], datf[[lastRowIndex ,2]], datf[[lastRowIndex ,3]], datf[[lastRowIndex ,4]])  
  DateAndTime <- emptyDatf <- nhour <- 0
  
  if(NROW(regularHourFreq) != 0)
  {
    #As the difference (lastTimeStamp - firstTimeStamp) comes as a day, It assert to be multiplied by 24 to convert into hour.  
    nhour <- (lastTimeStamp - firstTimeStamp)*24
    DateAndTime <- firstTimeStamp + hours(0:nhour)  
    emptyDatf <- tibble(DateAndTime) %>% filter(date(DateAndTime) %in% make_date(dataForAlgo$Year, dataForAlgo$Month, dataForAlgo$Day))
    for(colNo in 5:NCOL(dataForAlgo)) 
      emptyDatf <- emptyDatf %>% add_column(data = -1, .name_repair = make.unique)       
    
    emptyDatf <- emptyDatf %>% mutate(Year = year(DateAndTime), Month = month(DateAndTime), Day = day(DateAndTime), Hour = hour(DateAndTime))
    emptyDatf <- emptyDatf %>% select(Year:Hour, everything(), -DateAndTime)
    emptyDatf <- emptyDatf %>% filter(Hour %in% regularHourList[[1]])
    for(colNo in 5:NCOL(dataForAlgo)) 
      names(emptyDatf)[colNo] <- names(dataForAlgo)[colNo]

    dataForAlgo <- rows_update(emptyDatf, dataForAlgo, by=c("Year", "Day", "Month", "Hour"))        
 
    # rbindData %>% group_by(.data[[colnames(rbindData)[1]]]) %>% summarise(sum(.data[[colnames(rbindData)[2]]])) -> dataForAlgo 
    dataForAlgo$NumOfSalesOrder <- dataForAlgo$NumOfSalesOrder %>% na_if(-1) # %>% na_kalman()
    if("SalesQty" %in% colnames(dataForAlgo)) dataForAlgo$SalesQty <- dataForAlgo$SalesQty %>% na_if(-1) # %>% na_kalman()
  }
  DDMMYYwithFullHourlyFreq <- dataForAlgo %>% group_by(Year, Month, Day) %>% summarise(FreqHourPerDay = n())
  # cut the extra data from first and last date of dataForAlgo where hourly frequency is not full 
  DDMMYYwithFullHourlyFreq <- DDMMYYwithFullHourlyFreq %>% filter(FreqHourPerDay == max(DDMMYYwithFullHourlyFreq$FreqHourPerDay)) %>% select(Year, Month, Day)
  Date <- make_date(DDMMYYwithFullHourlyFreq$Year, DDMMYYwithFullHourlyFreq$Month, DDMMYYwithFullHourlyFreq$Day)
  dataForAlgo <- dataForAlgo %>% filter(make_date(Year, Month, Day) >= head(Date,1), make_date(Year, Month, Day) <= tail(Date,1))
  return(dataForAlgo)
}


