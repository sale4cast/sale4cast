dataImputation <- function(datf)
{
  a <- 10
  datf %>% select(Year, Month, Day, Hour) %>% group_by(Year, Month, Day) %>% summarise(FreqHourPerDay = n()) -> numOfHourWithinDay
  nDays <- NROW(numOfHourWithinDay)  
  #Imagine Day is Y-Axis in the scale of weekday 1-7 and Hour is along X-Axis in the scale of hour 0-23. Count appearance of nth Hour for each days. 
  datf %>% group_by(Hour) %>% summarise(freqOfAnHour = n()) %>% select(Hour, freqOfAnHour) -> freqOfAnHourOvernDays
  #Drop an hour of a day whose frequency is less than 50% in the time series and record the identity of effective hours into regularHour for forecast. 
  freqOfAnHourOvernDays %>% mutate(FreqRateOfAnHourOvernDays = 100*freqOfAnHour/nDays) -> freqOfAnHourOvernDays
  freqOfAnHourOvernDays %>% filter(FreqRateOfAnHourOvernDays > 49) %>% select(Hour) -> regularHour
  freqOfAnHourOvernDays %>% filter(FreqRateOfAnHourOvernDays > 49, FreqRateOfAnHourOvernDays < 100) -> irregulaHour
  # Filter hours from datf according to regularHour
  datf %>% filter(Hour %in% regularHour[[1]]) ->dataForAlgo
  
  # Calculate a frequency of a time series
  freqTS <- NROW(regularHour)
  #It is important to check whether the day begin with 00.00h at midnight. In this code, we assume, the begin with 00.00h. 
  hourList <- c(0:23)
  mapHour <- hourList %in% regularHour[[1]]
  # This block of code is to add two missing 9h in the time series 'dataForAlgo'.
  # Resultantly the time series hold a constant hour space for decomposition.
  firstTimeStamp <- make_datetime(datf[[1,1]], datf[[1,2]], datf[[1,3]], datf[[1,4]])
  lastRowIndex <- NROW(datf)
  lastTimeStamp <- make_datetime(datf[[lastRowIndex ,1]], datf[[lastRowIndex ,2]], datf[[lastRowIndex ,3]], datf[[lastRowIndex ,4]])  
  DateAndTime <- 0
  emptyDatf <- 0
  nhour <- 0
  
  if(NROW(irregulaHour) != 0)
  {
    #As the difference (lastTimeStamp - firstTimeStamp) comes as a day, It assert to be multiplied by 24 to convert into hour.  
    nhour <- (lastTimeStamp - firstTimeStamp)*24
    DateAndTime <- firstTimeStamp + hours(0:nhour)  
    Data <- rep(0, (nhour+1))
    emptyDatf <- tibble(DateAndTime)
    for(colNo in 5:NCOL(dataForAlgo)) 
    {
      emptyDatf %>% add_column(data = rep(0, (nhour+1)), .name_repair = make.unique) -> emptyDatf       
    }
    emptyDatf %>% mutate(Year = year(emptyDatf[[1]]), Month = month(emptyDatf[[1]]), Day = day(emptyDatf[[1]]), Hour = hour(emptyDatf[[1]])) -> emptyDatf
    emptyDatf %>% select(Year:Hour, everything(), -colnames(emptyDatf)[1]) -> emptyDatf  
    emptyDatf %>% filter(Hour %in% regularHour[[1]]) -> emptyDatf
    a <- 10
    for(colNo in 5:NCOL(dataForAlgo)) 
    {
      setnames(emptyDatf, c(colnames(emptyDatf)[colNo]), c(colnames(dataForAlgo)[colNo]))     
    }    

    dataForAlgo <- rows_update(emptyDatf, dataForAlgo, by=c("Year", "Day", "Month", "Hour"))        
 
    # rbindData %>% group_by(.data[[colnames(rbindData)[1]]]) %>% summarise(sum(.data[[colnames(rbindData)[2]]])) -> dataForAlgo 
    dataForAlgo %>% na_if(0) -> dataForAlgo
    a <- 10
    dataForAlgo <- na_kalman(dataForAlgo)    
    #  ggplot_na_imputations(dataForAlgo[[5]], imp)  
  }
  a <- 10
  # Next1 : cut the extra data from tail of dataForAlgo where the last observation found
  # Next2 : if a time series is big enough cut the extra data from the head of the dataForAlgo 
  dataForAlgo %>% group_by(Year, Month, Day) %>% summarise(FreqHourPerDay = n()) -> freqHourEachDay
  startRow <- NROW(dataForAlgo) %% max(freqHourEachDay[4])
  if(startRow != 0)
    dataForAlgo %>% tail(NROW(dataForAlgo) - startRow) -> dataForAlgo
  return(dataForAlgo)
}



