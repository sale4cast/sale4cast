findNumberLocale <- function(numVec) {
  groupingMark = "X"
  decimalMark = "X"
  
  for (rowIndex in 1:NROW(numVec)) {
    if(groupingMark != decimalMark){
      if(groupingMark != "X" && decimalMark != "X"){
        break;
      }
    }    
    num <- numVec[[rowIndex]]
    num <- trimws(num)
    splitNum <- strsplit(num, "")[[1]]
    nSpace <- which(splitNum == " ")
    front <-  Inf
    back  <- -Inf  
    if(length(nSpace) > 0)
    {
      front <- min(nSpace)
      back  <- max(nSpace)
    }
    if ((back != -Inf) && (substr(num,back+1,back+1) < '0' || substr(num,back+1,back+1) > '9')){
      num <- substr(num,0,back-1)
    }      
    a <- 10
    if((front != Inf) && (substr(num,front-1,front-1) < '0' || substr(num,front-1,front-1) > '9')){
      num <- substr(num,front+1, str_length(num))
    }
    splitNum <- strsplit(trimws(num), "")[[1]]
    delimIndex = 0
    if(length(delimIndex <- which(splitNum == ",")) > 0) commaInd <- max(delimIndex) else commaInd <- -Inf
    if(length(delimIndex <- which(splitNum == ".")) > 0)   dotInd <- max(delimIndex) else   dotInd <- -Inf 
    if(length(delimIndex <- which(splitNum == " ")) > 0) spaceInd <- max(delimIndex) else spaceInd <- -Inf 
    if(length(delimIndex <- which(splitNum == "'")) > 0) quoteInd <- max(delimIndex) else quoteInd <- -Inf       
    
    indexArray <- c(commaInd,dotInd,spaceInd,quoteInd)
    delimArray <- c(",","."," ","'")   
    delimCount <- sum( !(indexArray %in% "-Inf"))    
    a <- 10
    if(delimCount == 0){
      next
    }   
    else if(delimCount == 1){ 
      delimIndex <- which(!( indexArray %in% "-Inf"))        
      if(delimIndex == 1){
        if((str_length(num) - indexArray[1]) == 3){
          if(decimalMark != ","){
            groupingMark <- ","
          }
          next
        }
        else{
          decimalMark  <- ","
        }
      }
      else if(delimIndex == 2){
        if((str_length(num) - indexArray[2]) == 3){
          if(decimalMark != "."){
            groupingMark <- "."
          }
        }
        else{
          decimalMark <- "."
        }
      }
      else if(delimIndex == 3){
        groupingMark <- " "
      }
      else if(delimIndex == 4){
        groupingMark <- "'"
      }
    }   
    else if( delimCount == 2 ){
      delimIndex = which(!(indexArray %in% "-Inf"))     
      if(indexArray[delimIndex[1]] > indexArray[delimIndex[2]]){
        groupingMark <- delimArray[delimIndex[2]]
        decimalMark <- delimArray[delimIndex[1]]
      }
      else{
        groupingMark <- delimArray[delimIndex[1]]
        decimalMark <- delimArray[delimIndex[2]]   
      }     
    }
  } 
  return(list(groupingMark, decimalMark))
} 

checkInteger <- function(numVec) {
  spaceN <- currencyN <- numberN <- letterN <- letterMiddleN <- notNumberN <- 0
  spaceN    <- sum(str_detect(numVec, regex("[\\s]"))) / NROW(numVec)    
  numberN   <- sum(str_detect(numVec, regex("[0-9]"))) / NROW(numVec)        
  notNumberN   <- sum(str_detect(numVec, regex("[^0-9]"))) / NROW(numVec)        
  currencyN <- sum(str_detect(numVec, regex("[€$%£]"))) / NROW(numVec)
  letterN   <- sum(str_detect(numVec, regex("[a-zA-Z]"))) / NROW(numVec)
  letterMiddleN   <- sum(str_detect(numVec, regex(".[a-zA-Z]."))) / NROW(numVec)
  haveNum <- (numberN >= 0.8 && numberN <= 1.0)
  notHaveNum <- (notNumberN >= 0.8 && notNumberN <= 1.0)
  haveCurrency <- (currencyN >= 0.8 && currencyN <= 1.0)
  haveSpace <- (spaceN >= 0.8 && spaceN <= 1.0)
  haveLetter <- (letterN >= 0.8 && letterN <= 1.0)
  haveLetterMiddle <- (letterMiddleN >= 0.8 && letterMiddleN <= 1.0)    
  a <- 10
  if (haveNum && !notHaveNum) 
    sign = "integer"
  else if (haveNum && notHaveNum && haveCurrency) 
    sign = "integer"
  else if (haveNum && notHaveNum && haveCurrency && haveSpace) 
    sign = "integer"
  else if (haveNum && notHaveNum && haveLetter && haveSpace && !haveLetterMiddleC) 
    sign = "integer"
  else
    sign = "0"
  return(sign)
}

parseNumber <- function(numVec){
  
  if (NROW(numVec) > 50 ) testNumVec <- head(numVec, 50) 
  else testNumVec = numVec
  
  numberLocale <- findNumberLocale(testNumVec)
  print(paste("Grouping Mark = ", numberLocale[[1]]))
  print(paste("Decimal Mark = ", numberLocale[[2]]))
  
  groupingMark <- numberLocale[[1]]
  decimalMark  <- numberLocale[[2]]   
  a <- 10
  if(groupingMark == "X" && decimalMark != "X")
    numVec = parse_number(numVec, locale = locale(decimal_mark = decimalMark))
  else if(decimalMark == "X" && groupingMark != "X")
    numVec = parse_number(numVec, locale = locale(grouping_mark = groupingMark))
  else if(decimalMark != "X" && groupingMark != "X")
    numVec = parse_number(numVec, locale = locale(grouping_mark = groupingMark, decimal_mark = decimalMark))
  else if(groupingMark == "X" && decimalMark == "X" && checkInteger(numVec) == "integer")
    numVec = parse_number(numVec)
  else 
    numVec = "0"
  return(numVec)
}