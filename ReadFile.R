readingFiles<- function(fileName)({
  
  fLine <- readLines(fileName, n = 1)
  print(fLine)
  if(grepl(".xlsx",fileName) || grepl(".xls",fileName)){
    
    datf <- read.xlsx(fileName,colNames = TRUE)
    datf <- as.tibble(datf)
    print(datf)
    return(datf)
  }
  # https://raw.githubusercontent.com/ASKurz/Applied-Longitudinal-Data-Analysis-with-brms-and-the-tidyverse/master/data/tolerance1_pp.txt    
  #read .tsv files
  else if(grepl(".tsv",fileName)){
    datf <- read_tsv(fileName,col_types =  cols(.default = col_character()))
    return(datf)
  }
  
  #read .csv files with semicolon separator 
  else if(grepl(".csv",fileName)){
    
    if (grepl(";", fLine)){
      
      datf <- read_csv2(fileName,col_types =  cols(.default = col_character()))
      
    } 
    else{
      datf <- read_csv(fileName,col_types =  cols(.default = col_character()))
    }
    
    return(datf)
  }
  
  #read .txt and .dat files
  else if(grepl(".txt",fileName) || grepl(".dat",fileName)){
    
    if (grepl("\\|", fLine)){
      print("|")
      datf <- read_delim(fileName,delim = "|",col_types =  cols(.default = col_character()))
      
    } 
    else if (grepl(" ", fLine)){
      print("Space")
      datf <- read_delim(fileName,delim = " ",col_types =  cols(.default = col_character()))
    }
    else if (grepl("  ", fLine)){
      print("  ")
      datf <- read_delim(fileName,delim = "  ",col_types =  cols(.default = col_character()))
    }
    else if (grepl(";", fLine)){
      print(";")
      datf <- read_delim(fileName,delim = ";",col_types =  cols(.default = col_character()))
    }
    else if (grepl(",", fLine)){
      print(",")
      datf <- read_delim(fileName,delim = ",",col_types =  cols(.default = col_character()))
    }
    
    return(datf)
  }
  
})