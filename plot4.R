plot4 <- function() {
  
  data <- prepareData(convertDates = TRUE)
  
  #get image name from the function name
  
  imageFileName <- as.character(match.call()[[1]])
  imageFileName <- paste(imageFileName, ".png", sep = "")
  
  #plot the graph
  
  png(imageFileName,
      width = 480,
      height = 480,
      units = "px",
      bg = "white")
  
  #set 4 by 4 grid for several plots
  par(mfrow = c(2,2))
  
  #Plot 1
  plot(data$Date, data$Global_active_power, type="l", xlab = "", ylab = "Global Active Power")
  
  #Plot 2
  plot(data$Date, data$Voltage, type="l", xlab = "datetime", ylab = "Voltage")
  
  #Plot 3
  plot  (data$Date, data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  points(data$Date, data$Sub_metering_2, type = "l", col="red")
  points(data$Date, data$Sub_metering_3, type = "l", col="blue")
  legend(x="topright", legend = names(data)[6:8], col = c("black","red","blue"), lty=1)
  
  #Plot 4
  plot(data$Date, data$Global_reactive_power, type="l", xlab = "datetime", ylab = "Global_reactive_power")
  
  dev.off()
}

prepareData <- function(convertDates = FALSE) {
  
  library(data.table)
  
  # IMPORTANT!
  #
  # As agreed with the course instructors
  # in discussion forums, this code does not have to
  # download and unzip data every time, so we assume
  # the file is already downloaded from
  #
  # https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
  # 
  # and unzipped into current working directory.
  
  fileName <- "household_power_consumption.txt"
  
  # Let's read dates and calculate which rows we
  # need, in order to save some time & memory
  
  dates <- fread(fileName, colClasses = "character", select="Date")
  
  firstIndex <- dates[Date == "1/2/2007", which = TRUE][1]
  lastIndex <- tail(dates[Date == "2/2/2007", which = TRUE], n=1)
  numRows <- lastIndex - firstIndex + 1
  
  # Now let's read just rows that are needed. This time we're going
  # to skip the header, but we'll read column names later.
  
  data <- fread(fileName,
                colClasses = c(rep("character", 2), rep("numeric", 7)),
                skip = firstIndex,  #consider the header, so don't add 1
                nrows = numRows,
                na.strings = c("?"))
  
  # read column names
  
  colNames <- names(fread(fileName, nrows = 0))
  setnames(data, colNames)
  
  # convert dates & times from character data
  # and merge them to the Date column
  
  if (convertDates) {
    dt <- paste(data$Date, data$Time)
    dt <- as.POSIXct(strptime(dt, "%d/%m/%Y %H:%M:%S"))
    data[, `:=` (Date = dt, Time = NULL)]
    
    print("dates converted")
  }
  
  data
}