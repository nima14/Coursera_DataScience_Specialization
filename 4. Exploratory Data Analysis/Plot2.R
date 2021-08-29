Path <- dirname(rstudioapi::getSourceEditorContext()$path)

Data <- read.table(paste0(Path,"/household_power_consumption.txt"),
                   sep = ";",header = TRUE,na.strings = "?")

Data$Date <- as.Date(Data$Date , "%d/%m/%Y")

Data$DateTime <- strptime(paste(Data$Date, Data$Time, sep=" "), "%Y-%m-%d %H:%M:%S")
#---------------------------------------
  
  
SubData <- data.frame(subset(Data, Data$Date ==("2007-02-01") | Data$Date ==("2007-02-02")  ) )

#SubData$DateTime <- paste(SubData$Date,SubData$Time)

SubData$Global_active_power <- as.numeric( SubData$Global_active_power )

#--------------------------------------------
 ##Plot 
  
plot(SubData$DateTime,SubData$Global_active_power,
     ylab = "Global Active Power (kilowatts)",xlab = " ",type = "l")

dev.copy(png,file="plot2.png",height=480,width=480)

dev.off()
