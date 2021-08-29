Path <- dirname(rstudioapi::getSourceEditorContext()$path)

Data <- read.table(paste0(Path,"/household_power_consumption.txt"),
                   sep = ";",header = TRUE,na.strings = "?")

Data$Date <- as.Date(Data$Date , "%d/%m/%Y")

Data$DateTime <- strptime(paste(Data$Date, Data$Time, sep=" "), "%Y-%m-%d %H:%M:%S")
#---------------------------------------
  
  
SubData <- data.frame(subset(Data, Data$Date ==("2007-02-01") | Data$Date ==("2007-02-02")  ) )

#SubData$DateTime <- paste(SubData$Date,SubData$Time)

SubData$Global_active_power <- as.numeric( SubData$Global_active_power )

SubData$Sub_metering_1 <- as.numeric( SubData$Sub_metering_1 )

SubData$Sub_metering_2 <- as.numeric( SubData$Sub_metering_2 )

SubData$Sub_metering_3 <- as.numeric( SubData$Sub_metering_3 )
#--------------------------------------------
 ##Plot 
  
plot(SubData$DateTime,SubData$Sub_metering_1,
     ylab = "Global Active Power (kilowatts",xlab = " ",type = "l")

lines(SubData$DateTime,SubData$Sub_metering_2,
     ylab = "Global Active Power (kilowatts",xlab = " ",type = "l",col="red")

lines(SubData$DateTime,SubData$Sub_metering_3,
     ylab = "Global Active Power (kilowatts",xlab = " ",type = "l",col="blue")

legend("topright", lty=1, col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
       ,cex=0.45, title.adj=0.15)

title(main="Energy sub-metering")


dev.copy(png,file="plot3.png",height=480,width=480)

dev.off()
