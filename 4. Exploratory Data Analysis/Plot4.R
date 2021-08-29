Path <- dirname(rstudioapi::getSourceEditorContext()$path)

Data <- read.table(paste0(Path,"/household_power_consumption.txt"),
                   sep = ";",header = TRUE,na.strings = "?")

Data$Date <- as.Date(Data$Date , "%d/%m/%Y")

Data$DateTime <- strptime(paste(Data$Date, Data$Time, sep=" "), "%Y-%m-%d %H:%M:%S")
#---------------------------------------
  
  
SubData <- data.frame(subset(Data, Data$Date ==("2007-02-01") | Data$Date ==("2007-02-02")  ) )

#SubData$DateTime <- paste(SubData$Date,SubData$Time)

SubData$Global_active_power <- as.numeric( SubData$Global_active_power )

SubData$Voltage <- as.numeric( SubData$Global_active_power )

SubData$Global_reactive_power <- as.numeric( SubData$Global_reactive_power )

#--------------------------------------------
 ##Plot 
par(mfrow=c(2,2), mar=c(4,4,2,1))  

#Plot1
plot(SubData$DateTime,SubData$Global_active_power,
     ylab = "Global Active Power (kilowatts",xlab = " ",type = "l")

#Plot2
plot(SubData$DateTime,SubData$Voltage,
     ylab = "Voltage",xlab = "datetime",type = "l")

#Plot3
plot(SubData$DateTime,SubData$Sub_metering_1,
     ylab = "Global Active Power (kilowatts",xlab = " ",type = "l")

lines(SubData$DateTime,SubData$Sub_metering_2,
      ylab = "Global Active Power (kilowatts",xlab = " ",type = "l",col="red")

lines(SubData$DateTime,SubData$Sub_metering_3,
      ylab = "Global Active Power (kilowatts",xlab = " ",type = "l",col="blue")

legend("topright", lty=1, col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
       ,cex=0.3, title.adj=1.9)

#Plot4
plot(SubData$DateTime,SubData$Global_reactive_power,
     ylab = "Global_reactive_power",xlab = "datetime",type = "l")

dev.copy(png,file="plot4.png",height=480,width=480)

dev.off()
