Path <- dirname(rstudioapi::getSourceEditorContext()$path)

Data <- read.table(paste0(Path,"/household_power_consumption.txt"),
                   sep = ";",header = TRUE,na.strings = "?")

Data$Date <- as.Date(Data$Date , "%d/%m/%Y")
#---------------------------------------
  
  
SubData <- data.frame(subset(Data, Data$Date ==("2007-02-01") | Data$Date ==("2007-02-02")  ) )
#--------------------------------------------
 ##Plot 
  
hist(SubData$Global_active_power,main = "Global Active Power",col = "red",
     xlab = "Global Active Power(kilowatts)")

dev.copy(png,file="plot1.png",height=480,width=480)

dev.off()
