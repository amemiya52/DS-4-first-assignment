plot1 <- function(X,...){
        
        #install grDevices library
        library(grDevices)
        
        # Read the data table from the txt
        
        fulldata <- read.table("household_power_consumption.txt", header = TRUE, sep =";", stringsAsFactors = FALSE)
        
        # Change the column type of Date from Factor to Date
        fulldata$Date <- as.Date(fulldata$Date, format="%d/%m/%Y")
        
        #subset the data to the data to be used
        netdata0 <- subset(fulldata, fulldata$Date >= "2007-02-01")
        netdata1<- subset(netdata0, netdata0$Date <="2007-02-02")
        
        #Change column class into numeric from charecter
        netdata1$Global_active_power <- as.numeric(netdata1$Global_active_power)
        
        #Make the plot
        #Initialize the file
        png(filename="plot1.png", width = 480, height= 480)
        #Plot the histogram
        hist(netdata1$Global_active_power, col="red", main="Global Active Power", xlab ="Global Active Power (kilowatts)")
        #Close the device
        dev.off()
        
}