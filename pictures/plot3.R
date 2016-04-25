plot3 <- function(X,...){
        
        #install grDevices library
        library(grDevices)
        
        # Read the data table from the txt
        
        fulldata <- read.table("household_power_consumption.txt", header = TRUE, sep =";", stringsAsFactors = FALSE)
        
        # Change the column type of Date from Factor to Date
        # fulldata$Date <- as.Date(fulldata$Date, format="%d/%m/%Y")
        
        # Create a new column with the date and time as POIXlt and POSIXt
        dates <- fulldata$Date
        times <- fulldata$Time
        nettime <- paste(dates,times)
        # converting then the new vector into appropriate format
        nettime0 <- strptime(nettime, "%d/%m/%Y %H:%M:%S")
        
        # add new column to new dataframe
        fulldata0 <- cbind(fulldata, nettime0)
        
        #subset the data to the data to be used
        netdata0 <- subset(fulldata0, fulldata0$nettime0>= "2007-02-01")
        netdata1<- subset(netdata0, netdata0$nettime0 <"2007-02-03")
        
        
        #Change column class into numeric from charecter
        netdata1$Global_active_power <- as.numeric(netdata1$Global_active_power)
        
        #Make the plot
        #Initialize the file
        png(filename="plot3.png", width = 480, height= 480)
        # Make plot
        plot(netdata1$Sub_metering_1~netdata1$nettime0, type="n", xlab="", ylab="Energy sub metering")
        # Add lines
        lines(netdata1$Sub_metering_1 ~ netdata1$nettime0)
        lines(netdata1$Sub_metering_2 ~ netdata1$nettime0, col="red")
        lines(netdata1$Sub_metering_3 ~ netdata1$nettime0, col="blue")
        
        # Add legend
        legend("topright", lty=c(1,1,1), col = c("black", "red", "blue"), legend= c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        #Close the device
        dev.off()
        
}