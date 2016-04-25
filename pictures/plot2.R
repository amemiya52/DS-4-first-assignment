plot2 <- function(X,...){
        
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
        png(filename="plot2.png", width = 480, height= 480)
        # Make plot
        plot(netdata1$Global_active_power ~ netdata1$nettime0, type="n", xlab="", ylab="Global Active Power (kilowatts)")
        # Add line
        lines(netdata1$Global_active_power ~ netdata1$nettime0)
        #Close the device
        dev.off()
        
}