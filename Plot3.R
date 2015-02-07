Plot3 <- function(fileName) {
      
      library(data.table)
      library(grDevices)
      #Plot Number: 1 for Plot 1, 2 for Plot 2, 3 for Plot 3, 4 for Plot 4
      N = 3    
      
      # ---------------------------------------
      # LOAD AND CLEAN
      # ---------------------------------------
      
      # Load to data frame
      Data <- read.table(
            fileName, 
            header = TRUE, 
            sep = ";", 
            # colClasses = c(Factor, Factor,Numeric,Numeric,Numeric,Numeric,Numeric,Numeric,Numeric), 
            na.strings="?"
      )
      
      # Change class to data table
      Data <- data.table(Data)
      
      # Change class of Date column to Date
      Data$Date <- as.Date(Data$Date, "%d/%m/%Y")
      
      # Change class of Time column to POSIXct
      Data$Time <- 
            as.POSIXct(
                  strptime(
                        paste(
                              Data$Date,Data$Time), "%Y-%m-%d %H:%M:%S"
                  )
            )
      
      # Just keep dates from 01-02-2007 and 02-02-2007
      Data <- Data[Data$Date >= "2007-02-01",]
      Data <- Data[Data$Date <= "2007-02-02",]
      
      # ---------------------------------------
      # PLOT
      # ---------------------------------------
      
      if (N==3) {
            png(filename = "Plot3.png",
                width = 480,
                height=480
            )
            with(Data, 
                 plot(Time, 
                      Sub_metering_1, 
                      type="s", 
                      ylab = "Energy Sub Metering", 
                      xlab=""
                 )
            )
            with(Data, 
                 lines(Time, 
                       Sub_metering_2, 
                       type="s", 
                       col="red"
                 )
            )
            with(Data, 
                 lines(Time, 
                       Sub_metering_3, 
                       type="s", 
                       col="blue"
                 )
            )
            legend("topright", 
                   lty = 1, 
                   legend=c("Sub_metering_1", 
                            "Sub_metering_2", 
                            "Sub_metering_3"), 
                   col = c("black","red","blue")
            )
            dev.off()
      }

}
