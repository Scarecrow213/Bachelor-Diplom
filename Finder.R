require(tidyverse)
require(moments)

#read .csv file into dataframe "MyData"
MyData <- read.csv(file = "Data2.csv", header=TRUE, sep=";", dec=",")
#Show the table MyData
glimpse(MyData)

#Anomaly searchin function
#column - one of the columns of dataframe
#method - one of three methods used by the function
#k - search coefficient (for each method has different meaning)
searching <- function(column, method, k)
{
  #n-sigma method
  #k is n in n-sigma
  if (method == 1)
  {
    #firstBorder is mean minus n-sigma
    firstBorder <- mean(column, na.rm = TRUE) - k*sd(column, na.rm = TRUE)
    #secondBorder is mean plus n-sigma
    secondBorder <- mean(column, na.rm = TRUE) + k*sd(column, na.rm = TRUE)
    column <- ifelse(column < firstBorder | column > secondBorder, column, NA)
  }

  #central moments method
  #k is gap to calculate the coefficient of kurtosis
  if (method == 2)
  {
    #kurtosis coefficient for the entire column
    k1 <- kurtosis(column[1:length(column)], na.rm = TRUE)
    for(i in 1:length(column))
    {
      #kurtosis coefficient for k interval
      k2 <- kurtosis(column[(i+1):(i+1+k)], na.rm = TRUE)
      if (isTRUE(k2 < k1))
      {
        column[i] <- NA
      }
    }
  }

  #CUSUM method
  #k is a threshold for CUSUM method
  if (method == 3)
  {
    #First high CUSUM value
    sh <- c(0)
    z <- c(0)
    sh[1] <- 0
    #First low CUSUM value
    sl <- c(0)
    sl[1] <- 0
    #Mean value of column
    m <- mean(column)
    #Standart deviation of column
    sd <- sd(column)
    for(i in 1:length(column))
    {
      #Normalize observation
      z[i]<-(column[i]-m)/sd
      #High CUSUM value
      sh[(i+1)] <- max(0, sh[i]+z[i]-sd)
      #Low CUSUM value
      sl[(i+1)] <- max(0, sl[i]-z[i]-sd)
      if (isTRUE(sh[i] < k && sl[i] < k))
      {
        column[i] <- NA
      }
    }
  }
  return(column)
}

#Creating anomaly dataset
AFirstData <- searching(MyData$FirstData, 1, 3)
ASecondData <- searching(MyData$SecondData, 1, 3)
AData <- data.frame(AFirstData=AFirstData, ASecondData=ASecondData)
AThirdData <- searching(MyData$ThirdData, 1, 3)
AFourthData <- searching(MyData$FourthData, 1, 3)
AFifthData <- searching(MyData$FifthData, 1, 3)
ASixthData <- searching(MyData$SixthData, 1, 3)
ASeventhData <- searching(MyData$SeventhData, 1, 3)
AEighthData <- searching(MyData$EighthData, 1, 3)

#anomalyEntries function for searching complex anomalies
anomalyEntries <- function(Dataframe)
{
  #Anomalous dataframe
  Dataframe$N <- NA
  for(i in 1:nrow(Dataframe))
  {
    #Number of anomalies
    c <- 0
    #Counting anomalies line by line
    StrData <- Dataframe[i, ]
    for (j in StrData)
    {
      if(!is.na(j))
      {
        #Add 1 anomaly to number of anomalies
        c <- c + 1
      }
    }
    #Add new column with the number of anomalies
    Dataframe$N[i] <- c
  }
  return(Dataframe)
}

AData$AThirdData <- AThirdData
AData$AFourthData <- AFourthData
AData$AFifthData <- AFifthData
AData$ASixthData <- ASixthData
AData$ASeventhData <- ASeventhData
AData$AEighthData <- AEighthData

#Calculate complex anomalies in AData dataframe
AData<-anomalyentries(AData)

#Plotting function
#date - column of index or date
#column - column of dataframe
#anomalycolumn - column after searching function
plotting <- function(date, column, anomalyColumn)
{
  #Plot the main graph
  plot(date, column, main="Plot", ylab="Data", xlab="Indicator number", col="green", type="l")
  #Plot the anomaly points
  points(date, anomalyColumn, col="red")
}

#Plotting the histogram of complex anomalies
#Column with complex anomalies
histogram <- function(column)
{
  plot(column, main="Plot", ylab="Data", xlab="Indicator number", col="black", type="h")
}

#Plotting the complex anomalies
#date - column of index or date
#column - column of dataframe
#anomalycolumn - column with complex anomalies
#n - limit of complex anomalies
complexPlotting <- function(date, column, anomalyColumn, n)
{
  #Plot the main graph
  plot(date, column, main="Plot", ylab="Data", xlab="Indicator number", col="green", type="l")
  #Plot the complex anomalies
  for(i in 1:length(column))
  {
    if (isTRUE(anomalyColumn[i]>n))
    {
      points(date[i], column[i], col="red")
    }
  }
}