library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
setwd("C:/Users/sjlof/Documents/Thesis/")

#Reading in Data
data <- read.csv("salesPredictions.csv")
data <- data[1:10,1:639]
df <- as.data.frame(data)

df2 <- data.frame(t(df), stringsAsFactors = F)
colnames(df2) <- df2[1, ]   
df2 <- df2[-1, ]

##Storing sales values in vectors
#Getting actual sales values from dataframe and storing in vector
actualSales <- as.numeric(df2$`Actual Sales`)

#Getting LSTM sales values from dataframe and storing in vector
LSTM_CD <- as.numeric(df2$`LSTM CD Sales`)
LSTM_MD <- as.numeric(df2$`LSTM MD Sales`)
LSTM_C <- as.numeric(df2$`LSTM C Sales`)

#Getting RNN sales values from dataframe and storing in vector
RNN_CD <- as.numeric(df2$`RNN CD Sales`)
RNN_MD <- as.numeric(df2$`RNN MD Sales`)
RNN_C <- as.numeric(df2$`RNN C Sales`)

#Getting CNN sales values from dataframe and storing in vector
CNN_CD <- as.numeric(df2$`CNN CD Sales`)
CNN_MD <- as.numeric(df2$`CNN MD Sales`)
CNN_C <- as.numeric(df2$`CNN C Sales`)

###Creating Vectors of Data Types to run t-test
dataTypeCDMD <- rep("Cultural" , 638)
dataTypeCDMD <- c(dataTypeCDMD , rep("Musical" , 638))

dataTypeCDC <- rep("Cultural" , 638)
dataTypeCDC <- c(dataTypeCDC , rep("Combined" , 638))

dataTypeMDC <- rep("Musical" , 638)
dataTypeMDC <- c(dataTypeMDC , rep("Combined" , 638))

### T-test for LSTM models 
#Getting difference between actual Sales and Data Sales for LSTM models
diffLSTM_CD <- actualSales - LSTM_CD
diffLSTM_MD <- actualSales - LSTM_MD
diffLSTM_C <- actualSales - LSTM_C

#Creating Vectors of varying combinations of Data Sales
LSTM_Diff_CDMD <- c(diffLSTM_CD , diffLSTM_MD)
LSTM_Diff_CDMD <- abs(LSTM_Diff_CDMD)

LSTM_Diff_CDC <- c(diffLSTM_CD , diffLSTM_C)
LSTM_Diff_CDC <- abs(LSTM_Diff_CDC)

LSTM_Diff_MDC <- c(diffLSTM_MD , diffLSTM_C)
LSTM_Diff_MDC <- abs(LSTM_Diff_MDC)

#Creating dataframes and running t-test for the data combinations for LSTM models
LSTM_TTest_CDMD <- data.frame(LSTM_Diff_CDMD, dataTypeCDMD)
d1 <- aov(LSTM_Diff_CDMD ~ dataTypeCDMD , data = LSTM_TTest_CDMD)
summary(d1)

LSTM_TTest_CDC <- data.frame(LSTM_Diff_CDC , dataTypeCDC)
d2 <- aov(LSTM_Diff_CDC ~ dataTypeCDC , data = LSTM_TTest_CDC)
summary(d2)

LSTM_TTest_MDC <- data.frame(LSTM_Diff_MDC , dataTypeMDC)
d3 <- aov(LSTM_Diff_MDC ~ dataTypeMDC , data = LSTM_TTest_MDC)
summary(d3)

### T-test for RNN models 
#Getting difference between actual Sales and Data Sales for RNN models
diffRNN_CD <- actualSales - RNN_CD
diffRNN_MD <- actualSales - RNN_MD
diffRNN_C <- actualSales - RNN_C

#Creating Vectors of varying combinations of Data Sales
RNN_Diff_CDMD <- c(diffRNN_CD , diffRNN_MD)
RNN_Diff_CDMD <- abs(RNN_Diff_CDMD)

RNN_Diff_CDC <- c(diffRNN_CD , diffRNN_C)
RNN_Diff_CDC <- abs(RNN_Diff_CDC)

RNN_Diff_MDC <- c(diffRNN_MD , diffRNN_C)
RNN_Diff_MDC <- abs(RNN_Diff_MDC)

#Creating dataframes and running t-test for the data combinations for RNN models
RNN_TTest_CDMD <- data.frame(RNN_Diff_CDMD, dataTypeCDMD)
d4 <- aov(RNN_Diff_CDMD ~ dataTypeCDMD , data = RNN_TTest_CDMD)
summary(d4)

RNN_TTest_CDC <- data.frame(RNN_Diff_CDC , dataTypeCDC)
d5 <- aov(RNN_Diff_CDC ~ dataTypeCDC , data = RNN_TTest_CDC)
summary(d5)

RNN_TTest_MDC <- data.frame(RNN_Diff_MDC , dataTypeMDC)
d6 <- aov(RNN_Diff_MDC ~ dataTypeMDC , data = RNN_TTest_MDC)
summary(d6)

### T-test for CNN models 
#Getting difference between actual Sales and Data Sales for LSTM models
diffCNN_CD <- actualSales - CNN_CD
diffCNN_MD <- actualSales - CNN_MD
diffCNN_C <- actualSales - CNN_C

#Creating Vectors of varying combinations of Data Sales
CNN_Diff_CDMD <- c(diffCNN_CD , diffCNN_MD)
CNN_Diff_CDMD <- abs(CNN_Diff_CDMD)

CNN_Diff_CDC <- c(diffCNN_CD , diffCNN_C)
CNN_Diff_CDC <- abs(CNN_Diff_CDC)

CNN_Diff_MDC <- c(diffCNN_MD , diffCNN_C)
CNN_Diff_MDC <- abs(CNN_Diff_MDC)

#Creating dataframes and running t-test for the data combinations for CNN models
CNN_TTest_CDMD <- data.frame(CNN_Diff_CDMD, dataTypeCDMD)
d7 <- aov(CNN_Diff_CDMD ~ dataTypeCDMD , data = CNN_TTest_CDMD)
summary(d7)

CNN_TTest_CDC <- data.frame(CNN_Diff_CDC , dataTypeCDC)
d8 <- aov(CNN_Diff_CDC ~ dataTypeCDC , data = CNN_TTest_CDC)
summary(d8)

CNN_TTest_MDC <- data.frame(CNN_Diff_MDC , dataTypeMDC)
d9 <- aov(CNN_Diff_MDC ~ dataTypeMDC , data = CNN_TTest_MDC)
summary(d9)
