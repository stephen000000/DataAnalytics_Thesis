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

###
#Getting CNN sales values from dataframe and storing in vector
CNN_CD <- as.numeric(df2$`CNN CD Sales`)
CNN_MD <- as.numeric(df2$`CNN MD Sales`)
CNN_C <- as.numeric(df2$`CNN C Sales`)

#Creating Vectors of Model Type to run t-test
modelTypeLR <- rep("LSTM" , 638)
modelTypeLR <- c(modelTypeLR , rep("RNN" , 638))

### T-test for models trained using cultural
#Getting difference between actual Sales and Model Sales for Cultural Data
modelTypeLC <- rep("LSTM" , 638)
modelTypeLC <- c(modelTypeLC , rep("CNN" , 638))

modelTypeCR <- rep("CNN" , 638)
modelTypeCR <- c(modelTypeCR , rep("RNN" , 638))

### T-test for models trained using cultural
#Getting difference between actual Sales and Model Sales for Cultural Data
diffLSTMCD <- actualSales - LSTM_CD
diffRNNCD <- actualSales - RNN_CD
diffCNNCD <- actualSales - CNN_CD

#Creating Vectors of varying combinations of model Sales
CD_Diff_LR <- c(diffLSTMCD , diffRNNCD)
CD_Diff_LR <- abs(CD_Diff_LR)

CD_Diff_LC <- c(diffLSTMCD , diffCNNCD)
CD_Diff_LC <- abs(CD_Diff_LC)

CD_Diff_CR <- c(diffCNNCD , diffRNNCD)
CD_Diff_CR <- abs(CD_Diff_CR)

#Creating dataframes and running t-test for the model combinations for cultural data
CD_TTest_LR <- data.frame(CD_Diff_LR , modelTypeLR)
m1 <- aov(CD_Diff_LR ~ modelTypeLR , data = CD_TTest_LR)
summary(m1)

CD_TTest_LR <- data.frame(CD_Diff_LR , modelTypeLR)
m1 <- aov(modelTypeLR ~ CD_Diff_LR , data = CD_TTest_LR)
summary(m1)

CD_TTest_LC <- data.frame(CD_Diff_LC , modelTypeLC)
m2 <- aov(CD_Diff_LC ~ modelTypeLC , data = CD_TTest_LC)
summary(m2)

CD_TTest_CR <- data.frame(CD_Diff_CR , modelTypeCR)
m3 <- aov(CD_Diff_CR ~ modelTypeCR , data = CD_TTest_CR)
summary(m3)

### T-test for models trained using musical data
#Getting difference between actual Sales and Model Sales for Musical Data
diffLSTMMD <- actualSales - LSTM_MD
diffRNNMD <- actualSales - RNN_MD
diffCNNMD <- actualSales - CNN_MD


#Creating Vectors of varying combinations of model Sales
MD_Diff_LR <- c(diffLSTMMD , diffRNNMD)
MD_Diff_LR <- abs(MD_Diff_LR)

MD_Diff_LC <- c(diffLSTMMD , diffCNNMD)
MD_Diff_LC <- abs(MD_Diff_LC)

MD_Diff_CR <- c(diffCNNMD , diffRNNMD)
MD_Diff_CR <- abs(MD_Diff_CR)

#Creating dataframes and running t-test for the model combinations for musical data
MD_TTest_LR <- data.frame(MD_Diff_LR , modelTypeLR)
m4 <- aov(MD_Diff_LR ~ modelTypeLR , data = MD_TTest_LR)
summary(m4)

MD_TTest_LC <- data.frame(MD_Diff_LC , modelTypeLC)
m5 <- aov(MD_Diff_LC ~ modelTypeLC , data = MD_TTest_LC)
summary(m5)

MD_TTest_CR <- data.frame(MD_Diff_CR , modelTypeCR)
m6 <- aov(MD_Diff_CR ~ modelTypeCR , data = MD_TTest_CR)
summary(m6)


### T-test for models trained using combined data
#Getting difference between actual Sales and Model Sales for Combined Data
diffLSTMC <- actualSales - LSTM_C
diffRNNC <- actualSales - RNN_C
diffCNNC <- actualSales - CNN_C

#Creating Vectors of varying combinations of model Sales
C_Diff_LR <- c(diffLSTMC , diffRNNC)
C_Diff_LR <- abs(C_Diff_LR)

C_Diff_LC <- c(diffLSTMC , diffCNNC)
C_Diff_LC <- abs(C_Diff_LC)

C_Diff_CR <- c(diffCNNC , diffRNNC)
C_Diff_CR <- abs(C_Diff_CR)

#Creating dataframes and running t-test for the model combinations for combined data
C_TTest_LR <- data.frame(C_Diff_LR , modelTypeLR)
m7 <- aov(CD_Diff_LR ~ modelTypeLR , data = C_TTest_LR)
summary(m7)

C_TTest_LC <- data.frame(C_Diff_LC , modelTypeLC)
m8 <- aov(C_Diff_LC ~ modelTypeLC , data = C_TTest_LC)
summary(m8)

C_TTest_CR <- data.frame(C_Diff_CR , modelTypeCR)
m9 <- aov(C_Diff_CR ~ modelTypeCR , data = C_TTest_CR)
summary(m9)
