##Importing required libaries and setting working directory
library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
setwd("C:/Users/sjlof/Documents/Thesis/")

##Custom colour palette 
colour_RNN <- c("#F055D0" , "#B03E99" , "#702762")
colour_CNN <- c("#51F0EA" , "#3CB0AC" , "#26706E")
colour_LSTM <- c("#F0D03A" , "#B0972A" , "#70611B")

colour_Cultural <- c("#F055D0" , "#51F0EA" , "#F0D03A")
colour_Musical <- c("#B03E99" , "#3CB0AC" , "#B0972A")
colour_Combined <- c("#702762" , "#26706E" , "#70611B")

#Reading in Data
data <- read.csv("Results.csv")
data <- data[1:3,1:4]
data
#Adjusting data format
Model_Comparision_df <- subset (data)
colnames(Model_Comparision_df)[colnames(Model_Comparision_df) == "ï..Results"] <- "Model.Type"
levels(Model_Comparision_df$Model.Type) <- c("LSTM", "RNN", "CNN")

#Plots for comparison of models 
Model_Comparision_df%>%
  mutate(Model.Type = fct_reorder(Model.Type, Cultural.Data )) %>%
  ggplot(aes(x = Model.Type  ,y= Cultural.Data ))+
    geom_col(aes(fill=Model.Type))+
    geom_text(aes(x = Model.Type , y= Cultural.Data,label = Cultural.Data,vjust=-.5))+
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(name = "Model Type" , values = colour_Cultural)+
    ylab("Percentage of Test Samples Correctly Predicted")+
    xlab("Model Type")+
    ggtitle(label = "Comparision of Model Performances \n of Sales Prediction",
            subtitle = "Models trained using Cultural Data " )+
    theme_minimal()+
    theme (axis.text.x = element_text(angle = 0),
           axis.text = element_text(size =8),
           axis.title = element_text(size =10),
           panel.grid.major.x = element_blank(),
           legend.title = element_text(face="bold",
                                       size=11),
           legend.text = element_text(face="italic",
                                    size=10),
           plot.title = element_text(face="bold",))

Model_Comparision_df %>% 
  mutate(Model.Type = fct_reorder(Model.Type, Musical.Data )) %>%
  ggplot(aes(x = Model.Type  ,y= Musical.Data))+
    geom_col(aes(fill=Model.Type))+
    geom_text(aes(x = Model.Type , y= Musical.Data,label = Musical.Data,vjust=-.5))+
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(name = "Model Type" , values = colour_Musical)+
    ylab("Percentage of Test Samples Correctly Predicted")+
    xlab("Model Type")+
    ggtitle(label = "Comparision of Model Performances \n of Sales Prediction",
            subtitle = "Models trained using Musical Data " )+
    theme_minimal()+
    theme (axis.text.x = element_text(angle = 0),
           axis.text = element_text(size =8),
           axis.title = element_text(size =10),
           panel.grid.major.x = element_blank(),
           legend.title = element_text(face="bold",
                                       size=11),
           legend.text = element_text(face="italic",
                                      size=10),
           plot.title = element_text(face="bold",))
  
Model_Comparision_df %>% 
  mutate(Model.Type = fct_reorder(Model.Type, Combined.Data )) %>%
  ggplot(aes(x = Model.Type  ,y= Combined.Data))+
    geom_col(aes(fill=Model.Type))+
    geom_text(aes(x = Model.Type , y= Combined.Data,label = Combined.Data,vjust=-.5))+
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(name = "Model Type" , values = colour_Combined)+
    ylab("Percentage of Test Samples Correctly Predicted")+
    xlab("Model Type")+
    ggtitle(label = "Comparision of Model Performances \n of Sales Prediction",
            subtitle = "Models trained using Musical Data & Cultural Data" )+
    theme_minimal()+
    theme (axis.text.x = element_text(angle = 0),
           axis.text = element_text(size =8),
           axis.title = element_text(size =10),
           panel.grid.major.x = element_blank(),
           legend.title = element_text(face="bold",
                                       size=11),
           legend.text = element_text(face="italic",
                                      size=10),
           plot.title = element_text(face="bold",))



#Changing Data to compare data used
library(tibble)

Data_Comparision_df <- data.frame(t(Model_Comparision_df), stringsAsFactors = F)
colnames(Data_Comparision_df) <- Data_Comparision_df[1, ]                #assign 1st row to column name
Data_Comparision_df <- Data_Comparision_df[-1, ]
Data_Comparision_df <- rownames_to_column(Data_Comparision_df, "Data.Type") 
levels(Data_Comparision_df$Data.Type) <- c("Cultural.Data", "Musical.Data" , "Combined.Data")

Data_Comparision_df$LSTM <- as.numeric(as.character(Data_Comparision_df$LSTM))
Data_Comparision_df$RNN <- as.numeric(as.character(Data_Comparision_df$RNN))
Data_Comparision_df$CNN <- as.numeric(as.character(Data_Comparision_df$CNN))

#Plots for comparing training data on performance for each model type
Data_Comparision_df %>% 
  mutate(Data.Type = fct_reorder(Data.Type, LSTM)) %>%
  ggplot(aes(x=Data.Type,y=LSTM))+
    geom_col(aes(fill=Data.Type))+
    geom_text(aes(x = Data.Type , y= LSTM,label = LSTM,vjust=-.5))+
    scale_y_continuous(limits = c(0,100))+
    scale_fill_manual(name="Data Type" , values = colour_LSTM)+
    ylab("Percentage of Test Samples Correctly Predicted")+
    ggtitle(label = "Comparision of LSTM Model Performance \n of Sales Prediction based on training Data")+
    theme_minimal()+
    theme (axis.text.x = element_text(angle = 0),
           axis.text = element_text(size =8),
           axis.title = element_text(size =8),
           panel.grid.major.x = element_blank(),
           legend.title = element_text(face="bold",
                                       size=10),
          legend.text = element_text(face="italic",
                                      size=10),
          plot.title = element_text(face="bold",))

Data_Comparision_df %>%
  mutate(Data.Type = fct_reorder(Data.Type, RNN)) %>%
  ggplot(aes(x=Data.Type,y=RNN))+
    geom_col(aes(fill=Data.Type))+
    geom_text(aes(x = Data.Type , y= RNN,label = RNN,vjust=-.5))+
    scale_y_continuous(limits = c(0,100))+
    scale_fill_manual(name="Data Type" , values = colour_RNN)+
    ylab("Percentage of Test Samples Correctly Predicted")+
    ggtitle(label = "Comparision of RNN Model Performance \n of Sales Prediction based on training Data")+
    theme_minimal()+
    theme (axis.text.x = element_text(angle = 0),
           axis.text = element_text(size =8),
           axis.title = element_text(size =8),
           panel.grid.major.x = element_blank(),
           legend.title = element_text(face="bold",
                                       size=10),
           legend.text = element_text(face="italic",
                                      size=10),
           plot.title = element_text(face="bold",))

Data_Comparision_df %>% 
  mutate(Data.Type = fct_reorder(Data.Type, CNN)) %>%
  ggplot(aes(x=Data.Type,y=CNN))+
    geom_col(aes(fill=Data.Type))+
    geom_text(aes(x = Data.Type , y= CNN,label = CNN,vjust=-.5))+
    scale_y_continuous(limits = c(0,100))+
    scale_fill_manual(name="Data Type" , values = colour_CNN)+
    ylab("Percentage of Test Samples Correctly Predicted")+
    ggtitle(label = "Comparision of CNN Model Performance \n of Sales Prediction based on training Data")+
    theme_minimal()+
    theme (axis.text.x = element_text(angle = 0),
           axis.text = element_text(size =8),
           axis.title = element_text(size =8),
           panel.grid.major.x = element_blank(),
           legend.title = element_text(face="bold",
                                       size=10),
           legend.text = element_text(face="italic",
                                      size=10),
           plot.title = element_text(face="bold",))
