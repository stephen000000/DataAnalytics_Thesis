library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
setwd("C:/Users/sjlof/Documents/Thesis/")

##Custom colour palettes 
colour_Data <- c("#51F0EA" , "#F0D03A" ,"#F055D0" )
colour_Model <- c("#F055D0" , "#51F0EA" , "#F0D03A"  )

#Reading in Data
data <- read.csv("errorAnalysisDF.csv")
df <- as.data.frame(data)
colnames(df)[colnames(df) == "ï..Model.Type"] <- "ModelType"
df$Data.Type <- factor(df$Data.Type , labels = c("Combined", "Cultural", "Musical"))
levels(df$Data.Type) <- c("Cultural", "Musical", "Combined")

#Plot of Mean Absolute Error for Model Comparision
p1 <- df %>%
  mutate(Data.Type = fct_reorder(Data.Type, desc(MAE)))%>%
  ggplot(aes(x = Data.Type, y=MAE , group=ModelType))+
    geom_line(aes(linetype=ModelType , colour=ModelType), size=1)+
    geom_point(aes(shape = Data.Type , size = 2 , colour=ModelType))+
    scale_y_continuous(breaks = seq(0,400000,by=100000),
                       labels = c("0" , "100000" , "200000" , "300000" ,"400000"))+
    scale_colour_manual(name = "Model Type" , values = colour_Cultural)+
    ylab("Root Mean Square Error")+
    xlab("Data Type")+
    ggtitle(label = "Comparision of Data Type on Model Performances of Sales Prediction",
            subtitle = "Performance measured using Mean Absolute Error (MAE)")+
    theme_minimal()+
    theme (axis.text.x = element_text(angle = 0),
         axis.text = element_text(size =8),
         axis.title = element_text(size =10),
         panel.grid.major.x = element_blank(),
         legend.title = element_blank(),
         legend.text = element_text(face="italic",
                                    size=10),
         legend.position = "bottom",
         legend.box = "horizontal",
         plot.title = element_text(face="bold"))

p1 +guides(linetype = FALSE , size = FALSE) + expand_limits(y=0)

#Plot of Root Mean Square Error for Model Comparison
p2 <- df %>%
  mutate(Data.Type = fct_reorder(Data.Type, desc(RMSE)))%>%
  ggplot(aes(x = Data.Type, y=RMSE , group=ModelType))+
  geom_line(aes(linetype=ModelType , colour=ModelType), size=1)+
  geom_point(aes(shape = Data.Type , size = 2 , colour=ModelType))+
  scale_y_continuous(breaks = seq(0,900000,by=150000),
                     labels = c("0" , "150000" , "300000" ,"450000",
                                "600000" , "750000" ,"900000"))+
  scale_colour_manual(name = "Model Type" , values = colour_Cultural)+
  ylab("Root Mean Square Error")+
  xlab("Data Type")+
  ggtitle(label = "Comparision of Data Type on Model Performances of Sales Prediction",
          subtitle = "Performance measured using Root Mean Square Error (RMSE)")+
  theme_minimal()+
  theme (axis.text.x = element_text(angle = 0),
         axis.text = element_text(size =8),
         axis.title = element_text(size =10),
         panel.grid.major.x = element_blank(),
         legend.title = element_blank(),
         legend.text = element_text(face="italic",
                                    size=10),
         legend.position = "bottom",
         legend.box = "horizontal",
         plot.title = element_text(face="bold"))

p2 +guides(linetype = FALSE , size = FALSE) + expand_limits(y=0)

#Plot of Mean Absolute Error for Data Comparison
p3 <- df %>%
  mutate(Data.Type = fct_reorder(Data.Type, desc(MAE)))%>%
  mutate(ModelType = fct_reorder(ModelType, desc(MAE)))%>%
  ggplot(aes(x = ModelType, y=MAE))+
  geom_point(aes(shape =Data.Type , size = 2 , colour=ModelType))+
  facet_wrap(.~Data.Type)+
  scale_y_continuous(breaks = seq(0,400000,by=100000),
                     labels = c("0" , "100000" , "200000" , "300000" ,"400000"))+
  scale_colour_manual(name = "Model Type" , values = colour_Model)+
  ylab("Root Mean Square Error")+
  xlab("Data Type")+
  ggtitle(label = "Comparision of Data Type on Model Performances of Sales Prediction",
          subtitle = "Performance measured using Mean Absolute Error (MAE)")+
  theme_minimal()+
  theme (axis.text.x = element_text(angle = 0),
         axis.text = element_text(size =8),
         axis.title = element_text(size =10),
         panel.grid.major.x = element_blank(),
         legend.title = element_blank(),
         legend.text = element_text(face="italic",
                                    size=10),
         legend.position = "bottom",
         legend.box = "horizontal",
         plot.title = element_text(face="bold"))

p3 +guides(linetype = FALSE , size = FALSE) + expand_limits(y=0)

#Plot of Root Mean Square Error for Data Comparison
p4 <- df %>%
  mutate(Data.Type = fct_reorder(Data.Type, desc(RMSE)))%>%
  mutate(ModelType = fct_reorder(ModelType, desc(RMSE)))%>%
  ggplot(aes(x = ModelType, y=RMSE))+
  geom_point(aes(shape =Data.Type , size = 2 , colour=ModelType))+
  facet_wrap(.~Data.Type)+
  scale_y_continuous(breaks = seq(0,900000,by=150000),
                     labels = c("0" , "150000" , "300000" ,"450000",
                                 "600000" , "750000" ,"900000"))+
  scale_colour_manual(name = "Model Type" , values = colour_Model)+
  ylab("Root Mean Square Error")+
  xlab("Data Type")+
  ggtitle(label = "Comparision of Data Type on Model Performances of Sales Prediction",
          subtitle = "Performance measured using Root Mean Square Error (RMSE)")+
  theme_minimal()+
  theme (axis.text.x = element_text(angle = 0),
         axis.text = element_text(size =8),
         axis.title = element_text(size =10),
         panel.grid.major.x = element_blank(),
         legend.title = element_blank(),
         legend.text = element_text(face="italic",
                                    size=10),
         legend.position = "bottom",
         legend.box = "horizontal",
         plot.title = element_text(face="bold"))

p4 +guides(linetype = FALSE , size = FALSE) + expand_limits(y=0)

