####################### TASK 1 #######################
library(ISLR)
library(ggplot2)

data(Auto)
attach(Auto)

t2_gplot <- ggplot(Auto, aes(x=acceleration, y=mpg)) +
    geom_point(aes(size=cylinders, col=weight), alpha=0.8) +
    facet_wrap(~origin)+
    labs(title="Car parameters by origin")+
    scale_color_gradientn(colours = c("blue", "green", "yellow", "red"))
    plot(t2_gplot)
 
#############
rm(list=ls())
data(Carseats)
attach(Carseats)
head(Carseats)
help("Carseats")
summary(Carseats)
frame_quant <- data.frame(Carseats$Sales,
                         Carseats$CompPrice,
                         Carseats$Income,
                         Carseats$Advertising,
                         Carseats$Population,
                         Carseats$Price,
                         Carseats$Age,
                         Carseats$Education)
frame_quali <- data.frame(Carseats$ShelveLoc,
                          Carseats$Urban,
                          Carseats$US)

####################### TASK 2 #######################
correlation <- cor(frame_quant, method = c("pearson"))

library(ggplot2)
t2_corplot <- ggcorrplot::ggcorrplot(correlation, 
                                  type="lower",                     #dolna czesc
                                  lab=TRUE,                         #opisy kwadracikow
                                  lab_size=2,                       #wielkosc czcionki
                                  color=c("blue", "white", "red"))
plot(t2_corplot)

#the value with strongest relationsship with sales is Price
t2_gplot <- ggplot(Carseats, aes(x=Sales, y=Price))+              #Argumenty: Data, aes(tu osie chyba)
             geom_point() +                                 #scatter plot
             geom_smooth(method = "lm", col="orange")+      #regresja, kolor linii, se=False usuwa niepewnosc
             theme_bw() +
             labs(title = "Sales vs Price", subtitle = "Some subtitle", caption = "Some caption", x="Sales_", y="Price_")

plot(t2_gplot)

####################### TASK 3 #######################
t3_mean_values <- aggregate(Carseats$Price, by=list(Carseats$ShelveLoc),FUN=mean)
t3_sd_values <- aggregate(Carseats$Price, by=list(Carseats$ShelveLoc),FUN=sd)

t3_gplot <- ggplot(Carseats,aes(x = ShelveLoc, y = Price, fill = ShelveLoc))+ #fill wypelnia boxplota kolorkami chyba
            geom_boxplot() +
            labs(x = "Shelve Location", y="Price")  

plot(t3_gplot)

####################### TASK 4 #######################
t4_data_frame <- aggregate(Carseats$Sales, by=list(Carseats$Education),FUN=mean)
colnames(t4_data_frame) <- c("EducationLvl", "mean")
t4_sd <- aggregate(Carseats$Sales, by=list(Carseats$Education),FUN=sd)
t4_data_frame['Std_dev'] = t4_sd$x

library(dplyr)
t4_data_frame['is_US'] = "DontKnow"
education_values = unique(Carseats$Education)

for (val in education_values){
  tmp_edu <-filter(Carseats, Education == val) #PACKAGE DPLYR
  tmp_yes_us <-filter(tmp_edu, US=="Yes")
  if (nrow(tmp_yes_us)/nrow(tmp_edu) > (2/3) ){
    t4_data_frame$is_US[t4_data_frame$EducationLvl==val] <- "Yes" 
      
  } else {
    t4_data_frame$is_US[t4_data_frame$EducationLvl==val] <- "No" 
  }
}
t4_data_frame <- t4_data_frame[order(t4_data_frame$Std_dev),]
max_stddev <- max(t4_data_frame$Std_dev)
g3 <- ggplot(t4_data_frame, aes(x=reorder(EducationLvl, Std_dev), y=Std_dev))+
  geom_col(aes(fill=is_US))+ # or geom_bar with parameter stat = "identity",
  labs(x = "EducationLvl", y="Standard deviation")

  plot(g3)      

      

