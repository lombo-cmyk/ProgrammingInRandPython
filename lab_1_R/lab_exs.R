####################### TASK 1 #######################
rm(list=ls())
library(ISLR)
library(ggplot2)
library(dplyr)
data(Credit)
attach(Credit)
help("Credit")
summary(Credit)
head(Credit)
t1_vector <- 1:10
t1_function <- function(some_vector) {
  my_len<-length(some_vector)
  ret <- "different"
  for (i in 1:my_len){
    if (Credit$ID[i]==some_vector[i]){
    }else{
      return(ret)
    }
    
  }
  ret <- "identical"
  return(ret)
}
t1_function(t1_vector)
t1_isDiffrent_1 <- apply(Credit, MARGIN = 2, t1_function)
t1_isDiffrent_1
Credit$ID <- toString(Credit$ID)
t1_isDiffrent_2 <- apply(Credit, MARGIN = 2, t1_function)
t1_isDiffrent_2
Credit <- Credit[t1_isDiffrent_2 != "identical"]

data(Credit)
attach(Credit)
############################### TASK 2 ###############################
t2_quant <- data.frame(Credit$ID, 
                       Credit$Income, 
                       Credit$Limit, 
                       Credit$Rating, 
                       Credit$Cards, 
                       Credit$Age, 
                       Credit$Education, 
                       Credit$Balance)

t2_corr <- cor(t2_quant,method = "pearson") 
t2_corr_gplot <- ggcorrplot::ggcorrplot(t2_corr, type = "lower", lab = TRUE, lab_size = 3, colors = c("green", "yellow", "red"))
plot(t2_corr_gplot)
#The stronest correlation with Balance is CreditRating or CreditLimits, both are 0.86

t2_gplot <- ggplot(Credit, aes(x=Balance, y=Rating)) + 
                  geom_point(aes(col=Income)) + 
                  geom_smooth(method="lm", se=FALSE, col="orange") +
                  labs(title = "Credits" , 
                       subtitle = "Balance vs Limit", 
                       caption = "Some caption",
                       x = "Balance", y = "Rating") 

plot(t2_gplot)

############################### TASK 3 ###############################

t3_mean_values <- aggregate(Credit$Income, by=list(Credit$Ethnicity),FUN=mean)
t3_sd_values <- aggregate(Credit$Income, by=list(Credit$Ethnicity),FUN=sd)

t3_gplot <- ggplot(Credit, aes(Income)) + 
            geom_density((aes(fill=Ethnicity)), alpha=0.7)
plot(t3_gplot)


############################### TASK 4 ###############################
t4_data_frame <- Credit[order(Credit$Balance,desc(Credit$Income)),]
age <- t4_data_frame$Age[1]
#The person is 32 yrs old


############################### TASK 5 ###############################
library(dplyr)
Credit["new_variable"] <- "Idk"
Credit$new_variable[Credit$Age > 60]<-"Yes"
Credit$new_variable[Credit$Age<=60]<-"No"
t5_means <- aggregate(t2_quant, list(Credit$new_variable), mean)

############################### TASK 6 ###############################
t6_gplot <- ggplot(Credit,aes(x = Student, y = Balance, fill = Gender))+ 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="Accent")+
  labs(title="Credit card balance for students vs non-students", x = "Student", y="Balance")  

plot(t6_gplot)

                             