# Importing Required packages #

library(caret)
library(dplyr)
library(readr)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(stringr)

#Reading Raw Data as visa_raw
visa_raw = read.csv("C:/Users/SATHISH/Documents/R/Datasets/h1b_kaggle.csv", stringsAsFactors = F)

#Copying raw data to processing data

visa_raw -> visa
View(visa)

#Studing Data
head(visa)
str(visa)
dim(visa)
colnames(visa)

#Removing useless variable
visa$X <- NULL

visa$CASE_STATUS[visa$CASE_STATUS == "PENDING QUALITY AND COMPLIANCE REVIEW - UNASSIGNED"] <- "PENDING"

summary(visa)

######################************Visual Analysis**********#####################

######################*************CASE_STATUS*************#####################

case_status <- as.data.frame(visa %>% filter(!is.na(CASE_STATUS)) %>% group_by(CASE_STATUS) %>% 
                               summarise(Percentage = round(n()*100/nrow(visa),1)))

ggplot(data = case_status, aes(x=reorder(CASE_STATUS,Percentage),y=Percentage, fill=CASE_STATUS))+
      geom_bar(stat = "identity")+
      geom_text(aes(label= paste(Percentage,"%")),hjust = 1) +
      labs(x= "Case Status",y="Percent", title= "Status of Petitional Applications") +
      scale_y_continuous(breaks = seq(0,100,10))
  

######################************EMPLOYER_NAME******************###############


top_employer <- visa %>% group_by(EMPLOYER_NAME) %>%
                        summarise(count= n(), percent = round(count*100/nrow(visa),1)) %>%
                        arrange(desc(count)) %>%
                        top_n(15,wt=count)
  
ggplot(data = top_employer, aes(x=reorder(EMPLOYER_NAME,percent),
                                  y=percent,fill=EMPLOYER_NAME))+
    geom_bar(stat = "identity")+
    geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
    labs(x="EMPLOYER_NAME",y="PERCENT" ) + 
    theme(legend.position = "none")+
    coord_flip()
#####################***************SOC_NAME***************#####################

top_soc <- visa %>% group_by(SOC_NAME) %>%
                    summarise(count = n(), Percentage=round(count*100/nrow(visa),1)) %>%
                    arrange(desc(count)) %>%
                    top_n(15,wt=count)

ggplot(data=top_soc,aes(x=reorder(SOC_NAME,Percentage),y=Percentage,fill=SOC_NAME))+
                    geom_bar(stat = "identity")+
                    geom_text(aes(label=Percentage))+
                    labs(x="SOC_NAME")+
                    coord_flip()+
                    theme(legend.position = "none")
####################***********JOB_TITLE*************###########################


top_job <- visa %>% group_by(JOB_TITLE) %>%
                    summarise(count = n(), Percent = round(count*100/nrow(visa),1)) %>%
                    arrange(desc(count)) %>%
                    top_n(100,wt=count)

ggplot(data = top_job, aes(x = reorder(JOB_TITLE, Percent),
                                y = Percent, fill = JOB_TITLE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percent), vjust = 1.1, hjust = 1.2) + 
  labs(x = "JOB TITLE", y = "Petitions Made(in percentage)") +
  theme(legend.position = "none") +
  coord_flip()


##################***************FULL_TIME_POSITION***************###############


full_time <- visa %>% filter(!is.na(visa$FULL_TIME_POSITION)) %>%
                      group_by(FULL_TIME_POSITION) %>% 
                      summarise(count = n(), Percent = round(count*100/nrow(visa),1))
  
ggplot(data = full_time,
       aes(x = FULL_TIME_POSITION, y = full_time$Percent, fill = FULL_TIME_POSITION)) + 
  geom_bar(stat = "identity") +
  labs(y = "Petitions Made(in percentage)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_text(aes(label= paste(Percent,"%")),hjust = 1)


####################**************PREVAILING_WAGE****************#################

wage <-subset(visa, visa$PREVAILING_WAGE < quantile(visa$PREVAILING_WAGE,0.99,na.rm = T))

ggplot(data = wage, aes(x=PREVAILING_WAGE/1000))+geom_histogram(color="black",fill="blue",binwidth = 2.5)

###################*************WORK_SITE***********************##################

worksite <- as.data.frame(visa %>% filter(!is.na(WORKSITE)) %>% group_by(WORKSITE) %>% 
                                           summarise(Percentage = round(n()*100/nrow(visa),1))) %>%
                                             arrange(desc(count)) %>%top_n(15,wt=count)

###################***************YEAR**************************##################

year_denied <- visa %>% filter((!is.na(YEAR)) & 
               (visa$CASE_STATUS == "CERTIFIED-WITHDRAWN" | visa$CASE_STATUS == "WITHDRAWN" |visa$CASE_STATUS == "DENIED") ) %>% 
               group_by(YEAR, CASE_STATUS) %>% summarise(count = n())


gg_denied<- ggplot(data=year_denied,aes(x = as.numeric(as.character(YEAR)), y = count/1000)) + 
            geom_line(linejoin = "round", lineend = "round", aes(color = CASE_STATUS)) +
            geom_point() +
            coord_cartesian(ylim = c(0,50)) +
            scale_y_continuous(breaks = seq(0,50, 5)) +
            labs(title = "Denied Petition of Period 2011-2016", x = "YEAR",y = "Number of petitions(in thousands)")

year_certified <- visa %>% filter((!is.na(YEAR)) & (visa$CASE_STATUS == "CERTIFIED")) %>%
                  group_by(YEAR, CASE_STATUS) %>% summarise(count = n())

gg_certified <- ggplot(data = year_certified,aes(x = as.numeric(as.character(YEAR)), y = count/1000)) +
                  geom_line(linejoin = "round", lineend = "round", aes(color = CASE_STATUS)) +
                  geom_point() +
                  coord_cartesian(ylim = c(300,600)) +
                  scale_y_continuous(breaks = seq(0,650, 50)) +
                  labs(title = "Certified Petition of Period 2011-2016", x = "YEAR",y = "Number of petitions(in thousands)")

grid.arrange(gg_certified,gg_denied)

################*******************WORKSITE*******************######################

visa$WORKSITE <- factor(visa$WORKSITE)
top_worksite <- as.data.frame(visa %>% group_by(WORKSITE) %>%
                                summarise(count = n(), percent = round(count*100/nrow(visa),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(15, wt = count))


##############*********************Data Scientist***************####################

data_job_pattern <- "^DATA SCIENTIST*"
data_job_pattern
data_jobs <- subset(visa, grepl(data_job_pattern, toupper(visa$JOB_TITLE)) == T)
data_jobs

###################***********Data CLeaning & MODEL BUILDING***********#############




##########    ROC Curve   ##################

library(ROCR)
roc <- predict(glm, vtrain,  type = "prob")
roc
