#importing library
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(plyr)


#importing Data 
Forbes = read.csv("forbes_billionaires_geo.csv")

#viewing the data
View(Forbes)

#Top 10 Billionairs 
top_10_Billionairs = head(Forbes,10)

#arranging 
top_10_Billionairs = top_10_Billionairs %>% arrange(desc(NetWorth))

#Plotting bar-graph Top 10 Billionairs
ggplot(top_10_Billionairs, aes(x = reorder(Name, -NetWorth),y = NetWorth)) + geom_bar(stat = "Identity",width = 0.5, fill ="firebrick") + theme_classic()+xlab("Name of Billionairs") + ylab("Networth") +ggtitle("Top 10 Billionairs") +theme(plot.title = element_text(hjust = 0.5))

#Top 10 Countries
Top_10_Country <- Forbes %>% group_by(Country) %>% summarise(Count = n())

#arranging in higher order
Top_10_Country <-Top_10_Country %>% arrange(desc(Count))

#filtering top 10 Countries
Top_10_Country <- head(Top_10_Country, 10)

#graphing top 10 countries
ggplot(Top_10_Country, aes(x = reorder(Country, -Count), y = Count)) + geom_bar(stat = "Identity",width = 0.5, fill ="green") + theme_classic()+xlab("Countries") + ylab("NUmber of Billionairs") +ggtitle("Top 10 Countries with Most Numbers of Billionairs") +theme(plot.title = element_text(hjust = 0.5))

#Calculating Mean
Age_mean = median(Forbes$Age, trim = 0.3,na.rm = TRUE)

#replacing blank with mean of Age
Forbes$Age[is.na(Forbes$Age)]=63

#grouping ages in groups.
Age_group = ifelse(Forbes$Age >= 90,"90+",
                   ifelse(Forbes$Age >=80,"80-89",
                          ifelse(Forbes$Age >=70,"70-79",
                                 ifelse(Forbes$Age>=60,"60-69",
                                        ifelse(Forbes$Age >= 50,"50-59",
                                               ifelse(Forbes$Age>= 40,"40-49",
                                                      ifelse(Forbes$Age>30,"30-39",
                                                             ifelse(Forbes$Age >= 20, "20-29",Forbes$Age))))))))

#combining Age_group Column in forbes
Forbes_new <- cbind(Forbes,Age_group) 
View(Forbes_new)

#removing all null as well as NA Values
Forbes_new <- na.omit(mutate_all(Forbes_new, ~ifelse(. %in% c("N/A", "null", ""),  NA, .)))

#grouping Age 
Grouping_Age <- Forbes_new %>% group_by(Age_group) %>% summarise(Count = n())

#plotting graph
ggplot(Grouping_Age, aes(x = reorder(Age_group, -Count), y = Count)) + geom_bar(stat = "Identity",width = 0.5, fill ="firebrick") + theme_classic()+xlab("Age Groups") + ylab("Number of Billionairs") +ggtitle("Age Group with Most Numbers of Billionairs") +theme(plot.title = element_text(hjust = 0.5)) +geom_text(aes(label = Count))

#Insights: Most of the Billionairs(i.e, 756 ) are in the Age-Group of 60-69
#          and there is also one Billionair which is less than 20 years of age(18 years)


#grouping by martial status.
Martial_stat <- Forbes %>% group_by(Status) %>% summarise(Count = n())

#ploting graph
ggplot(Martial_stat, aes(x = reorder(Status, -Count), y = Count)) + geom_bar(stat = "Identity",width = 0.5, fill = "cyan") + theme_classic()+xlab("Martial Status") + ylab("Number of Billionairs") +ggtitle("Martial Status") +theme(plot.title = element_text(hjust = 0.5)) +geom_text(aes(label = Count))

#how many Childrens
Child <- Forbes %>% group_by(Children) %>% summarise(Count = n())

#removing NA Values from the data
Child <- na.omit(Child)

#ploting graph
ggplot(Child, aes(x = reorder(Children, -Count), y = Count)) + geom_bar(stat = "Identity",width = 0.5, fill = "#009999") + theme_classic()+xlab("Number of children") + ylab("Number of Billionairs") +ggtitle("How many childrens do billionairs have?") +theme(plot.title = element_text(hjust = 0.5)) +geom_text(aes(label = Count))

#Insights: Most of the Billonairs(533) have 2 children 

##Number of billionare that are selfmade
#Converting True & False
Forbes_new$Self_Status <- revalue(Forbes_new$Self_made, c(True = "Self-Made", False = "Not Self-Made"))
View(Forbes_new)

#grouping Self-Made status
Self_Made <- Forbes_new %>% group_by(Self_Status) %>% dplyr::summarise(n = n())

#ploting graph
ggplot(Self_Made, aes(x = reorder(Self_Status, -n), y = n)) + geom_bar(stat = "Identity",width = 0.5, fill = c("#009999", "#0000FF")) + theme_classic()+xlab("Self-made Status") + ylab("Number of Billionairs") +ggtitle("How many billionairs are Self-Made?") +theme(plot.title = element_text(hjust = 0.5)) +geom_text(aes(label = n))

#Insights:  As we camn see 708 people are Self-Made Billionairs
