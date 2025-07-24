

#tradedata analyzed by Rahin islam using r 






#loading the necessary libraries 

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
# Loading the dataset using readr library
# the dataset is about global trade with 59090 rows and 10 colums

tdata<- read_csv('https://raw.githubusercontent.com/vishalv91/Global-Trade-Analytics/refs/heads/master/Raw%20data.csv')

View(tdata)


#understanding the structure of the dataset

summary(tdata)

str(tdata)

dim(tdata)

names(tdata)
# output
# [1] "country_or_area" "year"            "comm_code"      
# [4] "commodity"       "flow"            "trade_usd"      
# [7] "weight_kg"       "quantity_name"   "quantity"       
# [10] "category"       

table(tdata$country_or_area)

#check the missing data

colSums(is.na(tradedata)) # there are some missing data , we would clean them later

#cleaning data

#method 1 : removing all datas

clean_data<- na.omit(tdata)

colSums(is.na(clean_data))

dim(clean_data)

#method 2 : filling the missing data using dplyr

library(dplyr)

newdata<- tdata %>%
  mutate(
    weight_kg=ifelse(is.na( weight_kg),mean( weight_kg, na.rm=T),  weight_kg),
    quantity=ifelse(is.na(quantity),mean(quantity, na.rm=T), quantity)
  )

dim(newdata)
colSums(is.na(newdata))  #no missing data now


#Explore the data

names(newdata)
 # variables names are
 # [1] "country_or_area" "year"            "comm_code"      
 # [4] "commodity"       "flow"            "trade_usd"      
 # [7] "weight_kg"       "quantity_name"   "quantity"       
 # [10] "category"       


#visualizing data using ggplot2 library

ggplot(data=newdata, aes(x=country_or_area, y=trade_usd))+
  geom_bar(stat = "identity", fill="red")+
  geom_text(aes(label=format(trade_usd, big.mark=",")))+
  labs(title = "How much money a country spends?",
       x="Country name", y="money spent")
#insignts 1: canada is spending more money on trades than austrailia and usa where usa is at second place;


#Grouping variables
library(dplyr)
year_group<- newdata %>%
  group_by(year) %>%
  summarize(
    sumofall= sum(trade_usd)
  )
 
year_group


#Barplot on year goup data

# install.packages("scales")

library(scales)
library(ggplot2)
ggplot(year_group, aes(x=year, y= sumofall))+
  geom_bar(stat = "identity", fill="skyblue")+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  labs(
    title="trade on the basis of year",
    x="year",
    y="trade in usd"
    
  )+
  theme_minimal()



#insignts 2: trade among countries is increasing fast which is showing the dependecy of countries is increasing or the growing demand.

library(ggplot2)
categorygrp<- newdata %>%
  group_by(category) %>%
  summarize(
    totaltrade=sum(trade_usd, na.rm =T),
    count=n()
  )

categorygrp

ggplot(categorygrp, aes(x=category, y= totaltrade))+
  geom_bar(stat="identity", fill='purple')+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  labs(
    title = "category wise product tade",
    x='çategories',
    y='usd spent on product'
  )+ theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#insgights 3: ceraels is the most traded category among countries


names(newdata)


library(dplyr)

flow_group<- newdata %>%
  group_by(flow,country_or_area)%>%
  summarize(
    total=sum(trade_usd)
  )
  
flow_group


library(ggplot2)
library(scales)
ggplot(flow_group, aes(x=flow, y=total, fill = country_or_area))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  labs(
    title="country wise import , export and re import",
    x="import , export and re-import",
    y="money spent"
  )+theme_minimal()

# insights: canada exports and imports more than usa and austrailia

# which product is exported and imported more in canada ?


import_fcan<- newdata %>%
  filter(flow=="Import" & country_or_area=="Canada")

group_importfcan<- import_fcan %>%
  group_by(category) %>%
  summarize(
    totalusd= sum(trade_usd)
  )

group_importfcan


ggplot(group_importfcan, aes(x=category, y= totalusd))+
  geom_bar(stat="identity", fill='black')+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  
  labs(
    title="most imported products in canada",
    x="products",
    y='money spent'
  )+ theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# insights ::edible_fruit_nuts_peel_of_citrus_fruit_melons,edible_vegetables_and_certain_roots_and_tubers   are most importerd in canada 

names(newdata)


#question: which product is imported and exportedf more in australia?


imexfaus<- newdata %>%
  group_by(category,flow) %>%
  summarize(
    totalusd= sum(trade_usd)
  )
imexfaus
library(scales)
ggplot(imexfaus, aes(x=category,y=totalusd, fill = flow ))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  labs(
    title = "which product is imported , exported and re imported in austrailia",
    x='category',
    y='money spent in usd'
    
  )+ theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#insights::cereals is exported the most and the country imports more edible fruit, nuts , peel of jus and fruit melons




gg<- newdata %>%
  group_by(country_or_area, year, flow) %>%
  summarize(
    totalusd= sum(trade_usd)
  )

View(gg)


names(gg)

#export trend of austrailia


extrendaus<- gg %>%
  filter(country_or_area=="Australia" & flow=="Export") %>%
  group_by(year)%>%
  summarize(
    totalusd
  )


ggplot(extrendaus, aes(x=year, y=totalusd))+
  geom_line(color="red")+
  labs(
    title = "trends of exports in austrilia",
    x="year", y="totalusd"
  )+ theme_minimal()+
  scale_y_continuous(labels = label_number(accuracy = 1))


#trends of imports in austrailia


imtrendaus<- gg %>%
  filter(country_or_area=="Australia" & flow=="Import") %>%
  group_by(year)%>%
  summarize(
    totalusd
  )


ggplot(imtrendaus, aes(x=year, y=totalusd))+
  geom_line(color="blue")+
  labs(
    title = "trends of Imports in austrilia",
    x="year", y="totalusd"
  )+ theme_minimal()+
  scale_y_continuous(labels = label_number(accuracy = 1))





#export trend of austrailia


extrendaus<- gg %>%
  filter(country_or_area=="Australia" & flow=="Export") %>%
  group_by(year)%>%
  summarize(
    totalusd
  )


ggplot(extrendaus, aes(x=year, y=totalusd))+
  geom_line(color="red")+
  labs(
    title = "trends of exports in austrilia",
    x="year", y="totalusd"
  )+ theme_minimal()+
  scale_y_continuous(labels = label_number(accuracy = 1))


#trends of imports in canada


imtrendcan<- gg %>%
  filter(country_or_area=="Canada" & flow=="Import") %>%
  group_by(year)%>%
  summarize(
    totalusd
  )


ggplot(imtrendcan, aes(x=year, y=totalusd))+
  geom_line(color="black")+
  labs(
    title = "trends of Imports in canada",
    x="year", y="totalusd"
  )+ theme_minimal()+
  scale_y_continuous(labels = label_number(accuracy = 1))


#trends of exports in canada


extrendcan<- gg %>%
  filter(country_or_area=="Canada" & flow=="Export") %>%
  group_by(year)%>%
  summarize(
    totalusd
  )


ggplot(extrendcan, aes(x=year, y=totalusd))+
  geom_line(color="red")+
  labs(
    title = "trends of exports in canada",
    x="year", y="totalusd"
  )+ theme_minimal()+
  scale_y_continuous(labels = label_number(accuracy = 1))


#by analyzing the trends of exports and imports in austrailia and canada we
#understand that , both exports and imports are booming by years 
# and ideal era of business to boom



































