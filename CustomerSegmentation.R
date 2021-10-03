library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)
library(DataExplorer)
library(readr)
library(funModeling)
library(purrr)
library(lubridate)
library(reshape2)
library(caret)


#Examining the Dataset
glimpse(data)

# Plotting missing values in a column (DataExplorer Package)
options(repr.plot.width=8, repr.plot.height=3)
plot_missing(data)

# Removing all the Negative values of Quantity and Price and also Remove the rows having values of CustomerID as NA
data <- data %>%
  mutate(Quantities = replace(Quantity, Quantity<=0, NA),
         PricePerUnit = replace(PricePerUnit, PricePerUnit<=0, NA))
data <- data %>%
  drop_na()

# After removing NA values, examining the dataset
glimpse(data)

# Adding a column TotalBill by multiplying
data <- data %>% 
  mutate(TotalBill = Quantities*PricePerUnit)
glimpse(data)

data$Date<- sapply(data$BillDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
data$Time<- sapply(data$BillDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})

data$BillDate = as.POSIXct(data$BillDate,format="%m-%d-%Y %H:%M")
data$Date = as.POSIXct(data$Date,format="%m-%d-%Y")

data$Days <- format(data$Date, format = "%d")
data$Month <- format(data$Date, format = "%m")
data$Year <- format(data$Date, format = "%Y")
data$dayOfWeek = wday(data$Date, label=TRUE)

data <- data %>% 
  mutate(HourOfDay = hour(BillDate))

data$BillNo <- as.factor(data$BillNo)
data$StockID <- as.factor(data$StockID)
data$CustomerNo <- as.factor(data$CustomerNo)
data$Countryy <- as.factor(data$Countryy)
data$Day <- as.factor(data$Day)
data$Month <- as.factor(data$Month)
data$Year <- as.factor(data$Year)
levels(data$Year) <- c(2010,2011)
data$HourOfDay <- as.factor(data$HourOfDay)
data$dayOfWeek <- as.factor(data$dayOfWeek)

# Most popular product 
MostPopular = data %>%
  group_by(StockID, Products) %>%
  summarise(Quantities = sum(Quantities)) %>%
  arrange(desc(Quantities))
ggplot(data= MostPopular[1:30,], aes(x= reorder(as.factor(Products), Quantities), y= Quantities, color = "Pink")) +
  geom_bar(stat = "identity") +
  labs(y = "Quantities", x = "Products",
       title = "Most Popular Product") +
  coord_flip()+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
rm(MostPopular)

MostPopular = data %>%
  group_by(StockID, Products) %>%
  summarise(Quantities = sum(Quantities)) %>%
  arrange(desc(Quantities))
MostPopular

#least popular product
options(repr.plot.width=6, repr.plot.height=3)
data %>% group_by(StockID, Products) %>% summarise(count= n()) %>% filter(count<10)

#Monthly sales
Monthlysale = data %>% group_by(Month, Year) %>% 
  summarise(TotalBill = sum(TotalBill)) %>%
  arrange(desc(TotalBill)) %>%
  ungroup()
ggplot(data= Monthlysale, aes(x= reorder(as.factor(Month), TotalBill), y= TotalBill, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(y = "Sales", x = "Month",
       title = "Monthly sales") +
  coord_flip()+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
rm(Monthlysale)


# Most sales occured in the Hours of the day
Mostproductinhours = data %>% group_by(HourOfDay) %>%
  summarise(TotalBill = sum(TotalBill)) %>%
  arrange(desc(TotalBill)) %>%
  ungroup()
ggplot(data= Mostproductinhours, aes(x= reorder(as.factor(HourOfDay), TotalBill), y= TotalBill, fill = HourOfDay))+
  geom_bar(stat = "identity") +
  labs(x = "HourOfDay", y = "Sales",
       title = "Most item sold at time of the day") +
  coord_flip()+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
rm(Mostproductinhours)

#Most sales occured in week day 
 Salesweekday = data %>% group_by(dayOfWeek) %>%
  +     summarise(TotalBill = sum(TotalBill)) %>%
  +     arrange(desc(TotalBill)) %>%
  +     ungroup()

 ggplot(data= Salesweekday, aes(x= reorder(as.factor(dayOfWeek), TotalBill), y= TotalBill, fill = dayOfWeek)) +
  +     geom_bar(stat = "identity") +
  +     labs(x = "Daysofweek", y = "Sales",
             +          title=="Most sales occured in week day") +
  +     coord_flip()+
  +     theme(axis.text.x = element_text(angle=65, vjust=0.6))
rm(Salesweekday)

 #Highest Money Spent by Customers
   customers_rich = data %>%
  +     group_by(CustomerNo) %>%
  +     summarise(TotalBill = sum(TotalBill)) %>%
  +     arrange(desc(TotalBill))
 customers_rich

 # Month day wise total sales
   Daywise_monthly = data %>% group_by(Month, Day)%>%
  +     summarise(TotalBill = sum(TotalBill)) %>%
  +     arrange(Month, Day) %>%
  +     ungroup()
 ggplot(data=Daywise_monthly, aes(x= Day, y= TotalBill, fill = as.factor(Month)))+
  +     geom_bar(stat = "identity")
 scale_x_continuous(breaks = seq(min(0), max(31), by = 1))+
  +     facet_wrap(~ month, ncol = 2) +
  +     labs(title = "Month daywise sales", X= "Day(s) of Month", y= "Sales", fill = "Day")

# Revenue by Date
 options(repr.plot.width=8, repr.plot.height=3)
 data %>%
  +     group_by(Date) %>%
  +     summarise(sales = sum(TotalBill)) %>%
  +     ggplot(aes(x = Date, y = sales)) + geom_line() + geom_smooth(method = 'auto', se = FALSE) + labs(x = 'Date', y = 'Revenue', title = 'Revenue by Date')

# Revenue by Day of Week
 data %>%
  +     group_by(dayOfWeek) %>%
  +     summarise(sales = sum(TotalBill)) %>%
  +     ggplot(aes(x = dayOfWeek, y = sales)) + geom_col() + labs(x = 'Day of Week', y = 'Revenue (£)', title = 'Revenue by Day of Week')

# Revenue by Hour of Day
 data %>%
  +     group_by(HourOfDay) %>%
  +     summarise(sales = sum(TotalBill)) %>%
  +     ggplot(aes(x = HourOfDay, y = sales)) + geom_col() + labs(x = 'Hour Of Day', y = 'Revenue (£)', title = 'Revenue by Hour Of Day')

# Countrywise Revenue (top10_summary)
 countryrevenue<- data %>%
  +     group_by(Countryy) %>%
  +     summarise(revenue = sum(TotalBill)) %>%
  +     ungroup() %>%
  +     arrange(desc(revenue))
 
   head(countryrevenue, n = 10)

# Countrywise Revenue (top5_plot_exUK)
 topFive <- data %>%
  +     filter(Countryy == 'Netherlands' | Countryy == 'EIRE' | Countryy == 'Germany' | Countryy == 'France' | Countryy == 'Australia')
 topFiveCSumm <- topFive %>%
  +     group_by(Countryy) %>%
  +     summarise(sales = sum(TotalBill)) %>%
  +     ungroup() %>%
  +     arrange(desc(sales))
 head(topFiveCSumm)

 # Sales as per country
   Salesincountry = data %>% group_by(Countryy) %>%
  +     summarise(Revenue = sum(TotalBill)) %>%
  +     arrange(desc(Revenue)) %>%
  +     ungroup()
 ggplot(data= Salesincountry, aes(x= reorder(as.factor(Countryy),Revenue), y= Revenue, fill = Countryy)) +
  +     geom_bar(stat = "identity") +
  +     labs(x = "Countryy", y = "Revenue",
             +          title == "Most sales in countries") +
  +     coord_flip()+
  +     theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Total Sales amount in year
 options(repr.plot.width=4, repr.plot.height=4)
 data %>% group_by(Year) %>% summarise(total_sales= sum(TotalBill)) %>% 
  +     ggplot(aes(x= Year, y= total_sales/1000000, fill = Year)) + 
  +     geom_bar(stat="identity") + labs(x="Year", y="Total Sales in million") 


# Total sales per year (2010 & 2011) in all countries
 options(repr.plot.width=6, repr.plot.height=8)
 data%>% filter(Countryy != "United Kingdom") %>% 
  +     group_by(Year, Countryy) %>% summarise(sales = sum(TotalBill)/1000000) %>%
  +     ggplot(aes(x=Countryy, y=sales, fill=Year)) + geom_bar(stat="identity") + coord_flip() +
  +     labs(x= "Country", y="Total Sales", title = "Total sales per year")


#################################      RFM ANALYSIS    #####################################################

 RFM1 = data %>% 
  +     group_by(CustomerNo) %>% 
  +     summarise(Recency =as.numeric(as.POSIXct("2012-01-01")-max(Date)),
                  +               Frequency==n_distinct(BillNo), Monetary = sum(TotalBill)/n_distinct(BillNo)) 

   RFM1= RFM1 %>% filter (Monetary>0)
RFM1










