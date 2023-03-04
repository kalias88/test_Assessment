###Read data###
efood2022= read.csv("/Users/dkali/Downloads/Assessment exercise dataset - orders.csv", encoding="UTF-8")
Sys.setlocale(locale = "Greek")
###libraries used###
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

###summary###
summary(efood2022)


###metrics-SQL queries###

ef_metrics = efood2022 %>% 
  group_by(cuisine) %>% 
  summarize(order= n_distinct(order_id),
            users = n_distinct(user_id),
            amount = sum(amount))


ef_metrics2 = ef_metrics %>% filter( order>=1000) %>% 
  mutate(freq= order/users,
         basket=amount/order)


ef_metrics3 = efood2022 %>% 
  group_by(city) %>% 
  summarize(order= n_distinct(order_id),
            users = n_distinct(user_id),
            amount = sum(amount))


break_metrics =
  efood2022 %>% 
  filter(cuisine=='Breakfast') %>% 
  group_by(city) %>% 
  summarize(breakorders=n_distinct(order_id),
            breakusers = n_distinct(user_id),
            Totbreakamount = sum(amount))


xx=merge(ef_metrics3, break_metrics, by='city') %>% filter( order>=1000) %>% 
  mutate(br_basket= Totbreakamount/breakorders,
         basket=amount/order,
         breakfreq= breakorders/breakusers ,
         freq= order/users,
         )

xxx <- xx[order(-xx$order),]

topx = head(xxx, 5)


efood2022 %>% 
  group_by(city,user_id) %>% 
  summarise(freq=n_distinct(order_id)/n_distinct(user_id)) %>% 
  ungroup() %>% 
  filter(freq>3) %>% 
  group_by(city) %>% 
  summarize(user_with_3 = n_distinct(user_id))

efood2022 %>% 
  filter(cuisine=='Breakfast') %>%   
  group_by(city,user_id) %>% 
  summarise(freq=n_distinct(order_id)/n_distinct(user_id)) %>% 
  ungroup() %>% 
  filter(freq>3) %>% 
  group_by(city) %>% 
  summarize(user_with_3 = n_distinct(user_id))







###normality test-normalization###

histamount=hist(efood2022$amount, 
     breaks = 100, xlim = c(0, 50) ,
     main = "Histogram of amount of orders", 
     xlab = "Amount", 
     ylab = "Frequency")

efood2022$log2amount = log2(efood2022$amount)

histlog2amount= hist(efood2022$log2amount, 
                     breaks = 100, xlim = c(0, 10) ,
                     main = "Histogram of log2(amount) of orders", 
                     xlab = "log2(Amount)", 
                     ylab = "Frequency")

###recode cuisine (breakfast Vs Other)###
efood2022$cuisine = recode(efood2022$cuisine, "Italian" = "other",  "Meat"= "other",  "Street food"= "other" )

###filter by cuisine###
breakfast =
  efood2022 %>% 
  filter(cuisine=='Breakfast') 

other=efood2022 %>% 
  filter(cuisine=='other') 


###boxplot by cuisine###
boxplot(efood2022$log2amount ~ efood2022$cuisine, 
        main = "Boxplot by cuisine", 
        xlab = "cuisine", 
        ylab = "log2(amount)")


###T-test###

t.test(breakfast$log2amount,other$log2amount )

mean(breakfast$amount)
mean(other$amount)

######SEGMENTATION########
###choose big cities###
bigcities <- table(efood2022$city)

df <- subset(efood2022, city %in% names(bigcities[bigcities >=1000]))

###create the necessary columns for RFM analysis###

# convert order_date to date format#
df$order_date <- as.Date(df$order_timestamp)

# calculate recency, group by customer ID and calculate frequency and monetary value#
df2=df %>% 
  arrange(user_id,order_date) %>% 
  group_by(user_id,cuisine) %>% 
  mutate(lag_date=lag(order_date)) %>% 
  ungroup() %>% 
  mutate(recency = as.numeric(order_date)-as.numeric(lag_date)) %>% 
  group_by(user_id,cuisine) %>% 
  summarize(recency_general = mean(recency,na.rm=TRUE),
            Frequency = n_distinct(order_id),
            Monetary_Value = sum(amount)) %>% 
  na.exclude() %>% 
  ungroup()
###Calculate RFM scores###

# assign quantiles to each metric#
quantiless=df2 %>%
  summarize(
    Recency_Quartile = ntile(recency_general, 4) , 
    Frequency_Quartile = ntile(Frequency, 4),
    Monetary_Quartile = ntile(Monetary_Value, 4)
  )

rfm_data22 <- df2 %>%
  cbind(quantiless)


# Calculate the RFM score#

rfm_data22$RFM_Score <- (0.25 * rfm_data22$Recency_Quartile) + (0.4 * rfm_data22$Frequency_Quartile) + (0.35 * rfm_data22$Monetary_Quartile)

rfm_data22$RFM_Score

# Define the RFM segments based on the RFM score#
rfm_data222 <- rfm_data22 %>%
  mutate(
    RFM_Segment = case_when(
      RFM_Score > 3 & RFM_Score <= 4 ~ "Best Customers",
      RFM_Score > 2 & RFM_Score <= 3 ~ "Loyal Customers",
      RFM_Score > 1 & RFM_Score <= 2 ~ "Potential Customers",
      RFM_Score >= 0 & RFM_Score <= 1 ~ "At Risk Customers",
    )
  )

###Segment customers based on their RFM scores###
qqq=rfm_data222 %>% 
  group_by(RFM_Segment,cuisine) %>% 
  summarize(users = n_distinct(user_id)) %>% 
  ungroup(RFM_Segment,cuisine)

###filter breakfast ###
breakfaRFM=  qqq %>% 
  filter(cuisine=="Breakfast")

breakfaRFM2= rfm_data222 %>% 
  filter(cuisine=="Breakfast")

### RFM Segment plots###

ggplot(breakfaRFM2, aes(x = recency_general, y = Monetary_Value, color = factor(RFM_Score))) +
  geom_point() +
  labs(x = "Recency (Days)", y = "Monetary Value", title = "RFM Segments")


ggplot(breakfaRFM2,aes(x = recency_general, y = Monetary_Value, color = RFM_Segment)) +
  geom_point() + scale_y_log10() +
  labs(x = "Recency", y = "log(Monetary)", color = "RFM Segment")




###bar chart###

breakfaRFM2 %>%
  group_by(RFM_Segment) %>%
  summarize(count = n()) %>%
  ggplot(aes(y = RFM_Segment, x = count)) +
  geom_bar(stat = "identity", fill = "#628EE5") +
  labs(x = "# of users", y = "", title = "RFM Segment Distribution")

###heatmap###
breakfaRFM2 %>%
  group_by(Recency_Quartile, Frequency_Quartile) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Frequency_Quartile, y = Recency_Quartile, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Frequency Score", y = "Recency Score", title = "RFM Heatmap")


breakfaRFM2 %>%
  group_by(Monetary_Quartile, Frequency_Quartile) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Frequency_Quartile, y = Monetary_Quartile, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  labs(x = "Frequency Score", y = "Monetary Score", title = "RFM Heatmap")


breakfaRFM2 %>%
  group_by(Monetary_Quartile, Recency_Quartile) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Recency_Quartile, y = Monetary_Quartile, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(x = "Recency Score", y = "Monetary Score", title = "RFM Heatmap")
###bubble###
qqw= rfm_data222 %>%
filter(cuisine=="Breakfast")
qqw %>% names()

ggplot(qqw, aes(x = recency_general, y = Monetary_Value ,color=RFM_Segment,size=Frequency)) +
  geom_point(alpha = 0.7) +
  scale_y_log10()+
  scale_x_log10()+
  scale_size(range = c(3, 10)) +
  labs(x = "log(Recency Score)", y = "log(Monetary Value Score)", size = "Frequency") +
  theme_minimal()
