# *************** COM145 - QUESTIONS .. MTA TRAFFIC (WS B12) ***************


# Lets first start by importing the prep file needed for this assignment

source("prep.R")

# Question 1: Which of the bridges had the highest EZPass use in 2019?
#            - What was the % EZPass used for that bridge in that year?

# Lets create a function that will return the name of the bridge when given an ID

daily_df %>%
  mutate(Year = year(mdy(Date))) %>%
  mutate(Abbr = getID(ID)) -> daily_df1

daily_df1 %>%
  filter(Year == "2019") %>%
  group_by(Abbr) %>%
  summarise(totalEZPass = sum(EZPass)) %>%
  arrange(desc(totalEZPass)) %>%
  head(.,1)

# Therefor, the bridge that had the highest EZPass use in 2019 was the TBX

daily_df1 %>%
  filter(Year == "2019") %>%
  filter(Abbr == "TBX") %>%
  summarise( percentageEZPass = (sum(EZPass) / (sum(EZPass) + sum(Cash)) * 100))

# 86% of the crossings for the TBX in the year 2019 were EZPasses as opposed to cash
  
  
# Question 2: For 2012 - 2022, which of the tunnels had the highest traffic volumes
#           - State the year and the tunnel which had the highest traffic volume

range <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

daily_df1 %>%
  filter(Year %in% range) %>%
  group_by(Abbr) %>%
  summarise(total = sum(EZPass) + sum(Cash)) %>%
  arrange(desc(total)) %>%
  head(.,1)

# The bridge with the most traffic between 2012 and 2022 was the BWB

daily_df1 %>%
  filter(Year %in% range) %>%
  mutate(Volume = EZPass + Cash) %>% # Will add two columns of a single record, then add it to Volume
  group_by(Year, Abbr) %>%
  summarise(totalVolume = sum(Volume)) %>%
  arrange(desc(totalVolume)) %>%
  head(.,1)

# In 2021 the VNB had the highest volume of traffic


# Question 3: For 2018, how many days did the traffic volume on the TNB exceed that of the BWB
#             (Total traffic, and for EZPass only)

# Lets create a data frame that will only contain the data records for 2018 and the two bridges
daily_df1 %>%
  filter(Year == "2018") %>%
  filter(Abbr == "BWB" | Abbr == "TNB") %>%
  mutate(Volume = EZPass + Cash) %>%
  group_by(Date, Abbr) %>%
  summarise(dailyVolume = sum(Volume)) -> daily_df2

daily_df2

daily_df2 %>%
  spread(Abbr, dailyVolume) -> daily_df3

daily_df3

max_volume <- max.col(daily_df3[, 2:3])
table(max_volume)  

# In 2018, the TNB had 14 days of more overall traffic than the BWB   

daily_df1 %>%
  filter(Year == "2018") %>%
  filter(Abbr == "BWB" | Abbr == "TNB") %>%
  group_by(Date, Abbr) %>%
  summarise(dailyEZPass = sum(EZPass)) -> daily_df4

daily_df4

daily_df4 %>%
  spread(Abbr, dailyEZPass) -> daily_df5

daily_df5

max_EZPass <- max.col(daily_df5[, 2:3])
table(max_EZPass)

# In 2018, the TNB had 30 days of more EZPass traffic than the BWB


# Question 4: Modify question (3) - so we only include weekdays

# Lets copy everything from question 3 and adjust some things

weekday = c(2, 3, 4, 5, 6)

daily_df1 %>%
  filter(Year == "2018") %>%
  filter(Abbr == "BWB" | Abbr == "TNB") %>%
  filter(wday(mdy(Date)) %in% weekday) %>% 
  mutate(Volume = EZPass + Cash) %>%
  group_by(Date, Abbr) %>%
  summarise(dailyVolume = sum(Volume)) -> daily_df6

daily_df6 %>%
  spread(Abbr, dailyVolume) -> daily_df7

daily_df7

max_volume <- max.col(daily_df7[, 2:3])
table(max_volume)  

# In 2018, the TNB had 4 weekdays of overall traffic that exceeded that of the BWB   

daily_df1 %>%
  filter(Year == "2018") %>%
  filter(Abbr == "BWB" | Abbr == "TNB") %>%
  filter(wday(mdy(Date)) %in% weekday) %>%
  group_by(Date, Abbr) %>%
  summarise(dailyEZPass = sum(EZPass)) -> daily_df8

head(daily_df8, 20)

daily_df8 %>%
  spread(Abbr, dailyEZPass) -> daily_df9

daily_df9

max_EZPass <- max.col(daily_df9[, 2:3])
table(max_EZPass)

# In 2018, the TNB had 7 weekdays of EZPass traffic that exceeded that of the BWB 


# Question 5: In 2021, what % of the total traffic for the RFK bridge was to/from Manhattan

daily_df1 %>%
  filter(Year == "2021") %>%
  filter(Abbr %in% c("TBX", "TBM")) %>%
  mutate(Total = Cash + EZPass) %>%
  group_by(Abbr) %>%
  summarise(yearlyTotal = sum(Total)) %>%
  group_by() %>%
  mutate(Percent = ( yearlyTotal/ sum(yearlyTotal) ) * 100 )

# The TBM had about 78% of the total traffic to/from Manhattan
# The TBX had about 22% of the total traffic to/from Manhattan

