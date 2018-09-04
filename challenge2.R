#! /usr/bin/env Rscript

library(ggplot2)
library(dplyr)
# library(stringr)
library(lubridate)

# 1. Read the data and plot number of visits vs. GoogleFluTrends

fludata <- tbl_df(read.csv("data/GoogleFlu/PreisMoat2014.csv", header=T))

p1 <- ggplot(fludata) + aes(x=WeeklyOutpatientVisitsforILI, y=GoogleFluTrends) +
  geom_point()
show(p1)


# 2. Calculate the (Pearson’s) correlation using cor

soln2 <- cor(fludata$WeeklyOutpatientVisitsforILI, fludata$GoogleFluTrends,
            method="pearson")
print(soln2) # 0.8829649


# 3. The data spans 2010-2013. In Aug 2013 Google Flu changed their algorithm.
#    Did this lead to improvements? Compare the data from Aug to Dec 2013 with
#    the same months in 2010, 2011, and 2012. For each, calculate the 
#    correlation, and see whether the correlation is higher for 2013.

soln3 <- fludata %>%
  mutate(Month=month(as.Date(WeekCommencing))) %>%
  mutate(Year=year(as.Date(WeekCommencing))) %>%
  filter(Month >= 8, Month <= 12) %>%
  group_by(Year) %>%
  summarise(Cor=cor(WeeklyOutpatientVisitsforILI, GoogleFluTrends, method="pearson"))

print(soln3) # no
