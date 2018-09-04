#! /usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(readr)

# To try your hand at p-hacking and overfitting, and show how these practices
# can lead to completely inane results, you are going to show the strong
# correlation between the tweets by President Obama (or presidential candidate
# Donald Trump) in 2016 and the number of crimes in Chicago. For example,
# here’s code showing that the number of tweets by Trump correlates with
# narcotics violations in Chicago.

# read tweets
trumptweetdata <- tbl_df(read_csv("data/Trump_Tweets_2016.csv"))
obamatweetdata <- tbl_df(read_csv("data/Obama_Tweets_2016.csv"))

# read crimes
crimes <- tbl_df(read_csv("data/Chicago_Crimes_2016.csv"))
