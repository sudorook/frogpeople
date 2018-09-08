#! /usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# read tweets
trumptweetdata <- tbl_df(read_csv("data/Trump_Tweets_2016.csv"))
obamatweetdata <- tbl_df(read_csv("data/Obama_Tweets_2016.csv"))

# read crimes
crimes <- tbl_df(read_csv("data/Chicago_Crimes_2016.csv"))
crimes[is.na(crimes)] <- 0

trumptweetdata$date <- ymd(paste(2016,trumptweetdata$month,trumptweetdata$day, sep="-"))
obamatweetdata$date <- ymd(paste(2016,obamatweetdata$month,obamatweetdata$day, sep="-"))
crimes$date <- ymd(paste(2016,crimes$month,crimes$day, sep="-"))

trumpdata <- inner_join(trumptweetdata, crimes)

for (name in unique(crimes$PrimaryType)) {
  tmp <- trumpdata %>% filter(PrimaryType==name)%>% group_by(date,n) %>%
    filter(str_detect(text, "Hillary|hillary|clinton|Clinton")) %>%
    summarise(tweets=n())
  tmp %>% ggplot() + aes(x=tweets,y=n) + geom_point()
  print(name)
  print(cor.test(log10(tmp$tweets+1),log10(tmp$n+1)))
}

tmp <- trumpdata %>% filter(PrimaryType=="CRIM SEXUAL ASSAULT")%>% group_by(date,n) %>%
  filter(str_detect(text, "Hillary|hillary|clinton|Clinton")) %>%
  summarise(tweets=n())
pl <- tmp %>% ggplot() + aes(x=tweets,y=n) + geom_point() + geom_smooth(method = "lm") +
  xlab("log(# Trump tweets about Clinton + 1)") + ylab("log(# Crim Sexual Assaults + 1)")
ggsave("tweet_and_cor.pdf", pl)
print(name)
print(cor.test(log10(tmp$tweets+1),log10(tmp$n+1)))

# [1] "CRIM SEXUAL ASSAULT"
# 
#         Pearson's product-moment correlation
# 
# data:  log10(tmp$tweets + 1) and log10(tmp$n + 1)
# t = 2.4786, df = 199, p-value = 0.01402
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.03551129 0.30416548
# sample estimates:
#       cor 
# 0.1730555 


# Potential title:
# Trump's language with respect to Hillary Clinton is causing an epidemic of
# sexual assaults, according to science.
