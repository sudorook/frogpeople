library(ggplot2)
library(dplyr)
library(readr)
Obama_tweets<-read.csv("../data/Obama_Tweets_2016.csv")
Trump_tweets<-read.csv("../data/Trump_Tweets_2016.csv")
crimes <- read.csv("../data/Chicago_Crimes_2016.csv")

trump <- Trump_tweets %>% group_by(day , month) %>% tally()
crimes <- left_join(crimes,trump %>% rename(trump=n))
crimes[is.na(crimes)] <- 0
head(crimes)

peaceViolations <- crimes %>% filter(PrimaryType == "PUBLIC PEACE VIOLATION")
pl <- ggplot(peaceViolations) + aes(x=log(trump+1),y=log(n+1)) + geom_point()+geom_smooth(method = "lm")+
      xlab("log(#Trump Tweets +1)") + ylab("log(# Gambling violations +1)")
show(pl)

print(cor.test(log(peaceViolations$trump+1),log(peaceViolations$n+1)))