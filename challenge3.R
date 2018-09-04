#! /usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(ggplot2)

# Avian influenza cases in humans usually arise from two viral subtypes, H5N1
# and H7N9. An interesting observation is that the age distributions for H5N1
# and H7N9 cases differ: older people are more likely to get very sick and die
# from H7N9, and younger people from H5N1. There’s no evidence for age-related
# differences in exposure. A recent paper showed that the risk of severe
# infection or death with avian influenza from 1997-2015 could be well
# explained by a simple model that correlated infection risk with the subtype
# of seasonal (non-avian) influenza a person was first exposed to in childhood.
# Different subtypes (H1N1, H2N2, and H3N2) have circulated in different years.
# Perhaps because H3N2 is more closely related to H7N9 than to H5N1, people
# with primary H3N2 infections seem protected from severe infections with H7N9.
# The complement is true for people first infected with H1N1 or H2N2 and later
# exposed to H5N1.
#
# Of course, we do not know the full infection history of any person who was
# hospitalized with avian influenza. We only know the person’s age and the year
# of hospitalization or death. To perform their analysis, the authors needed to
# calculate the probability that each case had a primary infection with each
# subtype, i.e., the probability that a person born in a given year was first
# infected with each subtype. Your challenge is to calculate these
# probabilities.
#
# The authors had to make some assumptions. First, they assumed that the risk
# of influenza infection is 28% in each year of life. Second, they assumed that
# the frequency of each circulating subtype could be inferred from the numbers
# of isolates sampled (primarily in hospitals) each year. These counts are
# given in subtype_counts.csv.

# The challenge: For every year between 1960 and 1996, calculate the
# probability that a person born in that year had primary infection with H1N1,
# H2N2, and H3N2. You must program defensively to pull this off.

# The counts are actually given for each influenza season in the U.S., which is
# slightly different from a calendar year, but you can ignore this. You’ll
# notice that “1” and “0” are used where we know (or will assume) that only one
# subtype was circulating. The authors made several other assumptions, but this
# is good enough for now.

risk = .28

birdfludata = tbl_df(read.csv("data/subtype_counts.csv", header=T))

meltyfludata = birdfludata %>% gather("Strain", "Count", 2:4)
meltyfludata <- meltyfludata %>% group_by(Year) %>%
  mutate(Frequency=Count/sum(Count))

ggplot(meltyfludata) + aes(x=Year, y=Frequency, group=Strain) + geom_line() +
  facet_grid(Strain~.)

people <- tbl_df(data.frame(Birthyear=as.integer(seq(1960,2017,1))))
people$pH1N1 <- 1:length(people$Birthyear)*0
people$pH2N2 <- 1:length(people$Birthyear)*0
people$pH3N2 <- 1:length(people$Birthyear)*0
people$pNone <- 1:length(people$Birthyear)*0+1

# use recursive function to compute probabilities
recursiveflu <- function(year, flu_f, people_f, risk=0.28) {
  fH1N1 <- filter(flu_f, Year == year, Strain=="H1N1")$Frequency
  fH2N2 <- filter(flu_f, Year == year, Strain=="H2N2")$Frequency
  fH3N2 <- filter(flu_f, Year == year, Strain=="H3N2")$Frequency
  # print(c(fH1N1,fH2N2,fH3N2,risk))
  
  people_f$pH1N1 <- people_f$pH1N1 + risk*fH1N1*people_f$pNone
  people_f$pH2N2 <- people_f$pH2N2 + risk*fH2N2*people_f$pNone
  people_f$pH3N2 <- people_f$pH3N2 + risk*fH3N2*people_f$pNone
  people_f$pNone <- people_f$pNone * (1-risk)
  # print(people_f)

  if (year == 2017) {
    return(people_f)
  } else {
    recursiveflu(year+1, flu_f, people_f)
  }
}

count = 1
for (year in seq(1960,2017,1)) {
  people[count,] = recursiveflu(year, meltyfludata, filter(people, Birthyear == year), .28)
  print(recursiveflu(year, meltyfludata, filter(people, Birthyear == year), .28))
  count = count + 1
}

soln = people %>% filter(Birthyear >= 1960, Birthyear <= 1996)
print(soln)
