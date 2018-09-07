#! /usr/bin/env Rscript

# load the packages
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

# INPUT
risk <- .28
start_year <- 1960
end_year <- 2017
challenge_end_year <- 1996

birdfludata <- tbl_df(read.csv("data/subtype_counts.csv", header=T))

meltyfludata <- birdfludata %>% gather("Strain", "Count",
                                       c("H1N1","H2N2","H3N2"))
meltyfludata <- meltyfludata %>% group_by(Year) %>%
  mutate(Frequency=Count/sum(Count))

pl_flufreq = ggplot(meltyfludata) + aes(x=Year, y=Frequency, group=Strain) +
  geom_line() + facet_grid(Strain~.)

people <- tbl_df(data.frame(Birthyear=as.integer(seq(start_year,end_year,1))))
people$pH1N1 <- 1:length(people$Birthyear)*0
people$pH2N2 <- 1:length(people$Birthyear)*0
people$pH3N2 <- 1:length(people$Birthyear)*0
people$pNone <- 1:length(people$Birthyear)*0+1

# use recursive function to compute probabilities
recursiveflu <- function(year, flu_f, people_f, risk=0.28) {
  fH1N1 <- filter(flu_f, Year == year, Strain=="H1N1")$Frequency
  fH2N2 <- filter(flu_f, Year == year, Strain=="H2N2")$Frequency
  fH3N2 <- filter(flu_f, Year == year, Strain=="H3N2")$Frequency
  
  people_f$pH1N1 <- people_f$pH1N1 + risk*fH1N1*people_f$pNone
  people_f$pH2N2 <- people_f$pH2N2 + risk*fH2N2*people_f$pNone
  people_f$pH3N2 <- people_f$pH3N2 + risk*fH3N2*people_f$pNone
  people_f$pNone <- people_f$pNone * (1-risk)

  if (year == end_year) {
    return(people_f)
  } else {
    recursiveflu(year+1, flu_f, people_f)
  }
}

yearNo <- 1
for (year in seq(start_year,end_year,1)) {
  people[yearNo,] <- recursiveflu(year, meltyfludata,
                                 filter(people, Birthyear == year), risk)
  print(people[yearNo,])
  yearNo <- yearNo + 1
}

soln <- people %>% filter(Birthyear >= start_year, Birthyear <= challenge_end_year)
print(as.data.frame(soln))

# Birthyear       pH1N1     pH2N2      pH3N2        pNone
#      1960 0.001341773 0.9277796 0.07087863 5.312327e-09
#      1961 0.001863574 0.8996939 0.09844255 7.378232e-09
#      1962 0.002588298 0.8606859 0.13672576 1.024754e-08
#      1963 0.003594858 0.8065082 0.18989689 1.423270e-08
#      1964 0.004992858 0.7312614 0.26374568 1.976764e-08
#      1965 0.006934525 0.6267520 0.36631345 2.745505e-08
#      1966 0.009631285 0.4816000 0.50876868 3.813202e-08
#      1967 0.013376784 0.2800000 0.70662316 5.296114e-08
#      1968 0.018578867 0.0000000 0.98142106 7.355714e-08
#      1969 0.025803982 0.0000000 0.97419592 1.021627e-07
#      1970 0.035838863 0.0000000 0.96416099 1.418926e-07
#      1971 0.049776199 0.0000000 0.95022360 1.970731e-07
#      1972 0.069133610 0.0000000 0.93086612 2.737126e-07
#      1973 0.096018902 0.0000000 0.90398072 3.801564e-07
#      1974 0.133359587 0.0000000 0.86663989 5.279950e-07
#      1975 0.185221648 0.0000000 0.81477762 7.333264e-07
#      1976 0.257252289 0.0000000 0.74274669 1.018509e-06
#      1977 0.357294846 0.0000000 0.64270374 1.414596e-06
#      1978 0.490816485 0.0000000 0.50918155 1.964716e-06
#      1979 0.618959042 0.0000000 0.38103823 2.728773e-06
#      1980 0.471117279 0.0000000 0.52887893 3.789962e-06
#      1981 0.444119343 0.0000000 0.55587539 5.263836e-06
#      1982 0.531762977 0.0000000 0.46822971 7.310884e-06
#      1983 0.349670801 0.0000000 0.65031904 1.015401e-05
#      1984 0.440719250 0.0000000 0.55926665 1.410278e-05
#      1985 0.250217861 0.0000000 0.74976255 1.958720e-05
#      1986 0.347131792 0.0000000 0.65284100 2.720445e-05
#      1987 0.480728608 0.0000000 0.51923361 3.778395e-05
#      1988 0.279669174 0.0000000 0.72027835 5.247771e-05
#      1989 0.355004578 0.0000000 0.64492254 7.288571e-05
#      1990 0.158014565 0.0000000 0.84188421 1.012302e-04
#      1991 0.214184600 0.0000000 0.78567480 1.405974e-04
#      1992 0.142787253 0.0000000 0.85701747 1.952742e-04
#      1993 0.128882936 0.0000000 0.87084585 2.712142e-04
#      1994 0.138198567 0.0000000 0.86142475 3.766864e-04
#      1995 0.189912666 0.0000000 0.80956416 5.231755e-04
#      1996 0.255398585 0.0000000 0.74387478 7.266327e-04
