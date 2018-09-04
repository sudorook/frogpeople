#! /usr/bin/env Rscript

library(dplyr)
library(tidyr)

nobeldata <- tbl_df(read.csv("data/nobel_nominations.csv", header=F,
                            col.names=c("Field", "Year", "ID")))

# 1. Take Chemistry (Che). Who received most nominations?

soln1 <- nobeldata %>%
  filter(Field == "Che") %>%
  group_by(ID) %>%
  summarise(Count=n()) %>%
  arrange(desc(Count)) %>%
  top_n(1, Count)

print(soln1) # Georges Urbain


# 2. Find all the researchers who received nominations in more than one field.

multiple_nominees <- nobeldata %>%
  select(Field, ID) %>%
  group_by(ID) %>%
  mutate(N_Fields = n_distinct(Field)) %>%
  filter(N_Fields > 1) %>%
  ungroup() %>%
  distinct(ID)

soln2 <- length(multiple_nominees$ID)
print(soln2) # 177


# 3. Take Physics (Phy). Which year had the largest number of nominees?

soln3 <- nobeldata %>%
  filter(Field == "Phy") %>%
  group_by(Year) %>%
  summarise(Count=n()) %>%
  top_n(1, Count)

print(soln3) # 57 in 1957, and 57 in 1963


# 4. What is the average number of nominees for each field? Calculate the
#    average number of nominee for each field across years.

soln4 <- nobeldata %>%
  group_by(Field) %>%
  summarise(Average=n()/length(unique(Year)))

print(soln4)

# Che    24.3
# Lit    31.4
# Med    49.8
# Pea    30.8
# Phy    26.0

