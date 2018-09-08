#! /usr/bin/env Rscript

library(readr)
library(ggplot2)
library(cowplot)

#
# Load and tidy data
#


data.file <- "data/Food_Inspections.csv.gz"
dat        <- read_csv(data.file)

class(dat) <- "data.frame"
cols     <- c(2:5,7,8,11,13,15,16)
colnames <- c("dba","aka","license","type","address","city","date",
              "results","latitude","longitude")
dat        <- dat[cols]
names(dat) <- colnames

get.third.entry <- function(x) { return(x[3]) }
out      <- strsplit(dat$date,"/")
dat$year <- sapply(out,get.third.entry)
dat$year <- as.numeric(dat$year)

dat <- subset(dat,
              type == "Restaurant" &
              city == "CHICAGO" &
              !is.na(latitude) &
              !is.na(longitude) &
              !is.na(license))

pdat <- subset(dat,
               grepl("pizz",dba,ignore.case = TRUE) |
               grepl("pizz",aka,ignore.case = TRUE))

rows <- order(pdat$year)
pdat <- pdat[rows,]
rows <- which(!duplicated(pdat$license))
pdat <- pdat[rows,]


#
# Trend line of pizza openings.
#

# Bar plot (from pizzaplots.R)
counts <- table(pdat$year)
counts <- counts[2:8]
counts <- as.data.frame(counts)
names(counts) <- c("year","count")

aes1 <- aes(x = year,y = count)
p1   <- ggplot(counts,aes1)
out  <- geom_col(fill = "darkblue",width = 0.5)
p1   <- ggplot_add(out,p1)
out  <- labs(y = "number of locations")
p1   <- ggplot_add(out,p1)

# Line plot
p2 <- ggplot(counts) +
  aes(x=as.numeric(year),y=as.numeric(count)) +
  geom_line() +
  ylab("number of locations") +
  scale_x_continuous(breaks=as.numeric(counts$year),labels=counts$year)
show(p2)


#
# Create color-coordinated plot of pizza shop openings and locations.
#

# Add fill to bar plot
aes1 <- aes(x=year,y=count,fill=2010+as.numeric(year))
p1   <- ggplot(counts,aes1)
out  <- geom_col(width=0.5)
p1   <- ggplot_add(out,p1)
out  <- labs(fill="year")
p1   <- ggplot_add(out,p1)
out  <- scale_fill_gradient2(low="skyblue",mid="white",
                             high="orangered",midpoint=2014)
p1   <- ggplot_add(out,p1)
out  <- labs(y = "number of locations")
p1   <- ggplot_add(out,p1)

# Generate geographic plot
aes2 <- aes(x = longitude,y = latitude, col = year)
p2   <- ggplot(pdat,aes2)
# out  <- geom_point(pch = 20, alpha=.2)
out  <- geom_point(pch=20)
p2   <- ggplot_add(out,p2)
out  <- scale_color_gradient2(low="skyblue",mid="white",
                              high="orangered",midpoint=2014)
p2   <- ggplot_add(out,p2)

# Join the plots
p1  <- ggplot_add(theme_cowplot(font_size = 10),p1)
p2  <- ggplot_add(theme_cowplot(font_size = 10),p2)
p12 <- plot_grid(p1,p2,labels = c("A","B"))

# Save as PDF.
ggsave("pizzaplot.pdf", plot=p12, width=8, height=4)
