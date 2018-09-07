library(ggplot2)
library(dplyr)
library(ggpubr)
library(reshape2)
library(scales)
library(gtools)

source(file = "readpheno.R")

class(pheno) 
TA_hist <- ggplot(data.frame(pheno$TA), aes(x = pheno$TA, col='gray')) +
  geom_histogram()
TA_hist

#Checks for normality
ggqqplot(pheno$TA)
outliers = boxplot(pheno$TA, plot=FALSE, horizontal = FALSE)$out
outliers
length(outliers)
TA_box <- boxplot(TA)
sort(outliers)
pheno <- pheno$TA[]

pheno_without_outliers <- pheno[(pheno$TA > 40.4)&(pheno$TA<74.9), ]
pheno_without_outliers <- pheno_without_outliers[(!is.na(pheno_without_outliers$TA))&(!is.na(pheno_without_outliers$tibia)),]




ggqqplot(pheno_without_outliers$TA)

#Scatter Plot

tavstibia <- ggplot(pheno, aes(x=TA, y=tibia)) + geom_point()
out <-  labs(title="TA Weight v.s. Tibia Length",
             x="TA Weight (mg)", y = "Tibia Length (mm)")
tavstibia <- ggplot_add(out,tavstibia)
tavstibia

tavstibia <- ggplot_add(geom_smooth(method="lm",), tavstibia)
tavstibia <- ggplot_add(geom_smooth(method = "lm", data = pheno_without_outliers), tavstibia)
tavstibia

summary(lm(TA~tibia, data = pheno))
summary(lm(TA~tibia, data = pheno_without_outliers))



#####3 

pheno$AvToneD3

D3_hist <- ggplot(data.frame(pheno$AvToneD3), aes(x = pheno$AvToneD3, col='gray')) +
  geom_histogram() + labs(x = "Freezing to Cue (Time)", title= "AvToneD3")
D3_hist

logit <- function(x) {
  return(log((x+.001)/(1-x+.001)))
  }

d3_logit <- logit(pheno$AvToneD3)

D3_logit_hist <- ggplot(data.frame(d3_logit), aes(x = d3_logit, col='gray')) +
  geom_histogram() + labs(x = "Logit", title= "AvToneD3 Logit")
D3_logit_hist 

outliers = boxplot(d3_logit, plot=FALSE, horizontal = FALSE)$out
outliers

head(pheno$AvToneD3)

av_vs_fc <- ggplot() + geom_boxplot(aes (x=pheno$FCbox, y = d3_logit))
av_vs_fc

