library(ggplot2)
library(dplyr)

#
# Read data
#

source("readpheno.R")
hmdp   <- read.csv("hmdp.csv",stringsAsFactors = FALSE)
hmdp   <- transform(hmdp,sex = factor(sex,c("M","F")))
hmdp   <- transform (hmdp,sex = factor(sex,c("M","F")))
hmdp   <- read.csv("hmdp.csv",stringsAsFactors = FALSE)
hmdp   <- transform(hmdp,sex = factor(sex,c("M","F")))
gwscan <- read.csv("gwscan.csv",stringsAsFactors = FALSE)
gwscan <- transform(gwscan,chr = factor(chr,1:19))
geno   <- read.csv("geno_rs29477109.csv",stringsAsFactors = FALSE)
geno   <- transform(geno,id = as.character(id))

# convert to tibbles
hmdp   <- tbl_df(hmdp)
gwscan <- tbl_df(gwscan)
geno   <- tbl_df(geno)
pheno <- tbl_df(pheno)

#
# Part A: 
#

# 1. Plot a histogram.
pl_ta <- pheno %>% ggplot() + aes(x=TA) + geom_histogram()
show(pl_ta) # yes, bell-shaped

pheno %>% select(TA,id) %>% arrange(TA) # 9 outliers (30.6 and below)

# 2. Plot tibia against TA.

# remove NAs
pheno_fixup <- pheno %>% filter(is.na(TA) != T, is.na(tibia) == F)

pl_ta_tibia <- pheno_fixup %>% ggplot() + aes(x=TA,y=tibia) + geom_point() 
cor(pheno_fixup$TA, pheno_fixup$tibia, method="pearson")
cor.test(pheno_fixup$TA, pheno_fixup$tibia, method="pearson")
raw_model_summary <- lm(pheno_fixup$tibia ~ pheno_fixup$TA)
summary(raw_model_summary)
# somewhat positive relationship (no outlier removed)

pheno_wo_ta_outlier <- pheno_fixup %>% filter(TA > 30.6)
pl_ta_tibia2 <- pheno_wo_ta_outlier %>% ggplot() + aes(x=TA,y=tibia) + geom_point()
cor(pheno_wo_ta_outlier$TA, pheno_wo_ta_outlier$tibia, method="pearson")
cor.test(pheno_wo_ta_outlier$TA, pheno_wo_ta_outlier$tibia, method="pearson")
nooutlier_model_summary <- lm(pheno$tibia ~ pheno$TA)
summary(nooutlier_model_summary)
# more cleanly positive after outliers removed.

# 3. Explore AvToneD3.
pheno_fixup <- pheno %>% filter(is.na(AvToneD3) == F)
pheno_fixup %>% ggplot() + aes(x=AvToneD3) + geom_histogram() # heavy right skew

logit <- function(x) {
  return(log((x + 0.001)/(1 - x + 0.001)))
}

pheno_fixup <- pheno_fixup %>% mutate(AvToneD3_logit = logit(AvToneD3))
pheno_fixup %>% ggplot() + aes(x=AvToneD3_logit) + geom_histogram() # more normal, many low outliers

pheno_fixup %>% ggplot() + aes(FCbox,AvToneD3_logit) + geom_boxplot() # yes


#
# Part B: Exploratory analyses of bone-mineral density data
#

pheno_bwd <- pheno %>%
  filter(is.na(BMD) != T)

p1_bwd_cfw <- pheno_bwd %>%
  ggplot() + aes(BMD) + geom_histogram(alpha=.5,fill="blue")
show(p1_bwd_cfw)


hmdp_bwd <- hmdp %>%
  mutate(BMD=1000*femur)
  filter(is.na(BMD) != T) %>%
  filter(sex == "M")

p1_bwd_hmdp <- hmdp_bwd %>%
  ggplot() + aes(BMD) + geom_histogram(alpha=.5,fill="green")
show(p1_bwd_hmdp)


p1_bwd_pheno_hmdp <- ggplot(hmdp_bwd) + aes(BMD) +
  geom_histogram(alpha=.5,fill="red",binwidth=2) +
  geom_histogram(data=pheno_bwd,alpha=.5,fill="blue",binwidth=2)
show(p1_bwd_pheno_hmdp)


#
# Part C
#

# 1. Make a Manhattan plot
gwscan$chr <- as.factor(gwscan$chr)

gwscan$abspos <- array(0, length(gwscan$pos))
for (idx in seq(1,19)) {
  if (idx == 1) {
    gwscan$abspos[gwscan$chr == idx] <- gwscan$pos[gwscan$chr == idx]
  } else {
    gwscan$abspos[gwscan$chr == idx] <- gwscan$pos[gwscan$chr == idx] +
      max(gwscan$abspos[gwscan$chr == (idx-1)])
  }
}

chrlabels <- as.character(seq(1,19))
chrstats <- gwscan %>%
  group_by(chr) %>%
  summarise(mean=mean(abspos), max=max(abspos), min=min(abspos))
breaks <- chrstats$mean
dim(breaks) <- length(chrstats$mean)

colors <- c("darkblue", "lightblue", "darkblue", "lightblue", "darkblue",
            "lightblue", "darkblue", "lightblue", "darkblue", "lightblue",
            "darkblue", "lightblue", "darkblue", "lightblue", "darkblue",
            "lightblue", "darkblue", "lightblue", "darkblue")

pl_manhattan <- gwscan %>% ggplot() + aes(x=abspos,y=abnormalBMD,color=chr) +
  geom_point(alpha=.2) + xlab("Chromosome") + ylab("-log10(P)") + 
  scale_color_manual(values=colors,guide=FALSE) + 
  scale_x_continuous(breaks=breaks,labels=chrlabels)
show(pl_manhattan)

gwscan %>% filter(abnormalBMD > 6) #324

# 10^-6 = .000001

qtldat <- gwscan %>% filter(as.integer(chr)==11, abnormalBMD > 6)
qtlrange <- max(qtldat$pos) - min(qtldat$pos) # 2.417040 MBp
# around 60-70
# yes, col1a1 is there.


# 2. Visualize relationship between genotype and phenotype.
dat <- inner_join(geno %>% filter(is.na(dosage) == F),
                  pheno %>% filter(is.na(BMD) == F))
dat %>% ggplot() + aes(dosage,BMD) + geom_point()
dat <- dat %>% mutate(geno = c("TT","CT","CC")[round(dosage)+1])
dat %>% ggplot() + aes(geno,BMD) + geom_boxplot() + xlab("Genotype")

cor(dat$dosage,dat$BMD)
