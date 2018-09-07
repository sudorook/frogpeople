# PART C 1.GWAS

gwscan <- read.csv("./data/gwscan.csv", stringsAsFactors = F)

# The last position (of SNP) for each chromosome
max_table <- gwscan %>%
  group_by(chr) %>%
  summarise(max_chr = max(pos))
for (i in 1:19) {
  max_table$sum[i] <- sum(max_table$max_chr[1:i])
}

# Add on last position to each relative position (on each chromosome) 
# to generate the overall position (real_pos)
gwscan$real_pos <- gwscan$pos
for (i in 2:19) {
  gwscan$real_pos[which(gwscan$chr == i)] <- gwscan$pos[which(gwscan$chr == i)] + max_table$sum[i-1]
}

# Calculate the middle position for each chromosome (for plotting x label)
breaks <- gwscan %>%
  group_by(chr) %>%
  summarise(median = median(pos))
max_table$median <- breaks$median
for (i in 2:19) {
  max_table$median[i] <- max_table$sum[i-1] + breaks$median[i]
}

# Set up the factor for color indication (by chromosome)
gwscan$color_ind = gwscan$chr%%2

# Plot the log10p-value along overall position
Manhattan <- gwscan %>%
  select(real_pos, abnormalBMD) %>%
  ggplot(aes(x= real_pos, y = abnormalBMD)) + 
  geom_point(aes(colour = factor(gwscan$color_ind)), show.legend = F)
# Set up color vector for distingusih different chromosomes
cols <- c("1" = '#006400', "0" = '#009999')
# Add colors, x and y scales for the plot
Manhattan <- Manhattan + scale_color_manual(values = cols) +
  scale_x_continuous(name = "Chromosome", breaks = max_table$median, label = c(1:19)) +
  scale_y_continuous(name = expression(paste("-Log"["10"],italic("P"))), breaks = seq(0, 18, 3), labels = seq(0, 18, 3))
ggsave("Manhattan.png", Manhattan, width = 30, height = 15, units = "cm", dpi = 300)

# Question 1: How many distinct regions of the genome are 
# strongly associated with abnormal BMD at this p-value threshold (> 6)?

##From figures: two major regions

Outliers <- gwscan %>%
  filter(abnormalBMD > 6)
##From table: two regions on chr 5 and 11

# Question 2: What p-value does a log10 p-value of 6 correspond to?
## P = 10^(-6)

# Question 3: identify the “quantitative trait locus” (QTL) 
# with the strongest association signal (>6 threshold)
gwscan <- gwscan %>%
  mutate(color_ind = replace(color_ind, abnormalBMD > 6, 2))
Manhattan_out <- gwscan %>%
  select(real_pos, abnormalBMD) %>%
  ggplot(aes(x= real_pos, y = abnormalBMD)) + 
  geom_point(aes(colour = factor(gwscan$color_ind)), show.legend = F)
cols <- c("2" = '#800000', "1" = '#006400', "0" = '#009999')
Manhattan_out <- Manhattan_out + scale_color_manual(values = cols) +
  scale_x_continuous(name = "Chromosome", breaks = max_table$median, label = c(1:19)) +
  scale_y_continuous(name = expression(paste("-Log"["10"],italic("P"))), breaks = seq(0, 18, 3), labels = seq(0, 18, 3))
ggsave("Manhattan_out.png", Manhattan_out, width = 30, height = 15, units = "cm", dpi = 300)


#  the size of the QTL in Megabases (Mb)
range_pos_5 <- Outliers %>%
  filter(chr == 5) %>%
  select(pos) %>%
  summarise(range_pos_5 = (max(pos) - min(pos))/ 1e+06)
## Chromosome 5: 2.884012 Mb

range_pos_11 <- Outliers %>%
  filter(chr == 11) %>%
  select(pos) %>%
  summarise(range_pos_11 = (max(pos) - min(pos))/ 1e+06)
## Chromosome11: 2.41704 Mb
## Col1a1 included