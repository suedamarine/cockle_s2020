library(tidyverse)

# load data
temp_sizes <- read.csv("data/temp_sizes.csv")

# mutate to get total count
cockle_sizes <- temp_sizes %>%
  mutate(total = y0 + y1 + y2)

cockleT <- rep(c(cockle_sizes$length[1:37]), times = cockle_sizes$total[1:37])

breaks = seq(3,40, by = 1)

cockleT.cut <- cut(cockleT, breaks, right=FALSE)

cockleT.frequency <- table(cockleT.cut)

sum_t_freq <- sum(cockleT.frequency) 

cockleT.relfreq <- cockleT.frequency / sum_t_freq

cumrelfreqT <- c(0,cumsum(cockleT.relfreq))

cu_h <- cumrelfreqT[13]

rel_freq.df <- data.frame(breaks, cumrelfreqT)

p <- ggplot(rel_freq.df, aes(x= breaks, y = cumrelfreqT)) + geom_point(col =  "#a6611a") + geom_line(col =  "#a6611a") + geom_vline(xintercept = 15, col = "#dfc27d", linetype = "dashed", alpha = 0.9) + geom_hline(yintercept = cu_h, col = "#dfc27d", linetype = "dashed", alpha = 0.9 ) + theme_minimal() + labs(x= "Length mm", y = "Cumulative Frequency")


# Open a pdf file
pdf("plots/cumulative_size.pdf") 

# 2. Create a plot
p

# Close the pdf file
dev.off() 


