# workshop 2 Graphs and Tables
# mammal body mass
# alina.zeng(at)ubc.ca
# June-21, 2021


# Data downloaded from: https://www.zoology.ubc.ca/~bio501/R/workshops/graphics.html#Data_set_1:_Mammal_body_mass

# libraries
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(Cairo)
library(ggplot2)
library(RColorBrewer)


setwd("C:/Users/alina/Documents/git/BIOL_510_Stats/R_workshops/2_graphs_and_tables")


# import data
d <- read.csv("mammals.csv",na.strings = "", header = T)

# Which continent has the greatest number of mammal species? 
# Which has the least? Make a table of the frequency of cases on each continent 
table(d$continent)

# You’ll notice in the frequency table for the variable “continent” that there’s 
# a typo in the data. One case is shown as having the continent “Af” rather than “AF”. 
# Fix this using the command line in R and recalculate the frequency table.

d$continent<-ifelse(d$continent=="Af", "AF", d$continent)


# How many extinct mammals are recorded in the data file? Use a frequency table to find out.
table(d$status)


# Create a two-way frequency table (contingency table) showing the status of mammal species on each continent.
table(d$status, d$continent)

# Judging by eye, which continent has the greatest number of extinctions 
# relative to the number of extant species?
  

# Plot the number of mammal species on each continent using a simple bar graph. ----
# Include a label for the y axis.

# basic R
barplot(table(d$continent),ylab = "Frequency")

# Barplot sorted by frequency
barplot(sort(table(d$continent), decreasing=TRUE), col="firebrick",   
        cex.names=0.8, las = 1, ylim=c(0,1600), ylab="Frequency")


# ggplot methods

# To order by category in ggplot, first make a new factor variable
d$continent_ordered <- factor(d$continent, 
              levels = names(sort(table(d$continent), decreasing = TRUE)) )

png(filename="mammal_continent.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(d, aes(x=continent_ordered)) + 
  geom_bar(stat="count", fill = "firebrick") +
  labs(x = "Continent", y = "Frequency") +
  labs(title = "Number of Mammal Species")+
  theme_bw()
dev.off()


# Generate a histogram of the body masses of mammal species. How informative is that?!
hist(d$mass.grams)  
  
# Create a new variable in the mammal data frame: the log (base 10) of body mass. 
d$mass_log <- log(d$mass.grams)

# Generate a histogram of log body mass. Is this more informative? Morphological 
# data commonly require a log-transformation to analyze.
hist(d$mass_log, col="firebrick", right = FALSE, las = 1, 
     xlab = "Log10 body mass", main = "") 

# ggplot2
png(filename="log_body_mass.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(d, aes(x = mass_log)) + 
  geom_histogram(fill = "firebrick", col = "black", binwidth = 0.5, boundary = 0) + 
  labs(x = "log10 body mass", y = "Frequency") + 
  theme_classic()
dev.off()

# Redo the previous histogram but use a bin width of 2 units. How much detail is lost?
# Redo the histogram but try a bin width of of 1; then try 0.5; and then 0.1. 
# Which bin width is superior?



# Redo the histogram, but display probability density instead of frequency.

# Plot density instead
hist(d$mass_log, col="firebrick", right = FALSE, las = 1, prob = TRUE,
     xlab = "Log10 body mass", main = "", breaks = seq(0, 8.5, by = 0.5))


png(filename="log_body_mass_density.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(d, aes(x = mass_log)) + 
  geom_histogram(fill = "firebrick", col = "black", binwidth = 0.5, 
                 boundary = 0, aes(y = ..density..)) + 
  labs(x = "log10 body mass", y = "Density") + 
  theme_classic()
dev.off()

# How does the frequency distribution of log body mass depart from a normal 
# distribution? Answer by visual examination of the histogram you just created. 
# Now answer by examining a normal quantile plot instead. Which display is more 
# informative?


# Normal quantile plot
qqnorm(d$mass_log)
qqline(d$mass_log) # adds the straight line for comparison through 1st and 3rd quartiles

# Optional: redraw the histogram of log body mass and superimpose a normal 
# density curve to assess help detect deviations from normality

# Histogram with best-fit normal curve superimposed.
# The curve function is fussy about the name of the variable: must be "x"
x <- d$mass_log
hist(x, col="firebrick", right = FALSE, las = 1, prob = TRUE,
     xlab = "Log10 body mass", main = "", breaks = seq(0, 8.5, by = 0.5))
m <- mean(x, na.rm = TRUE)
s <- sd(x, na.rm = TRUE)
curve(dnorm(x, mean = m, sd = s), col="red", lwd = 2, add = TRUE)



# Use a box plot to compare the distribution of body sizes (log scale most 
# revealing) of mammals having different extinction status. Are extinct mammals 
# similar to, larger than, or smaller than, extant mammals?

boxplot(d$mass_log)
boxplot(mass_log ~ status, data = d, ylab = "log10 body mass", 
        col = "goldenrod1", las = 1)


png(filename="boxplot.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(d, aes(status, mass_log)) +
  geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
  # coord_cartesian(xlim = c(7,11))+ #muting shrub route
  theme_classic() +  
  theme(axis.text.x = element_text(size = 12, angle = 0)) +
  labs(x = "Status", y = "log10 body mass")
dev.off()


#  Examine the previous box plot. How do the shapes of the body size distributions 
# compare between extinct and extant mammals?
      # Extinct mammals tend to have large mass compared to extant mammals.
      # The frequency distributions for these two groups also have opposite skew.


#  Redo the previous box plot but make box width proportional to the square root 
# of sample size. Add a title to the plot.
#Optional: Draw a violin plot to compare the frequency distribution of log body
# sizes of mammals having different extinction status. Which do you find is more 
# revealing about the shapes of the body size distributions: box plot or violin plot?
#  Use multiple histograms to compare the frequency distribution of log body 
# sizes of mammals having different extinction status. Stack the panels one above 
# the other. In this plot, how easy is it to visualize differences among treatments 
# in the distributions compared to your previous plots?
#  Make a table of the median log body mass of each extinction-status group of 
# mammals. Are the values consistent with the plotted distributions

# Violin plot
png(filename="violinplot.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(d, aes(x = status, y = mass_log)) + 
  geom_violin(fill = "goldenrod1") + 
  labs(x = "Status", y = "Log10 body mass") + 
  stat_summary(fun.y = mean,  geom = "point", color = "black") +
  theme_classic()
dev.off()


# Multiple histograms
png(filename="facet_boxplot.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(d, aes(x = mass_log)) + 
  geom_histogram(fill = "goldenrod1", col = "black", 
                 binwidth = 0.2, boundary = 0) +
  labs(x = "log10 body mass", y = "Frequency") + 
  facet_wrap(~status, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_classic()
dev.off()