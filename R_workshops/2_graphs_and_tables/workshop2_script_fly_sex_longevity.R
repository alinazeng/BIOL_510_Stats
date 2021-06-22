# workshop 2 Graphs and Tables
# fly sex and longevity
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
library(ggpubr)

setwd("C:/Users/alina/Documents/git/BIOL_510_Stats/R_workshops/2_graphs_and_tables")


# import data
fruitflies <- read.csv("fruitflies.csv",na.strings = "", header = T)



# Use a strip chart to examine the distribution of longevities in the treatment 
# groups. Try the jitter method to reduce overlap between points. If needed, 
# adjust the size or rotation of the treatment labels so that they all fit on 
# the graph. What pattern of differences between treatments in longevity is revealed?


stripchart(longevity.days ~ treatment, data=fruitflies, vertical=TRUE, method="jitter", 
           pch=16, col = "firebrick", cex.axis=0.7, ylab="Longevity (days)")

# Strip chart using ggplot
png(filename="strip_flies.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(fruitflies, aes(x=treatment, y=longevity.days)) +
  geom_jitter(color = "firebrick", size = 3, width = 0.15) +
  labs(x = "Treatment", y = "Longevity (days)") + 
  theme_classic()
dev.off()


# Box plot using ggplot
png(filename="boxplot_flies.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(fruitflies, aes(x=treatment, y=longevity.days)) +
  geom_boxplot(fill = "goldenrod1", width = 0.5) +
  labs(x = "Treatment", y = "Longevity (days)") + 
  theme_classic()
dev.off()


# arranging them on the same page
bp <- ggplot(fruitflies, aes(x=treatment, y=longevity.days)) +
  geom_boxplot(fill = "goldenrod1", width = 0.5) +
  labs(x = "Treatment", y = "Longevity (days)") + 
  theme_classic()
  
st <- ggplot(fruitflies, aes(x=treatment, y=longevity.days)) +
  geom_jitter(color = "firebrick", size = 3, width = 0.15) +
  labs(x = "Treatment", y = "Longevity (days)") + 
  theme_classic()

png(filename="boxplot_strip_flies.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=6, 
    res=300)
ggarrange(bp, st,        # http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()

#  Compare the strip chart to a box plot of the same data. Is the pattern in the data as clear in both types of plot?


# The variable thorax stands for thorax length, which was used as a measure of 
# body size. The measurement was included in case body size also affected 
# longevity. Produce a scatter plot of thorax length and longevity. Make 
# longevity the response variable (i.e., plot it on the vertical axis). 
# Is there a relationship?

# Scatter plot with ggplot
png(filename="body_length_flies.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days)) + 
  geom_point(size = 3, col = "firebrick") + 
  labs(x = "Thorax length (mm)", y = "Longevity (days)") + 
  theme_classic()
dev.off()




# Scatter plot with separate colors for each group using ggplot
png(filename="body_length_flies_treatment.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(fruitflies, aes(x = thorax.mm, y = longevity.days, colour = treatment, 
              shape = treatment)) + 
  geom_point(size = 2) + 
  labs(x = "Thorax length (mm)", y = "Longevity (days)") + 
  theme_classic()
dev.off()

png(filename="body_length_flies_treatment_lines.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(fruitflies, aes(x=thorax.mm, y=longevity.days, colour = treatment, 
              shape = treatment)) + 
  geom_point(size = 2) +
  geom_smooth(method = lm, size = 1, se = FALSE) +
  labs(x = "Thorax length (mm)", y = "Longevity (days)") + 
  theme_classic()
dev.off()

# Add lines; shown for ggplot method only



#  Redraw the scatter plot but this time use different symbols or colors for the different treatment groups. Add a legend to identify the symbols. Describe the pattern of differences between treatments.
Add scatterplot smoothers or linear regressions to the previous figure, one for each group. Do the differences in longevity among the treatments stand out when variation in body size is incorporated