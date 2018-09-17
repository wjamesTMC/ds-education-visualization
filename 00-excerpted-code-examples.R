# --------------------------------------------------------------------------------
#
# Visualization examples and exercises
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
data(heights)

# Dtacamp Exercises

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]

names(heights)
head(heights)

x <- heights$height
length(unique(x))
x <- heights$height
tab <- table(heights$height)
tab <- table(heights$height)
sum(tab == 1)

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
male_percentiles <- quantile(male, probs = c(0.1, 0.3, 0.5, 0.7, 0.9))
female_percentiles <- quantile(female, probs = c(0.1, 0.3, 0.5, 0.7, 0.9))
df <- data.frame("female" = female_percentiles, "male" = male_percentiles)
df

x <- heights$height[heights$sex == "Male"]
mean(69 < x & x <= 72)

x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72, avg, stdev) - pnorm(69, avg, stdev)

x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 81)
avg <- mean(x)
stdev <- sd(x)
approx <- pnorm(81, avg, stdev) - pnorm(79, avg, stdev)
exact / approx

p1 <- heights %>% 
     filter(sex=="Male") %>% ggplot(aes(height)) +
     geom_histogram(aes(y=..density..), binwidth = 1) + 
     geom_density(col="#00BFC4", adjust = 0.5)

p2 <- heights %>% 
     filter(sex=="Male") %>% ggplot(aes(height)) +
     geom_histogram(aes(y=..density..), binwidth = 1) + 
     geom_density(col="#00BFC4", adjust = 2)

grid.arrange(p1,p2, ncol=2)

#
# Distributions
#

# Numerical data, that are not categorical, also have distributions. In general,
# when data is not categorical, reporting the frequency of each entry is not an
# effective summary since most entries are unique. In our case study, while
# several students reported a height of 68 inches, only one student reported a
# height of 68.503937007874 inches and only one student reported a height
# 68.8976377952756 inches. We assume that they converted from 174 and 175
# centimeters respectively.

# Statistics textbooks teach us that a more useful way to define a distribution
# for numeric data is to define a function that reports the proportion of the
# data below

for all possible values of a. This function is called the cumulative distribution function (CDF). In statistics, the following notation is used:
     
     F
(
     a
)
=
     Pr
(
     x
     ≤
     a
)
