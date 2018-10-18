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

sum(tab == 1)

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)

# --------------------------------------------------------------------------------
# Distribution Function and Cumulative Distribution Function (CDF)
# --------------------------------------------------------------------------------

# It turns out that, in some cases, the average and the standard deviation are
# pretty much all we need to understand the data. We will learn data
# visualization techniques that will help us determine when this two number
# summary is appropriate. These same techniques will serve as an alternative for
# when two numbers are not enough.

# The most basic statistical summary of a list of objects or numbers is its
# distribution. The simplest way to think of a distribution is as a compact
# description of a list with many entries. This concept should not be new for
# most of you. For example, with categorical data, the distribution simply
# describes the proportion of each unique category. The sex represented in the
# heights dataset is:
     
#> 
#> Female   Male 
#>  0.227  0.773

# This two category frequency table is the simplest form of a distribution. We
# don’t really need to visualize it since one number describes everything we
# need to know: 23% are females and the rest are males. When there are more
# categories, then a simple barplot describes the distribution.

# This particular plot is simply showing us four numbers: one for each category.
# We usually use barplots to display a few numbers. Although this particular
# plot, a graphical representation of a frequency table, does not provide much
# more insight than a table itself, it is a first example of how we convert a
# vector into a plot that succinctly summarizes all the information in the
# vector. Once the data is numerical, the task of displaying distributions is
# more challenging.

# Cumulative distribution functions

# Numerical data, that are not categorical, also have distributions. In general,
# when data is not categorical, reporting the frequency of each entry is not an
# effective summary since most entries are unique. In our case study, while
# several students reported a height of 68 inches, only one student reported a
# height of 68.503937007874 inches and only one student reported a height
# 68.8976377952756 inches. We assume that they converted from 174 and 175
# centimeters respectively.

# Statistics textbooks teach us that a more useful way to define a distribution
# for numeric data is to define a function that reports the proportion of the
# data below for all possible values of a. This function is called the cumulative 
# distribution function (CDF). In statistics, the following notation is used:
     
#         F(a)=Pr(x≤a)

# Similar to what the frequency table does for categorical data, the CDF defines
# the distribution for numerical data. From the plot we can see that 34% of the
# values are below 65, since F(66)=F(66)= 0.164, or that 90% of the values are
# below 72, since F(72)=F(72)= 0.841, etc.. In fact, we can report the
# proportion of values between any two heights, say aa and bb, by computing
# F(b)−F(a)F(b)−F(a). This means that if we send this plot above to ET, he will
# have all the information needed to reconstruct the entire list. Paraphrasing
# the expression “a picture is worth a thousands word”, in this case, a picture
# is as informative as 812 numbers.

# A final note: because CDFs can be defined mathematically, as opposed to using
# data as we do here, the word empirical is added to distinguish and we use the
# term empirical CDF (ECDF) instead.

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

# The simplest way to make a histograms is to divide the span of our data into
# non-overlapping bins of the same size. Then, for each bin, we count the number
# of values that fall in that interval. The histogram plots these counts as bars
# with the base of the bar defined by the intervals.

x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 81)
avg <- mean(x)
stdev <- sd(x)
approx <- pnorm(81, avg, stdev) - pnorm(79, avg, stdev)
exact / approx

# Remember that smooth is a relative term. We can actually control the
# smoothness of the curve that defines the smooth density through an option in
# the function that computes the smooth density. Here are two examples using
# different degrees of smoothness on the same histogram:

p1 <- heights %>% 
     filter(sex=="Male") %>% ggplot(aes(height)) +
     geom_histogram(aes(y=..density..), binwidth = 1) + 
     geom_density(col="#00BFC4", adjust = 0.5)

p2 <- heights %>% 
     filter(sex=="Male") %>% ggplot(aes(height)) +
     geom_histogram(aes(y=..density..), binwidth = 1) + 
     geom_density(col="#00BFC4", adjust = 2)

grid.arrange(p1,p2, ncol=2)

# We need to make this choice with care as the resulting visualizations can
# change our interpretation of the data. We should select a degree of smoothness
# that we can defend as being representative of the underlying data. In the case
# of height, we really do have reason to believe that the proportion of people
# with similar heights should be the same. For example, the proportion that is
# 72 inches should be more similar to the proportion that is 71, than to the
# proportion that is 78 or 65. This implies that the curve should be pretty
# smooth; that is, more like the example on the right than on the left.

# As a final note, we point out that an advantage of smooth densities over
# histograms for visualization purposes is that densities makes it easier to
# compare two distributions. This is in large part because the jagged edges of
# the histogram add clutter. Here is an example comparing male and female
# heights:

heights %>% 
     ggplot(aes(height, fill=sex)) + 
     geom_density(alpha = 0.2)

library(dplyr)
library(NHANES)
data(NHANES)
## fill in what is needed
tab <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female")

library(dplyr)
library(NHANES)
data(NHANES)
## complete this line of code.
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>%
     filter(AgeDecade == " 20-29" & Gender == "female") %>%
     summarize(average = mean(BPSysAve, na.rm = TRUE), 
               standard_deviation = sd(BPSysAve, na.rm=TRUE))

library(dplyr)
library(NHANES)
data(NHANES)
## modify the code we wrote for previous exercise.
ref_avg <- NHANES %>%
     filter(AgeDecade == " 20-29" & Gender == "female") %>%
     summarize(average = mean(BPSysAve, na.rm = TRUE), 
               standard_deviation = sd(BPSysAve, na.rm=TRUE))  %>% .$average

library(dplyr)
library(NHANES)
data(NHANES)
## complete the line
NHANES %>%
     filter(AgeDecade == " 20-29"  & Gender == "female") %>%
     summarize(min = min(BPSysAve, na.rm = TRUE), max = max(BPSysAve, na.rm = TRUE))

library(dplyr)
library(NHANES)
data(NHANES)
##complete the line with group_by and summarize
NHANES %>%
     filter(Gender == "female") %>% group_by(AgeDecade) %>%
     summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

library(dplyr)
library(NHANES)
data(NHANES)
NHANES %>%
     filter(Gender == "male") %>% group_by(AgeDecade) %>%
     summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

library(NHANES)
data(NHANES)
NHANES %>%
     group_by(AgeDecade, Gender) %>%
     summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

library(dplyr)
library(NHANES)
data(NHANES)
NHANES %>%
     group_by(Race1) %>% 
     filter(Gender == "male" & AgeDecade == " 40-49") %>%  
     summarize(average = mean(BPSysAve, na.rm = TRUE), 
               standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>% 
     arrange(average)

