# --------------------------------------------------------------------------------
#
# The normal Distribution
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

# The normal distribution, also known as the bell curve and as the Gaussian
# distribution, is one of the most famous mathematical concepts in history. A
# reason for this is that approximately normal distributions occur in many
# situations. Examples include gambling winnings, heights, weights, blood
# pressure, standardized test scores, and experimental measurement errors. There
# are explanations for this, but we describe these in a later chapter. Here we
# focus on how the normal distribution helps us summarize data.

# Rather than using data, the normal distribution is defined with a mathematical
# formula. For any interval (a, b) the proportion of values in that interval can
# be computed using this formula:

#         Pr(a<x<b)=∫ba1√2πsexp{−12(x−ms)2}dx

# You don’t need to memorize or understand the details of the formula. But note
# that it is completely defined by just two parameters: m and s. The rest of the
# symbols in the formula represent the interval ends that we determine, a and b,
# and known mathematical constants π and e. These two parameters, m and s, are
# referred to as the average, also called the mean, and the standard deviation
# (SD) of the distribution respectively.

# The distribution is symmetric, centered at the average, and most values (about
# 95%) are within 2 SDs from the average.

# The fact that the distribution is defined by just two parameters implies that
# if a dataset is approximated by a normal distribution, all the information
# needed to describe the distribution can be encoded in just two numbers: the
# average and the standard deviation, which we now define for an arbitrary list
# of numbers.

# For a list of numbers contained in a vector x, the average is defined as:
     
     average <- sum(x) / length(x)

# and the SD is defined as:
     
     SD <- sqrt( sum( (x-mu)^2) / length(x))

# which can be interpreted as the average distance between values and their
# average.

# Let’s compute the values for the height for males which we will store in the
# object x;

     index <- heights$sex=="Male"
     x <- heights$height[index]

# The pre-built functions mean and sd [Footnote: SD divides by length(x)-1] can
# be used here:
     
     average <- mean(x)
     SD <- sd(x)
     c(average=average,SD=SD)
#> average      SD 
#>   69.31    3.61

# --------------------------------------------------------------------------------
# Standardized Units
# --------------------------------------------------------------------------------

# For data that is approximately normally distributed, it is convenient to think
# in terms of standard units. The standard unit of a value tells us how many
# standard deviations away from the average it is. Specifically, for a value x 
# we define it as 

z=(x−average)/SD

# If you look back at the formula for the normal distribution, you see that what
# is being exponentiated is −z/2−z/2. The maximum of exp−z2/2expa−z2/2 is when
# z=0z=0, which explains why the maximum of the distribution is at the mean. It
# also explains the symmetry since −z/2−z/2 is symmetric around 0.

# If we convert the normally distributed data to standard units, we can quickly
# know if, for example, a person is about average (z=0z=0), one of the largest
# (z=2z=2), one of the smallest (z=−2z=−2) or an extremely rare occurrence
# (z>3z>3 or z<−3z<−3). Remember that it does not matter what the original units
# are, these rules apply to data that is approximately normal.

# In R, we can obtain standard units using the function scale:

z <- scale(x)

# Now to see how many men are within 2 SDs from the average we simply type:

mean(abs(z) < 2)
#> [1] 0.95

# The proportion is about 95%, which is what the normal distribution predicts!
# To further confirm that in fact the approximation is a good one, we can use
# quantile-quantile plots.

# --------------------------------------------------------------------------------
# Quantile-quantile QQ plots
# --------------------------------------------------------------------------------

# A systematic way to assess how well the normal distribution fits the data is
# to check if the observed and predicted proportions match. In general, the
# approach of the QQ-plot is as follows:

# Define a series of proportions p=0.05,….95p=0.05,….95.

# For each pp, determine the value qq so that the proportion of values in the
# data below qq is pp. The qqs are referred to as the quantiles.

# To give a quick example, for the male heights data we have that:

mean(x <= 69.5)
#> [1] 0.515

# 50% are shorter or equal to 69 inches. This implies that if p=0.50p=0.50 then
# q=69.5q=69.5. Now we define a series of pp:
     
p <- seq(0.05, 0.95, 0.05)

# If the quantiles for the data match the quantiles for the normal, then it must
# be because the data follows a normal distribution.

# QUARTILE FUNCTION
# To obtain the quantiles from the data, we can use the quantile function like
# this:
     
observed_quantiles <- quantile(x, p)

# To obtain the theoretical normal distribution quantiles, with the
# corresponding average and SD, we use the qnorm function:
    
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))

# To see if they match or not, we plot them against each other and draw the
# identity line:
     
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# Notice that this code becomes much cleaner if we use standard units:
     
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# Percentiles are special cases of quantiles that are commonly used. The
# percentiles are the quantiles you obtain when setting p at the .01, .02, ...
# .99. We call, for example, the case of p = 0.25 the 25th percentile, which
# gives us a number for which 25% of the data is below. The most famous
# percentile is the 50th, also known as the median.

# For the normal distribution the median and average are the same, but this is
# generally not the case.

# Another special case that receives a name are the quartiles , which are
# obtained when setting p = 0.25, 0.50, and 0.75.

# Using the histogram, density plots and qq-plots, we have become convinced that
# the male height data is well approximated with a normal distribution. In this
# case, we report back to ET a very succinct summary: male heights follow a
# normal distribution with an average of 69.315 inches and a SD of 3.611 inches.
# With this information ET will have a good idea of what to expect when he meets
# our male students.

# --------------------------------------------------------------------------------
# 19.6 - Case Study on Self-Reported Heights
# --------------------------------------------------------------------------------

data(reported_heights)
str(reported_heights)
#> 'data.frame':    1095 obs. of  3 variables:
#>  $ time_stamp: chr  "2014-09-02 13:40:36" "2014-09-02 13:46:59" "2014-09-02 13:59:20" "2014-09-02 14:51:53" ...
#>  $ sex       : chr  "Male" "Male" "Male" "Male" ...
#>  $ height    : chr  "75" "70" "68" "74" ...

# Height is a character vector so we create a new column with the numeric version:
     
reported_heights <- reported_heights %>%
mutate(original_heights = height, height = as.numeric(height))

# We get a warning about NAs. This is because some of the self reported heights
# were not numbers. We can see why we get these:
     
     reported_heights %>% 
     filter(is.na(height)) %>% 
     head()

#>            time_stamp    sex height original_heights
#> 1 2014-09-02 15:16:28   Male     NA            5' 4"
#> 2 2014-09-02 15:16:37 Female     NA            165cm
#> 3 2014-09-02 15:16:52   Male     NA              5'7
#> 4 2014-09-02 15:16:56   Male     NA            >9000
#> 5 2014-09-02 15:16:56   Male     NA             5'7"
#> 6 2014-09-02 15:17:09 Female     NA             5'3"

# Some students self reported their heights using feet and inches rather than
# just inches. Others used centimeters and others were just trolling. For now we
# will remove these entries:
     
     reported_heights <- filter(reported_heights, !is.na(height))

# If we compute the average and standard deviation, we notice that we obtain
# strange results. The average and standard deviation are different from the
# median and MAD:
     
     reported_heights %>% 
     group_by(sex) %>%
     summarize(average = mean(height), sd = sd(height),
                median = median(height), MAD = mad(height))
     
#> # A tibble: 2 x 5
#>   sex    average    sd median   MAD
#>   <chr>    <dbl> <dbl>  <dbl> <dbl>
#> 1 Female    63.4  27.9   64.2  4.05
#> 2 Male     103.  530.    70    4.45

# This suggests that we have outliers, which is confirmed by simply creating a
# boxplot:
     
     reported_heights %>% 
     ggplot(aes(sex, height)) + 
     geom_boxplot()
