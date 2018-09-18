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


