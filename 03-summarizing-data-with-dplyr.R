# --------------------------------------------------------------------------------
#
# Summarizing Data with dplyr
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)

# The summarize function in dplyr provides a way to compute summary statistics
# with intuitive and readable code. We start with a simple example based on
# heights that computes the average and standard deviation for males:

s <- heights %>% 
filter(sex == "Male") %>%
summarize(average = mean(height), standard_deviation = sd(height))
s
#>   average standard_deviation
#> 1    69.3               3.61

# This takes our original data table as input, filters it to keep only males and
# then produces a new, summarized table with just the average and the standard
# deviation of heights. We get to choose the names of the columns of the
# resulting table. For example, above we decided to use average and
# standard_deviation, but we could have used other names just the same.

# Because the resulting table, stored in s, is a data frame, we can access the
# components with the accessor $, which in this case will be a numeric:
     
s$average
#> [1] 69.3
s$standard_deviation
#> [1] 3.61

# As with most other dplyr functions, summarize is aware of the variable names
# and we can use them directly. So when inside the call to the summarize
# function we write mean(height), it is accessing the column with the name, and
# then computing the average of the respective numeric vector. We can compute
# any other summary that operates on vectors and returns a single value. For
# example, we can add the median, min and max like this:

heights %>% 
filter(sex == "Male") %>%
summarize(median = median(height), minimum = min(height), maximum = max(height))
#>   median minimum maximum
#> 1     69      50    82.7

# We can obtain these three values with just one line using the quantiles
# function; e.g. quantile(x, c(0,0.5,1)) returns the min, median, and max of the
# vector x. However, if we attempt to use a function that returns two or more
# values:

heights %>% 
     filter(sex == "Male") %>%
     summarize(range = quantile(height, c(0, 0.5, 1)))

# we will receive an error: Error: expecting result of length one, got : 2. With
# the function summarize, we can only call functions that return a single value.
# In a later chapter, we will learn how to deal with functions that return more
# than one value.

# For another example of how we can use the summarize function, let’s compute
# the average murder rate for the United States. Remember our data table
# includes total murders and population size for each state and we have already
# used dplyr to add a murder rate column:

data(murders)
murders <- murders %>% mutate(murder_rate = total/population*100000)

# Remember that the US murder is not the average of the state murder rates:

summarize(murders, mean(murder_rate))
#>   mean(murder_rate)
#> 1              2.78

# This is because in the computation above the small states are given the same
# weight as the large ones. The US murder rate is the total US murders divided
# by the total US population. So the correct computation is:

us_murder_rate <- murders %>% summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate
#>   rate
#> 1 3.03

# This computation counts larger states proportionally to their size which
# results in a larger value.

# --------------------------------------------------------------------------------
# The dot operator
# --------------------------------------------------------------------------------

# The us_murder_rate object defined above represents just one number. Yet we are
# storing it in a data frame:

class(us_murder_rate)
#> [1] "data.frame"

# since, as most dplyr functions, summarize always returns a data frame.

# This might be problematic if we want to use the result with functions that
# require a numeric value. Here we show a useful trick for accessing values
# stored in data piped via %>%: when a data object is piped it can be accessed
# using the dot .. To understand what we mean take a look at this line of code:

us_murder_rate %>% .$rate 
#> [1] 3.03

# This returns the value in the rate column of us_murder_rate making it
# equivalent to us_murder_rate$rate. To understand this line, you just need to
# think of . as a placeholder for the data that is being passed through the
# pipe. Because this data object is a data frame, we can access its columns with
# the $.

# To get a number from the original data table with one line of code we can
# type:

us_murder_rate <- murders %>% 
     summarize(rate = sum(total) / sum(population) * 100000) %>%
     .$rate

us_murder_rate
#> [1] 3.03

# which is now a numeric:

class(us_murder_rate)
#> [1] "numeric"

# We will see other instances in which using the . is useful. For now, we will
# only use it to produce numeric vectors from pipelines constructed with dplyr.

# --------------------------------------------------------------------------------
# The group_by operator
# --------------------------------------------------------------------------------

# A common operation in data exploration is to first split data into groups and
# then compute summaries for each group. For example, we may want to compute the
# average and standard deviation for men’s and women’s heights separately. The
# group_by function helps us do this. If we type this:

heights %>% group_by(sex)
#> # A tibble: 1,050 x 2
#> # Groups:   sex [2]
#>   sex    height
#>   <fct>   <dbl>
#> 1 Male       75
#> 2 Male       70
#> 3 Male       68
#> 4 Male       74
#> 5 Male       61
#> 6 Female     65
#> # ... with 1,044 more rows

# the result does not look very different from heights, except we see this
# Groups: sex [2] when we print the object. Although not immediately obvious
# from its appearance, this is now a special data frame called a grouped data
# frame and dplyr functions, in particular summarize, will behave differently
# when acting on this object. Conceptually, you can think of this table as many
# tables, with the same columns but not necessarily the same number of rows,
# stacked together in one object. When we summarize the data after grouping,
# this is what happens:

heights %>% 
group_by(sex) %>%
summarize(average = mean(height), standard_deviation = sd(height))
#> # A tibble: 2 x 3
#>   sex    average standard_deviation
#>   <fct>    <dbl>              <dbl>
#> 1 Female    64.9               3.76
#> 2 Male      69.3               3.61

# The summarize function applies the summarization to each group separately. For
# another example, let’s compute the median murder rate in the four regions of
# the country:
     
murders %>% 
group_by(region) %>%
summarize(median_rate = median(murder_rate))
#> # A tibble: 4 x 2
#>   region        median_rate
#>   <fct>               <dbl>
#> 1 Northeast            1.80
#> 2 South                3.40
#> 3 North Central        1.97
#> 4 West                 1.29

# --------------------------------------------------------------------------------
# Sorting data frames with arrange
# --------------------------------------------------------------------------------

# When examining a dataset, it is often convenient to sort the table by the
# different columns. We know about the order and sort function, but for ordering
# entire tables, the dplyr function arrange is useful. For example, here we
# order the states by population size when we type:

murders %>% arrange(population) %>% head()
#>                  state abb        region population total murder_rate
#> 1              Wyoming  WY          West     563626     5       0.887
#> 2 District of Columbia  DC         South     601723    99      16.453
#> 3              Vermont  VT     Northeast     625741     2       0.320
#> 4         North Dakota  ND North Central     672591     4       0.595
#> 5               Alaska  AK          West     710231    19       2.675
#> 6         South Dakota  SD North Central     814180     8       0.983

# We get to decide which column to sort by. To see the states by population,
# from smallest to largest, we arrange by murder_rate instead:

murders %>% 
arrange(murder_rate) %>% 
head()
#>           state abb        region population total murder_rate
#> 1       Vermont  VT     Northeast     625741     2       0.320
#> 2 New Hampshire  NH     Northeast    1316470     5       0.380
#> 3        Hawaii  HI          West    1360301     7       0.515
#> 4  North Dakota  ND North Central     672591     4       0.595
#> 5          Iowa  IA North Central    3046355    21       0.689
#> 6         Idaho  ID          West    1567582    12       0.766

# Note that the default behavior is to order in ascending order. In dplyr, the
# function desc transforms a vector so that it is in descending order. To sort
# the table in descending order we can type:

murders %>% 
arrange(desc(murder_rate)) %>% 
head()
#>                  state abb        region population total murder_rate
#> 1 District of Columbia  DC         South     601723    99       16.45
#> 2            Louisiana  LA         South    4533372   351        7.74
#> 3             Missouri  MO North Central    5988927   321        5.36
#> 4             Maryland  MD         South    5773552   293        5.07
#> 5       South Carolina  SC         South    4625364   207        4.48
#> 6             Delaware  DE         South     897934    38        4.23

# NESTED SORTING

# If we are ordering by a column with ties, we can use a second column to break
# the tie. Similarly, a third column can be used to break ties between first and
# second and so on. Here we order by region then, within region, we order by
# murder rate:

murders %>% 
arrange(region, murder_rate) %>% 
head()
#>           state abb    region population total murder_rate
#> 1       Vermont  VT Northeast     625741     2       0.320
#> 2 New Hampshire  NH Northeast    1316470     5       0.380
#> 3         Maine  ME Northeast    1328361    11       0.828
#> 4  Rhode Island  RI Northeast    1052567    16       1.520
#> 5 Massachusetts  MA Northeast    6547629   118       1.802
#> 6      New York  NY Northeast   19378102   517       2.668

