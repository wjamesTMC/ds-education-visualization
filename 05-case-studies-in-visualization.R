# --------------------------------------------------------------------------------
#
# Case Study: Trends in World Health and Economics
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(Lahman)
library(HistData)

# We will be using the gapminder dataset provided in dslabs. This dataset was
# created using a number of spreadsheets available from the Gapminder
# Foundation.

data(gapminder)
head(gapminder)

# Looking at countries with the highest mortality rates
gapminder %>% 
     filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) %>% 
     select(country, infant_mortality)
#>     country infant_mortality
#> 1 Sri Lanka              8.4
#> 2    Turkey             11.6

# Start by looking at data from 50 years ago
filter(gapminder, year==1962) %>%
     ggplot( aes(fertility, life_expectancy)) +
     geom_point()

# To confirm that indeed these countries are from the regions we expect, we can
# use color to represent continent.

filter(gapminder, year==1962) %>%
     ggplot( aes(fertility, life_expectancy, color = continent)) +
     geom_point() 

# We could easily plot the 2012 data in the same way we did for 1962. But to
# compare, side by side plots are preferable. In ggplot2, we can achieve this by
# faceting variables: we stratify the data by some variable and make the same
# plot for each strata.

# To achieve faceting, we add a layer with the function facet_grid, which
# automatically separates the plots. This function lets you facet by up to two
# variables using columns to represent one variable and rows to represent the
# other. The function expects the row and column variables to be separated by a
# ~. Here is an example of a scatterplot with facet_grid added as the last
# layer:
     
filter(gapminder, year%in%c(1962, 2012)) %>%
     ggplot(aes(fertility, life_expectancy, col = continent)) +
     geom_point() +
     facet_grid(continent~year)

# We see a plot for each continent/year pair. However, this is just an example
# and more than what we want, which is simply to compare 1962 and 2012. In this
# case, there is just one variable and we use . to let facet know that we are
# not using one of the variables:
     
filter(gapminder, year%in%c(1962, 2012)) %>%
     ggplot(aes(fertility, life_expectancy, col = continent)) +
     geom_point() +
     facet_grid( . ~ year)

# This plot clearly shows that the majority of countries have moved from the
# developing world cluster to the western world one. In 2012, the western versus
# developing world view no longer makes sense. This is particularly clear when
# comparing Europe to Asia, the latter which includes several countries that
# have made great improvements.

# To explore how this transformation happened through the years, we can make the
# plot for several years. For example, we can add 1970, 1980, 1990, and 2000. If
# we do this, we will not want all the plots on the same row, the default
# behavior of facet_grid, since they will become too thin to show the data.
# Instead we will want to use multiple rows and columns. The function facet_wrap
# permits us to do this by automatically wrapping the series of plots so that
# each display has viewable dimensions:
     
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>% 
     filter(year %in% years & continent %in% continents) %>%
     ggplot( aes(fertility, life_expectancy, col = continent)) +
     geom_point() +
     facet_wrap(~year) 

# Fixed scales for better comparisons

# The default choice of the range of the axes is an important one. When not
# using facet, this range is determined by the data shown in the plot. When
# using facet, this range is determined by the data shown in all plots and
# therefore kept fixed across plots. This makes comparisons across plots much
# easier. For example, in the above plot, we can see that life expectancy has
# increased and the fertility has decreased across most countries. We see this
# because the cloud of points moves. This is not the case if we adjust the
# scales.

# Time series plots

# The visualizations above effectively illustrates that data no longer supports
# the western versus developing world view. Once we see these plots, new
# questions emerge. For example, which countries are improving more and which
# ones less? Was the improvement constant during the last 50 years or was it
# more accelerated during certain periods? For a closer look that may help
# answer these questions, we introduce time series plots.

# Time series plots have time in the x-axis and an outcome or measurement of
# interest on the y-axis. For example, here is a trend plot of United States
# fertility rates:
     
gapminder %>% 
     filter(country == "United States") %>% 
     ggplot(aes(year,fertility)) +
     geom_point()

# When the points are regularly and densely spaced, as they are here, we create
# curves by joining the points with lines, to convey that these data are from a
# single country. To do this we use the geom_line function instead of
# geom_point.

gapminder %>% 
     filter(country == "United States") %>% 
     ggplot(aes(year,fertility)) +
     geom_line()

# This is particularly helpful when we look at two countries. If we subset the
# data to include two countries, one from Europe and one from Asia, then adapt
# the code above:
     
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>% 
     ggplot(aes(year,fertility)) +
     geom_line()

# Unfortunately, this is not the plot that we want. Rather than a line for each
# country, the points for both countries are joined. This is actually expected
# since we have not told ggplot anything about wanting two separate lines. To
# let ggplot know that there are two curves that need to be made separately, we
# assign each point to a group, one for each country:
     
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>% 
     ggplot(aes(year,fertility, group = country)) +
     geom_line()
#> Warning: Removed 2 rows containing missing values (geom_path).

# But which line goes with which country? We can assign colors to make this
# distinction. A useful side-effect of using the color argument to assign
# different colors to the different countries is that the data is automatically
# grouped:
     
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>% 
     ggplot(aes(year,fertility, col = country)) +
     geom_line()
#> Warning: Removed 2 rows containing missing values (geom_path).

# Labels for legends

# For trend plots we recommend labeling the lines rather than using legends
# since the viewer can quickly see which line is which country. This suggestion
# actually applies to most plots: labeling is usually preferred over legends.

# We demonstrate how we can do this using the life expectancy data. We define a
# data table with the label locations and then use a second mapping just for
# these labels:
     
labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))

gapminder %>% 
     filter(country %in% countries) %>% 
     ggplot(aes(year, life_expectancy, col = country)) +
     geom_line() +
     geom_text(data = labels, aes(x, y, label = country), size = 5) +
     theme(legend.position = "none")

# Income distribution

# Another commonly held notion is that wealth distribution across the world has
# become worse during the last decades. When general audiences are asked if poor
# countries have become poorer and rich countries become richer, the majority
# answers yes. By using stratification, histograms, smooth densities, and
# boxplots, we will be able to understand if this is in fact the case. We will
# also learn how transformations can sometimes help provide more informative
# summaries and plots.

# Transformations

# The gapminder data table includes a column with the countries gross domestic
# product (GDP). GDP measures the market value of goods and services produced by
# a country in a year. The GDP per person is often used as a rough summary of a
# country’s wealth. Here we divide this quantity by 365 to obtain the more
# interpretable measure dollars per day. Using current US dollars as a unit, a
# person surviving on an income of less than $2 a day is defined to be living in
# absolute poverty. We add this variable to the data table:
     
gapminder <- gapminder %>%  mutate(dollars_per_day = gdp/population/365)

# Here is a histogram of per day incomes from 1970:
     
past_year <- 1970
gapminder %>% 
     filter(year == past_year & !is.na(gdp)) %>%
     ggplot(aes(dollars_per_day)) + 
     geom_histogram(binwidth = 1, color = "black")

# In this plot we see that for the majority of countries, averages are below $10
# a day. However, the majority of the x-axis is dedicated to the 35 countries
# with averages above $10. So the plot is not very informative about countries
# with values below $10 a day.

# It might be more informative to quickly be able to see how many countries have
# average daily incomes of about $1 (extremely poor), $2 (very poor), $4 (poor),
# $8 (middle), $16 (well off), $32 (rich), $64 (very rich) per day. These
# changes are multiplicative and log transformations convert multiplicative
# changes into additive ones: when using base 2, a doubling of a value turns
# into an increase by 1.

# Here is the distribution if we apply a log base 2 transform:
     
gapminder %>% 
     filter(year == past_year & !is.na(gdp)) %>%
     ggplot(aes(log2(dollars_per_day))) + 
     geom_histogram(binwidth = 1, color = "black")

# For an example in which base 10 makes more sense, consider population sizes. A
# log base 10 is preferable since the range for these is:
     
filter(gapminder, year == past_year) %>%
     summarize(min = min(population), max = max(population))
#>     min      max
#> 1 46075 8.09e+08

# Here is the histogram of the transformed values:
     
gapminder %>% filter(year == past_year) %>%
     ggplot(aes(log10(population))) +
     geom_histogram(binwidth = 0.5, color = "black")

# The advantage of using logged scales is that we see the original values on the
# axes. However, the advantage of showing logged scales is that the original
# values are displayed in the plot, which are easier to interpret. For example,
# we would see “32 dollars a day” instead of “5 log base 2 dollar a day”.

# As we learned earlier, if we want to scale the axis with logs, we can use the
# scale_x_ccontinuous function. So instead of logging the values first, we apply
# this layer (Note that the log base 10 transformation has its own function: 
# scale_x_log10(): but currently base 2 does not, although we could easily define 
# our own.
     
gapminder %>% 
     filter(year == past_year & !is.na(gdp)) %>%
     ggplot(aes(dollars_per_day)) + 
     geom_histogram(binwidth = 1, color = "black") +
     scale_x_continuous(trans = "log2")

# There are other transformations available through the trans argument. As we
# learn later on, the square root (sqrt) transformation, for example, is useful
# when considering counts. The logistic transformation (logit) is useful when
# plotting proportions between 0 and 1. The reverse transformation is useful
# when we want smaller values to be on the right or on top.

# The histogram showed us that the income distribution values show a dichotomy.
# However, the histogram does not show us if the two groups of countries are
# west versus the developing world.

# To see distributions by geographical region, we first stratify the data into
# regions and then examine the distribution for each. Because of the number of
# regions:
     
n_distinct(gapminder$region)
#> [1] 22

# looking at histograms or smooth densities for each will not be useful.
# Instead, we can stack boxplots next to each other:
     
p <- gapminder %>% 
     filter(year == past_year & !is.na(gdp)) %>%
     ggplot(aes(region, dollars_per_day)) 
p + geom_boxplot() 

# Now we can’t read the region names because the default gpplot2 behavior is to
# write the labels horizontally and, here, we run out of room. We can easily fix
# this by rotating the labels. Consulting the cheat sheet we find we can rotate
# the names by changing the theme through element_text. The hjust=1 justifies
# the text so that it is next to the axis.

p + geom_boxplot() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))

# There are a few more adjustments we can make to this plot that help uncover
# this reality. First, it helps to order the regions in the boxplots from poor
# to rich rather than alphabetically. This can be achieved using the reorder
# function. This function lets us change the order of the levels of a factor
# variable based on a summary computed on a numeric vector. Remember that many
# graphing functions coerce character vectors into a factor. The default
# behavior results in alphabetically ordered levels:
     
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)
#> [1] "Asia" "West"

value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)
#> [1] "West" "Asia"

# Second, we can use color to distinguish the different continents, a visual cue
# that helps find specific regions. Here is the code:
     
p <- gapminder %>% 
     filter(year == past_year & !is.na(gdp)) %>%
     mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
     ggplot(aes(region, dollars_per_day, fill = continent)) +
     geom_boxplot() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     xlab("")
p

# As with the histogram, if we remake the plot using a log scale:
     
p + scale_y_continuous(trans = "log2")

# Show the data. In many cases, we do not show the data because it adds clutter
# to the plot and obfuscates the message. In the example above, we don’t have
# that many points so adding them actually lets us see all the data. We can add
# this layer using geom_point():
     
p + scale_y_continuous(trans = "log2") +
     geom_point(show.legend = FALSE) 

# The exploratory data analysis above has revealed two characteristics about average income distribution in 1970. Using a histogram, we found a bimodal distribution with the modes relating to poor and rich countries. Then by stratifying by region and examining boxplots, we found that the rich countries were mostly in Europe and Northern America, along with Australia and New Zealand. We define a vector with these regions:
     
west <- c("Western Europe",
          "Northern Europe",
          "Southern Europe",
          "Northern America",
          "Australia and New Zealand")

# Now we want to focus on comparing the differences in distributions across
# time.

# We start by confirming that the bimodality observed in 1970 is explained by a
# “west versus developing world”" dichotomy. We do this by creating histograms
# for the previously identified groups. We create the two groups with an ifelse
# inside a mutate and then we use facet_grid to make a histogram for each group:

gapminder %>% 
filter(year == past_year & !is.na(gdp)) %>%
mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
ggplot(aes(dollars_per_day)) +
geom_histogram(binwidth = 1, color = "black") +
scale_x_continuous(trans = "log2") + 
facet_grid(. ~ group)

# Now we are ready to see if the separation is worse today than it was 40 years ago. We do this by faceting by both region and year:
     
past_year <- 1970
present_year <- 2010
gapminder %>% 
     filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
     mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
     ggplot(aes(dollars_per_day)) +
     geom_histogram(binwidth = 1, color = "black") +
     scale_x_continuous(trans = "log2") + 
     facet_grid(year ~ group)


# We remake the plots using only countries with data available for both years.
# In the data wrangling chapter, we will learn tidyverse tools that permit us to
# write efficient code for this, but here we can use simple code using the
# intersect function:
     
country_list_1 <- gapminder %>% 
     filter(year == past_year & !is.na(dollars_per_day)) %>% 
     .$country

country_list_2 <- gapminder %>% 
     filter(year == present_year & !is.na(dollars_per_day)) %>% 
     .$country

country_list <- intersect(country_list_1, country_list_2)

# These 108 account for 86 % of the world population, so this subset should be
# representative.

# To see which specific regions improved the most, we can remake the boxplots we
# made above but now adding the year 2010:
     
p <- gapminder %>% 
     filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
     ggplot() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     xlab("") +
     scale_y_continuous(trans = "log2")  

# and then using facet to compare the two years:
     
p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
     facet_grid(year~.)

# Instead of faceting, we keep the data from each year together and ask ggplot2
# to color (or fill) them depending on the year. Note that groups are
# automatically separated by year and each pair of boxplots drawn next to each
# other. Finally, because year is a number, we turn it into a factor since
# ggplot2 automatically assigns a color to each category of a factor:
     
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

# Density plots

# We have used data exploration to discover that the income gap between rich and
# poor countries has narrowed considerably during the last 40 years. We used a
# series of histograms and boxplots to see this. Here we suggest a succinct way
# to convey this message with just one plot. We will use smooth density plots.

# Let’s start by noting that density plots for income distribution in 1970 and
# 2010 deliver the message that the gap is closing:
     
gapminder %>% 
     filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
     ggplot(aes(dollars_per_day)) +
     geom_density(fill = "grey") + 
     scale_x_continuous(trans = "log2") + 
     facet_grid(year~.)

# We first need to learn how to make these smooth densities in a way that
# preserves information on the number of countries in each group. To understand
# why we need this, note the discrepancy in the size of each group:
     
# Yet when we overlay two densities, the default is to have the area represented
# by each distribution add up to 1, regardless of the size of each group:
     
gapminder %>% 
     filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
     mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
     ggplot(aes(dollars_per_day, fill = group)) +
     scale_x_continuous(trans = "log2") +
     geom_density(alpha = 0.2) + 
     facet_grid(year ~ .)

# Accessing computed variables

# To have the areas of these densities be proportional to the size of the
# groups, we can simply multiply the y-axis values by the size of the group.
# From the geom_density help file, we see that the functions compute a variable
# called count that does exactly this. We want this variable to be on the y-axis
# rather than the density.

# In ggplot2, we access these variables by surrounding the name with two dots
# ... So we will use the following mapping:
     
aes(x = dollars_per_day, y = ..count..)

# We can now create the desired plot by simply changing the mapping in the
# previous code chunk:
     
p <- gapminder %>% 
     filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
     mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
     ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
     scale_x_continuous(trans = "log2")

p + geom_density(alpha = 0.2) + facet_grid(year ~ .)

# If we want the densities to be smoother, we use the bw argument. We tried a
# few and decided on 0.75:
     
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

# ‘case_when’

# We can actually make this figure somewhat more informative. From the
# exploratory data analysis, we noticed that many of the countries that most
# improved were from Asia. We can easily alter the plot to show key regions
# separately.

# We introduce the case_when function which is useful for defining groups. It
# currently does not have a data argument (this might change) so we need to
# access the components of our data table using the dot placeholder:
     
gapminder <- gapminder %>% 
     mutate(group = case_when(
          .$region %in% west ~ "West",
          .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
          .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
          .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
          TRUE ~ "Others"))

# We turn this group variable into a factor to control the order of the levels:
     
gapminder <- gapminder %>% 
     mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia","Sub-Saharan Africa", "West")))

# Now we can now easily plot the densities for each region. We use color and size to clearly see the tops:
     
p <- gapminder %>% 
     filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
     ggplot(aes(dollars_per_day, y = ..count.., fill = group, color = group)) +
     scale_x_continuous(trans = "log2")

p + geom_density(alpha = 0.2, bw = 0.75, size = 2) + facet_grid(year ~ .)

# The plot is cluttered and somewhat hard to read. A clearer picture is
# sometimes achieved by stacking the densities on top of each other:
     
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)

# As a final point, we note that these distributions weigh every country the
# same. So if most of the population is improving, but living in a very large
# country, such as China, we might not appreciate this. We can actually weight
# the smooth densities using the weight mapping argument.

# Ecological fallacy

# Throughout this section, we have been comparing regions of the world. We have
# seen that, on average, some regions do better than others. In this section, we
# focus on describing the importance of variability within the groups when
# examining the relationship between a country’s infant mortality rates and
# average income.

# We start by comparing these quantities across regions, but, before doing this,
# we define a few more regions:
     
gapminder <- gapminder %>% 
     mutate(group = case_when(
          .$region %in% west ~ "The West",
          .$region %in% "Northern Africa" ~ "Northern Africa",
          .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
          .$region == "Southern Asia"~ "Southern Asia",
          .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
          .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
          .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# We then compute these quantities for each region:
     
surv_income <- gapminder %>% 
     filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
     group_by(group) %>%
     summarize(income = sum(gdp)/sum(population)/365,
               infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population)) 

surv_income %>% arrange(income)
#> # A tibble: 7 x 3
#>   group              income infant_survival_rate
#>   <chr>               <dbl>                <dbl>
#> 1 Sub-Saharan Africa   1.76                0.936
#> 2 Southern Asia        2.07                0.952
#> 3 Pacific Islands      2.70                0.956
#> 4 Northern Africa      4.94                0.970
#> 5 Latin America       13.2                 0.983
#> 6 East Asia           13.4                 0.985
#> # ... with 1 more row

# This shows a dramatic difference. While in the West less than 0.5% of infants
# die, in Sub-Saharan Africa the rate is higher than 6%!

# In this plot, we introduce the use of the limit argument which lets us change
# the range of the axes. We are making the range larger than the data requires
# because we will later compare this plot to one with more variability and we
# want the ranges to be the same. We also introduce the breaks argument, which
# lets us set the location of the axes labels. Finally, we introduce a new
# transformation, the logistic transformation.

# Based on the plot above, do we conclude that a country with a low income is
# destined to have low survival rate? Do we conclude that all survival rates in
# Sub-Saharan Africa are all lower than in Southern Asia, which in turn are
# lower than in the Pacific Islands, and so on?
     
# Jumping to this conclusion based on a plot showing averages is referred to as
# the ecological fallacy. The almost perfect relationship between survival rates
# and income is only observed for the averages at the region level. Once we show
# all the data, we see a somewhat more complicated story.

# We see that there is a large amount of variability. We see that countries from
# the same regions can be quite different and that countries with the same
# income can have different survival rates. For example, while on average
# Sub-Saharan Africa had the worse health and economic outcomes, there is wide
# variability within that group. Mauritius and Botswana are doing better than
# Angola and Sierra Leone, with Mauritius comparable to Western countries.
