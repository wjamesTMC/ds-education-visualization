# --------------------------------------------------------------------------------
#
# Visualization Principles (Misc Code Bits)
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

p1 <- gapminder %>% filter(year == 2012) %>%
     ggplot(aes(continent, life_expectancy)) +
     geom_point()
p2 <- p1 +
     scale_y_continuous(limits = c(0, 84))
grid.arrange(p2, p1, ncol = 2)

data(murders)
p1 <- murders %>% mutate(murder_rate = total / population * 100000) %>%
     ggplot(aes(state, murder_rate)) +
     geom_bar(stat="identity") +
     coord_flip() +
     xlab("")

p2 <- murders %>% mutate(murder_rate = total / population * 100000) %>%
     mutate(state = reorder(state, murder_rate)) %>%
     ggplot(aes(state, murder_rate)) +
     geom_bar(stat="identity") +
     coord_flip() +
     xlab("")
grid.arrange(p1, p2, ncol = 2)

heights %>% 
     ggplot(aes(sex, height)) + 
     geom_point() 

heights %>% 
     ggplot(aes(sex, height)) +
     geom_jitter(width = 0.1, alpha = 0.2) 

p2 <- heights %>% 
     ggplot(aes(height, ..density..)) +
     geom_histogram(binwidth = 1, color="black") +
     facet_grid(sex~.)
p2

p3 <- heights %>% 
     ggplot(aes(sex, height)) + 
     geom_boxplot(coef=3) + 
     geom_jitter(width = 0.1, alpha = 0.2) +
     ylab("Height in inches")
p3

grid.arrange(p1, p2, p3, ncol = 3)

color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p1 <- data.frame(x=1:8, y=1:8, col = as.character(1:8)) %>% ggplot(aes(x, y, color = col)) + geom_point(size=5)
p1 + scale_color_manual(values=color_blind_friendly_cols)

west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")

dat <- gapminder %>% 
     filter(year%in% c(2010, 2015) & region %in% west & 
                 !is.na(life_expectancy) & population > 10^7) 

dat %>%
     mutate(location = ifelse(year == 2010, 1, 2), 
            location = ifelse(year == 2015 & country %in% c("United Kingdom","Portugal"), location+0.22, location),
            hjust = ifelse(year == 2010, 1, 0)) %>%
     mutate(year = as.factor(year)) %>%
     ggplot(aes(year, life_expectancy, group = country)) +
     geom_line(aes(color = country), show.legend = FALSE) +
     geom_text(aes(x = location, label = country, hjust = hjust), 
               show.legend = FALSE) +
     xlab("") + ylab("Life Expectancy")

library(ggrepel)
dat %>% 
     mutate(year = paste0("life_expectancy_", year)) %>%
     select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>% 
     mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
            difference = life_expectancy_2015 - life_expectancy_2010) %>%
     ggplot(aes(average, difference, label = country)) + 
     geom_point() +
     geom_text_repel() +
     geom_abline(lty = 2) +
     xlab("Average of 2010 and 2015") + ylab("Difference between 2015 and 2010")

library(RColorBrewer)
display.brewer.all(type="seq")

library(RColorBrewer)
display.brewer.all(type="div")

# --------------------------------------------------------------------------------
# The section below is from chapter 24
# --------------------------------------------------------------------------------

data(us_contagious_diseases)
str(us_contagious_diseases)
#> 'data.frame':    18870 obs. of  6 variables:
#>  $ disease        : Factor w/ 7 levels "Hepatitis A",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ state          : Factor w/ 51 levels "Alabama","Alaska",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ year           : num  1966 1967 1968 1969 1970 ...
#>  $ weeks_reporting: int  50 49 52 49 51 51 45 45 45 46 ...
#>  $ count          : num  321 291 314 380 413 378 342 467 244 286 ...
#>  $ population     : num  3345787 3364130 3386068 3412450 3444165 ...

the_disease <- "Measles"
dat <- us_contagious_diseases %>%
     filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
     mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% 
     mutate(state = reorder(state, rate)) 

# We can now easily plot disease rates per year. Here are the measles data from California:

dat %>% ggplot(aes(year, state,  fill = rate)) +
     geom_tile(color = "grey50") +
     scale_x_continuous(expand=c(0,0)) +
     scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
     geom_vline(xintercept=1963, col = "blue") +
     theme_minimal() +  theme(panel.grid = element_blank()) +
     ggtitle(the_disease) + 
     ylab("") + 
     xlab("")

avg <- us_contagious_diseases %>%
     filter(disease==the_disease) %>% group_by(year) %>%
     summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

# Now to make the plot we simply use the geom_line geometry:
     
dat %>% ggplot() +
     geom_line(aes(year, rate, group = state),  color = "grey50", 
               show.legend = FALSE, alpha = 0.2, size = 1) +
     geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
     scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
     ggtitle("Cases per 10,000 by state") + 
     xlab("") + 
     ylab("") +
     geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
     geom_vline(xintercept=1963, col = "blue") 
#> Warning: Removed 180 rows containing missing values (geom_path).


