# --------------------------------------------------------------------------------
#
# <title>
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)

# The first step in creating a ggplot2 graph is to define a ggplot object. We do
# this with the function ggplot which initializes the graph. If we read the help
# file for this function, we see that the first argument is used to specify what
# data is associated with this object:
     
     ggplot(data = murders)

# We can also pipe the data. So this line of code is equivalent to the one above:
     
     murders %>% ggplot()

# What has happened above is that the object was created and because it was not
# assigned, it was automatically evaluated. But we can define an object, for
# example, like this:
     
p <- ggplot(data = murders)
class(p)
#> [1] "gg"     "ggplot"

# To render the plot associated with this object, we simply print the object p.
# The following two lines of code produce the same plot we see above:
     
print(p)
p

# In ggplot we create graphs by adding layers. Layers can define geometries,
# compute summary statistics, define what scales to use, or even change styles.
# To add layers, we use the the symbol +. In general a line of code will look
# like this:

#         DATA %>% ggplot() + LAYER 1 + LAYER 2 + … + LAYER N

# Usually, the first added layer defines the geometry. We want to make a
# scatterplot. So what geometry do we use?

# Taking a quick look at the cheat sheet, we see that the function used to
# create plots with this geometry is geom_point.

# For geom_point to know what to do, we need to provide data and a mapping. We
# have already connected the object p with the murders data table and, if we add
# as a layer geom_point, we will default to using this data. To find out what
# mappings are expected, we read the Aesthetics section of the help file
# geom_point help file.

# aes will be one of the functions you will most use. This function connects
# data with what we see on the graph. We refer to this connect as the aesthetic
# mappings. The outcome of this function is often used as the argument of a
# geometry function. This example produces a scatterplot of total murders versus
# population in millions:

murders %>% ggplot() + 
geom_point(aes(x = population/10^6, y = total))

# We can drop the x = and y = if we wanted to since these are the first and
# second expected arguments, as seen in the help page.

# We can also add a layer to the p object that has defined above as p <-
# ggplot(data = murders):
     
p + geom_point(aes(population/10^6, total))

# A second layer in the plot we wish to make involves adding a label to each
# point to identify the state. The geom_label and geom_text functions permit us
# to add text to the plot, without and with a rectangle behind the text
# respectively.

# Because each state (each point) has a label, we need an aesthetic mapping to
# make the connection. By reading the help file, we learn that we supply the
# mapping between point and label through the labelargument of aes. So the code
# looks like this:

p + geom_point(aes(population/10^6, total)) +
     geom_text(aes(population/10^6, total, label = abb))

# Each geometry function has many arguments other than aes and data. They tend
# to be specific to the function. For example, in the plot we wish to make, the
# points are larger than the default ones. In the help file we see that size is
# an aesthetic and we can change it like this:

p + geom_point(aes(population/10^6, total), size = 3) +
     geom_text(aes(population/10^6, total, label = abb))

# size is not a mapping, it affects all the points so we do not need to include
# it inside aes.

# Now that the points are larger, it is hard to see the labels. If we read the
# help file for geom_text, we see the nudge_x argument, which moves the text
# slightly to the right:
     
p + geom_point(aes(population/10^6, total), size = 3) +
     geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

# This is preferred as it makes it easier to read the text.

# --------------------------------------------------------------------------------
# Global versus local aesthetic mappings
# --------------------------------------------------------------------------------

# In the previous line of code, we define the mapping aes(population/10^6,
# total) twice, once in each geometry. We can avoid this by using a global
# aesthetic mapping. We can do this when we define the blank slate ggplot
# object. Remember that the function ggplot contains an argument that permits us
# to define aesthetic mappings:
     
args(ggplot)
#> function (data = NULL, mapping = aes(), ..., environment = parent.frame()) 
#> NULL

# If we define a mapping in ggplot, then all the geometries that are added as
# layers will default to this mapping. We redefine p:
     
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

# and then we can simply use code as follows:
     
p + geom_point(size = 3) + 
geom_text(nudge_x = 1.5)

# We keep the size and nudge_x argument in geom_point and geom_text respectively
# because we want to only increase the size of points and only nudge the labels.
# Also note that the geom_pointfunction does not need a label argument and
# therefore ignores it.

# If necessary, we can override the global mapping by defining a new mapping
# within each layer. These local definitions override the global. Here is an
# example:
     
p + geom_point(size = 3) +  
     geom_text(aes(x = 10, y = 800, label = "Hello there!"))

# Scales
# First, our desired scales are in log-scale. This is not the default so this
# change needs to be added through a scales layer. A quick look at the cheat
# sheet reveals the scale_x_continuous lets us control the behavior of scales.
# We use them like this

p + geom_point(size = 3) +  
geom_text(nudge_x = 0.05) + 
scale_x_continuous(trans = "log10") +
scale_y_continuous(trans = "log10")

# Because we are in the log-scale now, the nudge must be made smaller.
# This particular transformation is so common that ggplot2 provides specialized
# functions:

p + geom_point(size = 3) +  
geom_text(nudge_x = 0.05) + 
scale_x_log10() +
scale_y_log10()

# Labels and titles
# Similarly, the cheat sheet quickly reveals that to change labels and add a
# title, we use the following functions:

p + geom_point(size = 3) +  
geom_text(nudge_x = 0.05) + 
scale_x_log10() +
scale_y_log10() +
xlab("Populations in millions (log scale)") + 
ylab("Total number of murders (log scale)") +
ggtitle("US Gun Murders in 2010")

# We are almost there! All we have left to do is add color, a legend and
# optional changes to the style.

# Categories as colors
# We can change the color of the points using the col argument in the geom_point
# function. To facilitate exposition, we will redefine p to be everything except
# the points layer:
     
p <-  murders %>% ggplot(aes(population/10^6, total, label = abb)) +   
geom_text(nudge_x = 0.05) + 
scale_x_log10() +
scale_y_log10() +
xlab("Populations in millions (log scale)") + 
ylab("Total number of murders (log scale)") +
ggtitle("US Gun Murders in 2010")

# and then test out what happens by adding different calls to geom_point. We can
# make all the points blue by adding the color argument:
     
p + geom_point(size = 3, color ="blue")

# This, of course, is not what we want. We want to assign color depending on the
# geographical region. A nice default behavior of ggplot2 is that if we assign a
# categorical variable to color, it automatically assigns a different color to
# each category. It also adds a legend!
     
# To map each point to a color, we need to use aes since this is a mapping. We
# use the following code:
     
p + geom_point(aes(col=region), size = 3)

# The x and y mappings are inherited from those already defined in p. So we do
# not redefine them. We also move aes to the first argument since that is where
# the mappings are expected in this call.

# Here we see yet another useful default behavior: ggplot2 has automatically
# added a legend that maps color to region.

# Annotation and shapes

# We often want to add shapes or annotation to figures that are not derived
# directly from the aesthetic mapping. Examples in include labels, boxes, shaded
# areas and lines. Here we want to add a line that represents the average murder
# rate for the entire country. 

# Once we determine the per million rate to be rr, this line is defined by the
# formula: y=rxy=rx, with yy and xx our axes: total murders and population in
# millions respectively. In the log-scale this line turns into:
# log(y)=log(r)+log(x)loga(y)=loga(r)+loga(x). So in our plot it’s a line with
# slope 1 and intercept log(r)loga(r). To compute this value we use what we our
# dplyr skills:
     
r <- murders %>% 
     summarize(rate = sum(total) /  sum(population) * 10^6) %>% .$rate
     # To add a line we use the geom_abline function. ggplot2 uses ab in the name to remind us we are supplying the intercept (a) and slope (b). The default line has slope 1 and intercept 0 so we only have to define the intercept:
     p + geom_point(aes(col=region), size = 3) + 
     geom_abline(intercept = log10(r))

# Here geom_abline does not use any information from the data object.
     
# We can change the line type and color of the lines using arguments. Also, we
# draw it first so it doesn’t go over our points.

p <- p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
     geom_point(aes(col=region), size = 3) 

# Note that we redefined p.
