# exploratory data analysis from r for data science by Hadley Wickham

library(tidyverse)
# source("/Users/Whitmore/Library/Mobile Documents/com~apple~CloudDocs/Documents/Philippines/Phil_code/conleyte.R")
source("~/Documents/Philippines/Phil_code/conleyte.R")
leyte <- conleyte()

# visualize variation in your variables ####

# for categorical data use a bar chart
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# try with clownfish data - use tail color

leyte %>% tbl("clownfish", na.rm = T) %>% filter(!is.na(col)) %>% collect() %>% # the na.rm isn't working
  ggplot(mapping = aes(x = col)) +
  geom_bar()

# species
leyte %>% tbl("clownfish") %>% filter(!is.na(Spp)) %>% collect() %>% 
  ggplot(mapping = aes(x = Spp)) +
  geom_bar()

# found some errors, lets take a look at other fields
#anem depth
leyte %>% tbl("anemones") %>% filter(!is.na(AnemSpp), (!is.na(Depth_m))) %>% collect() %>% 
  ggplot(mapping = aes(x = as.numeric(Depth_m))) +
  geom_histogram(binwidth = 0.5) # histogram because it is continuous data
leyte %>% tbl("anemones") %>% filter(!is.na(AnemSpp), (!is.na(Depth_ft))) %>% collect() %>% 
  ggplot(mapping = aes(x = as.numeric(Depth_ft))) +
  geom_histogram(binwidth = 0.5) # histogram because it is continuous data

# need to bin this to see average depth of dive
leyte %>% 
  tbl("anemones") %>% 
  filter(!is.na(AnemSpp), (!is.na(Depth_ft))) %>%
  count(Depth_ft) %>% 
  collect() %>% 
  ggplot(mapping = aes(x = Depth_ft, y = n)) +
  geom_boxplot()

# anem num fish
leyte %>% tbl("anemones") %>% filter(!is.na(NumFish)) %>% collect() %>% 
  ggplot(mapping = aes(x = NumFish)) +
  geom_histogram(binwidth = 0.5)

# anem num fish - take a look at outliers
leyte %>% tbl("anemones") %>% filter(!is.na(NumFish), NumFish > 5) %>% collect() %>% 
  ggplot(mapping = aes(x = NumFish)) +
  geom_histogram(binwidth = 0.5)

# anem species
leyte %>% tbl("anemones") %>% filter(!is.na(AnemSpp)) %>% collect() %>% 
  ggplot(mapping = aes(x = AnemSpp)) +
  geom_bar()

# dominant species on anem
leyte %>% tbl("anemones") %>% filter(!is.na(Spp)) %>% collect() %>% 
  ggplot(mapping = aes(x = Spp)) +
  geom_bar()

leyte %>% tbl("diveinfo") %>% collect() %>% 
  ggplot(mapping = aes(x = name)) +
  geom_bar() +
  coord_flip()

# morning start time
leyte %>% tbl("diveinfo") %>% filter(as.Date(StartTime < 12:00:00), !is.na(StartTime)) %>% collect() %>% 
  ggplot(mapping = aes(x = StartTime)) +
  geom_histogram(binwidth = 0.5)


# compute how many observances
diamonds %>% 
  count(cut)

leyte %>% tbl("clownfish", na.rm = T) %>% filter(!is.na(col)) %>% count(col) %>% collect() # the na.rm isn't working

# for continuous data use a histogram
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# try with clownfish data - use size
leyte %>% tbl("clownfish") %>% filter(!is.na(size), Spp == "APCL") %>% collect() %>%
  ggplot(mapping = aes(x = size)) +
           geom_histogram(binwidth = 0.5)

# what about fish we clipped?
leyte %>% tbl("clownfish") %>% filter(!is.na(size), !is.na(ID)) %>% collect() %>%
  ggplot(mapping = aes(x = size)) +
  geom_histogram(binwidth = 0.5) +
  ggtitle("Distribution of size of clownfish from which DNA was taken")

# count
leyte %>% tbl("clownfish") %>% filter(!is.na(size), !is.na(ID)) %>% collect() %>% count(cut_width(size, 0.5))
  
  
# zoom in on data:
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)


# for clownfish
larger <- leyte %>% tbl("clownfish") %>% filter(!is.na(size), !is.na(ID), size > 8) %>% collect()
  
ggplot(data = larger, mapping = aes(x = size)) +
  geom_histogram(binwidth = 0.1)

# overlap many histograms as lines ####
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = larger, mapping = aes(x = size, colour = col)) +
  geom_freqpoly(binwidth = 0.1, na.rm = TRUE)
ggplot(data = larger, mapping = aes(x = size, colour = col)) +
  geom_freqpoly(binwidth = 0.1) # no difference with na.rm

# outliers ####
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# zoom to small data
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# take a look at the unusual data in the table
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  arrange(y)
unusual

# Exercises ####
# 
# Explore the distribution of each of the x, y, and z variables in diamonds. What do you learn? Think about a diamond and how you might decide which dimension is the length, width, and depth.
# 
# Explore the distribution of price. Do you discover anything unusual or surprising? (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)
# 
# How many diamonds are 0.99 carat? How many are 1 carat? What do you think is the cause of the difference?
# 
# Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming in on a histogram. What happens if you leave binwidth unset? What happens if you try and zoom so only half a bar shows?

# Missing Values ####

# you can either drop the row from your data set (not recommended)
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# or replace the values with using mutate
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y)) # if the values don't fit your range, change them to NA
# literally if y is less than 3 or greater than 20, change to NA, else keep y - ifelse(test, yes, no)

# ggplot warns when values go missing (or not, this didn't generate a message on Whitmore)
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

# to suppress warning:
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

# compare missing values with present values ####
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

# Exercises ####

# What happens to missing values in a histogram? What happens to missing values in a bar chart? Why is there a difference?

# What does na.rm = TRUE do in mean() and sum()?

# Covariation ####

# For example, let’s explore how the price of a diamond varies with its quality:
  
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# It’s hard to see the difference in distribution because the overall counts differ so much:
ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

# we’ll display density, which is the count standardised so that the area under each frequency polygon is one.
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# boxplot ####
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# try with size and tail color
leyte %>% tbl("clownfish") %>% filter(!is.na(size), !is.na(ID), !is.na(col)) %>% collect() %>% 
  ggplot(mapping = aes(x = col, y = size)) +
  geom_boxplot()

# reorder the categorical data ####
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

# reorder class based on the median value of hwy
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

leyte %>% tbl("clownfish") %>% filter(!is.na(size), !is.na(ID), !is.na(col)) %>% collect() %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = reorder(col, size, FUN = median), y = size), varwidth = TRUE)

# might want to flip x and y axes if you have long variable names
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

# Exercises ####

# Use what you’ve learned to improve the visualisation of the departure times of cancelled vs. non-cancelled flights.
# 
# What variable in the diamonds dataset is most important for predicting the price of a diamond? How is that variable correlated with cut? Why does the combination of those two relationships lead to lower quality diamonds being more expensive?
# 
# Install the ggstance package, and create a horizontal boxplot. How does this compare to using coord_flip()?
# 
# One problem with boxplots is that they were developed in an era of much smaller datasets and tend to display a prohibitively large number of “outlying values”. One approach to remedy this problem is the letter value plot. Install the lvplot package, and try using geom_lv() to display the distribution of price vs cut. What do you learn? How do you interpret the plots?
# 
# Compare and contrast geom_violin() with a facetted geom_histogram(), or a coloured geom_freqpoly(). What are the pros and cons of each method?
# 
# If you have a small dataset, it’s sometimes useful to use geom_jitter() to see the relationship between a continuous and categorical variable. The ggbeeswarm package provides a number of methods similar to geom_jitter(). List them and briefly describe what each one does.

# covariation in two categorical variables ####
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# Exercises ####
# 
# How could you rescale the count dataset above to more clearly show the distribution of cut within colour, or colour within cut?
# 
# Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read? How could you improve it?
# 
# Why is it slightly better to use aes(x = color, y = cut) rather than aes(x = cut, y = color) in the example above?

# two continuous variables
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# add transparency
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

# bin for large data sets
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

# Another option is to bin one continuous variable so it acts like a categorical variable. Then you can use one of the techniques for visualising the combination of a categorical and a continuous variable that you learned about. For example, you could bin carat and then for each group, display a boxplot:
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# change the width of a boxplot ####
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# Exercises ####
# 
# Instead of summarising the conditional distribution with a boxplot, you could use a frequency polygon. What do you need to consider when using cut_width() vs cut_number()? How does that impact a visualisation of the 2d distribution of carat and price?
# 
# Visualise the distribution of carat, partitioned by price.
# 
# How does the price distribution of very large diamonds compare to small diamonds. Is it as you expect, or does it surprise you?
# 
# Combine two of the techniques you’ve learned to visualise the combined distribution of cut, carat, and price.
# 
# Two dimensional plots reveal outliers that are not visible in one dimensional plots. For example, some points in the plot below have an unusual combination of x and y values, which makes the points outliers even though their x and y values appear normal when examined separately.
# 
# ggplot(data = diamonds) +
#   geom_point(mapping = aes(x = x, y = y)) +
#   coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
# 
# 
# Why is a scatterplot a better display than a binned plot for this case?

# Patterns and models ####

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

# remove the very strong relationship between price and carat so we can explore the subtleties that remain, The residuals give us a view of the price of the diamond, once the effect of carat has been removed.
library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds) # model is the linear model of log of price and log of carats

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid)) # a plot of how much carat is affecting price?

# Once you’ve removed the strong relationship between carat and price, you can see what you expect in the relationship between cut and price: relative to their size, better quality diamonds are more expensive.

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid)) # better cut diamonds are slightly more expensive

# ggplot2 calls ####
# explicit code
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# concise code
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# piped concise
diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()








