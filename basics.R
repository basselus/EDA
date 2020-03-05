
# PART 1

a <- 2
b <- -3
sig_sq <- 0.5
x <- runif(40)
y <- a + b * x + rnorm(40, sd = sqrt(sig_sq))
avg_x <- mean(x)
write(avg_x, "avg_x.txt")
plot(x, y)
abline(a, b, col = "purple")

#first we will set the same seed, 1234
set.seed(1234)

# make a vector called y of 100 random numbers from a normal distribution
y=rnorm(100)

# find the 10th value in the vector y (do this using a command)
y[10]

# check the help to see what the mean of y should be (default value of the command you used)
help(rnorm) # default values are 0 and 1

# find the actual mean of the vector y
mean(y)

# make a vector called x of 100 integers using every other number (odd numbers). Hint: first = 1, last = 199
x = seq(from = 1, to = 100)

# make a scatter plot of y versus x
plot(x, y, main="scatterplot example", xlab = "x", ylab = "y")

# add an orange line at y = 0
abline() # to be corrected

# make a histogram of y
hist(y, col = "blue")


# PART 2

install.packages("gapminder")
library(gapminder)
library(tidyverse)

# Analyse dataset

# looking at dataset Gapminder

class(gapminder)
gapminder
head(gapminder)
tail(gapminder)
names(gapminder)
ncol(gapminder)
nrow(gapminder)
length(gapminder)
dim(gapminder)
summary(gapminder)

# Doing some plots

plot(lifeExp ~ year, gapminder)
plot(lifeExp ~ gdpPercap, gapminder)
plot(lifeExp ~ log(gdpPercap), gapminder)

# using data frame terminology - dollar signs for columns

head(gapminder$lifeExp)
summary(gapminder$lifeExp)
hist(gapminder$lifeExp, col = "green")
class(gapminder$continent)
summary(gapminder$continent)
levels(gapminder$continent)
nlevels(gapminder$continent)
table(gapminder$continent)
barplot(table(gapminder$continent))


# using ggplot
p <-ggplot(filter(gapminder, continent != "Oceania"),
           aes(x = gdpPercap, y = lifeExp))
p <- p + scale_x_log10()

# our background was set up as an object called p
p + geom_point()
p + geom_point(aes(color = continent))
p + geom_point(alpha = (1/3), size = 3) + geom_smooth(lwd = 3, se = FALSE)
p + geom_point(alpha = (1/3), size = 3) +
  facet_wrap(~ continent) +
  geom_smooth(lwd = 1.5, se = FALSE, colour = "orange")


# PART 3

# Load data
require(ggplot2)
file_name <- file.choose()
df1 <- read.csv(file_name, header = TRUE, stringsAsFactors = FALSE)
summary(df1$Mean)

# build plots
p1 <- ggplot(df1, aes(Slice,Mean)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "point", size=2, colour="orange") +
  geom_smooth() +
  labs(x = "Frame", y = "GFP")
p1
p2 <- ggplot(df1, aes(Slice,Area)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "point", size=2, colour="orange") +
  geom_smooth() +
  labs(x = "Frame", y = "Area (pixels)")
p2
p3 <- ggplot(df1, aes(Area,Mean)) +
  geom_point()
p3

# correct scaling
# 1 pixel is 0.069 * 0.069 um
pxSize <- 0.069 * 0.069
df1$Area_um2 <- df1$Area * pxSize
p4 <- ggplot(df1, aes(Area_um2,Mean)) +
  geom_point()
p4

# 5 sec per frame
df1$Time <- (df1$Slice - 1) * 5
p5 <- ggplot(df1, aes(Time,Mean)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "point", size=2, colour="orange") +
  geom_smooth() +
  labs(x = "Time (s)", y = "GFP")
p5

# count spots per frame
df2 <- as.data.frame(table(df1$Slice))

# note the column names
names(df2) <- c("Slice","Count")
df2$Time <- (df2$Slice - 1) * 5

# doesn't work - why?
str(df2)
df2$Slice <- as.integer(df2$Slice)
str(df2)
df2$Time <- (df2$Slice - 1) * 5
p6 <- ggplot(df2, aes(Time,Count)) +
  geom_point() +
  scale_y_continuous(limits = c(0,100)) +
  geom_smooth() +
  labs(x = "Time (s)", y = "Puncta")
p6


