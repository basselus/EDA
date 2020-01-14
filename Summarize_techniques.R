
#***************** BASIC FUCNTIONS ***************************
#***** apply, lapply, sapply, tapply, by, sqldf, plyr ********
#*************************************************************

# Preload needed packages

packages = c("sqldf", "plyr")
package_check <- lapply(packages, FUN = function(x){
  
  if (!require(x, character.only=T)) {
    
    install.packages(x, dependencies = T)  
    library(x, character.only=T)
  }
  
})


#*************************************************************
#************************** APPLY ****************************
#*************************************************************

m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
m

  # apply the mean calculation to rows (margin=1)

apply(m,1,mean)  

  # apply the mean calculation to rows (margin=2)

apply(m,2,mean)



#*************************************************************
#************************** LAPPLY ****************************
#*************************************************************

# applies the mean method to each element in a list, return the same length of list 
# as the origional list

l <- list(a = 1:10, b = 11:20, c=21:30)
lapply(l, mean)
lapply(X = l, FUN = mean)
lapply(l, sum)



#*************************************************************
#************************** SAPPLY ***************************
#*************************************************************

# sapply() - does the same thing as lapply(), but returns a vector/matrix instead of a list
l <- list(a = 1:10, b = 11:20)
s <- sapply(l, mean)


#*************************************************************
#************************** TAPPLY ***************************
#*************************************************************

# tapply is useful when we want to apply a function within groups of observations or specific subsets of a dataframe

# Generating dummy dataset

medical.data <-
  data.frame(patient = 1:100,
             age = rnorm(100, mean = 60, sd = 12),
             type = gl(2, 50,
                            labels = c("Treatment", "Control")))

# Generic example => tapply (summary variable, Group variable, function)
# 1st example : Conditional distribution of age mean by treatment type
attach(medical.data)
tapply(age, type, mean)

# 2nd example (iris data)
attach(iris)
summary(iris)
head(iris)
tapply(iris$Petal.Length, Species, mean)



#*************************************************************
#************************** BY *******************************
#*************************************************************

# by() - an object-oriented wrapper for tapply applied to data frames
# Generic example => by(data, data$byvar, function)
by(iris[,1:4], Species, colSums)
by(iris$Sepal.Length, list(iris$Species), mean)

#*************************************************************
#************************** SQLDF ****************************
#*************************************************************


# this package helps use the same mecahnism through sql query
library(sqldf)
summarization <- sqldf('select Species, avg("Petal.Length") `Petal.Length_mean` from iris where Species is not null group by Species')
summarization


#*************************************************************
#************************** DDPLY ****************************
#*************************************************************

# ddply() - same as sqldf, tapply and by functions
library(plyr)
ddply(iris, "Species", summarise, Petal.Length_mean = mean(Petal.Length))
