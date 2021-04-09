# Element 2: Objects (nouns) ----
# Anything that exists is an object

# Load packages ----
# This should already be loaded if you executed the commands in the previous file.
library(tidyverse)

# Vectors - 1-dimensional, homogeneous
# Everything in the values section
foo1
myNames

# 4 most common 
# "user-defined atomic vector types"
# Logical - TRUE/FALSE, T/F, 1/0 (Boolean)
# Integer - whole numbers
# Double - real numbers (float)
# Character - All values (string)

# Numeric - Generic reference to int or dbl

# check
typeof(foo1)
typeof(myNames)

foo3 <- c("Liver", "Brain", "Testes", "Muscle",
          "Intestine", "Heart")
typeof(foo3)

foo4 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
typeof(foo4)

# Homogenous types:
test <- c(1:10, "bob")
test
typeof(test)

# We can't do math:
mean(test)

# R has a type hierarchy

# Solution: Coercion to another type
# use an as.*() function
test <- as.numeric(test)
test

# Now we can do math: but deal with the NA
mean(test, na.rm = TRUE)

# Lists - 1-dimensional, heterogenous
typeof(plant_lm)

# how many elements:
length(plant_lm)
length(foo1) # also works for vectors

# attributes (meta data)
attributes(plant_lm)
# 13 named elements

# use common "accessor" functions for attributes
names(plant_lm)

# Anything that's named can be called with $
plant_lm$coefficients # a 3-element named dbl (numeric) vector
plant_lm$residuals # a 30-element dbl vector
plant_lm$model # dataframe

# Add comment as metadata:
comment(plant_lm) <- "I love R so much!"
attributes(plant_lm)

# Add comment as an actual list item:
plant_lm$myComment <- "But python not so much :/"
plant_lm$myComment

# What is class?
# An attribute to an object
attributes(plant_lm)
# can also access with "accessor" function:
class(plant_lm)
# class tells R functions what to do with this object
# e.g.
summary(plant_lm) # get t-test and ANOVA summary from an "lm"
summary(PlantGrowth) # summarise each column in a "dataframe"

# Dataframes - 2-dimensional, heterogeneous
class(PlantGrowth)
# A special class of type list...
typeof(PlantGrowth)
# ...where each element is a vector of the SAME length!
# Rows = observations
# Columns = variables

# Make a data frame from scratch:
foo_df <- data.frame(foo4, foo3, foo2)
foo_df

# update metadata (names)
names(foo_df) <- myNames
foo_df

# Call each variable by name:
foo_df$quantity # as a vector
foo_df$healthy
# Basic functions:
str(foo_df) # structure
glimpse(foo_df)
summary(foo_df)

# also...
dim(foo_df)
nrow(foo_df)
ncol(foo_df)

