# Intro to R ----
# Rick Scavetta

# Always begin your R scripts with a descriptive header

# Remove objects from Environment ----
# (not widely recommended, but it's still useful)
rm(list = ls())

# Load packages ----
# Install once, but initialize each time you start a new session
library(tidyverse)

# Basic R syntax ----
n <- log2(8) # the log2 of 8
n # shortcut to print(n)

# Exercise 1 ----
# Can you identify all the parts of the above commands?

# Exercise 2 ----
# Incomplete commands :/
# What happens if you execute this command?
n <- log2(8)
          
          