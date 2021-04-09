# SILAC Analysis
# Protein profiles during myocardial cell differentiation

# Load packages ----
library(tidyverse)
library(glue)

# Part 0: Import data ----
protein_df <- read.delim("data/Protein.txt")
# protein_tbl <- read_tsv("data/Protein.txt") # Make a tibble from the start
# Examine the data:
glimpse(protein_df)
# glimpse(protein_tbl)
# str(protein_df)

# tibbles and classic data frames print differently to the screen
protein_df %>% 
  as_tibble()

# protein_tbl

# Quantify the contaminants ----
sum(protein_df$Contaminant == "+") # 16

# Also works:
protein_df %>% 
  count(Contaminant)

# Also: not as nice
# protein_cont <- protein_df %>% 
#   filter(Contaminant != "") 
# dim(protein_cont) # nrow X ncol
# nrow(protein_cont)

# results in a DF (easier to work with)
protein_df %>% 
  count(Contaminant)

# results in a named vector or a "table" (more tricky to work with)
table(protein_df$Contaminant)
table(mtcars$cyl, mtcars$am)

# Proportion of contaminants
# wrong: here we're taking the *ratio* not the proportion
# sum(protein_df$Contaminant == "+")/sum(protein_df$Contaminant != "+")

# What about this? can't take the sum of a character vector
# sum(protein_df$Contaminant == "+")/sum(protein_df$Contaminant)

# both of these work:
sum(protein_df$Contaminant == "+")/length(protein_df$Contaminant)
sum(protein_df$Contaminant == "+")/nrow(protein_df)

# Percentage of contaminants (just multiply proportion by 100)

# Transformations & cleaning data ----

# Remove contaminants ====
# literally: filter for all "non-contaminants"
protein_df <- protein_df %>% 
  filter(Contaminant != "+")

# log 10 transformations of the intensities ====
protein_df$Intensity.H <- log10(protein_df$Intensity.H)
protein_df$Intensity.M <- log10(protein_df$Intensity.M)
protein_df$Intensity.L <- log10(protein_df$Intensity.L)


# Add the intensities ====
# protein_df$Intensity.H.M <- sum()
protein_df$Intensity.H.M <- protein_df$Intensity.H + protein_df$Intensity.M
protein_df$Intensity.M.L <- protein_df$Intensity.M + protein_df$Intensity.L

# log2 transformations of the ratios ====


# Part 2: Query data using filter() ----
# Exercise 9.2 (Find protein values) ====

# Either:
# 1 - Add "_MOUSE" to each query
# 2 - remove "_MOUSE" from the search space (use functions from stringr)
# 3 - Use fuzzy matching (regular expressions)

query <- c("GOGA7", "PSA6", "S10AB")
# Method 1:
query_2 <- paste0(query, "_MOUSE")
glue("{query}_MOUSE")

# %in% is the equivalent of asking many == with |
protein_df %>% 
  filter(Uniprot %in% query_2)

# Why doesn't this work? This does "pure" vector recycling
protein_df %>%
  filter(Uniprot == query_2)

# Method 2:
protein_df <- protein_df %>% 
  mutate(Uniprot_clean = str_remove_all(Uniprot, "_MOUSE"))

protein_df %>% 
  filter(Uniprot_clean %in% query)

# Exercise 9.3 (Find significant hits) and 10.2 ====
# For the H/M ratio column, create a new data 
# frame containing only proteins that have 
# a p-value less than 0.05


# Exercise 10.4 (Find top 20 values) ==== 


# Exercise 10.5 (Find intersections) ====

