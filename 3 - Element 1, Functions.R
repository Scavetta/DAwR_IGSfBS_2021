# Element 1: Functions (i.e. "verbs") ----
# Everything that happens, is because of a function

# Arithmetic operators
34 + 6

# BEDMAS - order of operations
# brackets, expon, div, mult, add, sub
2 - 3/4 # 1.25
(2 - 3)/4 # -0.25

# Use objects in place of numbers
n <- 34
p <- 6
# so...
n + p

# Generic functions have a form:
# fun_name(fun_arg = ...)

# Call args by name or position

# i.e.
log(x = 8, base = 2) # long form, naming
log(8, 2) # long form, positional matching
log(8, base = 2) # common: mix naming and position
log2(8) # short form, positional matching
log10(1000)
log(base = 2, 8) # works, but confusing!

# Funs can have 0 to many un-named args
ls()
# Args can be named or un-named

# e.g. combine
xx <- c(3, 8, 9, 23)
xx

myNames <- c("healthy", "tissue", "quantity")
myNames

# How is + a function?
p + n
# this is actually...
`+`(p, n)

# A regular sequence of numbers
seq(from = 1, to = 100, by = 7)
# typically:
foo1 <- seq(1, 100, 7)
foo1

# use objects in functions:
foo2 <- seq(1, n, p)
foo2

# regular sequence of 1 interval
seq(1, 10, 1)
# Use the colon operator instead:
1:10

# Two major types of math functions:
# Aggregation functions
# 1 output value (typically)
# mean, sd, n, var, median, max, min, sum, prod

# Transformation functions
# same number of output as input
# log, [0,1], z-scores, sqrt, exponents
# subtract background
# +, -, /, ...

# Exercise: Are these transformation or aggregation?
foo2 + 100 # Trans 
foo2 + foo2 # Trans
sum(foo2) + foo2 # Agg, then Trans
1:3 + foo2 # Trans
sum(foo2, foo2)
# FUNDAMENTAL CONCEPT: VECTOR RECYCLING

1:4 + foo2

# 3 types of messages:
# Information: Neutral
# Warning: Possible problem
# Error: Full stop

# Exercise: Calculate y = 1.12x − 0.4 for xx
# i.e. y = mx + b
