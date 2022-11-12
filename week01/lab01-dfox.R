source("http://www.openintro.org/stat/data/present.R")
d <- present

## q1
dim(d)
head(d)

# there are 3 variables

## q2
d$year

# years 1940 => 2002

## q3
d$girls

# use d%girls

## q4
plot(x = present$year, y = present$girls, type = "l")

# There is initially an increase in the number of girls born, which peaks around 1960. After 1960 there is a decrease in the number of girls born, but the number begins to increase again in the early 1970s. Overall the trend is an increase in the number of girls born in the US since the 1940s.

## q5
plot(present$year, present$boys + present$girls, type = "l")
d$total <- d$boys + d$girls
d$total
d

# 1961

## q6
plot(d$year, d$boys / d$total, type="l")

# true

## q7
d$boys > d$girls

# Every year there are more girls born than boys.

## q8
plot(d$year, d$boys / d$girls, type = "l")

# There is initially a decrease in the boy-to-girl ratio, and then an increase between 1960 and 1970, followed by a decrease.

## q9
max(abs(d$boys - d$girls))
d$diff <- abs(d$boys - d$girls)
d$diff
d

# 1963