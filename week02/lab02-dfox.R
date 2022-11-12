### q1

## To find the dissimilarity between nominal attributes, calculate the ratio of the
## total number of mismatches between two data points to the total number of attributes

## To find the dissimilarity between asymmetric binary attributes, calculate the ratio 
## of the total number of mismatch values between two data points to the total number 
## of attributes that are not both negative.

## To find the dissimilarity between numeric attributes, take the absolute difference between
## the corresponding attributes and sum them together.

## To find the dissimilarity between term-frequency vectors, take the dot product
## of two vectors divided by the product of the length of two vectors.

### q2
o1 <- c(22, 1, 42, 10)
o2 <- c(20, 0, 36, 8)

## euclidean
euclidean <- function(a, b) {
  sqrt(sum((a-b)^2))
}

euclidean(o1, o2) # 6.71

## manhattan
manhattan <- function(a, b) {
  c <- abs(a-b)
  c <- sum(c)
  return(c)
}

manhattan(o1, o2) # 11

## minkowski
dist(rbind(o1, o2), method="minkowski", p=3) # 6.15

## supremum
dist(rbind(o1, o2), method="maximum") # 6

### q3
X <- c(1.5, 2, 1.6, 1.2, 1.5)
Y <- c(1.7, 1.9, 1.8, 1.5, 1.0)
df <- data.frame(X, Y)

## 1
dist(df) # euclidean

dist(df, method="manhattan") # manhattan

dist(df, method="maximum") # supremum

library(lsa)
cosine(X, Y) # cosine

## 2
scale(df)
dist(scale(df))
dist(df)

## 3

# Yes this made a difference. I think this is because the normalized data is returning
# squared differences that are smaller than 1. Therefore, when when the square root is
# taken, the values becomes larger. Hence, we see larger values with the normalized data.