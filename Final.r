library("hflights")

# Define one of these variables as the random variable X and the other as the 
# random variable Y. One of these variables must be skewed to the right (positively)

# X
dist <- hflights["Distance"]
dist <- dist$Distance
muX <- mean(dist)
# 787.7832
medX <- median(dist)
# 809
# Skewed left
sdX <- sd(dist,na.rm = TRUE)

# Y
aDel <- hflights["ArrDelay"]
aDel <- aDel$ArrDelay
muY <- mean(aDel,na.rm = TRUE)
# 7.094334
medY <- median(aDel,na.rm = TRUE)
# 0
# Skewed right
sdY <- sd(aDel,na.rm = TRUE)

joint <- hflights[c("Distance","ArrDelay")]

# Assume the small letter "x" is estimated as the 3d quartile of the X variable,
# and the small letter "y" is estimated as the 2d quartile of the Y variable
x <- quantile(dist)["75%"]
y <- quantile(aDel,na.rm = TRUE)["50%"]

total <- nrow(joint)
#Individual Counts
Xgtx <- length(dist[dist > x])
Xltx <- length(dist[dist <= x])
Ygty <- length(aDel[aDel > y])
Ylty <- length(aDel[aDel <= y])

pXgtx <- pnorm(x,muX,sdX,lower.tail = FALSE)
pXltx <- pnorm(x,muX,sdX,lower.tail = TRUE)
pYgty <- pnorm(y,muY,sdY,lower.tail = FALSE)
pYlty <- pnorm(y,muY,sdY,lower.tail = TRUE)

#Joint Counts
Xgtx_Ygty <- nrow(subset(joint,Distance > x & ArrDelay > y))
Xgtx_Ylty <- nrow(subset(joint,Distance > x & ArrDelay < y))
Xltx_Ygty <- nrow(subset(joint,Distance < x & ArrDelay > y))
Xltx_Ylty <- nrow(subset(joint,Distance < x & ArrDelay < y))

# _ = AND; G = GIVEN

pXgtx_Ygty <- Xgtx_Ygty / total
pXgtx_Ylty <- Xgtx_Ylty / total
pXltx_Ygty <- Xltx_Ygty / total
pXltx_Ylty <- Xltx_Ylty / total

pXgtxGYgty <- Xgtx_Ygty / Ygty
pXgtxGYlty <- Xgtx_Ylty / Ylty
pXltxGYgty <- Xltx_Ygty / Ygty
pXltxGYlty <- Xltx_Ylty / Ylty
pYgtyGXgtx <- Xgtx_Ygty / Xgtx
pYgtyGXltx <- Xgtx_Ylty / Xltx
pYltyGXgtx <- Xltx_Ygty / Xgtx
pYltyGXltx <- Xltx_Ylty / Xltx

# a. P(X>x | Y>y)
a <- (pYgtyGXgtx * pXgtx) / ((pYgtyGXgtx * pXgtx) + (pYgtyGXltx * pXltx))
# The probability of a random Distance measurement being above
# the third quartile of measured Distances, given that the Arrival Delay is 
# know to be above the second quartile for Arrival Delays, is 0.5388653

# b. P(X>x, Y>y)
b <- pXgtx * pYgtyGXgtx
# The probability of a random Distance measurement being above
# the third quartile of measured Distances, AND that a random Arrival Delay is 
# above the second quartile for Arrival Delays, is 0.1356907

# c. P(X<x | Y>y)
c <- (pYgtyGXltx * pXltx) / ((pYgtyGXltx * pXltx) + (pYgtyGXgtx * pXgtx))
# The probability of a random Distance measurement being below
# the third quartile of measured Distances, given that the Arrival Delay is 
# know to be above the second quartile for Arrival Delays, is 0.4611347

# d. P(X<x | Y>y)
d <- (pYgtyGXltx * pXltx) / ((pYgtyGXltx * pXltx) + (pYgtyGXgtx * pXgtx))
# The probability of a random Distance measurement being below
# the third quartile of measured Distances, given that the Arrival Delay is 
# know to be above the second quartile for Arrival Delays, is 0.4611347

#Joint Counts
Xgtx_Ygty <- nrow(subset(joint,Distance > x & ArrDelay > y))
Xgtx_Ylty <- nrow(subset(joint,Distance > x & ArrDelay < y))
Xltx_Ygty <- nrow(subset(joint,Distance < x & ArrDelay > y))
Xltx_Ylty <- nrow(subset(joint,Distance > x & ArrDelay > y))

# In addition, make a table of counts
countTab <- matrix(c(Xltx_Ylty
                     ,Xgtx_Ylty
                     ,Ylty
                     ,Xltx_Ygty
                     ,Xgtx_Ygty
                     ,Ygty
                     ,Xltx
                     ,Xgtx
                     ,total)
                   ,3,3)

#        [,1]   [,2]   [,3]
# [1,]  81518  79306 170773
# [2,]  27836  26760  56723
# [3,] 120576 110542 227496

# Does splitting the data in this fashion make them independent?

# Let A be the new variable 
# counting those observations above the 3d quartile for X, and let B be the new 
# variable counting those observations for the 2d quartile for Y.
A <- Xgtx
B <- Ygty
# Does P(A|B)=P(A)P(B)? Check mathematically, and then evaluate by running a Chi Square
test <- (pXgtxGYgty == (pXgtx * pYgty))
# mathematically, this stament is FALSE

library("MASS")
tbl <- table(joint)
chisq.test(tbl)
# As the p-value is very low, we reject the null hypothesis that the variables
# are independent of each other

# Provide univariate descriptive statistics and appropriate plots
summary(joint)
#     Distance         ArrDelay      
# Min.   :  79.0   Min.   :-70.000  
# 1st Qu.: 376.0   1st Qu.: -8.000  
# Median : 809.0   Median :  0.000  
# Mean   : 787.8   Mean   :  7.094  
# 3rd Qu.:1042.0   3rd Qu.: 11.000  
# Max.   :3904.0   Max.   :978.000  
#                  NA's   :3622  

hist(dist)
hist(aDel)
plot(density(dist))
plot(density(aDel))

# Provide a 95% CI for the difference in the mean of the variables
tCL <- qt(.975,total - 1)
lower <- muX - muY - (tCL * (sdX - sdY))
upper <- muX - muY + (tCL * (sdX - sdY))
# -48.32548 < (meanX - meanY) < 1609.703

# Derive a correlation matrix for two of the quantitative variables you selected.
corMatr <- cor(joint, use = "complete.obs")
#             Distance     ArrDelay
# Distance  1.000000000 -0.004434254
# ArrDelay -0.004434254  1.000000000

# Test the hypothesis that the correlation
# between these variables is 0 and provide a 99% confidence interval.
cor.test(dist,aDel,method = "pearson", conf.level = 0.99)
# true correlation is not equal to 0
# 99 percent confidence interval:
# -0.009877962 < r <  0.001009717

# Analysis:
# Given a random sample from the data set, I am 95% confident that the difference in 
# means will be between -48 and 1609. There is also sufficient evidence to reject the
# hypothesis that there is no correlation between the two variables, meaning they are
# probably somewhat related. There is a correlation of -0.004434254 between the two
# variables, indicating a weak negative correlation (as one goes up, the other goes 
# down). Given a random sampling, I would be 99% certain that the correlation would lie
# between -0.009877962 and 0.001009717

#  Invert your correlation matrix. (This is known as the precision matrix
# and contains variance inflation factors on the diagonal.)
precMatr <- solve(corMatr)
#             Distance    ArrDelay
# Distance 1.000019663 0.004434341
# ArrDelay 0.004434341 1.000019663

# Multiply the correlation matrix by the precision matrix
m1 <- corMatr %*% precMatr
#               Distance ArrDelay
# Distance  1.000000e+00        0
# ArrDelay -8.673617e-19        1

# and then multiply the precision matrix by the correlation matrix
m2 <- precMatr %*% corMatr
#              Distance ArrDelay
# Distance  1.000000e+00        0
# ArrDelay -8.673617e-19        1

# Analysis: The correlation matrix shows a pattern in the behavior of Distance and 
# Arrival Delay. When one goes up, the other goes down. However, this does not necessarily
# mean that they are directly related. There is always the possibility of a third variable
# having an effect on both of them, and they would therefore be otherwise independent
# of one another. The precision matrix would imply no direct connection if the values 
# were zero. I believe the positive value does imply that there is in fact a direct
# connection between flight distance and arrival delay, albeit a small one.

# For your variable that is skewed to the right, shift it so that the minimum 
# value is above zero
minY <- min(aDel,na.rm = TRUE)
aDelShift <- na.omit(aDel + 71.00)
# fit an exponential probability density function. Find the optimal value of lambda for
# this distribution, and then take 1000 samples from this exponential distribution 
# using this value. Plot a histogram and compare it with a histogram of your original
# variable. Plot a histogram and compare it with a histogram of your original variable.
expPDF <- fitdistr(aDelShift,"exponential")
lambda <- expPDF$estimate
PDFdata <- rexp(1000,lambda)
hist(PDFdata)
hist(aDel)

# Using the exponential pdf, find the 5th and 95th percentiles using the cumulative 
# distribution function (CDF)
p95 <- qnorm(0.95,expPDF$estimate,expPDF$sd)
p05 <- qnorm(0.05,expPDF$estimate,expPDF$sd)
  
  




