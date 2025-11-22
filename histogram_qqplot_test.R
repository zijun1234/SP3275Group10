setwd('~/Desktop/SP3275')
library(readxl)

library(ggplot2)

library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(ROCR)
library('arules') 
library('arulesViz')
data <- read_excel("clusterbybiomass.xlsx")

x = data$right_behind
y = data$right_between
var.test(x, y)






# Extract numeric vector
x <- data$left_behind
x <- x[ !is.na(x) ]  
# Histogram with density estimate
hist(x, freq = FALSE, 
     main = "Histogram for left behind",
     xlab = "Shoot density", col = "lightblue", border = "white")

# Add kernel density estimate
lines(density(x), col = "blue", lwd = 2)

# Add normal curve with same mean & sd
curve(dnorm(x, mean(x), sd(x)), 
      col = "red", lwd = 2, add = TRUE)

legend("topright",
       legend = c("Density", "Normal"),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")

qqnorm(x, main = "Q-Q plot for left behind")
qqline(x, col = "red", lwd = 2)


shapiro.test(x)









# Extract numeric vector
x <- data$left_between
x <- x[ !is.na(x) ]  
# Histogram with density estimate
hist(x, freq = FALSE, 
     main = "Histogram for left between",
     xlab = "Shoot density", col = "lightblue", border = "white")

# Add kernel density estimate
lines(density(x), col = "blue", lwd = 2)

# Add normal curve with same mean & sd
curve(dnorm(x, mean(x), sd(x)), 
      col = "red", lwd = 2, add = TRUE)

legend("topright",
       legend = c("Density", "Normal"),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")

qqnorm(x, main = "Q-Q plot for left between")
qqline(x, col = "red", lwd = 2)


shapiro.test(x)






# Extract numeric vector
x <- data$right_between
x <- x[ !is.na(x) ]  
# Histogram with density estimate
hist(x, freq = FALSE, 
     main = "Histogram for right between",
     xlab = "Shoot density", col = "lightblue", border = "white")

# Add kernel density estimate
lines(density(x), col = "blue", lwd = 2)

# Add normal curve with same mean & sd
curve(dnorm(x, mean(x), sd(x)), 
      col = "red", lwd = 2, add = TRUE)

legend("topright",
       legend = c("Density", "Normal"),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")

qqnorm(x, main = "Q-Q plot for right between")
qqline(x, col = "red", lwd = 2)


shapiro.test(x)



# Extract numeric vector
x <- data$right_behind
x <- x[ !is.na(x) ]  
# Histogram with density estimate
hist(x, freq = FALSE, 
     main = "Histogram for right behind",
     xlab = "Shoot density", col = "lightblue", border = "white")

# Add kernel density estimate
lines(density(x), col = "blue", lwd = 2)

# Add normal curve with same mean & sd
curve(dnorm(x, mean(x), sd(x)), 
      col = "red", lwd = 2, add = TRUE)

legend("topright",
       legend = c("Density", "Normal"),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")

qqnorm(x, main = "Q-Q plot for right behind")
qqline(x, col = "red", lwd = 2)


shapiro.test(x)







left_behind <- c(
  8800,2222.222222,933.3333333,1422.222222,3555.555556,11244.44444,
  1066.666667,4044.444444,533.3333333,888.8888889,666.6666667,8311,
  6444,8978,1289,1956,711,1422,2800,1956,2311,3022.222222,1733.333333,
  3777.777778,17279.52654,7288.888889,17007.19852,10076.02274,19706.23563
)

left_between <- c(
  15555.55556,14266.66667,19777.77778,24088.88889,21777.77778,20444.44444,
  28844.44444,37200,26800,18711.11111,2088.888889,11200,23022.22222,
  12488.88889,4311.111111,3600,6800,2533.333333,12844.44444,13288.88889,
  9244.444444,7644.444444,6800,8844.444444,3911.111111,1111.111111,
  2444.444444,7733.333333,5777.777778,15377.77778,22222.22222,6711.111111,
  29333.33333,13333.33333,7777.777778,5955.555556,14488.88889,9600,
  9155.555556,3600,9466.666667,2755.555556,7244.444444
)



wilcox.test(left_between, left_behind,
            alternative = "greater",
            exact = FALSE)



