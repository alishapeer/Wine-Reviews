#CS544_Project
#Peermohamed_Alisha
#getwd()
#setwd("/Users/alishapeermohamed/Documents/Boston University/Masters - Data Science/Foundations of R/Project/CS544Final_Peermohamed")
wine_reviews <- as.data.frame(read.csv('Wine_Reviews.csv', header = TRUE))
#lots of missing values in region_2 variable
wine_reviews <- wine_reviews[ , !(names(wine_reviews) %in% c('region_2'))]
class(wine_reviews$country)
class(wine_reviews$province)
class(wine_reviews$variety)
class(wine_reviews$designation)
#converting certain columns from factor to characters
wine_reviews$country <- as.character(wine_reviews$country)
wine_reviews$province <- as.character(wine_reviews$province)
wine_reviews$variety <- as.character(wine_reviews$variety)
wine_reviews$designation <- as.character(wine_reviews$designation)
#removing tuples from countries that have fewer than 1000 count
table(wine_reviews$country)
d <- which(table(wine_reviews$country) >= 1000);d
data <- subset(wine_reviews, country %in% (names(d)))
table(data$country)

#Analyzing the Data
#categorical variable: Countries Discluding the US
notUS <- subset(data, country != 'US')
table(notUS$country)
options(digits = 3)
100 * table(notUS$country)/nrow(data)
barlabels <- names(table(notUS$country))
b <- barplot(table(notUS$country), col = c('darkseagreen'), ylim = c(0, 24000),
             xaxt = 'n', ylab = '# of wine reviews', 
             main = 'Number of wine reviews outside of the US')
axis(side=1, at=b , labels=barlabels, las = 2, cex.axis = 0.75)
mtext(side = 1, text = "Country", line = 4)
axis(side = 2, at = c(0, 2500, 5000, 20000))

#numeric data : points
points <- data$points
mean(points)
table(points)
summary(points)
fivenum(points)
range(points)
var(points)
sd(points)
boxplot(points, horizontal = TRUE, xaxt= 'n', main = 'Spread of Wine Review points')
axis (side = 1, at = fivenum(points), labels = TRUE)

#One Pair of Two or More
#one set: price and points
library(dplyr)
price <- data$price
pricevspoints <- data %>%
  select(points, price)
plot(pricevspoints, main = 'Points Received vs. Price',
     col = 'cornflowerblue', ylim = c(0,600), xlab = 'Points', ylab = 'Price')

# Implementation of additional feature - linear regression model
lm(formula = points ~ price, data = pricevspoints)
abline(86.4608,  0.0407, col = 'red')


#Density Distribution of one variable (numeric)
hist(points, prob = TRUE, col = 'palevioletred', 
     xlab = 'points', ylab = 'Density', 
     main = 'Distribution of Points Received')

# Drawing various samples of Price:
price <- data$price; price
price <- price[!is.na(price)]
barplot(prop.table(table(price)), xlab = 'price',xlim = c(0, 250), col = 'maroon4', 
        ylab = 'Proportion', main = 'Density Distribution of Price')
mean(price)
sd(price)


#Drawing with sample size = 5
samples <- 10000
sample.size <- 5
xbar5 <- numeric(samples)
for (i in 1:samples) {
  xbar5[i] <- mean(sample(price, size = sample.size, replace = TRUE))
}
hist(xbar5, prob = TRUE, col = 'maroon4' , breaks = 35, xlim = c(10,80), 
     ylim = c(0, 0.05), main = 'Sample Size = 5')
mean(xbar5)
sd(xbar5)


#Drawing with sample size = 10
samples <- 10000
sample.size <- 10
xbar10 <- numeric(samples)
for (i in 1:samples) {
  xbar10[i] <- mean(sample(price, size = sample.size, replace = TRUE))
}
hist(xbar10, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.05), main = 'Sample Size = 10')
mean(xbar10)
sd(xbar10)


#Drawing with sample size = 20
samples <- 10000
sample.size <- 20
xbar20 <- numeric(samples)
for (i in 1:samples) {
  xbar20[i] <- mean(sample(price, size = sample.size, replace = TRUE))
}
hist(xbar20, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.07), main = 'Sample Size = 20')
mean(xbar20)
sd(xbar20)


#Drawing with sample size = 30
samples <- 10000
sample.size <- 30
xbar30 <- numeric(samples)
for (i in 1:samples) {
  xbar30[i] <- mean(sample(price, size = sample.size, replace = TRUE))
}
hist(xbar30, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.08), main = 'Sample Size = 30')
mean(xbar30)
sd(xbar30)

#Drawing with sample size = 40
samples <- 10000
sample.size <- 40
xbar40 <- numeric(samples)
for (i in 1:samples) {
  xbar40[i] <- mean(sample(price, size = sample.size, replace = TRUE))
}
hist(xbar40, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.09), main = 'Sample Size = 40')
mean(xbar40)
sd(xbar40)


par(mfrow = c(2,2))
hist(xbar10, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.05), main = 'Sample Size = 10')
hist(xbar20, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.07), main = 'Sample Size = 20')
hist(xbar30, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.08), main = 'Sample Size = 30')
hist(xbar40, prob = TRUE, col = 'maroon4', breaks = 35, 
     xlim = c(10,80), ylim = c(0, 0.09), main = 'Sample Size = 40')

par(mfrow = c(1,1))

#Comparing the Mean and Standard Deviation Across Samples
means <- c(mean(price), mean(xbar5), mean(xbar10), mean(xbar20), 
           mean(xbar30), mean(xbar40))
standev <- c(sd(price), sd(xbar5), sd(xbar10), sd(xbar20), 
             sd(xbar30), sd(xbar40))
options(digits = 3)
comparison <- cbind(means, standev)
rownames(comparison) <- c('All Prices', 'Sample Size = 5', 
                          'Sample Size = 10', 'Sample Size = 20',
                          'Sample Size = 30', 'Sample Size = 40')
colnames(comparison) <- c('  Mean', ' SD')
comparison
#Central Limit Theorem holds as the mean for the sample mean distributions is the same as the mean of the data. 
#The standard deviation decreases as the sample size increases.

#Other sampling methods 
#using sample size = 30
library(sampling)

#Simple random sampling with replacement
N = nrow(data)
n = 30
s <- srswr(n, N)
rows <- (1:N)[s!= 0]
sample.1 <- data[rows, ]
addmargins(table(sample.1$points))

#Systematic sampling
k <- ceiling(N / n)
r <- sample(k, 1)
rows <- seq(r, by = k, length = n)
sample.2 <- data[rows, ]
table(sample.2$price)

#Systematic Sampling based on Inclusion Probabilities
probinc <- inclusionprobabilities(data$points, n)
s <- UPsystematic(probinc)
sample.3 <- data[s != 0, ];b
table(sample.3$points)

#Stratified Sampling - Unequal Strata
# based on Country
options(digits = 3)
freq <- table(data$country)
st.sizes <- n * freq / sum(freq)
st.sizes <- ceiling(st.sizes)
st <- strata(data, stratanames = c('country'), 
             size = st.sizes, method = 'srswor', description = TRUE)
sample.4 <- getdata(data, st)
table(st$country)

##Implementation of Feature Not Mentioned in Specifications
# First extracted reviews only in the US
library(dplyr)
US <- subset(data, country == 'US')
table(US$province)
# Combined all provinces in the US that had count of 500 or fewer
combine <- names(which(table(US$province) < 500));combine #remaining states to combine into 'Other' category
US$province [which(US$province %in% combine)] = 'Other'
table(US$province)
provincevspoints <- US %>%
  select(province, points)
provincevspoints <- arrange(provincevspoints,province);head(provincevspoints)
California <- (subset(provincevspoints, provincevspoints$province == 'California'))$points
Newyork <- (subset(provincevspoints, provincevspoints$province == 'New York'))$points
Oregon <- (subset(provincevspoints, provincevspoints$province == 'Oregon'))$points
Other <- (subset(provincevspoints, provincevspoints$province == 'Other'))$points
Virginia <- (subset(provincevspoints, provincevspoints$province == 'Virginia'))$points
Washington <- (subset(provincevspoints, provincevspoints$province == 'Washington'))$points
a <- cbind(California, Newyork, Oregon, Other, Virginia, Washington)
colnames(a) <- names(table(US$province))
#Graphical depiction of State vs. Points received data
boxplot(a, main = 'Spread of Wine Review points by US State', 
        col = rainbow(6), cex.axis = 0.75, ylab = 'Points Awarded')





