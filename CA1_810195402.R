usedCar <- read.csv("~/Desktop/Term 8/SI/CA/Phase 1/usedcar.csv")
# 0 #
## b ##
print(paste("Number of records: ", nrow(usedCar)))
print(paste("Number of features: ", ncol(usedCar)))
## c ##
print(sapply(usedCar, function(x) sum(is.na(x))))
usedCars <- usedCar[!is.na(usedCar$price),]
usedCars$economy <- ifelse(is.na(usedCars$economy), mean(usedCars$economy, na.rm=TRUE), usedCars$economy)
usedCars$odometer <- ifelse(is.na(usedCars$odometer), mean(usedCars$odometer, na.rm=TRUE), usedCars$odometer)
usedCars$litres <- ifelse(is.na(usedCars$litres), median(usedCars$litres, na.rm=TRUE), usedCars$litres)
usedCars$cylinders <- ifelse(is.na(usedCars$cylinders), median(usedCars$cylinders, na.rm=TRUE), usedCars$cylinders)
print(sapply(usedCars, function(x) sum(is.na(x))))
drops <- c("rownum")
usedCars <- usedCars[ , !(names(usedCars) %in% drops)]

# 1 #
## a ##
qqnorm(usedCars$price)
qqline(usedCars$price)
## b ##
usedCars$categorial_price <- NA
usedCars$categorial_price[usedCars$price < 50000] <- "B"
usedCars$categorial_price[usedCars$price < 15000] <- "C"
usedCars$categorial_price[usedCars$price < 5000] <- "D"
usedCars$categorial_price[usedCars$price >= 50000] = "A"
colors = c("red", "yellow", "green", "violet")
classes = c("A: ", "B: ", "C: ", "D: ")
percentages = paste(prop.table(table(usedCars$categorial_price))*100, "%", sep="")
labels = paste0(classes, percentages)
pie(table(usedCars$categorial_price), col = colors, labels = labels, main = "Categorial prices")
## c ##
bins <- seq(0, 100000, 1000)
hist(usedCars$price[usedCars$price < 100000], breaks=bins, main = "Histogram of cars prices (bin width = 1000)", xlab = "Price")
## d ##
usedCars <- usedCars[usedCars$price < 100000,]
plot(density(usedCars$price), main = "Density of price", xlab = "Price")
## e ##
library(PerformanceAnalytics)
print(skewness(usedCars$price))
## f ##
print(paste("Mean: ", mean(usedCars$price)))
print(paste("Variance: ", var(usedCars$price)))
print(paste("Standard deviation: ", sd(usedCars$price)))
## g ##
plt <- boxplot(usedCars$price)
print(plt)

# 2 #
## a ##
body_types <- table(usedCars$body_type)
barplot(body_types, las=2)
## b ##
body_types <- sort(body_types)
barplot(body_types, las=2, horiz = TRUE, main = "Barplot of Body Types")
## c ##
library(plyr)
print(count(usedCars, "body_type"))
## d ##
library(ggplot2)
violin <- ggplot(usedCars, aes(x = body_type, 
                     y = price)) + 
  geom_violin(fill = "lightBlue", color = "#473e2c") + 
  labs(x = "Body Types", y = "Price")
plot(violin)

# 3 #
## a ##
usedCars <- usedCars[usedCars$odometer < 500000,]
plot(ggplot(usedCars, aes(x=odometer, y=price)) + geom_point())
## b ##
plot(ggplot(usedCars, aes(x=odometer, y=price, shape=make, color=make)) + geom_point())
## c ##
print(cor(usedCars$odometer, usedCars$price))
print(cor.test(usedCars$odometer, usedCars$price))
## d ##
library(ggExtra)
hex <- ggplot(usedCars, aes(odometer, price))
plot(ggMarginal(hex + geom_point(col="transparent") + geom_hex(bins=15), type="histogram", size=10))
## e ##
plot(ggplot(usedCars, aes(x=odometer, y=price) ) +
  geom_bin2d(bins = 20) +
  scale_fill_continuous(type = "viridis") +
  theme_bw())

# 4 #
## a ##
cor_mat <- usedCars[ , c(1, 8, 11, 15)]
plot(cor_mat , pch=20 , cex=1.5 , col="#69b3a2")
## c ##
library(corrplot)
col<- colorRampPalette(c("darkblue", "white", "red"))(20)
corrplot(cor(cor_mat), type="upper", method="color", addCoef.col = "black", col=col)
## d ##
library("scatterplot3d")
scatterplot3d(cor_mat$price, cor_mat$odometer, cor_mat$economy, angle = 60, pch = 12, color="steelblue",
              xlab = "Price", ylab = "Odometer", zlab = "Economy")
## e ##
colors <- c("#999999", "#E69F00")
colors <- colors[as.numeric(usedCars$make)]
scatterplot3d(usedCars[,c(1, 8, 15)], pch = 16, color=colors, type="h")
legend("right", legend = levels(usedCars$make), col =  c("#999999", "#E69F00"), pch = 16)

# 5 #
## a ##
usedCars <- usedCars[!usedCars$body_type=="",]
usedCars$body_type <- droplevels(usedCars$body_type)
print(table(usedCars$body_type, usedCars$make))
## b ##
library(dplyr)
group_bar <- usedCars %>% group_by(body_type, make) %>% tally()
plot(ggplot(group_bar, aes(fill=body_type, y=n, x=make)) + 
  geom_bar(position="dodge", stat="identity"))
## c ##
plot(ggplot(group_bar, aes(fill=body_type, y=n, x=make)) + 
  geom_bar(position="fill", stat="identity"))
## d ##
mosaicplot(table(usedCars$colour, usedCars$make), col = hcl(c(120, 10)), las=2, main="Colour and Make mosaic plot")

# 6 #
## a ##
x_bar <- mean(usedCars$price)
z_star <- -qnorm(0.01)
s <- sd(usedCars$price)
SE <- s / sqrt(length(usedCars$price))
upper_bound <- x_bar + (z_star * SE)
lower_bound <- x_bar - (z_star * SE)
print(paste("98% CI: [", lower_bound, ", " , upper_bound, "]"))
## c ##
#plot(ggplot(usedCars, aes(x="outcome name", y=mean(price))) +
#  geom_bar(position = 'dodge', stat='identity', width=.5) +
#  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
#                width = 0.2,
#                linetype = "solid",
#                position = position_dodge(width = 0.5),
#                color="red", size=1))
## d ##
sample_indices <- sample(nrow(usedCars), 80)
sampled_prices <- usedCars$price[sample_indices]
x_bar <- mean(sampled_prices)
se <- sd(usedCars$price) / sqrt(80)
z_stat <- (x_bar - 21000) / se
p_value <- pnorm(z_stat, lower.tail = FALSE)
print(paste("p-value: ", p_value))
## e ##
x_bar <- mean(usedCars$price)
z_star <- -qnorm(0.025)
s <- sd(usedCars$price)
SE <- s / sqrt(length(usedCars$price))
upper_bound <- x_bar + (z_star * SE)
lower_bound <- x_bar - (z_star * SE)
print(paste("95% CI: [", lower_bound, ", " , upper_bound, "]"))
## f ##
actual_average <- mean(usedCars$price)
s <- sd(usedCars$price)
SE <- s / sqrt(80)
boundary <- -qnorm(0.025)
z_stat_bar <- SE * boundary + 21000
z_stat <- (z_stat_bar - actual_average) / SE
power <- pnorm(z_stat, lower.tail = FALSE)
print(paste("Power: ", power))
beta <- 1 - power
print(paste("Type II error: ", beta))

# 7 #
sample_indices <- sample(nrow(usedCars), 25)
sampled_data <- usedCars[sample_indices,]
## b ##
point_estimate <- mean(sampled_data$odometer) - mean(sampled_data$price)
df <- min(length(sampled_data$odometer), length(sampled_data$price)) - 1
t_star <- -qt(0.25, df = df)
se <- sqrt((sd(sampled_data$price)**2 / length(sampled_data$price)) + (sd(sampled_data$odometer)**2 / length(sampled_data$odometer)))
lower_bound <- point_estimate - t_star * se
upper_bound <- point_estimate + t_star * se
print(paste("CI: [", lower_bound, ",", upper_bound, "]"))
t_stat <- point_estimate / se
p_value <- pt(t_stat, df=df, lower.tail = FALSE)
print(paste("P-value: ", p_value))

# 8 #
resamples <- lapply(1:100, function(i) sample(usedCars$price, replace = TRUE))
samples.median <- sapply(resamples, median)
## a ##
samples.median <- sort(samples.median)
print(paste("CI: [", samples.median[6], ",", samples.median[95], "]"))
## b ##
median_mean <- mean(samples.median)
median_se <- sqrt(var(samples.median)) / 10
t_star <- -qt(0.025, df=99)
upper_bound <- median_mean + t_star * median_se
lower_bound <- median_mean - t_star * median_se
print(paste("CI: [", lower_bound, ",", upper_bound, "]"))

# 9 #
manual_cars <- usedCars$price[usedCars$transmission == "Manual"]
auto_cars <- usedCars$price[usedCars$transmission == "Automatic"]
man_mean <- mean(manual_cars)
man_s <- sqrt(var(manual_cars))
man_len <- length(manual_cars)
auto_mean <- mean(auto_cars)
auto_s <- sqrt(var(auto_cars))
auto_len <- length(auto_cars)
point_estimate <- auto_mean - man_mean
null_h <- 0
se <- sqrt((man_s^2/man_len)+(auto_s^2/auto_len))
df <- min(man_len, auto_len) - 1
t_stat <- (point_estimate - null_h) / se
p_value <- pt(t_stat, df=df, lower.tail = FALSE)
print(paste("P-value: ", p_value))
boxplot(price~transmission,
        data=usedCars[!usedCars$transmission == "", ],
        main="Box plot for diffrent transmissions",
        xlab="Transmission",
        ylab="price",
        col="lightblue",
        border="black"
)