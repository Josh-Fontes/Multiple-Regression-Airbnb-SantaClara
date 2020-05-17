




# setup
setwd("C:\\Users\\Julio\\Documents\\JOSH\\SJSU\\math 261A\\math261a R data")


library(readxl)
airbnb <- read_excel("listings.xlsx")
View(airbnb)



library(ggplot2)  # data visualization
library(leaps)  # variable selection stuff
library(MASS) # boxcox



## Data Cleaning
airbnb$id <- NULL
airbnb$neighbourhood_group <- NULL
airbnb$calculated_host_listings_count <- NULL
airbnb$host_id <- as.character(airbnb$host_id)
airbnb$last_review <- as.character(airbnb$last_review)


airbnb$reviews_per_month <- sapply(airbnb$reviews_per_month, 
                                   function(x) {
                                     if (is.na(x))
                                       return(0)
                                     return(x)
                                   })

airbnb$last_review <- sapply(airbnb$last_review,
                             function(x) {
                               if (is.na(x))
                                 return("Unknown")
                               
                               words <- unlist(strsplit(x, split = "-"))
                               return(words[1])
                             })



## Checking for multicolinearity

attach(airbnb)

airbnb.matrix <- model.matrix(price ~ longitude + latitude + neighbourhood + room_type + minimum_nights + number_of_reviews  + last_review + reviews_per_month + availability_365, data = airbnb)[, -1]



continuous.variables <-  c("longitude", "latitude", "minimum_nights", "number_of_reviews", "reviews_per_month", "availability_365")
continuous.correlations <- cor(airbnb.matrix[,continuous.variables])
diag(solve(continuous.correlations))



b <- model.matrix(price ~ neighbourhood + room_type + last_review, data = airbnb)[, -1]
diag(solve(cor(b)))


diag(solve(cor(airbnb.matrix)))  # need to fix high multicollinearity in the variable last_review


table(airbnb$last_review) # see what appears the most (what appears the most should be our baseline)

airbnb$last_review <- as.factor(airbnb$last_review)
airbnb$last_review <- relevel(airbnb$last_review, ref = "2018") # making 2018 be our baseline because that is the level that appears the most
airbnb.matrix <- model.matrix(price ~ longitude + latitude + neighbourhood + room_type + minimum_nights + number_of_reviews  + last_review + reviews_per_month + availability_365, data = airbnb)[, -1]
diag(solve(cor(airbnb.matrix))) # you should see way better VIFs






### Variable selection process


exhaustive <- regsubsets(price^(-0.5) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365, data = airbnb, nvmax = 6, nbest = 3, method = "exhaustive")
summary(exhaustive)



info.exhaustive  <- summary(exhaustive)

k <- as.numeric(rownames(info.exhaustive$which))
selection.matrix <- apply(info.exhaustive$which[, -1], 2, function(x) return(as.numeric(x)))

MSRes <- info.exhaustive$rss /  (nrow(airbnb) - 6 - 1)
R2 <- info.exhaustive$rsq
AdjR2 <- info.exhaustive$adjr2
Cp <- info.exhaustive$cp
BIC <- info.exhaustive$bic

cbind(k, selection.matrix, MSRes, R2, AdjR2, Cp, BIC)


plot(exhaustive, scale = "Cp")








#### FINDING THE BEST MODEL ###





### basic full model
fit.full <- lm(price ~ latitude + longitude + availability_365 + minimum_nights + number_of_reviews + reviews_per_month + room_type + neighbourhood + last_review, data = airbnb) 
summary(fit.full)



# studentized residuals and predicted values
students.full <- rstudent(fit.full)
fv_fit.full <- fitted.values(fit.full)

# residualplots
plot(students.full ~ fv_fit.full)
qqnorm(students.full) # qqplot
abline(0,1)



### picking the best response variable transformation
BC <- boxcox(fit.full, lambda = seq(-1, 0, 1/10))
BC$x[BC$y==max(BC$y)]  # we decided the best transformation was the reciprocal square root of price





# transformation
fit.bc_sqrt <- lm(I(price^(-0.5)) ~ latitude + longitude + availability_365 + minimum_nights + number_of_reviews + reviews_per_month + room_type + neighbourhood + last_review, data = airbnb)
summary(fit.bc_sqrt)

# studentized residuals and predicted values
students.bc_sqrt <- rstudent(fit.bc_sqrt)
fv_fit.bc_sqrt <- fitted.values(fit.bc_sqrt)



# residual plots
plot(students.bc_sqrt ~ fv_fit.bc_sqrt)
qqnorm(students.bc_sqrt, col = "gray50")
abline(0,1, col = "red") # some deviation at the tails


plot(fit.bc_sqrt, which = 4)  # cooks distance









# the following forward variable selection process is done "by hand".  Still acceptable, but a more effective/quicker/less tedious way of doing this is to install the "oslrr" package.  I do not exactly know how to use this but I am sure I can figure it out.
#install.packages("oslrr")



##### Forward Variable Selection Method
fit.0 <- lm(I(price^(-0.5)) ~ 1, data = airbnb) # model with only the y-int
add1(fit.0, I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## reviews per month wins
# p-value = 3.156x10^-16
# F = 67.0904


fit.1 <- lm(I(price^(-0.5)) ~ reviews_per_month, data = airbnb)
add1(fit.1, I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## room type wins
# p-value = 2.2x10^-16
#F = 3712.2537


fit.2 <- lm(I(price^(-0.5)) ~ room_type + reviews_per_month, data = airbnb)
add1(fit.2, I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## longitude wins
# p-value = 2.2x10^-16
# F = 99.8995


fit.3 <- lm(I(price^(-0.5)) ~ longitude + room_type + reviews_per_month, data = airbnb)
add1(fit.3,  I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## availability 365 wins
# p-value = 8.8x10^-16
# F = 65.2161



fit.4 <- lm(I(price^(-0.5)) ~ longitude + availability_365 + reviews_per_month + room_type, data = airbnb)
add1(fit.4,  I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## last review wins
# p-value = 2.2x10^(-16)
# F = 34.6046




fit.5 <- lm(I(price^(-0.5)) ~ longitude + availability_365 + reviews_per_month + room_type + last_review, data = airbnb)
add1(fit.5,  I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## minimum nights wins
# p-value = 2.2x10^(-16)
# F = 68.9977


fit.6 <- lm(I(price^(-0.5)) ~ longitude + availability_365 + reviews_per_month + room_type + last_review + minimum_nights, data = airbnb)
add1(fit.6,  I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## neighborhood wins
# p-value = 6.75^(-13)
# F = 6.1004






fit.7 <- lm(I(price^(-0.5)) ~ longitude + availability_365 + reviews_per_month + room_type + last_review + minimum_nights + neighbourhood, data = airbnb)
add1(fit.7,  I(price^(-0.5)) ~ longitude + latitude + minimum_nights + number_of_reviews  + reviews_per_month + availability_365 + last_review + neighbourhood + room_type, test = "F")
## no adds


# final model
fit.7




# fitting the final model
fit.final <- lm(I(price^(-0.5)) ~ longitude + availability_365 + reviews_per_month + room_type + last_review + minimum_nights + neighbourhood, data = airbnb)
summary(fit.final)
coefficients(fit.final)


# studentized residuals and predicted values
students.final <- rstudent(fit.final)
fv_fit.final <- fitted.values(fit.final)



# residual plots
plot(students.final ~ fv_fit.final)
qqnorm(students.final, col = "gray50")
abline(0,1, col = "red") # some deviation at the tails








###########################################################################







## exploring longitude

range(longitude) # west to east

west <- subset(price, longitude <= -122)
east <- subset(price, longitude > -122)
length(west)
length(east) # more locations found east of 122 longitude.  Probably because San Jose has by far the most locations and is east of 122 longitude


par(mfrow = c(1,2))
boxplot(west, main = "west")
boxplot(east, main = "east")
par(mfrow = c(1,1))



summary(west)
summary(east) # one huge outlier in San Jose (is this an influential point? should we explore what happens if we remove this from our analysis?) ANS - no, not an influential point.  check the cook's distance plot

t.test(east, west, alternative = "greater")  # seems like east neighbourhoods have more expensive airbnbs (could be because of what longitude I split the two groups, could be outliers, could just be because San Jose is expensive)








class(neighbourhood)
neighbourhood <- as.factor(neighbourhood) # should we do this before we fit our models?  (i checked and nothing changed about the results of the final model)
levels(neighbourhood)






#### DATA VISUALIZATION STUFF 


# residual plots of model 1
p <- ggplot(airbnb, aes(x = fv_fit.full, y = students.full, color = "#ff663f", alpha = 0.5))
p + geom_abline(slope = 0, intercept = 0, color = "#0098c3", size = 1.2) + geom_point() + labs(title = "Studentized Residual Plot of Model 1", y = "Studentized Residuals", x = "Predicted Values") + theme_classic() + theme_bw() + theme(legend.position="none")

p <- qplot(sample = students.full, colour = "#ff663f", alpha = 0.5)
p + geom_abline(slope = 1, intercept = 0, color = "#0098c3", size = 1.2) + labs(title = "Normal Q-Q Plot of Studentized Residuals of Model 1", x = "Theoretical", y = "Studentized Residuals") + theme_classic() + theme_bw() + theme(legend.position="none")







fit.model2 <- lm(I(price^(-0.5)) ~ latitude + longitude + availability_365 + minimum_nights + number_of_reviews + reviews_per_month + room_type + neighbourhood + last_review, data = airbnb)
summary(fit.model2)

# studentized residuals and predicted values
students.model2 <- rstudent(fit.model2)
fv_fit.model2 <- fitted.values(fit.model2)



# residual plots
plot(students.model2 ~ fv_fit.model2)
qqnorm(students.model2, col = "gray50")
abline(0,1, col = "red") # some deviation at the tails


# residual plots of model 2
p <- ggplot(airbnb, aes(x = fv_fit.model2, y = students.model2, color = "#ff663f", alpha = 0.5))
p + geom_abline(slope = 0, intercept = 0, color = "#0098c3", size = 1.2) + geom_point() + labs(title = "Studentized Residual Plot of Model 2", y = "Studentized Residuals", x = "Predicted Values") + theme_classic() + theme_bw() + theme(legend.position="none")

p <- qplot(sample = students.model2, colour = "#ff663f", alpha = 0.5)
p + geom_abline(slope = 1, intercept = 0, color = "#0098c3", size = 1.2) + labs(title = "Normal Q-Q Plot of Studentized Residuals of Model 2", x = "Theoretical", y = "Studentized Residuals") + theme_classic() + theme_bw() + theme(legend.position="none")












# residual plots of the final model
p <- ggplot(airbnb, aes(x = fv_fit.final, y = students.final, color = room_type, alpha = 0.5))
p + geom_point() + labs(title = "Residual Plot of Final Model", y = "Studentized Residuals", x = "Predicted Values") + theme_classic() + theme_bw()

p <- qplot(sample = students.final, colour = "#ff663f", alpha = 0.5)
p + geom_abline(slope = 1, intercept = 0, color = "#0098c3", size = 1.2) + labs(title = "Normal Q-Q Plot of Studentized Residuals of Final Model", x = "Theoretical", y = "Studentized Residuals") + theme_classic() + theme_bw() + theme(legend.position="none")



# for presentation
p <- ggplot(airbnb, aes(x = fv_fit.model2, y = students.model2, color = room_type, alpha = 0.5))
p + geom_abline(slope = 0, intercept = 0, color = "#0098c3", size = 1.2) + geom_point() + labs(title = "Studentized Residual Plot of Model 2", y = "Studentized Residuals", x = "Predicted Values") + theme_classic() + theme_bw()













# price by neighbourhood
p <- ggplot(airbnb, aes(x = neighbourhood, y = price))
p + geom_boxplot() + labs(title = "Side-by-side Boxplot", subtitle = "Price by Neighbourhood and Room-Type", y = "Price", x = "Neighbourhood") + theme(axis.text.x=element_text(angle = -90, hjust = 0))


# price by room type
p <- ggplot(airbnb, aes(x = room_type, y = price))
p + geom_boxplot() + labs(title = "Side-by-side Boxplot", subtitle = "Price by Room-Type", y = "Price", x = "Room Type") + theme(axis.text.x=element_text(angle = -90, hjust = 0))


# number of reviews by room type
p <- ggplot(airbnb, aes(x = room_type, y = number_of_reviews))
p + geom_boxplot() + labs(title = "Side-by-side Boxplot", subtitle = "Number of Reviews by Room-Type", y = "Number of Reviews", x = "Room Type") + theme(axis.text.x=element_text(angle = -90, hjust = 0))








SanJose <- subset(airbnb, neighbourhood == "San Jose")
View(SanJose)

detach(airbnb)
attach(SanJose)


# price in san jose by room type
p <- ggplot(SanJose, aes(x = room_type, y = price, color = room_type))
p + geom_boxplot() + labs(title = "Side-by-side Boxplot", subtitle = "Price by Room Type in San Jose", y = "Price", x = "Room Type") + theme(axis.text.x=element_text(angle = -90, hjust = 0))

detach(SanJose)










attach(airbnb)
# summary statistics

# price by neighbourhood
p <- ggplot(airbnb, aes(x = neighbourhood, y = price))
p + geom_boxplot() + labs(title = "Side-by-side Boxplot", subtitle = "Price by Neighbourhood", y = "Price", x = "Neighbourhood") + theme(axis.text.x=element_text(angle = -90, hjust = 0)) # trying to understand this shit

tapply(price, neighbourhood, median) # monte sereno has highest median (up in the hills? nice area/scenery?)
tapply(price, neighbourhood, mean) # how is gilroy more expensive than milpitas, what kind of person stays at a bed and breakfast in gilroy
# San Jose has a pretty low average, even with a huge outlier which means that it must have mostly cheap bnb's
tapply(price, neighbourhood, range)
sqrt(tapply(price, neighbourhood, var))  # san jose has highest variance (could be from outlier)
tapply(price, neighbourhood, max)
tapply(price, neighbourhood, min)
tapply(price, neighbourhood, length) # only 7 bnb's in Monte Sereno and they all not too expensive.. only 13 in gilroy and they mostly pretty cheap (judging by means, max, min, median, var...)

tapply(reviews_per_month, neighbourhood, mean)
tapply(number_of_reviews, neighbourhood, mean)

# how to see # of room types per neighborhood??







tapply(availability_365, neighbourhood, mean)
tapply()







tapply(price, room_type, median)
tapply(price, room_type, mean)
sqrt(tapply(price, room_type, var))
tapply(price, room_type, max)
tapply(price, room_type, min)





# boxplots


boxplot(price ~ neighbourhood, las = 2, main = "Price by City")
boxplot(minimum_nights ~ neighbourhood, las = 2, main = "Minimum Nights by City")
boxplot(availability_365 ~ neighbourhood, las = 2, main = "Availability (days per yr) by City")
boxplot(number_of_reviews ~ neighbourhood, las = 2, main = "Number of Reviews by City")
boxplot(reviews_per_month ~ neighbourhood, las = 2, main = "Reviews per Month by City")

boxplot(price ~ room_type, main = "Price by Room Type")
boxplot(minimum_nights ~ room_type, main = "Minimum Nights by Room Type")
boxplot(availability_365 ~ room_type, main = "Availability by Room Type")
boxplot(number_of_reviews ~ room_type, main = "Number of Reviews by Room Type")
boxplot(reviews_per_month ~ room_type, main = "Reviews per Month by Room Type")








# histograms

hist(number_of_reviews)
hist(price, breaks = 100, xlim = c(0,2500))
hist(availability_365)
hist(reviews_per_month)
hist(minimum_nights)
hist(longitude)
hist(latitude)




# Scatterplots

plot(price ~ minimum_nights)
plot(price ~ number_of_reviews)
plot(price ~ availability_365)
plot(price ~ reviews_per_month)




plot(number_of_reviews ~ availability_365)
plot(reviews_per_month ~ availability_365)
plot(minimum_nights ~ availability_365)




plot(number_of_reviews ~ reviews_per_month)
plot(minimum_nights ~ reviews_per_month)


plot(number_of_reviews ~ minimum_nights)



plot(price ~ calculated_host_listings_count)
plot(minimum_nights ~ calculated_host_listings_count)
plot(availability_365 ~ calculated_host_listings_count)






# scatter plots by latitude

plot(longlitude ~ latitude)
plot(price ~ latitude)
plot(log(price) ~ latitude)
plot(availability_365 ~ latitude)
plot(number_of_reviews  ~ latitude)
plot(minimum_nights ~ latitude)
plot(reviews_per_month ~ latitude)

# scatter plots by longitude
plot(price ~ longitude)
plot(availability_365 ~ longitude)
plot(reviews_per_month ~ longitude)
plot(minimum_nights ~ longitude)
plot(number_of_reviews ~ longitude)




