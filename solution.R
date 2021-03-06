### Importing the Data ###

download.file("https://drivendata.s3.amazonaws.com/data/2/public/9db113a1-cdbe-4b1c-98c2-11590f124dd8.csv",
              "training.csv")
download.file("https://drivendata.s3.amazonaws.com/data/2/public/5c9fa979-5a84-45d6-93b9-543d1a0efc41.csv",
              "test.csv")
train <- read.csv("training.csv")
test <- read.csv("test.csv")
attach(train)

### EDA - Basic Summaries ###

str(train)
str(test)
table(Made.Donation.in.March.2007)
(138/438)*100
# 138 of 438 donors, or 31.5%, gave in March 2007.

### EDA - Missing Data ###

sum(is.na(train))

# There are no missing values in this data.

### EDA - Exploratory Graphs ###

# There are effectively 3 input variables: Months.since.First.Donation,
# Months.since.Last.Donation, and Number.of.Donations, which should be 
# be represented in the colors of the French flag.  Total.Volume.Donated..c.c
# is consistent at 250 c.c. per donation.

par(mfrow = c(1, 3))
with(train, {
        hist(Months.since.First.Donation, col = "blue", 
             main = "Months Since 1st Donation")
        hist(Months.since.Last.Donation, main = "Months Since Last Donation")
        hist(Number.of.Donations, col = "red", main = "Total # of Donations")
})

# Now let's look for correlations to the output Made.Donation.in.March.2007

cor(train)

first <- lm(Months.since.First.Donation ~ Made.Donation.in.March.2007, train)
last <- lm(Months.since.Last.Donation ~ Made.Donation.in.March.2007, train)
totnum <- lm(Number.of.Donations ~ Made.Donation.in.March.2007, train)

par(mfrow = c(1, 3))
with(train, {
        plot(Made.Donation.in.March.2007, Months.since.First.Donation, 
             col = "blue", main = "Months Since 1st Donation")
        abline(first, lwd = 2)
        plot(Made.Donation.in.March.2007, Months.since.Last.Donation, 
             main = "Months Since Last Donation")
        abline(last, lwd = 2)
        plot(Made.Donation.in.March.2007, Number.of.Donations, col = "red",
             main = "Total # of Donations")
        abline(totnum, lwd = 2)
})

# Months.since.First.Donation does not appear to have a strong correlation to
# a blood donation in March 07.  The other two variables have stronger
# correlations

# lmDat <- data.frame(Months.since.Last.Donation, Number.of.Donations,
#                    Months.since.First.Donation, Made.Donation.in.March.2007)
# for (i in 3) {
#        glmFit <- glm(Made.Donation.in.March.2007 ~ lmDat[i], 
#                      family = "binomial", data = lmDat)
#}
# This is broken

require(ggplot2)
qplot(Months.since.First.Donation, Months.since.Last.Donation, data = train, 
      facets = .~Made.Donation.in.March.2007, col = Made.Donation.in.March.2007, 
      alpha = 3/4, ylim = c(0,25), geom = c("point", "smooth"), method = "lm")
# This shows that there is a correlation between months since first and last 
# Donations

pairs(cbind(Months.since.Last.Donation,Months.since.First.Donation,Number.of.Donations), gap = 0, panel = panel.smooth)

# From Jon (check line 60)
# first time donors
blood$first.time <- ifelse(blood$Number.of.Donations == 1,1,0)
# months between first donation and last donation
blood$active.months <- ifelse(blood$first.time == 1,0,(blood$Months.since.First.Donation - blood$Months.since.Last.Donation) - 1)
# donation cycle - how often does the person donate
blood$cycle <- ifelse(blood$active.months == 0,1,blood$active.months)/blood$Number.of.Donations


# Let's look at this: http://www.ats.ucla.edu/stat/r/dae/logit.htm
library(aod)
library(ggplot2)
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial") #generalized linear model
summary(mylogit)

glmFit <- glm(Made.Donation.in.March.2007 ~ Number.of.Donations + Months.since.First.Donation + Months.since.Last.Donation, family = "binomial")
confint(glmFit) # Calculating confidence intervals with log likelihood
confint.default(glmFit) # Calculating CI using standard errors
exp(cbind(OR = coef(glmFit), confint(glmFit))) # odds ration with 95% confidence interval
# This shows that the odds of giving in March 07 increases by the OR column with each unit increase in Number of donations, months since first donation, etc.
newdata1 <- with(train, data.frame(NumDon = mean(Number.of.Donations), MonFir = mean(Months.since.First.Donation), MonLas = mean(Months.since.Last.Donation)))
# newdata1$Model <- predict(glmFit, newdata = newdata1, type = "response") # START BACK HERE


# Applying the above model to our dataset
LogitReg <- glm(Made.Donation.in.March.2007 ~ Months.since.First.Donation +
                        Months.since.Last.Donation + Number.of.Donations,
                        family = "binomial")
summary(LogitReg)
# Here we see the following:
# The original function we defined for LogitReg
# The deviance residuals, or the measure of the model's fit
# The output coefficients and standard errors, for example:
#       For every one unit change in months since first donation, the log odds of
#       giving blood in Mar 07 decreases by .0188
# Below this are fit indices

confint(LogitReg)
# this gives us confidence intervals

# Add a column of data which expresses the interval over which each
# donor has donated.
Training.Data <- data.frame(
  Training.Data,
  Months.Elapsed =
    Training.Data$Months.since.First.Donation
    - Training.Data$Months.since.Last.Donation
    + 1
)
# Add a column which expresses the rate at which each donor donated,
# for the period over which they have donated.
Training.Data <- data.frame(
  Training.Data,
  Donations.per.Month =
    Training.Data$Number.of.Donations
    / Training.Data$Months.Elapsed
)
# Output raw data.
Training.Data
