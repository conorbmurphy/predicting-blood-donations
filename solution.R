# Get the training data provided by the challenge.
download.file("https://drivendata.s3.amazonaws.com/data/2/public/9db113a1-cdbe-4b1c-98c2-11590f124dd8.csv",
              "training.csv")
download.file("https://drivendata.s3.amazonaws.com/data/2/public/5c9fa979-5a84-45d6-93b9-543d1a0efc41.csv",
              "test.csv")
train <- read.csv("training.csv")
test <- read.csv("test.csv")

# EDA

require(lattice)
require(ggplot2)
cor(train)

# There are effectively 3 input variables: Months.since.First.Donation,
# Months.since.Last.Donation, and Number.of.Donations, which should be 
# be represented in the colors of the French flag
par(mfrow = c(1, 3))
with(train, {
        hist(Months.since.First.Donation, col = "blue", main = "Months Since First")
        hist(Months.since.Last.Donation, main = "Months Since Last")
        hist(Number.of.Donations, col = "red", main = "Total # Donations")
})

# Now let's look for correlations to the output Made.Donation.in.March.2007
first <- lm(Months.since.First.Donation ~ Made.Donation.in.March.2007, train)
last <- lm(Months.since.Last.Donation ~ Made.Donation.in.March.2007, train)
totnum <- lm(Number.of.Donations ~ Made.Donation.in.March.2007, train)

par(mfrow = c(1, 3))
with(train, {
        plot(Made.Donation.in.March.2007, Months.since.First.Donation, 
             col = "blue", main = "Months Since First")
        abline(first, lwd = 2)
        plot(Made.Donation.in.March.2007, Months.since.Last.Donation, 
             main = "Months Since Last")
        abline(last, lwd = 2)
        plot(Made.Donation.in.March.2007, Number.of.Donations, col = "red",
             main = "Total # Donations")
        abline(totnum, lwd = 2)
})



qplot(Made.Donation.in.March.2007, Months.since.Last.Donation, data = train)
qplot(Made.Donation.in.March.2007, Months.since.First.Donation, data = train)
qplot(Made.Donation.in.March.2007, Number.of.Donations, data = train)


xyplot(Months.since.Last.Donation ~ Number.of.Donations | Made.Donation.in.March.2007,
       data = train, 
       layout = c(2,1))


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
