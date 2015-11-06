download.file("https://drivendata.s3.amazonaws.com/data/2/public/9db113a1-cdbe-4b1c-98c2-11590f124dd8.csv", "dat1.csv")
download.file("https://drivendata.s3.amazonaws.com/data/2/public/5c9fa979-5a84-45d6-93b9-543d1a0efc41.csv", "dat2.csv")
dat1 <- read.csv("dat1.csv") # training dataset w/ donation in Mar 2007
dat2 <- read.csv("dat2.csv") # test dataset has solution

# Means we no longer have to specify the object:
attach(dat1)

# From Gabe's contribution to github
dat1 <- data.frame(
  dat1,
  Months.Elapsed =
    dat1$Months.since.First.Donation
    - dat1$Months.since.Last.Donation
    + 1
)
dat1 <- data.frame(
  dat1,
  Donations.per.Month =
    dat1$Number.of.Donations
    / dat1$Months.Elapsed
)
summary(dat1$Donations.per.Month) # gives 5 number summary for donations per month
cor(dat1[2:6]) # shows covariance in colums 2:6
