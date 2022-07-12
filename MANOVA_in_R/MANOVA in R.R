# Load Libraries
library("mvnormtest")
library("car")

# Question
# It is well-known that men are more likely to have heart attacks than women. 
# How does gender (sex) influence some of the heart attack predictors like resting blood pressure (trestbps) 
# and cholesterol (chol)?


# Ensure Variables are Numeric

str(heartAttacks$trestbps)
str(heartAttacks$chol)



# Subsetting

keeps <- c("trestbps", "chol")
heartAttacks1 <- heartAttacks[keeps]

heartAttacks2 <- heartAttacks1[1:5000,]



#Format as a Matrix
  
heartAttacks3 <- as.matrix(heartAttacks2)



# Test Assumptions

# Multivariate Normality

heartAttacks4 <- na.omit(heartAttacks3)

mshapiro.test(t(heartAttacks4))


# Homogeneity of Variance
leveneTest(heartAttacks$trestbps, heartAttacks$chol, data=heartAttacks)



# Testing for backers.
leveneTest(heartAttacks$trestbps, heartAttacks$chol, data=heartAttacks)


# Absence of Multicollinearity
cor.test(heartAttacks$trestbps, heartAttacks$chol, method="pearson", use="complete.obs")


# The Analysis
  
MANOVA <- manova(cbind(trestbps, chol) ~ sex, data = heartAttacks)
summary(MANOVA)


# Post Hocs

# ANOVAs as Post Hocs

summary.aov(MANOVA, test = "wilks") 


# Conclusion Statement:
#There is a significant influence in men for heart attacks due to high cholestrol as well as high blood pressure.