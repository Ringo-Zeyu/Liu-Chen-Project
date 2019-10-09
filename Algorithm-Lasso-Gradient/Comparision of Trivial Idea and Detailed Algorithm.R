# Simple case study with Boston Housing Dataset
library(mlbench)
data("BostonHousing")
fulldata <- BostonHousing
fulldata <- subset(fulldata, select = -c(chas, zn)) # Delete 2 columns(Categorical)

# previous dataset(1:200 rows of the full dataset)
prvsdata <- fulldata[1:200, ]
# Standardize all variables
for (i in 1:12){
  prvsdata[ ,i] <- (prvsdata[ ,i] - mean(prvsdata[ ,i])) / sd(prvsdata[ ,i]) 
}
# Use Previous dataset to fit a simpe linear regression and store the coefficients
prvscoeff <- lm(medv ~ 0 + ., data = prvsdata)$coefficients
prvscoeff <- as.vector(prvscoeff)
# By now, we have already got the previous parameters set

# Now, we work on the new dataset
newdata <- fulldata[201:506, ]
y <- newdata$medv
x <- newdata[ ,1:11]

# Standardize covariate variables
xstan <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
for (i in 1:11){
  xstan[ ,i] <- (x[ ,i] - mean(x[ ,i])) / sd(x[ ,i]) 
}

# Standardize outcome variable
ystan <- rep(NA, nrow(x))
ystan <- (y - mean(y)) / sd(y)

################### First, we try our detailed algorithm mechanism
# Use new dataset to fit the Update LASSO Regression ALgorithm with previous parameters set
newcoeff_detail <- updateLasso(x = xstan, y = ystan, prvs = prvscoeff, p = 1)


################### Second, we try our trivial idea mechanism
# Adjust for our y
ystanadj <- ystan - xstan%*%prvscoeff
ystanadj <- as.vector(ystanadj)
# Use the adjusted y and new x to fit the Normal LASSO Regression Algorithm
tmp <- normalLasso(x = xstan, y = ystanadj, p = 1)
newcoeff_trivial <- tmp + prvscoeff

# See the difference of these 2 mechanisms' results
newcoeff_trivial - newcoeff_detail

