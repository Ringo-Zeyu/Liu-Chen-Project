library(TSdist)
library(robustHD)
# Create the Update LASSO regression algorithm, assume we do have previous knowledge, parameters set = prvs
updateLasso <- function(x, y, prvs, p = 1, maxiter = 100000, minerror = 10^(-5)){
  prvs <- as.vector(prvs) # Treat the previous parameter set as a vector
  # Number of rows and columns in x
  nobs <- nrow(x)
  ncov <- ncol(x)
  # Initialization
  m <- 0
  w <- prvs / p #Adjust for update LASSO algorithm
  delta <- 100
  # Keep loop if the maxiter hasn't achieved and the result hasn't converged
  while ((m <= maxiter) && (delta > minerror)) {
    m <- m + 1
    deri <- as.vector(2*(p^2)*t(x)%*%x%*%w - 2*p*t(x)%*%y)
    # Create a matrix to search for the direction vector
    whole <- rbind(deri, -deri)
    # Return the index of the minimal value entry
    # There may exist ties, if so, we just pick up the first minimal entry's index
    indx <- which(whole == min(whole), arr.ind = TRUE)[1, ]
    ridx <- indx[1] # row index, if 1, deri; if 2, -deri
    cidx <- indx[2]
    # Create the direction vector
    v <- rep(0, ncov)
    if (ridx == 1){
      v[cidx] <- 1
    } else{
      v[cidx] <- -1
    }
    v <- v + prvs / p # Adjust for update LASSO algorithm
    # Determine the best value of a, based on our calculation, need to find some parameters first
    pa1 <- as.numeric((-2)*p*t(y)%*%x%*%w)
    pa2 <- as.numeric((-2)*p*t(y)%*%x%*%v)
    pa3 <- as.numeric(2*(p^2)*t(w)%*%t(x)%*%x%*%v)
    pa4 <- as.numeric((p^2)*t(w)%*%t(x)%*%x%*%w)
    pa5 <- as.numeric((p^2)*t(v)%*%t(x)%*%x%*%v)
    
    second <- pa4 + pa5 - pa3 # parameter for a^2
    first <- pa2 + pa3 - pa1 - 2*pa4 # parameter for a
    cent <- (-1)*first / (2*second) # position of center line
    
    # No second order
    if(second == 0){
      if (first > 0){
        a <- 0
      } else{
        a <- 1
      }
    }
    
    # second order < 0
    if(second < 0){
      if (cent < 0.5){
        a <- 1
      } else{
        a <- 0
      }
    }
    
    # second order > 0
    if(second > 0){
      if (cent > 0 & cent < 1){
        a <- cent
      } else if(cent <= 0){
        a <- 0
      } else{
        a <- 1
      }
    }
    
    # Update w
    wold <- w
    w <- w + a*(v - w)
    # Update new error delta
    delta <- EuclideanDistance(wold, w)
  }
  # Can't find converge result
  if ((m > maxiter) & (delta > minerror)){
    print("Sorry, but no converge result found, please try larger iteration times or larger error level")
  } else{
    beta <- p*w
    return(beta)
  }
}

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

# Use new dataset to fit the Update LASSO Regression ALgorithm with previous parameters set
newcoeff <- updateLasso(x = xstan, y = ystan, prvs = prvscoeff)
newcoeff
prvscoeff
