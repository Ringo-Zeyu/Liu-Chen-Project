# Library necessary pacakges
library(robustHD)
library(glmnet)

# Create an empty matrix, we totally create 100 dataset, for each dataset, we calculate the MSEs of test dataset with and without previous knowledge
mse.matrix <- matrix(, nrow = 2, ncol = 100)
# Initial beta for previous data
beta.p <- c(3, 1.5, 2.5, 0, 2, 0, 0)
# Initial beta for new data
beta.n <- c(3, 1.5, 2.5, 0, 2, 0, 0, 1.8, 1.3, 0)

for(dt in 1:100){
  # Create an empty dataframe to store all the previous convariates
  pre.covariate <- matrix(, nrow = 2000, ncol = 7)
  # Generate the error term 
  error.p <- rnorm(n = 2000)
  # Generate all convariates
  for(i in 1:7){
    pre.covariate[ ,i] <- rnorm(n = 2000)
  }
  # Generate the outcome Y
  pre.Y <- pre.covariate %*% beta.p + 3 * error.p 
  # Combine covariates and outcome, get previous data
  data.prevs <- cbind(pre.covariate, pre.Y)
  
  # Create an empty dataframe to store all the new covariates
  new.covariate <- matrix(, nrow = 350, ncol = 10)
  # Generate the error term 
  error.n <- rnorm(n = 350)
  # Generate all convariates
  for(i in 1:10){
    new.covariate[ ,i] <- rnorm(n = 350)
  }
  # Generate the outcome Y
  new.Y <- new.covariate %*% beta.n + 3 * error.n
  # Combine covariates and outcome, get new data
  data <- cbind(new.covariate, new.Y)
  data.new <- data[1:150, ]
  data.test <- data[151:350, ]
  
  # Divide the previous data into train and validation parts
  data.prevs.train <- data.prevs[1:1000, ]
  data.prevs.valid <- data.prevs[1001:2000, ]
  
  # Divide the new data into train and validation parts
  data.new.train <- data.new[1:75, ]
  data.new.valid <- data.new[76:150, ]
  
  #########################################################################
  #                   Without Previous data Case                          #
  #########################################################################
  
  # Firstly, directly find the new coefficients without previous knowledge
  new.beta.without <- getcoeff(train = data.new.train, valid = data.new.valid, offset.include = FALSE,
                               intercept = FALSE, standardize = FALSE, alpha = 1, nfold = 5, family = "gaussian",
                               type.measure = "mse")
  # Secondly, use the test data to test for this beta and find the MSE
  mse.without <- runtest(test = data.test, para = new.beta.without)
  
  
  #########################################################################
  #                   With Previous data Case                             #
  #########################################################################
  
  # Firstly, find the previous coefficients
  prevs.beta <- getcoeff(train = data.prevs.train, valid = data.prevs.valid, offset.include = FALSE, 
                         intercept = FALSE, standardize = FALSE, alpha = 1, nfold = 10, family = "gaussian",
                         type.measure = "mse")
  # Rewrite the prevs.beta in the length of the number of covariates in new data
  prevs.beta <- equal.length(previous = prevs.beta, ncurrent = 10)
  # Secondly, find the new coefficients with previous knowledge
  new.beta.with <- getcoeff(train = data.new.train, valid = data.new.valid, offset.include = TRUE, prevs.coef = prevs.beta, 
                            intercept = FALSE, standardize = FALSE, alpha = 1, nfold = 5, family = "gaussian",
                            type.measure = "mse")
  # Thirdly, use the test data to test for this beta and find the MSE
  mse.with <- runtest(test = data.test, para = new.beta.with)
  
  
  # Store the result
  mse.matrix[1,dt] <- mse.without
  mse.matrix[2,dt] <- mse.with
}

# Transfer it into the dataframe
mse.matrix <- as.data.frame(mse.matrix)

# Add row names for it
rownames(mse.matrix) <- c("MSE without Previous Knowledge", "MSE with Previous Knowledge")

# Calculate the mean and sd of each row
show <- data.frame(apply(mse.matrix, 1, mean), apply(mse.matrix, 1, sd))
colnames(show) <- c("mean", "sd")
show
