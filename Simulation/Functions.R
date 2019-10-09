# Sometimes, the new dataset contains more covariates than previous dataset. To be more specific, new dataset contains
# all covariates in previous dataset, it also contain some new convariates.
# Under such case, previous data fitted coefficients has length m, new data coefficients has length n, n > m
# The first m positions of new data coefficients corresponds to previous data covariates, the remaining n-m positions are new coefficients
# We want to make the previous data fitted coefficients also have length n, the last n-m positions have coefficients 0
equal.length <- function(previous, ncurrent){
  # Get the number of covariates of the previous data
  nprevs <- length(previous)
  # Creat a new vector with length equaling to new data number of covariates
  adj.prevs <- rep(NA, ncurrent)
  # The first nprevs positions of this new vector are the same as the previous coefficients
  adj.prevs[1:nprevs] <- previous
  # The remainning positions of adj.prevs should be filled with 0
  adj.prevs[(nprevs+1):ncurrent] <- 0
  # Return this new coefficients vector
  return(adj.prevs)
}



# Create a function to standardize the covariates
standardize.x <- function(covariate){
  for (i in 1:ncol(covariate)) {
    covariate[ ,i] <- standardize(covariate[ ,i])
  }
  return(covariate)
}



# Create a function to centralize the response
centralize.y <- function(response){
  response <- response - mean(response)
  return(response)
}



# Create a function to clean the "out"(dgCMatrix) and extract coefficients
clean.dgc <- function(dgcmatrix, ncoeff){
  # Make the dgcmatrix as a dataframe
  result <- as.data.frame(summary(dgcmatrix))
  # Reduce the variable indicator by 1 because we don't consider the intercept
  result$i <- result$i - 1
  # Create an empty vector to store the estimated coefficients
  est.cof <- rep(NA, ncoeff)
  # Store all estimated coefficients in the est.cof (include 0)
  for (k in 1:ncoeff){
    if(k %in% result$i){
      est.cof[k] <- result[result$i == k, ]$x
    } else{
      est.cof[k] <- 0
    }
  }
  return(est.cof)
}



# Create a function that takes in the train data and the valid data, then output the fitted coefficients
getcoeff <- function(train, valid, offset.include = FALSE, prevs.coef, intercept = FALSE, standardize = FALSE,
                     alpha, nfold, family = c("gaussian","binomial","poisson","multinomial","cox","mgaussian"),
                     type.measure = c("deviance", "mse", "mae", "class", "auc")){
  # Get the number of columns
  num.c <- ncol(train)
  
  # Standardize the covariates in train data and valid data
  train[ ,1:num.c-1] <- standardize.x(train[ ,1:num.c-1])
  valid[ ,1:num.c-1] <- standardize.x(valid[ ,1:num.c-1])
  # Centralize the response in train data and valid data
  train[ ,num.c] <- centralize.y(train[ ,num.c])
  valid[ ,num.c] <- centralize.y(valid[ ,num.c])
  
  # If not include the offset term
  if(!offset.include){
    # Fit a cross validation model using the valid data and get the optimal value of lambda
    cv.fit <- cv.glmnet(x = valid[ ,1:num.c-1], y = valid[ ,num.c], alpha = alpha, type.measure = type.measure,
                        nfolds = nfold, family = family, intercept = intercept, standardize = standardize)
    # Optimal value of lambda
    opt.lbd <- cv.fit$lambda.min
    
    # Fit the train dataset and get the estimated coefficients under the optimal value of lambda
    mod <- glmnet(x = train[ ,1:num.c-1], y = train[ ,num.c], family = family, intercept = intercept,
                  standardize = standardize, alpha = alpha)
    # Get the estimated coefficients under the optimal value of lambda
    out <- coef(mod, s = opt.lbd)
    
    # We need to do some cleaning work on "out" in order to extract coefficients
    est.beta <- clean.dgc(dgcmatrix = out, ncoeff = num.c-1)
  } else{ # include the offset term
    # Calculate the offset term for train data and valid data
    off.train <- train[ ,1:num.c-1] %*% prevs.coef
    off.valid <- valid[ ,1:num.c-1] %*% prevs.coef
    
    # Fit a cross validation model using the valid data and get the optimal lambda, with offset term
    cv.fit <- cv.glmnet(x = valid[ ,1:num.c-1], y = valid[ ,num.c], alpha = alpha, type.measure = type.measure,
                        offset = off.valid, nfolds = nfold, family = family, intercept = intercept, 
                        standardize = standardize)
    # Optimal value of lambda
    opt.lbd <- cv.fit$lambda.min
    
    # Fit the train dataset and get the estimated coefficients under the optimal value of lambda, with offset term
    mod <- glmnet(x = train[ ,1:num.c-1], y = train[ ,num.c], family = family, intercept = intercept,
                  standardize = standardize, alpha = alpha, offset = off.train)
    # Get the estimated coefficients under the optimal value of lambda
    out <- coef(mod, s = opt.lbd)
    
    # We need to do some cleaning work on "out" in order to extract coefficients
    est.k <- clean.dgc(dgcmatrix = out, ncoeff = num.c-1)
    # Add previous coefficients back, get the fitted beta coeffcients
    est.beta <- est.k + prevs.coef
  }
  
  # Return the estimated coefficients
  return(est.beta)
}



# Create a function that takes in test dataset and trained coefficients, the output the MSE of the test dataset
runtest <- function(test, para){
  # Get the number of columns
  num.c <- ncol(test)
  # Standardize the covariates in test data
  test[ ,1:num.c-1] <- standardize.x(test[ ,1:num.c-1])
  # Centralize the response in test data because we don't want to include the intercept
  test[ ,num.c] <- centralize.y(test[ ,num.c])
  
  # Get the predicted value
  y.hat <- test[ ,1:num.c-1] %*% para
  # Get the residual
  rsd <- test[ ,num.c] - y.hat
  # Get the mean square error
  mse <- mean(rsd^2)
  
  # Return the calculated MSE of the test dataset
  return(mse)
}