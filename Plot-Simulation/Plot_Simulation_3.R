# This part of code is used to generate the simulation graph for Generation Mechanism 1
# New Data Size V.S Boxplot

# Library necessary pacakges
library(robustHD)
library(glmnet)
library(ggplot2)
library(reshape2)

# New Data Size we want to try in order to draw the graph
try.size <- c(50, 100, 150, 200, 250, 300)

# Create an empty matrix to store the MSEs we get
# 200 rows because for each new data size, we will get 100 "MSE without previous knowledge"
# and 100 "MSE with previous knowledge". 7 columns: column 2-7 corresponding to 6 new data sizes want to try
# Column 1 is label of either "MSE without previous knowledge" or "MSE with previous knowledge"
graph.data <- as.data.frame(matrix(, nrow = 200, ncol = 7))
colnames(graph.data) <- c("label", "50 New Data", "100 New Data", "150 New Data",
                          "200 New Data", "250 New Data", "300 New Data")
# The 1:100 rows store the MSEs without previous knowledge
# The 101:200 rows store the MSEs with previous knowledge
graph.data[1:100, 1] <- "Without Previous"
graph.data[101:200, 1] <- "With Previous"

# Loop for each try.size
for (s in 1:6){
  # Get the new data size for this loop
  newsize <- try.size[s]
  # Create an empty matrix, we totally create 100 dataset, for each dataset, we calculate the MSEs of test dataset with and without previous knowledge
  mse.matrix <- matrix(, nrow = 2, ncol = 100)
  # Initial beta for previous data
  beta.p <- c(3, 1.5, 2.5, 0, 2, 0, 0)
  # Initial beta for new data
  beta.n <- c(2.8, 1.3, 2.5, 1.2, 0, 0.3, 0, 1.8, 1.3, 0)
  
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
    
    # Create an empty dataframe to store all the new covariates(train and test)
    new.covariate <- matrix(, nrow = (newsize+200), ncol = 10)
    # Generate the error term 
    error.n <- rnorm(n = (newsize+200))
    # Generate all convariates
    for(i in 1:10){
      new.covariate[ ,i] <- rnorm(n = (newsize+200))
    }
    # Generate the outcome Y
    new.Y <- new.covariate %*% beta.n + 3 * error.n
    # Combine covariates and outcome, get new data
    data <- cbind(new.covariate, new.Y)
    data.new <- data[1:newsize, ]
    data.test <- data[(newsize+1):(newsize+200), ]
    
    # Divide the previous data into train and validation parts
    data.prevs.train <- data.prevs[1:1000, ]
    data.prevs.valid <- data.prevs[1001:2000, ]
    
    # Divide the new data into train and validation parts
    data.new.train <- data.new[1:(newsize/2), ]
    data.new.valid <- data.new[((newsize/2)+1):newsize, ]
    
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
  # Store the MSEs into grpah.data
  graph.data[1:100, s+1] <- mse.matrix[1, ]
  graph.data[101:200, s+1] <- mse.matrix[2, ]
}

# Get the data in a specific format by melting it
graph.data.m <- melt(graph.data, id.vars = "label")

# Plot out the box plot
ggplot(data = graph.data.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=label)) +
  xlab("New Data Size(Train & Validation Data)") +
  ylab("Mean squared error of test data") + 
  ggtitle("Distribution of MSE for Generating Mechanism 3 with different New Data size") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title="Previous Knowledge"))