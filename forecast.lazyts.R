predictor.lazyts <- function(d) {
  ## Team members:
  # - Alessandro Ciancetta
  # - Ramon Talvi
  # - Alessandro Tenderini
  # - Akash Yadav
  
  ## Scale the data. Get mean and standard deviations 
  ## to scale back the final prediction
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  ## get the target
  y <- d_scaled[,1]    
  ## get the principal components of the other variables
  x <- d_scaled[,-1]   
  eigen_list <- prcomp(x)
  f <- eigen_list$x[,1:4] 
  
  ## define the dataframes 
  d_pcr <- as.data.frame(cbind(
    y = y[2:length(y)], y_lag =  y[1:(length(y)-1)],  
    f[1:(nrow(f)-1),]))
  d_test <- as.data.frame(cbind(y_lag=y,f))
  d_test <- d_test[nrow(d_test), ]
  
  ## Principal compoent regression
  m_pcr <- lm(y ~ ., data = d_pcr)
  y_hat <- predict(m_pcr, newdata = d_test)*d_sd[1] + d_mean[1]
  y_hat
}


## NOT RUN
## Evaluating the method
# setwd("~/Universita/BSE/Courses/Term2 - Financial Econometrics/forecast-competition/")
# d <- read.csv("forecast-competition-training.csv")
# data <- d
# 
# test_size <- 100
# y_test <- d[401:500,1]
# y_hat <- c()
# for(t in 1:test_size){
#   idx_train <- nrow(d)-test_size+t-1
#   d_train <- d[1:idx_train,]
#   y_hat[t] <- predictor.lazyts(d_train)
# }
# 
# plot(y_test, type = "l")
# lines(y_hat, type = "l", col="red")
# 
# cat("RMSE =", sqrt(mean((y_test-y_hat)^2)))
## END NOT RUN




