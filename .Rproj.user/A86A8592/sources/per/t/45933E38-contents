
# custom function to carry get MLE and MOM of one sample
estimators <- function(number, parameter) {
  sample <- runif(number, min = 0, max = parameter)
  mom <- mean(sample)*2
  mle <- max(sample)
  return(c(mom, mle))
}

# custom function to find MSE of estimators using Monte Carlo with n = 1000

mse.monte.carlo <- function(number, parameter) {
  sample.estimators <- replicate(1000, estimators(number, parameter))
  sample.estimators <- (sample.estimators-parameter)^2
  sample.estimators.mom <- sample.estimators[1,]
  sample.estimators.mle <- sample.estimators[2,]
  return(c(mean(sample.estimators.mom), mean(sample.estimators.mle)))
}

# getting MSE of estimators for all combinations of n and theta
mse.1.1 <- mse.monte.carlo(1,1)
mse.1.1
mse.1.5 <- mse.monte.carlo(1,5)
mse.1.5
mse.1.50 <- mse.monte.carlo(1,50)
mse.1.50
mse.1.100 <- mse.monte.carlo(1,100)
mse.1.100
mse.2.1 <- mse.monte.carlo(2,1)
mse.2.1
mse.2.5 <- mse.monte.carlo(2,5)
mse.2.5
mse.2.50 <- mse.monte.carlo(2,50)
mse.2.50
mse.2.100 <- mse.monte.carlo(2,100)
mse.2.100
mse.3.1 <- mse.monte.carlo(3,1)
mse.3.1
mse.3.5 <- mse.monte.carlo(3,5)
mse.3.5
mse.3.50 <- mse.monte.carlo(3,50)
mse.3.50
mse.3.100 <- mse.monte.carlo(3,100)
mse.3.100
mse.5.1 <- mse.monte.carlo(5,1)
mse.5.1
mse.5.5 <- mse.monte.carlo(5,5)
mse.5.5
mse.5.50 <- mse.monte.carlo(5,50)
mse.5.50
mse.5.100 <- mse.monte.carlo(5,100)
mse.5.100
mse.10.1 <- mse.monte.carlo(10,1)
mse.10.1
mse.10.5 <- mse.monte.carlo(10,5)
mse.10.5
mse.10.50 <- mse.monte.carlo(10,50)
mse.10.50
mse.10.100 <- mse.monte.carlo(10,100)
mse.10.100
mse.30.1 <- mse.monte.carlo(30,1)
mse.30.1
mse.30.5 <- mse.monte.carlo(30,5)
mse.30.5
mse.30.50 <- mse.monte.carlo(30,50)
mse.30.50
mse.30.100 <- mse.monte.carlo(30,100)
mse.30.100

# plotting with different values of theta but same value of n
par(mfrow=c(2,2))
plot(c(1,5,50,100), c(mse.1.1[1], mse.1.5[1], mse.1.50[1], mse.1.100[1]), type = "b", xlab = "theta", ylab = "Mean Squared Error", col = "green", main = "Where n=1")
lines(c(1,5,50,100), c(mse.1.1[2], mse.1.5[2], mse.1.50[2], mse.1.100[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,5,50,100), c(mse.2.1[1], mse.2.5[1], mse.2.50[1], mse.2.100[1]), type = "b", xlab = "theta", ylab = "Mean Squared Error", col = "green", main = "Where n=2")
lines(c(1,5,50,100), c(mse.2.1[2], mse.2.5[2], mse.2.50[2], mse.2.100[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,5,50,100), c(mse.3.1[1], mse.3.5[1], mse.3.50[1], mse.3.100[1]), type = "b", xlab = "theta", ylab = "Mean Squared Error", col = "green", main = "Where n=3")
lines(c(1,5,50,100), c(mse.3.1[2], mse.3.5[2], mse.3.50[2], mse.3.100[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,5,50,100), c(mse.5.1[1], mse.5.5[1], mse.5.50[1], mse.5.100[1]), type = "b", xlab = "theta", ylab = "Mean Squared Error", col = "green", main = "Where n=5")
lines(c(1,5,50,100), c(mse.5.1[2], mse.5.5[2], mse.5.50[2], mse.5.100[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,5,50,100), c(mse.10.1[1], mse.10.5[1], mse.10.50[1], mse.10.100[1]), type = "b", xlab = "theta", ylab = "Mean Squared Error", col = "green", main = "Where n=10")
lines(c(1,5,50,100), c(mse.10.1[2], mse.10.5[2], mse.10.50[2], mse.10.100[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,5,50,100), c(mse.30.1[1], mse.30.5[1], mse.30.50[1], mse.30.100[1]), type = "b", xlab = "theta", ylab = "Mean Squared Error", col = "green", main = "Where n=30")
lines(c(1,5,50,100), c(mse.30.1[2], mse.30.5[2], mse.30.50[2], mse.30.100[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

# plotting with different values of n but same value of theta
plot(c(1,2,3,5,10,30), c(mse.1.1[1], mse.2.1[1], mse.3.1[1], mse.5.1[1], mse.10.1[1], mse.30.1[1]), type = "b", xlab = "n", ylab = "Mean Squared Error", col = "green", main = "Where theta=1")
lines(c(1,2,3,5,10,30), c(mse.1.1[2], mse.2.1[2], mse.3.1[2], mse.5.1[2], mse.10.1[2], mse.30.1[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,2,3,5,10,30), c(mse.1.5[1], mse.2.5[1], mse.3.5[1], mse.5.5[1], mse.10.5[1], mse.30.5[1]), type = "b", xlab = "n", ylab = "Mean Squared Error", col = "green", main = "Where theta=5")
lines(c(1,2,3,5,10,30), c(mse.1.5[2], mse.2.5[2], mse.3.5[2], mse.5.5[2], mse.10.5[2], mse.30.5[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,2,3,5,10,30), c(mse.1.50[1], mse.2.50[1], mse.3.50[1], mse.5.50[1], mse.10.50[1], mse.30.50[1]), type = "b", xlab = "n", ylab = "Mean Squared Error", col = "green", main = "Where theta=50")
lines(c(1,2,3,5,10,30), c(mse.1.50[2], mse.2.50[2], mse.3.50[2], mse.5.50[2], mse.10.50[2], mse.30.50[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

plot(c(1,2,3,5,10,30), c(mse.1.100[1], mse.2.100[1], mse.3.100[1], mse.5.100[1], mse.10.100[1], mse.30.100[1]), type = "b", xlab = "n", ylab = "Mean Squared Error", col = "green", main = "Where theta=100")
lines(c(1,2,3,5,10,30), c(mse.1.100[2], mse.2.100[2], mse.3.100[2], mse.5.100[2], mse.10.100[2], mse.30.100[2]), type = "b", col="blue")
legend("topright", legend = c("MOM", "MLE"), col=c("green","blue"), lty = 1, pch = 1, inset = 0.00, ncol = 1, cex=0.9, bty="n")

# negative log likelihood function

neg.loglik.fun <- function(par, dat)
{
  result <- length(dat) * log(par)-(par+1)*sum(log(dat))
  return(-result)
}

# initializing array with values
x <- c(21.72,14.65,50.42,28.78,11.23)

# using optim function to get MLE estimate
ml.est <- optim(par=1, fn=neg.loglik.fun, method = "L-BFGS-B", lower=0.01, hessian=TRUE, dat=x)
ml.est

# calculating the standard error
se <- (1/ml.est$hessian)^0.5
se

# finding the confidence interval of 95%
ml.est$par + c(-1,1)*se*qnorm(0.975)