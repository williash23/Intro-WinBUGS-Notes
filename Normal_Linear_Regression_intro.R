#  Normal Linear Regression - introduction and code (Chapter 8)
#  Kery - Introduction to WinBUGS for Ecologists (2010)
#  8/11/2015
################################################################################

#  Load packages and set up BUGS directories
library("R2WinBUGS")
library("lme4")
bd <- "C:/Program Files/WinBUGS14/"		
working.directory = getwd()
bugs.directory = bd

#  Remember the simple linear regression model algebraic description
#   y[i] = α + β * x[i] + ε[i]
#   The linear model described here is different than the one described for the t-test
#   because here x (the explanatory variable) can be continuous (i.e., it doesn't have to have 
#   a binary categorical value)

#  Pretend/easy data set generation for birds
n <- 16							# Number of years
a = 40							# Intercept
b = -1.5							# Slope
sigma2 = 25				# Residual variance

x <- 1:16 						# Values of covariate year
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
y <- a + b*x + eps		# Assemble data set
plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)

#  Analysis using R for frequentist approach
print(summary(lm(y ~ I(x+1989))))
abline(lm(y~ I(x+1989)), col = "blue", lwd = 2)

#  Analysis using WinBUGS (and R2WinBUGS) for Bayesian approach
#   Define BUGS model
sink("linreg.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)
 sigma ~ dunif(0, 100)

# Likelihood
 for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*x[i]
 }

# Derived quantities
 tau <- 1/ (sigma * sigma)
 p.decline <- 1-step(beta)		# Probability of decline

# Assess model fit using a sums-of-squares-type discrepancy
 for (i in 1:n) {
    residual[i] <- y[i]-mu[i]		# Residuals for observed data
    predicted[i] <- mu[i]				# Predicted values
    sq[i] <- pow(residual[i], 2)	# Squared residuals for observed data

# Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu[i], tau) # one new data set at each MCMC iteration
    sq.new[i] <- pow(y.new[i]-predicted[i], 2)	# Squared residuals for new data
 }
 fit <- sum(sq[])			# Sum of squared residuals for actual data set
 fit.new <- sum(sq.new[])		# Sum of squared residuals for new data set
 test <- step(fit.new - fit)		# Test whether new data set more extreme
 bpvalue <- mean(test)			# Bayesian p-value
}
",fill=TRUE)
sink()

#   Bundle data
win.data <- list("x","y", "n")

#   Inits function
inits <- function(){ list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1))}

#   Parameters to estimate
params <- c("alpha","beta", "p.decline", "sigma", "fit", "fit.new", "bpvalue", "residual", 
					 "predicted")

#  MCMC settings
nc = 3  ;  ni=1200  ;  nb=200  ;  nt=1

#  Start Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters = params, model = "linreg.txt",
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE)

print(out, dig = 3)

#  Goodness-of-Fit assessment in Bayesian analyses

#  Residual plot
#   One commonly produced graphical check of the residuals of a linear model is a plot 
#   of the residuals against the predicted values. Under the normal linear regression 
#   model, residuals are assumed to be a random sample from one single normal 
#   distribution. There should be no visible structure in the residuals. In particular, the 
#   scatterplot of the residuals should not have the shape of a fan which would indicate 
#   that the variance is not constant but is larger, or smaller, for larger responses.
plot(out$mean$predicted, out$mean$residual, main = "Residuals vs. predicted values",
las = 1, xlab = "Predicted values", ylab = "Residuals")
abline(h = 0)

#  Posterior predictive check and Bayesian p-value
lim <- c(0, 3200)
plot(out$sims.list$fit, out$sims.list$fit.new, main = "Graphical posterior predictive check",
		las = 1, xlab = "SSQ for actual data set", ylab = "SSQ for ideal (new) data sets", 
		xlim = lim, ylim = lim)
abline(0, 1)

mean(out$sims.list$fit.new > out$sims.list$fit)	# Bayesian p-value

#  Forming predictions
plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)
abline(lm(y~ I(x+1989)), col = "blue", lwd = 2)
pred.y <- out$mean$alpha + out$mean$beta * x
points(1990:2005, pred.y, type = "l", col = "red", lwd = 2)
text(1994, 20, labels = "blue - ML; red - MCMC", cex = 1.2)

predictions <- array(dim = c(length(x), length(out$sims.list$alpha)))
for(i in 1:length(x)){
   predictions[i,] <- out$sims.list$alpha + out$sims.list$beta*i
}
LPB <- apply(predictions, 1, quantile, probs = 0.025) # Lower bound
UPB <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound

plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)
points(1990:2005, out$mean$alpha + out$mean$beta * x, type = "l", col = "black", lwd = 2)
points(1990:2005, LPB, type = "l", col = "grey", lwd = 2)
points(1990:2005, UPB, type = "l", col = "grey", lwd = 2)
