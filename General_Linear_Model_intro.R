#  General Linear Model for ANCOVA - introduction and code (Chapter 11)
#  Kery - Introduction to WinBUGS for Ecologists (2010)
#  8/12/2015
################################################################################

#  General Linear Model  (!= GLM)
#   Expresses continuous response variable as a linear combination of the effects of 
#   discrete and/or continuous explanatory variables plis a single random contribution 
#   from a normal distribution, whose variance is estimated. 
#  Algebraic description (means parameterization)
#   y[i] = α[j,i] + β[j,i] * x[i] + ε[i]
#		j: populations A, B, C 		i: individual
#		so α[j,i] --- means the α (intercept) for the population that i is in
#		Assumption that residuals are normally distributed around 0.
#  Algebraic description (effects parameterization)
#   y[i] = αA + β1 * x[B,i] +  β2 * x[C,i] +  β3 * x[body,i] +  β4 * x[B, i] * x[body,i] +  
#				β5 * x[C, i] * x[body,i] + ε[i]
#		Assumption that residuals are normally distributed around 0.

################################################################################
#  Script to run general linear model 

#  Load packages and set up BUGS directories
library("R2WinBUGS")
library("lme4")
bd <- "C:/Program Files/WinBUGS14/"		
working.directory = getwd()
bugs.directory = bd

#  Data generation
n.groups <- 3
n.sample <- 10
n <- n.groups * n.sample		# Total number of data points
x <- rep(1:n.groups, rep(n.sample, n.groups)) # Indicator for population
pop <- factor(x, labels = c("Pyrenees", "Massif Central", "Jura"))
length <- runif(n, 45, 70)		# Obs. body length (cm) is rarely less than 45

Xmat <- model.matrix(~ pop*length)
print(Xmat, dig = 2)
beta.vec <- c(-250, 150, 200, 6, -3, -4)

lin.pred <- Xmat[,] %*% beta.vec	# Value of lin.predictor
eps <- rnorm(n = n, mean = 0, sd = 10)	# residuals
mass <- lin.pred + eps			# response = lin.pred + residual
hist(mass)				# Inspect what weâ€™ve created
matplot(cbind(length[1:10], length[11:20], length[21:30]), cbind(mass[1:10], mass[11:20],
mass[21:30]), ylim = c(0, max(mass)), ylab = "Body mass (g)", xlab = "Body length (cm)",
col = c("Red","Green","Blue"), pch = c("P","M","J"), las = 1, cex = 1.2, cex.lab = 1.5)

#  Analysis using R for frequentist approach
summary(lm(mass ~ pop * length))

beta.vec
cat("And the residual SD was 10 \n")

#  Analysis using WinBUGS (and R2WinBUGS) for Bayesian approach
#   Define BUGS model
sink("lm.txt")
cat("
model {

# Priors
 for (i in 1:n.group){
    alpha[i] ~ dnorm(0, 0.001)		# Intercepts
    beta[i] ~ dnorm(0, 0.001)		# Slopes
 }
 sigma ~ dunif(0, 100)			# Residual standard deviation
 tau <- 1 / ( sigma * sigma)

# Likelihood
 for (i in 1:n) {
    mass[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[pop[i]] + beta[pop[i]]* length[i]
 }

# Derived quantities
# Define effects relative to baseline level
 a.effe2 <- alpha[2] - alpha[1]		# Intercept Massif Central vs. Pyr.
 a.effe3 <- alpha[3] - alpha[1]		# Intercept Jura vs. Pyr.
 b.effe2 <- beta[2] - beta[1]		# Slope Massif Central vs. Pyr.
 b.effe3 <- beta[3] - beta[1]		# Slope Jura vs. Pyr.

# Custom tests
 test1 <- beta[3] - beta[2]		# Slope Jura vs. Massif Central
}
",fill=TRUE)
sink()

#   Note here: length covariate is not centered and scaled so estimates are off
#   Bundle data
win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop),
length = length, n.group = max(as.numeric(pop)), n = n)

#   Inits function
inits <- function(){ list(alpha = rnorm(n.groups, 0, 2),
beta = rnorm(n.groups, 1, 1), sigma = rlnorm(1))}

#   Parameters to estimate
parameters <- c("alpha", "beta", "sigma", "a.effe2", "a.effe3",
"b.effe2", "b.effe3", "test1")

#   MCMC settings
ni <- 1200
nb <- 200
nt <- 2
nc <- 3

#   Start Markov chains
out <- bugs(win.data, inits, parameters, "lm.txt", n.thin=nt,
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 3)			# Bayesian analysis
beta.vec				# Truth in the data-generating process
summary(lm(mass ~ pop * length))	# The ML solution again

#   Note here: length covariate IS centered and scaled so estimates are  good
#   Data passed to WinBUGS
win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop),
length = as.numeric(scale(length)), n.group = max(as.numeric(pop)), n = n)

#   Start Markov chains
out <- bugs(win.data, inits, parameters, "lm.txt", n.thin=nt,
n.chains=nc, n.burnin=nb, n.iter=ni, debug = FALSE)

#  Inspect results
print(out, dig = 3)
print(lm(mass ~ pop * as.numeric(scale(length)))$coefficients, dig = 4)



