#  Using data simulation to to look at effect size and sample size impacts on being able to 
#   detect an impact of covariate 
#   i.e., if the effect of the covariate is X in truth, how large of sample size do we need to be
#   able to detect it? How does variance change this?
#  Adapated from Kery and Schaub 2010.
################################################################################

# =============================================================================
#  Load packages
# =============================================================================

library("R2WinBUGS")
library("lme4")
bd <- "C:/Program Files/WinBUGS14/"		


	#  Remember the simple linear regression model algebraic description
	#   y[i] = α + β * x[i] + ε[i]



# =============================================================================
#  Simulate a data set with a covariate. These are values that we would know from out data
#  collection process. Example question: does length have an impact on mass of adults?
# =============================================================================
	
	# ----------------------
	#  Number of habitats
	n_hab <- 2
	
	# ----------------------
	#  Number of sample units (individuals) per habitat
	n_samp <- 20
	
	# ----------------------
	#  Total number of data points
	n <- n_hab * n_samp		
	
	# ----------------------
	# Assign individuals (1 to n_samp) to habitats
	x <- rep(1:n_hab, rep(n_samp, n_hab))
	
	# ----------------------
	# Generate length values using a uniform distribution
	length <- runif(n, 45, 70)

	# ----------------------
	# Center and scale covariate
	length_sc <- as.numeric(scale(length, center = TRUE))

	
	
# =============================================================================
#  Simulate "Truth". We would not know these values but we are setting up a "truth" situation
#   so that we can compare impacts of sample size, effect size, and variance.
# =============================================================================
	
	# ----------------------
	# Effect size of covariate mass (our explanatory variable)
	beta_val <- 2
	
	# ----------------------
	#  Intercept (starting point) for mass (our response variable)
	int <- 1
	
	# ----------------------
	#  Random variation in mass (our response variable)
	eps <- rnorm(n = n, mean = 0, sd = 10)
	
	# ----------------------
	#  Make a holder for generated observations
	mass <- matrix(nrow = n, ncol = 1)
	
	# ----------------------
	#  Generate our measured mass (response variable) using our known effect of covariate
	#   and random variation
	for(j in 1:n){
		mass[j] <- int + beta_val*length[j] + eps[j]
		}
	

	
	
# =============================================================================
#  Set up model in WinBUGS language.
# =============================================================================
	
	# ----------------------
	#  Define model
	sink("lm.txt")
	cat("
	model {

	# ----------------------
	# Set up uninformative priors
	 for (i in 1:n_hab){
		
		# ----------------------
		# Intercept
		alpha[i] ~ dnorm(0, 0.001)	

		# ----------------------
		# Effect of covariate
		beta[i] ~ dnorm(0, 0.001)	
		
		}
	 
		# ----------------------
		# Residual variance
		sigma ~ dunif(0, 100)			
		tau <- 1 / ( sigma * sigma)

	# ----------------------
	# Specific likelihood (linear predictor)
	 for (i in 1:n) {
	
		# ----------------------
		# Distribution from which observations occur
		mass[i] ~ dnorm(mu[i], tau)
		
		# ----------------------
		# Linear predictor of how the covariate impacts 
		#   the observations from the distribution
		mu[i] <- alpha[] + beta[length_sc[i]]
		
		}

	# ----------------------
	#  Derived quantities; defining effects relative to baseline level
	 a_effe2 <- alpha[2] - alpha[1]		
	 a_effe3 <- alpha[3] - alpha[1]		
	 b_effe2 <- beta[2] - beta[1]		
	 b_effe3 <- beta[3] - beta[1]		

	# ----------------------
	# Custom tests
	 test1 <- beta[3] - beta[2]		
	 
	}
	",fill=TRUE)
	sink()



# =============================================================================
#  Run model
# =============================================================================
	
	# ----------------------
	# Bundle data
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
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
# =============================================================================
#  Simulate a data set with a covariate.
#   Our question: does length have an impact on mass of adults?
# =============================================================================
	
	# ----------------------
	#  Number of habitats
	n_hab <- 2
	
	# ----------------------
	#  Number of sample units (individuals) per habitat
	n_samp <- 20
	
	# ----------------------
	#  Total number of data points
	n <- n_hab * n_samp		
	
	# ----------------------
	# Assign individuals (1 to n_samp) to habitats
	x <- rep(1:n_hab, rep(n_samp, n_hab))
	
	# ----------------------
	# Name habitats so they're easy to see
	#pop <- factor(x, labels = c("Urban", "Forest"))
	
	# ----------------------
	# Generate length values using a uniform distribution
	length <- runif(n, 45, 70)		


# =============================================================================
#  Set up model in WinBUGS language.
# =============================================================================
	
	# ----------------------
	#  Define model
	sink("lm.txt")
	cat("
	model {

	# ----------------------
	# Set up uninformative priors
	 for (i in 1:n_hab){
		
		# ----------------------
		# Intercept
		alpha[i] ~ dnorm(0, 0.001)	

		# ----------------------
		# Effect of covariate
		beta[i] ~ dnorm(0, 0.001)	
		
		}
	 
		# ----------------------
		# Residual variance
		sigma ~ dunif(0, 100)			
		tau <- 1 / ( sigma * sigma)

	# ----------------------
	# Specific likelihood (linear predictor)
	 for (i in 1:n) {
	
		# ----------------------
		# Distribution from which observations occur
		mass[i] ~ dnorm(mu[i], tau)
		
		# ----------------------
		# Linear predictor of how the covariate impacts 
		#   the observations from the distribution
		mu[i] <- alpha[pop[i]] + beta[pop[i]]* length[i]
		
		}

	# ----------------------
	#  Derived quantities; defining effects relative to baseline level
	 a_effe2 <- alpha[2] - alpha[1]		
	 a_effe3 <- alpha[3] - alpha[1]		
	 b_effe2 <- beta[2] - beta[1]		
	 b_effe3 <- beta[3] - beta[1]		

	# ----------------------
	# Custom tests
	 test1 <- beta[3] - beta[2]		
	 
	}
	",fill=TRUE)
	sink()

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

#  Pretend/easy data set generation for birds

#  Sample Size
n <- 

#  Covariate values


x <- 1:16 						# Values of covariate year
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
y <- a + b*x + eps		# Assemble data set










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
