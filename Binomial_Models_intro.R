#  Binomial (logistic regression) - introduction and code (Chapter 17 & 18)
#  Kery - Introduction to WinBUGS for Ecologists (2010)
#  8/11/2015
################################################################################

#  Binomial distribution
#   Number of successes in N trials, where the Pr(success) = p.
#   r ~ Binomial (N,p)
#   special case: N=1 --- Bernoulli distribution

#  Binomial "t-Test" (i.e., logisitic regression contrasting 2 groups )
#   1. Statistical Distribution: C[i] ~ Binomial (N, p[i])
#   2. Link function: logit --- logit(p[i]) = log(p[i]/(1-p[i]))
#			Note: inverse logit --- inv. logit = exp(p)/(exp(p) +1)
#         i.e., this is the link for the linear predictor
#   3. Linear predictor: α + β * x[i]

#  Script to run binomial "t-Test" example with R2WinBUGS	
#  Load packages and set up BUGS directories
library("R2WinBUGS")
library("lme4")
bd <- "C:/Program Files/WinBUGS14/"		
working.directory = getwd()
bugs.directory = bd

#  Data generation 
N <- 50					# Binomial total (Number of coin flips)
p.cr <- 13/50				# Success probability Cross-leaved
p.ch <- 29/50				# Success probability Chiltern gentian

C.cr <- rbinom(1, 50, prob = p.cr)   ;   C.cr # Add Binomial noise
C.ch <- rbinom(1, 50, prob = p.ch)   ;   C.ch # Add Binomial noise
C <- c(C.cr, C.ch)
species <- factor(c(0,1), labels = c("Cross-leaved gentian","Chiltern gentian"))

#  Analysis using R for frequentist approach
summary(glm(cbind(C, N - C) ~ species, family = binomial))
predict(glm(cbind(C, N - C) ~ species, family = binomial), type = "response")

#  Analysis using WinBUGS (and R2WinBUGS) for Bayesian approach
#   Define BUGS model
sink("Binomial.t.test.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0,0.01)
 beta ~ dnorm(0,0.01)

# Likelihood
 for (i in 1:n) {
    C[i] ~ dbin(p[i], N) 		# Note p before N
    logit(p[i]) <- alpha + beta *species[i]
 }
# Derived quantities
 Occ.cross <- exp(alpha) / (1 + exp(alpha))
 Occ.chiltern <- exp(alpha + beta) / (1 + exp(alpha + beta))
 Occ.Diff <- Occ.chiltern - Occ.cross	# Test quantity
}
",fill=TRUE)
sink()

#  Bundle data
win.data <- list(C = C, N = 50, species = c(0,1), n = length(C))

#  Inits function
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1))}

#  Parameters to estimate
params <- c("alpha", "beta", "Occ.cross", "Occ.chiltern", "Occ.Diff")

#  MCMC settings
nc <- 3
ni <- 1200
nb <- 200
nt <- 2

#  Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params,
model.file="Binomial.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
n.iter=ni, debug = TRUE)

print(out, dig = 3)

par(mfrow = c(3,1))
hist(out$sims.list$Occ.cross, col = "grey", xlim = c(0,1), main = "",
	xlab = "Occupancy Cross-leaved", breaks = 30)
abline(v = out$mean$Occ.cross, lwd = 3, col = "red")
hist(out$sims.list$Occ.chiltern, col = "grey", xlim = c(0,1), main = "",
	xlab = "Occupancy Chiltern", breaks = 30)
abline(v = out$mean$Occ.chiltern, lwd = 3, col = "red")
hist(out$sims.list$Occ.Diff, col = "grey", xlim = c(0,1), main = "",
	xlab = "Difference in occupancy", breaks = 30)
abline(v = 0, lwd = 3, col = "red")

################################################################################
#  Binomial ANCOVA
#   Adds discrete and continuous covariates (explanatory variables) to the linear predictor 
#   of a binomial GLM (binomial response variable)
#   1. Statistical Distribution: C[i] ~ Binomial ( p[i], N[i])
#   2. Link function: logit --- logit(p[i]) = log(p[i]/(1-p[i]))
#			Note: inverse logit --- inv. logit = exp(p)/(exp(p) +1)
#         i.e., this is the link for the linear predictor
#   3. Linear predictor: α + β * x[i] + ...

#  Script to run binomial "ANCOVA" example with R2WinBUGS	
#  Load packages and set up BUGS directories
library("R2WinBUGS")
library("lme4")
bd <- "C:/Program Files/WinBUGS14/"		
working.directory = getwd()
bugs.directory = bd

#  Data generation
n.groups <- 3
n.sample <- 10
n <- n.groups * n.sample
x <- rep(1:n.groups, rep(n.sample, n.groups))
pop <- factor(x, labels = c("Jura", "Black Forest", "Alps"))

wetness.Jura <- sort(runif(n.sample, 0, 1))
wetness.BlackF <- sort(runif(n.sample, 0, 1))
wetness.Alps <- sort(runif(n.sample, 0, 1))
wetness <- c(wetness.Jura, wetness.BlackF, wetness.Alps)

N <- round(runif(n, 10, 50) )		# Get discrete Uniform values

Xmat <- model.matrix(~ pop*wetness)
print(Xmat, dig = 2)

beta.vec <- c(-4, 1, 2, 6, 2, -5)

lin.pred <- Xmat[,] %*% beta.vec	# Value of lin.predictor
exp.p <- exp(lin.pred) / (1 + exp(lin.pred)) # Expected proportion

C <- rbinom(n = n, size = N, prob = exp.p) # Add binomial noise
hist(C)					# Inspect what we've created

par(mfrow = c(2,1))
matplot(cbind(wetness[1:10], wetness[11:20], wetness[21:30]), cbind(exp.p[1:10],
exp.p[11:20], exp.p[21:30]), ylab = "Expected black", xlab = "", col = c("red","green","blue"),
pch = c("J","B","A"), lty = "solid", type = "b", las = 1, cex = 1.2, main = "", lwd = 1)

matplot(cbind(wetness[1:10], wetness[11:20], wetness[21:30]), cbind(C[1:10]/N[1:10],
C[11:20]/N[11:20], C[21:30]/N[21:30]), ylab = "Observed black", xlab = "Wetness index",
col = c("red","green","blue"), pch = c("J","B","A"), las = 1, cex = 1.2, main = "")
par(mfrow = c(1,1))

#  Analysis using R for frequentist approach
summary(glm(cbind(C, N-C) ~ pop * wetness, family = binomial))
beta.vec

#  Analysis using WinBUGS (and R2WinBUGS) for Bayesian approach
#   Define BUGS model
sink("glm.txt")
cat("
model {

# Priors
 for (i in 1:n.groups){
    alpha[i] ~ dnorm(0, 0.01)		# Intercepts
    beta[i] ~ dnorm(0, 0.01)		# Slopes
 }

# Likelihood
 for (i in 1:n) {
    C[i] ~ dbin(p[i], N[i])
    logit(p[i]) <- alpha[pop[i]] + beta[pop[i]]* wetness[i] # Baseline Jura

# Fit assessments: Pearson residuals and posterior predictive check
    Presi[i] <- (C[i]-N[i]*p[i]) / sqrt(N[i]*p[i]*(1-p[i]))	# Pearson residuals
    C.new[i] ~ dbin(p[i], N[i])		# Create replicate data set
    Presi.new[i] <- (C.new[i]-N[i]*p[i]) / sqrt(N[i]*p[i]*(1-p[i]))
 }

# Derived quantities
# Recover the effects relative to baseline level (no. 1)
 a.effe2 <- alpha[2] - alpha[1]		# Intercept Black Forest vs. Jura
 a.effe3 <- alpha[3] - alpha[1]		# Intercept Alps vs. Jura
 b.effe2 <- beta[2] - beta[1]		# Slope Black Forest vs. Jura
 b.effe3 <- beta[3] - beta[1]		# Slope Alps vs. Jura

# Custom tests
 test1 <- beta[3] - beta[2]		# Difference slope Alps -Black Forest

# Add up discrepancy measures
 fit <- sum(Presi[])
 fit.new <- sum(Presi.new[])
}
",fill=TRUE)
sink()


#   Bundle data
win.data <- list(C = C, N = N, pop = as.numeric(pop), n.groups = n.groups,
wetness = wetness, n = n)

#   Inits function
inits <- function(){ list(alpha = rlnorm(n.groups, 3, 1), beta = rlnorm(n.groups, 2, 1))} # Note log-normal inits

#   Parameters to estimate
params <- c("alpha", "beta", "a.effe2", "a.effe3", "b.effe2", "b.effe3",
"test1", "Presi", "fit", "fit.new")

#   MCMC settings
ni <- 1500
nb <- 500
nt <- 5
nc <- 3

#   Start Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters.to.save = params, model.file = "glm.txt",
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE)

par(mfrow = c(1,2), cex = 1.5)
plot(out$mean$Presi, ylab = "Residual", las = 1)
abline(h = 0)
plot(wetness, out$mean$Presi, ylab = "Residual", las = 1)
abline(h = 0)

plot(out$sims.list$fit, out$sims.list$fit.new, main = "", xlab = "Discrepancy actual data",
ylab = "Discrepancy ideal data")
abline(0,1, lwd = 2, col = "black")

mean(out$sims.list$fit.new > out$sims.list$fit)

print(out, dig = 3)			# Bayesian analysis
beta.vec				# Truth in data generation
print(glm(cbind(C, N-C) ~ pop * wetness, family = binomial)$coef, dig = 4) # The ML solution

