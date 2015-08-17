#  Key Components of (Generalized) Linear Models - introduction and code (Chapters 6 & 7)
#  Kery - Introduction to WinBUGS for Ecologists (2010)
#  8/10/2015
################################################################################

#  Pretend/easy data set for snakes
mass <- c(6,8,5,7,9,11)
pop <- factor(c(1,1,2,2,3,3))
region <- factor(c(1,1,1,1,2,2))
hab <- factor(c(1,2,3,1,2,3))
svl <- c(40,45,39,50,52,57)

################################################################################
#  Linear model - model of the mean

#  Linear model formulation
lm(mass ~ 1)
	#   Covariate with a single value of 1 for all individual snakes
	#   Algebraically: individual's mass[i] = overall mean (intercept) +
	#								  individual's deviation from mean(i.e. residual)[i]
		#   Residual: unexplained, snake-specific contributions to an individual's mass
		#   Assumption that residuals are normally distributed around overall mean

	#  Look at design matrix
	model.matrix(mass~1)

################################################################################
#  t-Test (speficication of a t-Test as a linear model)
	#   Interested in the effect of a single, binary (categorical) explanatory variable (region)
	#   on a continuous response variable (mass).

#  Effects parameterization
	#   Region 1 is the baseline and the parameter shows the difference 
	#   between mass in region 2 and region 1 -- i.e., the effect of region 2 on mass.
lm(mass~region)		
	#  Algebraically: 
	#   individual's mass[i] = constant (intercept) (α) + another constant (β) * value for region  
	#												 of ind. [i] + ind.'s deviation (i.e. residual)[i]
			#   Assumption that residuals are normally distributed around 0

	#   Gives p-value (and other more important information)
	summary(lm(mass ~ region))

	#  Look at design matrix
	model.matrix(~region)

#  Notes
	#  The effects parameterization tests for a difference between the means in the
	#   two regions; this is equivalent to testing whether the effect of region 2, 
	#   i.e., parameter b, is equal to zero.

#  Means parameterization
lm(mass~region-1)		
	#   Parameters directly represent group means (i.e., the estimated expected mass 
	#   of snakes in each region)

	#  Look at design matrix
	model.matrix(~region-1)

################################################################################
#  Simple Linear Regression
	#   Interested in the effect of a continuous explanatory variable (svl) on a continuous 
	#   response variable (mass).

#  Effects parameterization
lm(mass~svl)
	#  Algebraically: 
	#   ind.'s mass[i] = α (intercept) + β * value for svl of ind. [i] + 
	#									 ind.'s deviation [i]

	#  Look at design matrix
	model.matrix(~svl)

#  Notes 
	#  The interpretation of the parameters α and β is thus that of a baseline, representing 
	#   the expected value of the response (mass) at a covariate value of svl = 0, and a 
	#   difference (or an effect) representing the change in mass for each unit change in svl.

	#  The intercept is biologically nonsense; it says that a snake of zero length
	#   weighs −5.6 mass units! This illustrates that a linear model can only ever be a
	#   useful characterization of a biological relationship over a restricted range of
	#   the explanatory variables.

	??? To make intercept sensible ???

################################################################################
#  One-way ANOVA
	#   Interested in the effect of a single (categorical) explanatory variable with 
	#	  (population) on a continuous response variable (mass). (If categorical variable
	#   has only 2 levels, you can use the t-test).

#  Effects parameterization
lm(mass~pop)
	#  Algebraically: 
	#   ind.'s mass[i] = α (intercept)+ β * value for pop. [j] of ind. [i] + 
	#									 ind.'s deviation [i]

	#  Look at design matrix
	model.matrix(~pop)

#  Means parameterization
lm(mass~pop-1)
	#  Algebraically: 
	#   ind.'s mass[i] = α (mean for mass in each pop. [j]) * pop. value for ind. [i] + 
	#									 ind.'s deviation [i]

	#  Look at design matrix
	model.matrix(~pop-1)

#  Notes
	#  Again, the interpretation of the first parameter of the model is the same for both 
	#   parameterizations: it is the mean body mass in population 1.
	#   However, while in the effects parameterization, the parameters 2 and 3
	#   correspond to differences in the means , but they represent the actual expected 
	#   body mass for snakes in each of the three populations in the means parameterization.

################################################################################
#  Two-way ANOVA
	#  Interested in the effect of two (categorical) explanatory variables 2+ levels (region 
	#   and habitat) on a continuous response variable (mass).

#  Notes
	#  Main- and interaction-effects are different ways of combining the effects of the two 
	#   explanatory variables. Main = additive; interaction = multiplicative.
	#   Interaction-effects lets the effect of one factor level depend on the level of 
	#   the other factor.

#  Effects parameterization (main-effects)
lm(mass ~ region + hab)
	#  Algebraically: Note simplified notation
	#   mass[i] = α + β[j,i] * region[i] + δ [k,i] * hab[i] + ε[i]
		#  Here, α is the expected mass of a snake in habitat 1 and region 1. There is only 1 
		#	  parameter β[j], so the subscript could as well be dropped. It specifies the difference
		#	  in the expected mass between snakes in region 2 and snakes in region 1. We need 2 
		#    parameters δ[k] to specify the differences in the expected mass for snakes in 
		#    habitats 2 and 3, respectively, relative to those in habitat 1.

		#  We arbitrarily set to zero the effects of one level for each factor. The effects of the
		#   remaining levels then get the interpretation of differences relative to the base level.
		#   It does not matter which level is used as a baseline or reference, but often 
		#   stats programs use the first or the last level of each factor. R sets the effects of the first 
		#   level to zero.

	#  Look at design matrix
	model.matrix(~region + hab)

#  Means parameterization (main-effects) 
	???  Not possible to specify  ???

#  Effects parameterization (interaction-effects)
lm(mass ~ region * hab)
	#  Algebraically:
	#   mass[i] = α + β[j,i] * region[i] + δ[k,i] * hab[i] + γ[j,k,i] * region[i] * hab[i] + ε[i]
		#  The new coefficients, γ[j,k] of which there are 2, specify the interaction effects 
		#   between these two factors.

	#  Look at design matrix
	model.matrix(~region * hab)
		
#  Means parameterization (interaction-effects) 
lm(mass ~ region * hab-1-region-hab)
	#  Algebraically:
	#   mass[i] = α[j,k,i] * region[i] * hab[i] + ε[i]
	
	#  Look at design matrix
	model.matrix(~ region * hab-1-region-hab)

################################################################################
#  ANCOVA
	#  Interested in the effect of both categorical and continuous explanatory variables (e.g., 
	#   pop. and svl) on a continuous response variable (mass).
	
#  Notes
	#  First, we may think that the relationship between mass and svl is the same in all 
	#   populations, or worded in another way, that the mass differences among populations
	#   do not depend on the length of the snake. In statistical terms, this would be 
	#   represented by a main-effects (additive) model. 
	
	#  Second if we admitted that the mass–length relationship might differ among
	#   populations or that the differences in mass among populations might depend on the 
	#   length of a snake, we would fit an interaction-effects (multiplicative) model.

#  Effects parameterization: main-effects (additive) model 
lm(mass ~ pop + svl)
	#  Algebraically:
	#   mass[i] = α + β[j,i] * pop[i] + δ * svl[i] + ε[i]
	
	#  Look at design matrix
	model.matrix(lm(mass ~ pop + svl))
		#  The intercept, signifies the expected mass in population 1 at the point 
		#   where the value of the covariate is equal to 0, i.e., for a snake of length 0.
		
#  Means parameterization: main-effects (additive) model 
	#  Algebraically
	#   mass[i] = α[j,i] * pop[i] + δ * svl[i] + ε[i]
	
	#  Look at design matrix
	model.matrix(lm(mass ~ pop + svl-1))
		#   The parameters associated with the first three columns in the design
		#   matrix now directly represent the intercepts for each population, while
		#   that associated with the fourth column denotes the common slope of the
		#   mass svl relationship
	
#  Effects parameterization: interaction-effects (multiplicative) model
lm(mass ~ pop * svl)	
	#  Algebraically
	#   mass[i] = α[j,i] * β[j,i] * pop[i] + δ * svl[i] + γ[j,i] * svl[i] * pop[i] + ε[i]
		
	#  Look at design matrix
	model.matrix(lm(mass ~ pop * svl))	
		#  The parameters associated with the first three columns in this design
		#   matrix signify the population 1 intercept and the effects of populations
		#   2 and 3, respectively, i.e., the difference in intercepts. The parameter associated
		#   with the fourth column, svl, is the slope of the mass–length regression
		#   in the first population, and the parameters associated with the last
		#   two columns are the differences between the slopes in populations 2
		#   and 3 relative to the slope in population 1

#  Means parameterization: interaction-effects (multiplicative) model
	#  Algebraically
	#   mass[i] = α[j,i] *  pop[i] + δ[j,i] * svl[i] + ε[i]
		#  The only change relative to the main-effects model is that we added a subscript
		#   j to the effect δ of svl, meaning that we now estimate three slopes δ, one for 
		#   each population, instead of a single one that is common to snakesfrom all three 
		#   populations.

	#  Look at design matrix
	model.matrix(lm(mass ~ (pop * svl-1-svl))) 
		#  This parameterization of the ANCOVA model with interaction between population 
		#   and svl contains parameters that have the direct interpretation as intercepts and 
		#   slopes of the three mass svl relationships (one in each population).

################################################################################
#  Script to run t-Test with equal variances with R2WinBUGS	

#  Load packages and set up BUGS directories
library("R2WinBUGS")
library("lme4")
bd <- "C:/Program Files/WinBUGS14/"		
working.directory = getwd()
bugs.directory = bd

#  Data generation - style 1
n1 <- 60				# Number of females
n2 <- 40				# Number of males
mu1 <- 105				# Population mean of females
mu2 <- 77.5				# Population mean of males
sigma <- 2.75				# Average population SD of both

n <- n1+n2				# Total sample size
y1 <- rnorm(n1, mu1, sigma)		# Data for females
y2 <- rnorm(n2, mu2, sigma)		# Date for males
y <- c(y1, y2)				# Aggregate both data sets
x <- rep(c(0,1), c(n1, n2))		# Indicator for male
boxplot(y ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)

#  Data generation - style 2
n <- n1+n2				# Total sample size
alpha <- mu1				# Mean for females serves as the intercept
beta <- mu2-mu1				# Beta is the difference male-female
E.y <- alpha + beta*x			# Expectation
y.obs <- rnorm(n = n, mean = E.y, sd = sigma)	# Add random variation
boxplot(y.obs ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)

#  Analysis using R for frequentist approach
fit1 <- lm(y ~ x)			# Analysis of first data set
fit2 <- lm(y.obs ~ x)			# Analysis of second data set
summary(fit1)
summary(fit2)

anova(fit1)
anova(fit2)

model.matrix(fit1)
model.matrix(fit2)

#  Analysis using WinBUGS (and R2WinBUGS) for Bayesian approach
#   Define BUGS model
sink("ttest.txt")
cat("
model {

# Priors
 mu1 ~ dnorm(0,0.001)			# Precision = 1/variance
 delta ~ dnorm(0,0.001)			# Large variance = Small precision
 tau <- 1/ (sigma * sigma)
 sigma ~ dunif(0, 10)

# Likelihood
 for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- mu1 + delta *x[i]
    residual[i] <- y[i] - mu[i]		# Define residuals
 }

# Derived quantities: one of the greatest things about a Bayesian analysis
 mu2 <- mu1 + delta			# Difference in wingspan
}
",fill=TRUE)
sink()

#   Bundle data
win.data <- list("x", "y", "n")

#   Inits function
inits <- function(){list(mu1=rnorm(1), delta=rnorm(1), sigma = rlnorm(1))}

#   Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma", "residual")

#   MCMC settings
nc <- 3		# Number of chains
ni <- 1000	# Number of draws from posterior for each chain
nb <- 1		# Number of draws to discard as burn-in
nt <- 1		# Thinning rate

#   Start Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters = params, model = "ttest.txt",
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE, working.directory = getwd())

print(out, dig = 3)

#   Plot residuals to check assumption that variances are equal
plot(1:100, out$mean$residual)
abline(h = 0)

boxplot(out$mean$residual ~ x, col = "grey", xlab = "Male", ylab = "Wingspan residuals (cm)", las = 1)
abline(h = 0)

################################################################################
#  Script to run t-Test with equal variances with R2WinBUGS	

#  Data generation
n1 <- 60				# Number of females
n2 <- 40				# Number of males
mu1 <- 105				# Population mean for females
mu2 <- 77.5				# Population mean for males
sigma1 <- 3				# Population SD for females
sigma2 <- 2.5				# Population SD for males

n <- n1+n2				# Total sample size
y1 <- rnorm(n1, mu1, sigma1)		# Data for females
y2 <- rnorm(n2, mu2, sigma2)		# Data for males
y <- c(y1, y2)				# Aggregate both data sets
x <- rep(c(0,1), c(n1, n2))		# Indicator for male
boxplot(y ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)


#  Analysis using R for frequentist approach
t.test(y ~ x)

#  Analysis using WinBUGS (and R2WinBUGS) for Bayesian approach
#  Define BUGS model
sink("h.ttest.txt")
cat("
model {

# Priors
 mu1 ~ dnorm(0,0.001)
 mu2 ~ dnorm(0,0.001)
 tau1 <- 1 / ( sigma1 * sigma1)
 sigma1 ~ dunif(0, 1000) 		# Note: Large var. = Small precision
 tau2 <- 1 / ( sigma2 * sigma2)
 sigma2 ~ dunif(0, 1000)

# Likelihood
 for (i in 1:n1) {
    y1[i] ~ dnorm(mu1, tau1)
 }

 for (i in 1:n2) {
    y2[i] ~ dnorm(mu2, tau2)
 }

# Derived quantities
 delta <- mu2 - mu1
}
",fill=TRUE)
sink()

#   Bundle data
win.data <- list("x", "y1", "y2", "n1", "n2")

#   Inits function
inits <- function(){ list(mu1=rnorm(1), mu2=rnorm(1), sigma1 = rlnorm(1), sigma2 = rlnorm(1))}

#   Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma1", "sigma2")

#   MCMC settings
nc <- 3					# Number of chains
ni <- 2000				# Number of draws from posterior for each chain
nb <- 500				# Number of draws to discard as burn-in
nt <- 1					# Thinning rate

#   Unleash Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters = params, model = "h.ttest.txt",
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE)

print(out, dig = 3)

#   Plot residuals to check assumption that variances are equal
plot(1:100, out$mean$residual)
abline(h = 0)

boxplot(out$mean$residual ~ x, col = "grey", xlab = "Male", ylab = "Wingspan residuals (cm)", las = 1)
abline(h = 0)