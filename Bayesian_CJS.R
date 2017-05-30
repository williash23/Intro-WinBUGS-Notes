 # Intro to JAGS and Bayesian CJS
  # WILD 595 - Lab
  # Edited by Anna Moeller 
  # 9/23/2016

  # Load packages
  library(mcmcplots)
  library(R2jags)
  
################
  # Prepare data
  
  # read data file 
  dipper <- read.csv( "School/Demographic Parameters/Lab Day 4 Bayesian CJS/Dipper.csv" )
  
  # Subset data to make encounter history matrix
  EH <- as.matrix(dipper[, 1:7])
  
  # Define your dimensions
  #   Everything later will be a matrix with nAnimal rows and nYears columns
  nYears <- ncol( EH )
  nAnimal <- nrow( EH )
  
  # Make a vector of the first capture occasions.
  # This is used to:
  #   1. Create initial values for z
  #   2. Go into the model, so you can assign a 1 to the first capture and then 
  #      keep working from there (but not before)

  # Paul's original way of writing this
  # # Determine when each individual was first captured
  # get.first <- function(x){
  # # a function to identify the time period of the first capture from an encounter history matrix
  #   return( min(which( x != 0 ) ) )
  # }
  # # Run get first
  # fall <- apply( EH, 1, get.first )

  # For some reason this makes more sense to me than the way Paul wrote it
  first <- apply(EH, 1, function(x){
    min(which(x != 0))
  })
  
  # We need to remove animals that were first captured in the last year
  #   This is because we condition on first capture, so these don't add
  #   anything to our analysis
  EH <- EH[-which(first == nYears), ]
  f <- first[-which(first == nYears)]
  
  # Redefine number of individuals because now we deleted some
  nAnimal <- nrow( EH )
  
  # Write a function to generate initial values for z
  # z has to agree with y (NA before the animal was captured and 1 after that)
  #   This is because we never observe death and the animal is always potentially alive
  # You have to always assume it could be alive, because if you start with z = 0, 
  #   your math will become terrible
  # You can't give an initial value to the first occasion because you assign it in the model
  #   with z[f] <- 1
  #   Assigned things CANNOT have initial values (even though technically it will still run)
  z.init.fn <- function(ch, f){
    for( i in 1:nrow(ch) ){
      ch[i, 1:(f[i])] <- NA
      ch[i, (f[i] + 1):ncol(ch)] <- 1
    }
    return(ch)
  }
  # z.init <- z.init.fn( EH, f ) 
  # We don't have to call this here because we can call the function in the 
  #   list of initial values to feed to JAGS (I tested it and it works)
  
#########################
  # Run constant model (phidot.pdot)
  # 2 parameters: phi and p
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = f, nind = nAnimal, nocc = nYears)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, f),
      b0.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3)
    )
  }

  # Parameters to monitor in JAGS
  cjs.params <- c("b0.phi", "b0.p","mean.phi", "mean.p")
  
  # MCMC specifications
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phidot_pdot_res <- jags( cjs.data, 
                      cjs.inits,
                      cjs.params,
                      "School/Demographic Parameters/Lab Day 4 Bayesian CJS/cjs_phidot_pdot.txt",
                      n.chains=nc, 
                      n.iter=ni, 
                      n.burnin=nb,
                      n.thin=nt
                    )
  
  # Look at results
  phidot_pdot_res
  mcmcplot(phidot_pdot_res)
  
######################
# Run phitime_ptime
  # 12 parameters: phi[t] and p[t] (t = 1:6)
  
  # Change initial values to a vector of length nYears-1 (there are 7 occasions 
  #   (6 recapture occasions) so we need 6 initial values)
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, f),
      b0.phi = runif(nYears - 1, -3, 3),
      b0.p = runif(nYears - 1, -3, 3)
    )
  }
  
  # run the MCMC chain in JAGS
  phitime_ptime_res <- jags( cjs.data, 
                      cjs.inits,
                      cjs.params,
                      "School/Demographic Parameters/Lab Day 4 Bayesian CJS/cjs_phitime_ptime.txt",
                      n.chains=nc, 
                      n.iter=ni, 
                      n.burnin=nb,
                      n.thin=nt
  )
  
  # Look at results
  phitime_ptime_res
  mcmcplot(phitime_ptime_res)

#######################  
# Run phitimesex_ptime
  # 13 parameters: phi[t] (for females), phi[t] + b1 (for males), p[t]
  #    6 + 6 + 1 = 13
  
  cjs.data <- list( y = EH, f = f, nind = nAnimal, nocc = nYears, male = dipper$male)
  
  # Reduce the number of parameters to track because there are a lot now
  # Also monitor our sex parameter
  cjs.params <- c("b0.phi", "b0.p", "b1.sex")
  
  # Add an initial value for b1.sex
  # Need an initial value for every parameter (each sex-year for phi and year for p)
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, f),
      b0.phi = runif(nYears - 1, -3, 3),
      b0.p = runif(nYears - 1, -3, 3),
      b1.sex = runif(1, -3, 3)
    )
  }
  
  # run the MCMC chain in JAGS
  phitime_add_sex_ptime_res <- jags( cjs.data, 
                             cjs.inits,
                             cjs.params,
                             "School/Demographic Parameters/Lab Day 4 Bayesian CJS/cjs_phitime+sex_ptime.txt",
                             n.chains=nc, 
                             n.iter=ni, 
                             n.burnin=nb,
                             n.thin=nt
  )
  
  # Look at results
  phitime_add_sex_ptime_res
  mcmcplot(phitime_add_sex_ptime_res)
  
#################  
  #### I did this one on my own! 
# Run phisex_psex
  # 4 parameters: phi = b0 + b1*male, p = b2 + b3*male
  
  # Gather data
  cjs.data <- list( y = EH, f = f, nind = nAnimal, nocc = nYears, male = dipper$male)
  
  # Parameters to monitor
  cjs.params <- c("b0.phi", "b1.phi", "b0.p", "b1.p")
    
  # Initial values
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, f),
      b0.phi = runif(1, -3, 3),
      b1.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3),
      b1.p = runif(1, -3, 3)
    )
  }
  
  # Run in JAGS
  # run the MCMC chain in JAGS
  phisex_psex_res <- jags( cjs.data, 
                           cjs.inits,
                           cjs.params,
                           "School/Demographic Parameters/Lab Day 4 Bayesian CJS/cjs_phisex_psex.txt",
                           n.chains=nc, 
                           n.iter=ni, 
                           n.burnin=nb,
                           n.thin=nt
  )
  
  # View results
  phisex_psex_res
  
###################################
  # Write a model for flood effects
  # phi = b0 + b1*flood (only affects phi3)
  
  # This is probably a ridiculous way to do this but here we go
  # Corresponds with indexing on phi (1-6)
  flood <- c(0, 0, 1, 0, 0, 0)
  
  # Gather data
  cjs.data <- list(y = EH, f = f, nind = nAnimal, nocc = nYears, flood = flood)
  
  # Parameters to monitor
  cjs.params <- c("b0.phi", "b1.phi", "b0.p")
  
  # Initial values
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, f),
      b0.phi = runif(1, -3, 3),
      b1.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3)
    )
  }
  
  # Run in JAGS
  # run the MCMC chain in JAGS
  phiflood_pdot_res <- jags( cjs.data, 
                           cjs.inits,
                           cjs.params,
                           "School/Demographic Parameters/Lab Day 4 Bayesian CJS/cjs_phiflood_pdot.txt",
                           n.chains=nc, 
                           n.iter=ni, 
                           n.burnin=nb,
                           n.thin=nt
  )
  
  # View results
  phiflood_pdot_res
  