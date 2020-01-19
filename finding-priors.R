# Jags-Ydic-XmetMulti-Mlogistic.R 
# Accompanies the book:
#   Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition: 
#   A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.

source("DBDA2E-utilities.R")

#===============================================================================
genMCMC = function( data , xName="x" , yName="y" , 
                    numSavedSteps=10000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,yName]
  x = as.matrix(data[,xName],ncol=length(xName))
  # Do some checking that data make sense:
  if ( any( !is.finite(y) ) ) { stop("All y values must be finite.") }
  if ( any( !is.finite(x) ) ) { stop("All x values must be finite.") }
  cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
  show( round(cor(x),3) )
  cat("\n")
  flush.console()
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y ,
    Nx = dim(x)[2] ,
    Ntotal = dim(x)[1]
  )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "beta0" ,  "beta" ,  
                  "zbeta0" , "zbeta" )
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model="models/setting-priors-model.txt" ,
                          monitor=parameters ,
                          data=dataList ,
                          #inits=initsList ,
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps ,
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function

#===============================================================================
# Code Main

myData = read.csv( file="dataset/movie-dataset-with-scores.csv" )
# Taking a small sample of the dataset to find the prior
# In our case, we take all the movies that are released before 1970
# The prior isn't necessiraly the best, but it should work better for 
# our analysis than a random guess.
sampleData = myData[(myData[,"release_year"] < 2010) & (myData[,"release_year"] > 2000),]
yName = "target" 
xName = c('runtime', 'actor_name_score', 'studio_score','crew_member_score', 
          'season_autumn', 'season_spring','season_summer')
numSavedSteps=15000 ; thinSteps=2
fileNameRoot = 'finding-priors-'

# Running MCMC
mcmcCoda = genMCMC( data=sampleData , xName=xName , yName=yName , 
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps , 
                    saveName=fileNameRoot )
priors = as.matrix(mcmcCoda)
pNames = c("zbeta0", "zbeta[1]", "zbeta[2]", "zbeta[3]", "zbeta[4]", 
           "zbeta[5]", "zbeta[6]", "zbeta[7]")
scaledPriors = priors[, pNames]
scaledNames = c('beta0','runtime', 'actor_name_score', 'studio_score',
                'crew_member_score', 'season_autumn', 'season_spring',
                'season_summer')
colnames(scaledPriors) = scaledNames
write.table(scaledPriors, file = "dataset/movie-dataset-priors.csv", sep = ",", col.names = TRUE,
            qmethod = "double", row.names = FALSE)