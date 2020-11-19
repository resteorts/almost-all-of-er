library(blink)
library(tidyverse)

################
# PREPARE DATA #
################
#df <- read.csv("sv-standard.csv")
#df <- read.csv("untrc.csv")
df <- read.csv("./sv-mauricio/sv-mauricio.csv")
#df <- df[-c(1,2)]
ent_id <- df$HandID

# Filter out records with ground truth, leaving dept 1 and 7
df <- df[!is.na(ent_id),]
ent_id <- ent_id[!is.na(ent_id)]


df$ID <- 1:nrow(df)
numRecords <- nrow(df)

# Get list of known first names and last names
knownFirstname <- read_csv("./sv-mauricio/known-firstname.csv")
knownFirstname <- knownFirstname$token[knownFirstname$known == 1]
knownLastname <- read_csv("./sv-mauricio/known-lastname.csv")
knownLastname <- knownLastname$token[knownLastname$known == 1]

# Get neighboring departments
deptCodes <- read_csv("./sv-mauricio/code-dept.csv")
deptNeighbors <- read_csv("./sv-mauricio/neighboring-dept.csv") %>% 
  inner_join(deptCodes, by = c("DEPT1" = "DEPT")) %>% 
  inner_join(deptCodes, by = c("DEPT2" = "DEPT")) %>%
  transmute(dept1 = as.character(as.numeric(CODE.x)), 
            dept2 = as.character(as.numeric(CODE.y)), sim = 1.0)
# Add in reverse pairs
deptNeighbors <- bind_rows(deptNeighbors, deptNeighbors %>% rename(dept1 = dept2, dept2 = dept1))

# Get neighboring municipalities
muniCodes <- read_csv("./sv-mauricio/code-muni.csv")
muniNeighbors <- read_csv("./sv-mauricio/neighboring-muni.csv") %>% 
  inner_join(muniCodes, by = c("MUNI1" = "MUNI")) %>% 
  inner_join(muniCodes, by = c("MUNI2" = "MUNI")) %>%
  transmute(muni1 = as.character(as.numeric(CODE.x)), 
            muni2 = as.character(as.numeric(CODE.y)), sim = 1.0)
# Add in reverse pairs
muniNeighbors <- bind_rows(muniNeighbors, muniNeighbors %>% rename(muni1 = muni2, muni2 = muni1))

####################################
# CUSTOM SIM FUNCTIONS             #
####################################

#' Similarity function for Hispanic names
#'
#' @param x a character vector
#' @param y a character vector
#' @param sep separator for tokens/words (uses white space by default)
#' @param knownTokens a character vector of known tokens (default is NULL)
#' @returns a length(x) × length(y) similarity matrix
unitHispanicSimilarity <- function(x, y, sep = '\\s+', knownTokens = NULL) {
  # Split into tokens (words)
  tokens1 <- strsplit(x, sep)
  tokens2 <- strsplit(y, sep)
  
  # Preallocate similarity matrix for output
  out <- matrix(0.0, nrow = length(tokens1), ncol = length(tokens2))
  
    if (!is.null(knownTokens)) {
    # Convert known tokens to environment for faster look-up
    knownList <- setNames(replicate(length(knownTokens), 1, simplify = FALSE), knownTokens)
    knownEnv <- list2env(knownList, hash = TRUE, size = length(knownList))
  }
  
  # Function to compute the symmetrized Monge-Elkan similarity for a single 
  # pair of tokens
  meSim <- function(t1, t2) {
    maxSim1 <- numeric(length=length(t1))
    knownDistinct1 <- logical(length=length(t1))
    maxSim2 <- numeric(length=length(t2))
    knownDistinct2 <- logical(length=length(t2))
    for (i in seq_along(t1)) {
      for (j in seq_along(t2)) {
        sim <- unitLevenshteinSimilarity(t1[i], t2[j])
        bothKnownDistinct <- FALSE
        if (!is.null(knownTokens) && t1[i] != t2[j] &&
            exists(t1[i], envir = knownEnv, inherits = FALSE) && 
            exists(t2[i], envir = knownEnv, inherits = FALSE)) {
          bothKnownDistinct <- TRUE
        }
        if (sim > maxSim1[i]) { maxSim1[i] <- sim; knownDistinct1[i] <- bothKnownDistinct }
        if (sim > maxSim2[j]) { maxSim2[j] <- sim; knownDistinct2[j] <- bothKnownDistinct }
      }
    }
    maxSim1 <- ifelse(knownDistinct1, 0, maxSim1)
    maxSim2 <- ifelse(knownDistinct2, 0, maxSim2)
    # Symmetrize
    return(max(length(t1)/sum(1.0/maxSim1), length(t2)/sum(1.0/maxSim2)))
  }
  
  # Function to compute an asymmetric similarity for a single pair of tokens 
  asymSim <- function(t1, t2) {
    if (length(t1) < length(t2)) {
      # If t2 contains extra tokens, similarity is zero (can't distort 
      # true name by adding names)
      return(0)
    } else {
      # Get symmetrized Monge-Elkan similarity
      me <- meSim(t1, t2)
      # Assign 0.95 weight to Monge-Elkan and 0.05 weight to num. tokens 
      # similarity
      #return(1.0/(0.95/me + 0.05*length(t1)/length(t2)))
      return(me)
    }
  }
  
  # Loop over all combinations in input character vectors
  for (i in seq_len(length(tokens1))) {
    for (j in seq_len(length(tokens2))) {
      out[i, j] <- asymSim(tokens1[[i]], tokens2[[j]])
    }
  }
  
  return(out)
}

# Function below is currently unused
#' Similarity function for Hispanic names
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param ran range used to normalize score. Default option of 'NULL' uses  
#' the range of c(x, y).
#' @returns a length(x) × length(y) similarity matrix
unitAbsoluteDifference <- function(x, y, ran = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (is.null(ran)) {
    ran <- range(c(x, y))
    ran <- ran[2] - ran[1]
  }
  if (!is.numeric.scalar(ran) || ran <= 0) { stop("invalid range") } 
  
  out <- matrix(x, nrow=length(x), ncol=length(y))
  out <- sweep(out, 2, y, FUN = "-")
  out <- 1.0 - abs(out/ran)
  
  if (any(out > 1.0)) warning("scores not on the unit interval")
  
  return(out)
}

#' Similarity function using a lookup table
#' 
#' @param x a named list containing two vectors of values to compare
#' @param lookupTable data frame containing similarities for pairs of values
#' @param simColname name of column containing similarities
#' @param defaultSim similarity to use if value pair is not found in the 
#' lookup table. Defaults to NA.
#' @param matchSim similarity to use if value pair is an exact match. Defaults 
#' to NA.
#' @returns a pairwise similarity matrix
lookupSimilarity <- function(x, lookupTable, simColname, defaultSim = NA, 
                             matchSim = NA) {
  if (!is.list(x) || is.null(names(x)) || length(x) != 2)
    stop("x must be a named list containing two vectors")
  if (!is.character(simColname) || !is.scalar(simColname))
    stop("simColname must be a scalar character")
  if (!is.numeric.scalar(defaultSim))
    stop("defaultSim must be a numeric scalar")
  vectorNames <- names(x)
  if (!all(vectorNames %in% colnames(lookupTable)))
    stop("names(x) must be a subset of colnames(lookupTable)")
  if (!(simColname %in% colnames(lookupTable)))
    stop("simColname doesn't exist in lookupTable")
  if (!is.numeric(lookupTable[[simColname]]))
    stop("simColname in lookupTable must be numeric")
  
  duplicatedRows <- duplicated(lookupTable[vectorNames])
  if (any(duplicatedRows)) warning("removing duplicated rows in lookupTable")
  lookupTable <- lookupTable[!duplicatedRows,]
  
  # Generate all combinations for the vectors in x
  valuePairs <- expand.grid(x, stringsAsFactors=FALSE)
  # Fill in values for combinations using lookupTable
  valuePairs <- merge(valuePairs, lookupTable, by=vectorNames, all.x=TRUE, all.y=FALSE)
  if (!is.na(matchSim)) valuePairs[valuePairs[[vectorNames[1]]] == valuePairs[[vectorNames[2]]], simColname] <- matchSim 
  # Keep only the values
  values <- valuePairs[[simColname]]
  rm(valuePairs)
  
  # Fill NAs with a defaultSim if given
  if (!is.na(defaultSim)) values[is.na(values)] <- defaultSim
  dim(values) <- sapply(x, length)
  return(values)
}

#################
# PREPARE MODEL #
#################
#clusteringPrior <- PYPPrior(1.0, 0.8)
clusteringPrior <- DPPrior(1.0)
#clusteringPrior <- CouponCollectingPrior(numRecords)

lowDistortionPrior <- BetaShapeParameters(1.0, numRecords * 0.1)
modDistortionPrior <- BetaShapeParameters(1.0, numRecords * 0.1 * 0.2)

firstnameSimFn <- SimilarityFn(maxSimilarity = 50.0, threshold = 35.0, 
                               unitSimilarity = function(x, y) unitHispanicSimilarity(x, y, knownTokens = knownFirstname))
lastnameSimFn <- SimilarityFn(maxSimilarity = 50.0, threshold = 35.0, 
                              unitSimilarity = function(x, y) unitHispanicSimilarity(x, y, knownTokens = knownLastname))

muniSimFn <- SimilarityFn(maxSimilarity = 10.0, threshold = 7.0, 
                          unitSimilarity = function(x, y) lookupSimilarity(list("muni1" = x, "muni2" = y), lookupTable = muniNeighbors, simColname = "sim", defaultSim = 0, matchSim = 1))

deptSimFn <- SimilarityFn(maxSimilarity = 10.0, threshold = 7.0, 
                          unitSimilarity = function(x, y) lookupSimilarity(list("dept1" = x, "dept2" = y), lookupTable = deptNeighbors, simColname = "sim", defaultSim = 0, matchSim = 1))

# Note increasing inverse temperature in string distribution seems to improve 
# accuracy.
attributeSpecs <- c(
  #Attribute("firstname", LevenshteinSimilarityFn(30.0, 21.0), distortionPrior),
  #Attribute("lastname", LevenshteinSimilarityFn(30.0, 21.0), distortionPrior),
  Attribute("firstname", firstnameSimFn, lowDistortionPrior),
  Attribute("lastname", lastnameSimFn, lowDistortionPrior),
  #CategoricalAttribute("dept", lowDistortionPrior),
  #CategoricalAttribute("muni", lowDistortionPrior),
  Attribute("dept", deptSimFn, lowDistortionPrior),
  Attribute("muni", muniSimFn, lowDistortionPrior),
  #Attribute("day", LevenshteinSimilarityFn(10.0, 7.0), lowDistortionPrior),
  #Attribute("month", LevenshteinSimilarityFn(10.0, 7.0), lowDistortionPrior),
  #Attribute("year", LevenshteinSimilarityFn(10.0, 7.0), lowDistortionPrior)
  CategoricalAttribute("day", modDistortionPrior),
  CategoricalAttribute("month", modDistortionPrior),
  CategoricalAttribute("year", modDistortionPrior)
)

system.time(
state <- blink2::initialize(df, integer(length=numRecords), attributeSpecs, clusteringPrior)
)

#################
# RUN INFERENCE #
#################
# For d-blink we used a thinning interval of 10, burn-in = 10000, number of 
# samples = 9000 (possibly overkill)
result <- blink2::sample(state, numSamples=100, thinningInterval=10, burninInterval=2000)

# Uncomment to add more samples
#result <- blink2::sample(result, numSamples=100, thinningInterval=10, burninInterval=0)

###############
# DIAGNOSTICS #
###############
# Trace plot for number of unique entities represented in the data
bind_cols(iteration = result@history$iteration, 
          numLinkedEntities = result@history$numLinkedEntities) %>% 
  ggplot(aes(x = iteration, y = numLinkedEntities)) + geom_line() + 
  labs(x = "Iteration", y = "# linked entities")

# Trace plot for amount of distortion in the record attributes
# Check that the distortion has stabilized for each attribute, and that it is 
# not too high (e.g. below ~50%)
bind_cols(iteration = result@history$iteration, 
          as_tibble(result@history$numDistortions/numRecords*100)) %>% 
  gather(key = "attribute", value="percDistorted", -iteration) %>% 
  ggplot(aes(x = iteration, y = percDistorted)) + 
  geom_line(aes(colour = attribute, linetype = attribute)) + 
  labs(x = "Iteration", y = "% distorted", col = "Attribute", linetype = "Attribute")

# Posterior distribution over number of unique entities
qplot(result@history$numLinkedEntities, geom = "histogram") + 
  xlab("Population size") + ylab("Frequency")

###############################
# POINT ESTIMATE & EVALUATION #
###############################
# Get a point estimate of the most likely clustering from the posterior samples
smpc <- sharedMostProbableClusters(result)

# Evaluate the point estimate using pairwise metrics
# TODO: look at clustering metrics like adjusted Rand index
predMatches <- clustersToPairs(smpc)

# Filter out record pairs that don't have ground truth
tempPredMatches <- split(predMatches, seq(nrow(predMatches))) # list of rows 
tempPredMatches <- Filter(function(x) !any(is.na(ent_id[c(x[[1]], x[[2]])])), tempPredMatches)  # apply filter to rows
predMatches <- do.call(rbind, tempPredMatches) # convert back to dataframe
rm(tempPredMatches)

trueMatches <- clustersToPairs(membershipToClusters(ent_id))
effNumRecords <- sum(!is.na(ent_id))
cm <- blink2::confusionMatrix(predMatches, trueMatches, numPairs=effNumRecords*(effNumRecords-1)/2)
blink2::pairwiseMetrics(cm)

################
# CHECK ERRORS #
################
# Uncomment to print out predicted clusters with two or more records
#for (c in smpc) { if (length(c) > 1) print( df[c,] ) }

# View record pairs that are incorrectly classified
combinedMatches <- merge(cbind(predMatches, pred = TRUE), 
                         cbind(trueMatches, truth = TRUE), 
                         by = c("recordId_1", "recordId_2"), all = TRUE)
combinedMatches$pred[is.na(combinedMatches$pred)] <- FALSE
combinedMatches$truth[is.na(combinedMatches$truth)] <- FALSE
errorPairs <- combinedMatches[combinedMatches$pred != combinedMatches$truth, ]
errorPairs <- merge(errorPairs, df, by.x = "recordId_1", by.y = "ID")
errorPairs <- merge(errorPairs, df, by.x = "recordId_2", by.y = "ID", suffixes = c("._1", "._2"))
errorPairs <- errorPairs %>% select(-c("X1._1", "X1._2", "HandID._1", "HandID._2"))
View(errorPairs)

# View records with predicted entity id and true entity id (HandID)
smpc.memb <- clust2memb(smpc, num.items = numRecords) # Convert from cluster representation to membership representation
pred.truth <- cbind(data.frame(pred = smpc.memb), df)
pred.truth <- pred.truth[order(pred.truth$HandID),]
View(pred.truth)