library(exchangeableER)
library(tidyverse)

############
# READ DATA 
############
exptName <- paste0("sv_ours_ewens_", gsub("[ :]", "_", date()))
message("Experiment has prefix: ", exptName)

# exptName <- paste0("sv_ours_pyp_", gsub("[ :]", "_", date()))
message("Experiment has prefix: ", exptName)

# exptName <- paste0("sv_ours_coupon_", gsub("[ :]", "_", date()))
message("Experiment has prefix: ", exptName)

records <- read_csv("sv-mauricio.csv")

# Mauricio's unique entity id
trueMembership <- records$HandID

# Filter out records with ground truth, leaving dept 1 and 7
if (FALSE) {
  records <- records[records$dept %in% c(1,7),]
  trueMembership <- trueMembership[records$dept %in% c(1,7)]  
}

# Replace ID after filtering
numRecords <- nrow(records)
records$X1 <- 1:numRecords

############################
# CUSTOM DISTANCE FUNCTIONS
############################

#' #' Modified Monge-Elkan Similarity
#' #' 
#' #' Asymmetric and includes weighting based on token vector length agreement
#' #' 
#' #' @param v1 a character vector
#' #' @param v2 a character vector
#' #' @param sim.inner inner similarity function which takes a pair of token 
#' #' vectors as arguments
#' #' @param sep separator for tokens/words (uses white space by default)
#' #' @returns a length(v1)*length(v2) pairwise similarity matrix
#' sim_MongeElkanModified <- function(v1, v2, sim.inner, sep = "\\s+") {
#'   if (!is.vector(v1) || !is.vector(v2))
#'     stop("v1 and v2 must be vectors")
#'   if (!(is.character(sep) && length(sep) == 1)) 
#'     stop("sep must be a string")
#'   
#'   # Tokenize
#'   tokens1 <- strsplit(v1, sep)
#'   tokens2 <- strsplit(v2, sep)
#'   
#'   # Preallocate similarity matrix for output
#'   out <- matrix(0.0, nrow = length(tokens1), ncol = length(tokens2))
#'   
#'   # Loop over all combinations in input character vectors
#'   for (i in seq_len(length(tokens1))) {
#'     for (j in seq_len(length(tokens2))) {
#'       t1 <- tokens1[[i]]
#'       t2 <- tokens2[[j]]
#'       if (length(t1) < length(t2)) {
#'         out[i,j] <- 0.0
#'       } else {
#'         scores <- sim.inner(t1, t2)
#'         maxScores1 <- apply(scores, 1, max)
#'         maxScores2 <- apply(scores, 2, max)
#'         me <- max(length(t1)/sum(1.0/maxScores1), length(t2)/sum(1.0/maxScores2))
#'         # Convert unit similarity to unit distance, with weighting based on
#'         # token vector length agreement
#'         out[i,j] <- 1.0/(0.95/me + 0.05*length(t1)/length(t2))
#'       }
#'     }
#'   }
#'   return(out)
#' }

################
# PREPARE MODEL
################

# Distance function for the first name attribute
knownFirstname <- read.csv("./known-firstname.csv", stringsAsFactors = FALSE)
knownFirstname <- knownFirstname$token[knownFirstname$known == 1]
dist_Firstname <- transformDistFn(function(x, y) {
    # Convert known tokens to environment for faster look-up
    knownList <- setNames(replicate(length(knownFirstname), 1, simplify = FALSE), knownFirstname)
    knownEnv <- list2env(knownList, hash = TRUE, size = length(knownList))
    sim.inner <- function(x,y) sim_NormLevenshtein(x, y, knownStrings = knownEnv, knownPenalty = 0.8)
    1.0 - sim_MaximalMatch(x, y, sim.inner = sim.inner, miss.sim.left = 0, miss.sim.right = 0.7, mean.type = 'harmonic')
  }, scalingFactor = 10.0, threshold = 3.0)

# Distance function for the last name attribute
knownLastname <- read.csv("./known-lastname.csv", stringsAsFactors = FALSE)
knownLastname <- knownLastname$token[knownLastname$known == 1]
dist_Lastname <- transformDistFn(function(x, y) {
    # Convert known tokens to environment for faster look-up
    knownList <- setNames(replicate(length(knownLastname), 1, simplify = FALSE), knownLastname)
    knownEnv <- list2env(knownList, hash = TRUE, size = length(knownList))
    sim.inner <- function(x,y) sim_NormLevenshtein(x, y, knownStrings = knownLastname, knownPenalty = 0.8)
    1.0 - sim_MaximalMatch(x, y, sim.inner = sim.inner, miss.sim.left = 0, miss.sim.right = 0.7, mean.type = 'harmonic')
  }, scalingFactor = 10.0, threshold = 3.0)

# Distance to assign to neighboring geographic regions
neighborDist <- 0.2

# Distance function for the municipality attribute
muniCodes <- read.csv("code-muni.csv", colClasses = c("character", "numeric"))
muniOverlapNames <- read.csv("namesoverlap-muni.csv", colClasses = "numeric") %>% 
  transmute(muni1 = as.character(CODE1), muni2 = as.character(CODE2), 
            dist = neighborDist)
muniNeighbors <- read.csv("neighboring-muni.csv", stringsAsFactors = FALSE) %>% 
  merge(muniCodes, by.x = "MUNI1", by.y = "MUNI") %>% 
  merge(muniCodes, by.x = "MUNI2", by.y = "MUNI") %>%
  transmute(muni1 = as.character(CODE.x), muni2 = as.character(CODE.y), 
            dist = neighborDist)
muniNeighbors <- rbind(muniNeighbors, muniOverlapNames)
dist_Muni <- transformDistFn(function(x, y) {
    dist_LookupTable(x, y, muniNeighbors, valueColnames = c("muni1", "muni2"), 
                     distanceColname = "dist", addSymmetricDistances = TRUE,
                     removeDuplicates = TRUE)
  }, scalingFactor = 1.0, threshold = Inf)

# Distance function for the department attribute
deptCodes <- read.csv("code-dept.csv", colClasses = c("character", "numeric"))
deptNeighbors <- read.csv("neighboring-dept.csv", stringsAsFactors = FALSE) %>% 
  merge(deptCodes, by.x = "DEPT1", by.y = "DEPT") %>% 
  merge(deptCodes, by.x = "DEPT2", by.y = "DEPT") %>%
  transmute(dept1 = as.character(CODE.x), dept2 = as.character(CODE.y), 
            dist = neighborDist)
dist_Dept <- transformDistFn(function(x, y) {
    dist_LookupTable(x, y, deptNeighbors, valueColnames = c("dept1", "dept2"), 
                     distanceColname = "dist", addSymmetricDistances = TRUE, 
                     removeDuplicates = TRUE)
  }, scalingFactor = 1.0, threshold = Inf)

# Distance function for the date attributes
dist_Day <- transformDistFn(dist_AbsoluteDifference, scalingFactor = 0.1, 
                            threshold = Inf)
dist_Month <- transformDistFn(dist_AbsoluteDifference, scalingFactor = 0.1, 
                              threshold = Inf)
dist_Year <- transformDistFn(dist_AbsoluteDifference, scalingFactor = 0.1, 
                             threshold = 0.4) # threshold at 4 year diff


clustPrior <- PitmanYorRP(alpha = GammaRV(1, 1/100), d = BetaRV(1, 1))
# clustPrior <- EwensRP(alpha = GammaRV(1, 1/100))
# clustPrior <- GeneralizedCouponRP(m = ShiftedNegBinomRV(1.5, 1/10000), kappa = GammaRV(1, 1/100))

lowDistortionPrior <- BetaRV(1, 5)
unifDistortionPrior <- BetaRV(1, 1)
highDistortionPrior <- BetaRV(3, 1)

# Note increasing inverse temperature in string distribution seems to improve 
# accuracy.
attributeSpecs <- c(
  "firstname" = Attribute(distFn = dist_Firstname, 
                          distortionProbPrior = lowDistortionPrior, 
                          distortionDistPrior = DirichletProcess(0.01), 
                          entityDistPrior = DirichletRV(1.0)),
  "lastname" = Attribute(distFn = dist_Lastname, 
                         distortionProbPrior = lowDistortionPrior, 
                         distortionDistPrior = DirichletProcess(0.01), 
                         entityDistPrior = DirichletRV(1.0)),
  "dept" = Attribute(distFn = dist_Dept, 
                     distortionProbPrior = lowDistortionPrior, 
                     distortionDistPrior = DirichletProcess(0.01), 
                     entityDistPrior = DirichletRV(1.0)),
  "muni" = Attribute(distFn = dist_Muni, 
                     distortionProbPrior = lowDistortionPrior, 
                     distortionDistPrior = DirichletProcess(0.01), 
                     entityDistPrior = DirichletRV(1.0)),
  "year" = Attribute(distFn = dist_Year, 
                     distortionProbPrior = lowDistortionPrior, 
                     entityDistPrior = DirichletRV(1.0)),
  "day" = CategoricalAttribute(distortionProbPrior = lowDistortionPrior, 
                               entityDistPrior = DirichletRV(1.0)),
  "month" = CategoricalAttribute(distortionProbPrior = lowDistortionPrior, 
                                 entityDistPrior = DirichletRV(1.0))
)

system.time(
model <- exchangeableER::initializeERModel(records, attributeSpecs, clustPrior, recordIdColname = 'ID')
)

# save(model, file = "model-coupon.Rdata")
# save(model, file = "model-ewens.Rdata")
# save(model, file = "model-pyp.Rdata")

# ATTN: I think we should save the step above here as it takes a very long time to run. 

################
# RUN INFERENCE 
################
#result <- exchangeableER::runInference(model, numSamples=10, 
                                       thinningInterval=1, 
                                       burninInterval=0)

result <- exchangeableER::runInference(model, numSamples=500, 
                                        thinningInterval=10, 
                                        burninInterval=2000)

# Uncomment to add more samples
 # result <- exchangeableER::runInference(result, numSamples=400,
                                      thinningInterval=10, burninInterval=0)

save(result, file = paste0("results/", exptName, "_result.Rdata"))

##############
# DIAGNOSTICS 
##############
# Trace plot for number of unique entities represented in the data
mcpar <- attr(result@history, 'mcpar')
it <- seq.int(from = mcpar[1], to=mcpar[2], by=mcpar[3])
bind_cols(iteration = it, 
          numLinkedEntities = result@history$numLinkedEntities) %>% 
  ggplot(aes(x = iteration, y = numLinkedEntities)) + geom_line() + 
  labs(x = "Iteration", y = "# linked entities")
ggsave(filename=paste0("results/", exptName, "-trace-num-entities.png"))

# Trace plot for amount of distortion in the record attributes
# Check that the distortion has stabilized for each attribute, and that it is 
# not too high (e.g. below ~50%)
bind_cols(iteration = it, 
          as_tibble(result@history$numDistortions/numRecords*100)) %>% 
  gather(key = "attribute", value="percDistorted", -iteration) %>% 
  ggplot(aes(x = iteration, y = percDistorted)) + 
  geom_line(aes(colour = attribute, linetype = attribute)) + 
  labs(x = "Iteration", y = "% distorted", col = "Attribute", linetype = "Attribute")
ggsave(filename=paste0("results/", exptName, "-trace-distortion.png"))

if (hasName(result@history, "clustParams")) {
  bind_cols(iteration = it,
            as_tibble(result@history$clustParams)) %>%
    gather(key = "clust.param", value="value", -iteration) %>% 
    ggplot(aes(x=iteration, y=value)) + 
    geom_line() + facet_grid(clust.param~., scales = "free_y") + 
    xlab("Iteration") + ylab("Value") + 
    ggtitle("Trace plot: clustering hyperparameters")
  ggsave(filename=paste0("results/", exptName, "-trace-clust-params.png"))
}

# Posterior distribution over number of unique entities
qplot(result@history$numLinkedEntities, geom = "histogram") + 
  xlab("Population size") + ylab("Frequency")
ggsave(filename=paste0("results/", exptName, "-hist-num-entities.png"))

##############################
# POINT ESTIMATE & EVALUATION 
##############################
# Get a point estimate of the most likely clustering from the posterior samples
predClusters <- sharedMostProbableClusters(result)
predMatches <- clustersToPairs(predClusters)

#' Dataframe to Pairs
#' 
#' @description
#' Produces a Pairs object in canonical form
#' 
#' @param pairs a dataframe where the rows are record pairs and the columns 
#' contain the ides of each record in the pair
#' @return a `Pairs` object
canonicalizePairs <- function(pairs) {
  ID.x <- pairs[[1]]
  ID.y <- pairs[[2]]
  temp <- ID.x
  ID.x <- ifelse(ID.x <= ID.y, ID.x, ID.y)
  ID.y <- ifelse(temp <= ID.y, ID.y, temp)
  pairs <- data.frame(ID.x = ID.x, ID.y = ID.y, stringsAsFactors = FALSE)
  class(pairs) <- c(class(pairs), "Pairs")
  pairs
}

# We only have ground truth in depts 1 and 7. So for evaluation purposes, 
# filter out record pairs not in those depts. 
predMatches <- merge(predMatches, records, by.x = "ID.x", by.y = "ID") %>% 
  merge(records, by.x = "ID.y", by.y = "ID") %>% 
  filter(dept.x %in% c(1,7) & dept.y %in% c(1,7)) %>% 
  select(ID.x, ID.y) %>% 
  canonicalizePairs

# Uncomment to use Mauricio's ground truth
# trueClustering <- membershipToClusters(trueMembership[!is.na(trueMembership)], 
#                                        ids = records$ID[!is.na(trueMembership)])
# trueMatches <- clustersToPairs(trueClustering)

# Get true record pairs with confidence greater than 0.5
matches <- matches <- read_csv("matches.csv") %>% 
  transmute(ID.x = ID_1, ID.y = ID_2, match_confidence)

trueMatches <- matches %>% 
  filter(match_confidence >= 0.5) %>% 
  select(ID.x, ID.y) %>% 
  canonicalizePairs

effNumRecords <- sum(records$dept %in% c(1,7))

# Evaluation metrics ignoring ground truth match confidence
sink(paste0("results/", exptName, "_eval.txt"))
cm <- confusionMatrix(predMatches, trueMatches, numPairs=effNumRecords*(effNumRecords-1)/2)
pairwiseMetrics(cm)
sink()

###############
# CHECK ERRORS 
###############
combinedPairs <- predMatches %>% mutate(pred = 1) %>% 
  merge(matches, all = TRUE) %>% 
  replace_na(list(pred = 0, match_confidence = 0)) %>%
  merge(records, by.x = "ID.x", by.y = "ID", all.x = TRUE) %>%
  merge(records, by.x = "ID.y", by.y = "ID", all.x = TRUE) %>% 
  select(-c("HandID.x", "HandID.y", "X1.x", "X1.y")) %>%
  transform(prob=pairwiseMatchProbability(result, ID.x, ID.y))

# Uncomment to print out predicted clusters with two or more records
#for (c in predClusters) { if (length(c) > 1) print( records[c,] ) }

# View ambiguous and incorrectly classified record pairs
View(combinedPairs)
