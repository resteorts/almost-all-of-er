library(sadinle14)
library(tidyverse)
library(exchangeableER)

############
# READ DATA 
############
exptName <- paste0("sv_sadinle_", gsub("[ :]", "_", date()))
message("Experiment has prefix: ", exptName)

records <- read_csv("sv-mauricio.csv")

# Mauricio's unique entity id
trueMembership <- records$HandID

# Filter depts 1 and 7
#records <- records[(records$dept == 1) | (records$dept == 7),]

# Replace ID after filtering
numRecords <- nrow(records)
records$X1 <- 1:numRecords

###############################
# ATTRIBUTE DISTANCE FUNCTIONS
###############################
# First name
knownFirstname <- read.csv("known-firstname.csv", stringsAsFactors = FALSE)
knownFirstname <- knownFirstname$token[knownFirstname$known == 1]
dist_Firstname <- function(x, y) {
  # Convert known tokens to environment for faster look-up
  knownList <- setNames(replicate(length(knownFirstname), 1, simplify = FALSE), knownFirstname)
  knownEnv <- list2env(knownList, hash = TRUE, size = length(knownList))
  sim.inner <- function(x,y) sim_NormLevenshtein(x, y, knownStrings = knownEnv, knownPenalty = 0.8)
  1.0 - sim_MaximalMatch(x, y, sim.inner = sim.inner, miss.sim.left = 0.5, return.matrix = FALSE)
}

# Last name
knownLastname <- read.csv("known-lastname.csv", stringsAsFactors = FALSE)
knownLastname <- knownLastname$token[knownLastname$known == 1]
dist_Lastname <- function(x, y) {
  # Convert known tokens to environment for faster look-up
  knownList <- setNames(replicate(length(knownLastname), 1, simplify = FALSE), knownLastname)
  knownEnv <- list2env(knownList, hash = TRUE, size = length(knownList))
  sim.inner <- function(x,y) sim_NormLevenshtein(x, y, knownStrings = knownEnv, knownPenalty = 0.8)
  1.0 - sim_MaximalMatch(x, y, sim.inner = sim.inner, miss.sim.left = 0.5, return.matrix = FALSE)
}

# Distance to assign to neighboring geographic regions (essentially arbitrary)
neighborDist <- 0.2

# Municipality
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
dist_Muni <- function(x, y) dist_LookupTable(x, y, muniNeighbors, 
                                             valueColnames = c("muni1", "muni2"), 
                                             distanceColname = "dist", 
                                             addSymmetricDistances = TRUE, 
                                             removeDuplicates = TRUE, 
                                             return.matrix = FALSE)

# Department
deptCodes <- read.csv("code-dept.csv", colClasses = c("character", "numeric"))
deptNeighbors <- read.csv("neighboring-dept.csv", stringsAsFactors = FALSE) %>% 
  merge(deptCodes, by.x = "DEPT1", by.y = "DEPT") %>% 
  merge(deptCodes, by.x = "DEPT2", by.y = "DEPT") %>%
  transmute(dept1 = as.character(CODE.x), dept2 = as.character(CODE.y), 
            dist = neighborDist)
dist_Dept <- function(x, y) dist_LookupTable(x, y, deptNeighbors, 
                                             valueColnames = c("dept1", "dept2"), 
                                             distanceColname = "dist", 
                                             addSymmetricDistances = TRUE, 
                                             removeDuplicates = TRUE, 
                                             return.matrix = FALSE)

###############
# DEFINE MODEL 
###############
scoringFns <- list(
  firstname = dist_Firstname,
  lastname = dist_Lastname,
  year = function(x, y) dist_AbsoluteDifference(x, y, return.matrix = FALSE),
  month = function(x, y) dist_AbsoluteDifference(x, y, return.matrix = FALSE),
  day = function(x, y) dist_AbsoluteDifference(x, y, return.matrix = FALSE),
  muni = dist_Muni
)

scoringBreaks <- list(
  firstname = c(-Inf,.01,.15,.3,Inf),
  lastname = c(-Inf,.01,.15,.3,Inf),
  year = c(-Inf,0,1,3,Inf),
  month = c(-Inf,0,1,3,Inf),
  day = c(-Inf,0,2,7,Inf),
  muni = c(-Inf,0,Inf)
)

# Only consider pairs for which the munis are neighboring or missing AND 
# the depts are neighboring or missing
pairs <- records %>% 
  computePairs_AttributeDist(index.col = "muni", id.col = "ID", dist.fn = dist_Muni, 
                             dist.cutoff = neighborDist, include.na = TRUE) %>% 
  filter(is.na(dept.x) | is.na(dept.y) | dist_Dept(dept.x, dept.y) <= neighborDist) %>%
  computeScores(scoringFns) %>% 
  discretizeScores(scoringBreaks)

# Set truncation points to zero
lambda <- list(
  firstname = rep(0, 3),
  lastname = rep(0, 3),
  year = rep(0, 3),
  month = rep(0, 3),
  day = rep(0, 3),
  muni = rep(0, 1)
)

# ATTN: I wonder if we might want to do one comparison to the truncated beta to illustrate why we # are making the switch and to illustrate the instability. 


# MS: this one is the one I'd use
# lambda <- list(
#   firstname = c(0.8,0.85,0.99),
#   lastname = c(0.8,0.85,0.99),
#   year = c(0.8,0.85,0.99),
#   month = c(0.8,0.85,0.99),
#   day = rep(.6,3),
#   muni = rep(.8,1)
# )
# # MS: sensitivity 1: not very sensitive
# lambda <- list(
#   firstname = c(0.85,0.9,0.99),
#   lastname = c(0.85,0.9,0.99),
#   year = c(0.85,0.9,0.99),
#   month = c(0.85,0.9,0.99),
#   day = rep(.7,3),
#   muni = rep(.85,1)
# )
# # MS: sensitivity 2: not very sensitive
# lambda <- list(
#   firstname = c(0.9,0.95,0.99),
#   lastname = c(0.9,0.95,0.99),
#   year = c(0.9,0.95,0.99),
#   month = c(0.9,0.95,0.99),
#   day = rep(.8,3),
#   muni = rep(.9,1)
# )

# This is chosen by me as I don't understand how one is supposed to choose these in practice. 
lambda <- list(
  firstname = c(0.9,0.95,0.99),
  lastname = c(0.9,0.95,0.99),
  year = c(0.9,0.95,0.99),
  month = c(0.9,0.95,0.99),
  day = rep(.6,3),
  muni = rep(.8,1)
)

# Uniform prior
alpha1 <- lapply(lambda, function(x) rep(1, length(x)))
beta1 <- lapply(lambda, function(x) rep(1, length(x)))

# Additional criteria to determine whether a pair is a candidate match
criterion1 <- (pairs$firstname) < 4 & (pairs$lastname < 4)  # good agreement on name
criterion1[is.na(criterion1)] <- TRUE          # don't drop if agreement level is NA
criterion2 <- TRUE
# criterion2 <- (pairs$month < 4) & (pairs$year < 4)          # good agreement on date
# criterion2[is.na(criterion2)] <- TRUE          # don't drop if agreement level is NA

pairs[['candidate']] <- criterion1 & criterion2

model <- sadinle14::initializeModel(pairs, lambda, alpha1, beta1, 
                                    id.cols = c("ID.x", "ID.y"), 
                                    candidate.col = "candidate")
                                    
result <- sadinle14::runInference(model, numSamples = 1000, 
                                  burninInterval = 5000, thinningInterval = 10)  
                                                                   
# ATTN: This run here seems like over kill. 
# result <- sadinle14::runInference(model, numSamples = 1000, 
                                  burninInterval = 10000, thinningInterval = 10)
                                  
save(result, file=paste0("results/", exptName, ".Rdata"))

linkageChain <- completeLinkageChain(result, records$ID)

###################
# DIAGNOSTIC PLOTS 
###################
mcpar <- attr(result@history, 'mcpar')
it <- seq.int(from=mcpar[1], to=mcpar[2], by=mcpar[3])

pop.size <- apply(result@history$linkage, 1, 
                  function(x) length(unique(x))) + nrow(records) - length(result@state@recordIds)
bind_cols(iteration = it,
          numLinkedEntities = pop.size) %>%
  ggplot(aes(x = iteration, y = numLinkedEntities)) + geom_line() +
  labs(x = "Iteration", y = "# linked entities")
ggsave(paste0("results/", exptName, "-trace-num-entities.png"))

bind_cols(as_tibble(result@history$m), iteration = it) %>%
  gather(key = "key", value = "value", -iteration) %>% 
  extract(key, into = c("attribute", "level"), regex = "(\\w+)\\[(\\d+)\\]$") %>% 
  ggplot(aes(x = iteration, y = value)) + 
  facet_grid(attribute~., scales = "free_y") + 
  geom_line(aes(color = level)) + 
  labs(x = "Iteration", y = "m", color = "Level")
ggsave(paste0("results/", exptName, "_trace-m-star.png"))

bind_cols(as_tibble(result@history$u), iteration = it) %>%
  gather(key = "key", value = "value", -iteration) %>% 
  extract(key, into = c("attribute", "level"), regex = "(\\w+)\\[(\\d+)\\]$") %>% 
  ggplot(aes(x = iteration, y = value)) + 
  facet_grid(attribute~., scales = "free_y") + 
  geom_line(aes(color = level)) + 
  labs(x = "Iteration", y = "u", color = "Level")
ggsave(paste0("results/", exptName, "_trace-u-star.png"))


# ATTN: I think it would be better to give a posterior distribution here with a credible interval and one idea is to overlap ours with Mauricio's. 

qplot(pop.size, geom = "histogram") + xlab("Population size") + ylab("Frequency")
ggsave(paste0("results/", exptName, "-hist-num-entities.png"))

##############################
# POINT ESTIMATE & EVALUATION
##############################
# Get a point estimate of the matching record pairs
mpc <- mostProbableClusters(linkageChain)
predClustering <- sharedMostProbableClusters(mpc)
predMatches <- clustersToPairs(predClustering)

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
#                                        ids = as.character(records$ID[!is.na(trueMembership)]))
# trueMatches <- clustersToPairs(trueClustering)

# Use our ground truth, regarding pairs with confidence >= 0.5 as matches
matches <- read_csv("matches.csv") %>% 
  transmute(ID.x = as.character(ID_1), ID.y = as.character(ID_2), match_confidence)
trueMatches <- matches %>% 
  filter(match_confidence >= 0.5) %>% 
  select(ID.x, ID.y) %>% 
  canonicalizePairs

effNumRecords <- sum(records$dept %in% c(1,7))

sink(paste0("results/", exptName, "_eval.txt"))
cm <- confusionMatrix(predMatches, trueMatches, numPairs=effNumRecords*(effNumRecords-1)/2)

# ATTN: I think it would be worth doing a comparion with these to the truncated beta and illustrating why these are not sensitive. This is just a suggestion for discussion. 

pairwiseMetrics(cm)
sink()

combinedPairs <- predMatches %>% mutate(pred = 1) %>% 
  merge(matches, all = TRUE) %>% 
  replace_na(list(pred = 0, match_confidence = 0)) %>%
  merge(records, by.x = "ID.x", by.y = "ID", all.x = TRUE) %>%
  merge(records, by.x = "ID.y", by.y = "ID", all.x = TRUE) %>% 
  select(-c("HandID.x", "HandID.y", "X1.x", "X1.y"))

# Uncomment to print out predicted clusters with two or more records
#for (c in smpc) { if (length(c) > 1) print( records[c,] ) }

# Uncomment to view ambiguous and incorrectly classified record pairs
# ATTN: Is there something that we can do with this for the paper? I would be happy to go through and give my match confidence and then further motivate why we think the model should be quite flexible and ground through in this situation is perhaps very difficult, whereas in other conflicts it's more reasonable. 

View(combinedPairs)
