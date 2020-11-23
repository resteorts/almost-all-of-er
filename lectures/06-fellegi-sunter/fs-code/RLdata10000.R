source("runFS.R")
source("evaluate.R")
set.seed(24052018)

# -------------------------------- Load data --------------------------------- #
# Read each file into a separate data frame
fileDir <- "../datasets/"
file.name <- "RLdata10000.csv"
filesDf <- read.csv(paste0(fileDir,file.name), na.strings=c("NA"), 
                     stringsAsFactors = FALSE, colClasses = "character")

# ------------------------------- Configuration ------------------------------ #
linkingFields    <- c("fname_c1", "lname_c1", "by", "bm", "bd")
strLinkingFields <- c("fname_c1", "lname_c1")
blockPasses      <- list(c("by"), c("bm", "bd"))
recIdField       <- "rec_id"
entIdField       <- "ent_id"
strDist          <- "levenshtein"
strCutoff        <- 0.70
dataName         <- "RLdata10000"
threshold        <- -10.0
sampleSize       <- c(5, 50) # This give 100 data points
#sampleSize       <- c(5, 25)

log <- file(paste0(dataName, ".log"), open = "wt")
sink(log, type = "message")

runFS(filesDf, linkingFields, strLinkingFields, recIdField, entIdField, blockPasses, 
      strDist, strCutoff, dataName, threshold)
      
evaluate(filesDf, linkingFields, strLinkingFields, recIdField, 
         entIdField, blockPasses, strDist, strCutoff, dataName, 
         threshold, sampleSize)

sink(type="message")

# Uncomment to generate CSV file of candidate pairs
#source("generatePairsCSV.R")
#pairs.dataName <- "RLdata10000_pairs.csv"
#generatePairsCSV(filesDf, linkingFields, strLinkingFields, recIdField, entIdField, 
#                 blockPasses, strDist, pairs.dataName)
