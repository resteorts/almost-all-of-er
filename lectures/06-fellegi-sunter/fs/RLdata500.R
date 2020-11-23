source("runFS.R")
source("evaluate.R")
set.seed(24052018)

# -------------------------------- Load data --------------------------------- #
# Read each file into a separate data frame
fileDir <- "../datasets/"
file.name <- "RLdata500.csv"
filesDf <- read.csv(paste0(fileDir,file.name), na.strings=c("NA"), 
                     stringsAsFactors = FALSE, colClasses = "character")

# ------------------------------- Configuration ------------------------------ #
linkingFields     <- c("fname_c1", "lname_c1", "by", "bm", "bd")
strLinkingFields <- c("fname_c1", "lname_c1")
recIdField       <- "rec_id"
entIdField       <- "ent_id"
blockPasses       <- list("by", "bm")
strDist         <- "levenshtein"
strCutoff       <- 0.70
dataName         <- "RLdata500"
threshold        <- -10.0
sampleSize       <- c(5, 50)

log <- file(paste0(dataName, ".log"), open = "wt")
sink(log, type = "message")

runFS(filesDf, linkingFields, strLinkingFields, recIdField, entIdField, blockPasses, 
      strDist, strCutoff, dataName, threshold)

# ATTN: Error here. Error in evaluate(filesDf, linkingFields, strLinkingFields, recIdField,  : 
  # We only support exactly two blocking passes      
evaluate(filesDf, linkingFields, strLinkingFields, recIdField, 
         entIdField, blockPasses, strDist, strCutoff, dataName, 
         threshold, sampleSize)

sink(type="message")
