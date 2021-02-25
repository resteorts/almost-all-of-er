## ----------------------------------------------
## fastLink tutorial
## Ted Enamorado (WUSTL)
## Duke University
## ----------------------------------------------

## ----------------------------------------------
## Let's get familiar with the data
## ----------------------------------------------

## Read data
records <- read.csv("RLdata10000.csv")

## Let's take a look at the first 4 records in RLdata10000
head(records, 4)

## Count the number of unique ids
length(unique(records$ent_id))

## Linkage Fields
linkageFields <- c("fname_c1", "lname_c1", "by", "bm", "bd")

## Exact matching
exact.match <- merge(records, records, by = linkageFields)

## Number of self-matches
sum(exact.match$rec_id.x == exact.match$rec_id.y)

## Number of non-self matches
sum(exact.match$rec_id.x != exact.match$rec_id.y)

## Who are they?
head(exact.match[exact.match$rec_id.x != exact.match$rec_id.y, c(linkageFields)], 4)

## ----------------------------------------------
## fastLink
## ----------------------------------------------

## Load fastLink 
## Recommended installation: GitHub
## https://github.com/kosukeimai/fastLink
library(fastLink)

## MAC users if you run into GitHub installation
## problems, just send me an email for detailed
## instructions
## Current version: 0.6.0
packageVersion("fastLink")

## Linkage Fields (same as above)
linkageFields <- c("fname_c1", "lname_c1", "by", "bm", "bd")

## Fields that we will compare based on
## a string similarity measure
stringDistFields <- c("fname_c1", "lname_c1")

## Fields for which we have 3 
## possible agreement values
## Agree, Partially Agree, Disagree
partialMatchFields <- c("fname_c1", "lname_c1")

## Running fastLink for the first time
out <- fastLink(dfA = records, dfB = records, 
                varnames = linkageFields,
                stringdist.match = stringDistFields,
                partial.match = partialMatchFields,
                cut.a = 0.94, cut.p = 0.84,  
                dedupe = FALSE)

## Changing Dissimilarity Measures
out <- fastLink(dfA = records, dfB = records, 
                varnames = linkageFields,
                stringdist.method = "lv", # Renormalized Levenshtein
                stringdist.match = stringDistFields,
                partial.match = partialMatchFields,
                cut.a = 0.94, cut.p = 0.84,  
                dedupe = FALSE)

## The fastLink output contains the following objects:
names(out)

## The indices of each matched pair can be found in out$matches
head(cbind(out$matches$inds.a, out$matches$inds.b), 6)

## Counts and FS weights for each patterns can be found in out$EM$patterns.w
## Legend: {2 = Agree}; {1 = Partially Agree}; {0 = Disagree} 
round(tail(out$EM$patterns.w[, 1:7]), 3)

## By default it is {0.85}, but it can be easily changed
out <- fastLink(dfA = records, dfB = records, 
                varnames = linkageFields,
                stringdist.match = stringDistFields,
                partial.match = partialMatchFields,
                cut.a = 0.94, cut.p = 0.84,  
                threshold.match = 0.90,
                dedupe = FALSE)

## Deduplicated Dataset:
## If a record is duplicated according to
## the results from fastLink, then the duplicated 
## record(s) will have the same id
## The new unique id is called dedupe.ids
recordsfL <- getMatches(dfA = records, dfB = records, fl.out = out)

## Let's count how unique records 
## fastLink finds:
length(unique(recordsfL$dedupe.ids))

## The deduplicated data looks like this:
head(recordsfL, 4)

## Some examples of known duplicates
recordsfL[recordsfL$ent_id == 20, ] 
recordsfL[recordsfL$ent_id == 77, ] 

## ----------------------------------------------
## Accuracy
## ----------------------------------------------

## Confusion Table
recordsfL$dupfL <- ifelse(duplicated(recordsfL$dedupe.ids), 
                          "Duplicated", "Not duplicated")
recordsfL$dupTrue <- ifelse(duplicated(recordsfL$ent_id), 
                            "Duplicated", "Not duplicated")

confusion <- table("fastLink" = recordsfL$dupfL,
                   "True" = recordsfL$dupTrue)
confusion

## True Positives, False Positives, and False Negatives:
TP <- confusion[1, 1]
FP <- confusion[1, 2]
FN <- confusion[2, 1]

## False Discovery Rate:
FDR <- round(FP/(FP + TP), 3)
FDR

## False Negative Rate:
FNR <- round(FN/1000, 3)
FNR

## Precision:
PRE <- 1 - FDR
PRE

## Recall
REC <- 1 - FNR 
REC

## We can add numeric comparisons using dissimilarity
numericMatchFields <- c("by")

## Make sure these are of class numeric
records$by <- as.numeric(records$by) 

out2 <- fastLink(dfA = records, dfB = records, 
                 varnames = linkageFields,
                 stringdist.match = stringDistFields,
                 cut.a = 0.94, cut.p = 0.84,
                 numeric.match = numericMatchFields,
                 cut.a.num = 1.5,
                 partial.match = partialMatchFields,
                 threshold.match = 0.90,
                 dedupe = FALSE)

## How Well Did We Do?

## Deduplicated data with numeric comparison for birth year
recordsfL2 <- getMatches(dfA = records, dfB = records, 
                         fl.out = out2)

## Let's count how unique records 
## fastLink finds:
length(unique(recordsfL2$dedupe.ids))

## ----------------------------------------------
## Blocking
## ----------------------------------------------

## For illustration purposes:
## Pool together those cases with values outside
## a "normal" range:
records$by2 <- records$by
records$by2[records$by < 1924] <- 1923
records$by2[records$by > 2008] <- 2009

## Traditional Blocking:
blockby <- blockData(records, records, varnames = "by2")
linkageFields2 <- c("fname_c1", "lname_c1", "bm", "bd")

## Looping Across Blocks
results <- list()

for(j in 1:length(blockby)) {
  records.temp <- records[blockby[[j]]$dfA.inds, ]
  
  out.temp <- fastLink(dfA = records.temp, dfB = records.temp, 
                       varnames = linkageFields2,
                       stringdist.match = stringDistFields,
                       partial.match = partialMatchFields,
                       cut.a = 0.92, cut.p = 0.84,  
                       threshold.match = 0.90,
                       dedupe = FALSE)
  
  records.temp <- getMatches(dfA = records.temp, 
                             dfB = records.temp, 
                             fl.out = out.temp)
  
  records.temp$dedupe.ids <-  paste0("B", j, "_", 
                                     records.temp$dedupe.ids)                	
  
  results[[j]] <- records.temp
}

## How Many Unique Entities?
recordsfL.blockE <- do.call('rbind', results)
length(unique(recordsfL.blockE$dedupe.ids))

## An alternative that works quite well in practice is
## to produce non-overlapping blocks via k-means
## If time allows, we will work together on coding this one!
