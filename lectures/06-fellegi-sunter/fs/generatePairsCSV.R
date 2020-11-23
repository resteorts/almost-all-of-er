require(RecordLinkage)

# Generates a CSV of candidate pairs for a data frame. Only includes pairs that 
# agree on one of the blocking variables in `blockPasses`. Uses the RecordLinkage 
# package under the hood.
generatePairsCSV <- function(filesDf, linkingFields, strLinkingFields, 
                             recIdField, entIdField, blockPasses, 
                             strDist, dataName) {
  # Index record pair comparisons (may require GBs of free disk space)
  rpairs <- RLBigDataDedup(filesDf, 
                           blockfld = blockPasses, 
                           identity = as.factor(filesDf[,entIdField]),
                           exclude = setdiff(colnames(filesDf), linkingFields), 
                           strcmp = strLinkingFields, 
                           strcmpfun = strDist)
  
  # Extract the pairs (expressed as row ids into the original data frame) together 
  # with labels (match/non-match)
  pairs <- rpairs@pairs[,c("id1", "id2")]
  labels <- rpairs@pairs[,"is_match"]
  
  # Remove RLBigDataDedup object, since we've extracted everything we need
  rm(rpairs)
  
  # Extract data for left/right pair ids
  leftData <- filesDf[pairs$id1, c(recIdField, linkingFields)]
  rightData <- filesDf[pairs$id2, c(recIdField, linkingFields)]
  
  # Prepend variable names with "left_" and "right_" to match DeepMatcher
  colnames(leftData) <- paste0("left_", colnames(leftData))
  colnames(rightData) <- paste0("right_", colnames(rightData))
  
  finalPairs <- cbind(labels, leftData, rightData)
  
  write.csv(finalPairs, dataName, row.names=FALSE)
}
