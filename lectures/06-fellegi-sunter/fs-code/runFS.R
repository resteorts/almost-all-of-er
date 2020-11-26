require(RecordLinkage)
require(ffbase)

# Runs Fellegi-Sunter based record linkage and saves the candidate links to 
# disk
runFS <- function(filesDf, linkingFields, strLinkingFields, 
                  recIdField, entIdField, blockPasses, 
                  strDist, strCutoff, dataName, threshold) {
  # Iterate over conjuctions (blocking passes)
  for (bv in blockPasses) {
    blockString <- paste(bv, collapse = ".")
    message("*** Starting blocking pass \"", blockString, "\" ***")
    
    # Get wall clock running time
    t1 <- Sys.time()
    
    # Index record pair comparisons (requires ~1.5GB free disk space)
    # Note: we provide the ground truth cluster membership now, since this function 
    # is able to convert it to a pairwise format which we need for evaluation later 
    # on.
    message("Indexing record pair comparisons...")
    rpairs <- RLBigDataDedup(filesDf, 
                             blockfld = bv, 
                             identity = as.factor(filesDf[,entIdField]),
                             exclude = union(setdiff(colnames(filesDf), linkingFields), bv), 
                             strcmp = strLinkingFields, 
                             strcmpfun = strDist)
    
    vLinkingFields <- setdiff(colnames(rpairs@pairs), c("id1", "id2", "is_match"))
    message("Verify linking fields: ", paste(vLinkingFields, collapse=", "), "\n")
    
    # Extract the ground truth match status for the pairs, then remove from the 
    # rpairs object.
    is_match <- clone.ff(rpairs@pairs$is_match)
    numPairs <- length(is_match)
    rpairs@pairs$is_match <- ffrep.int(as.integer(NA), numPairs)
    
    # Run the EM algorithm to estimate the M and U probabilities
    rpairs <- emWeights(rpairs, cutoff = strCutoff, verbose = TRUE)
    
    #scores <- paste(as.ram(log2(rpairs@M/rpairs@U)), collapse = ", ")
    #message("Scores for each pattern are [", scores, "]")
    
    # Make predictions by applying a threhsold
    result <- emClassify(rpairs, threshold.upper = threshold)
    
    # Re-insert ground truth
    result@data@pairs$is_match <- update(result@data@pairs$is_match, from = is_match)
    
    # Get pairs above threshold in a data frame
    resultDf <- getPairs(result, filter.link="link", single.rows = TRUE, 
                          withWeight = TRUE, withMatch = TRUE, withClass = FALSE)
    
    csvPath <- paste0(dataName, "_candidates_", paste(bv, collapse = "."), ".csv")
    message("Writing results to disk at ", csvPath)
    write.table(resultDf[,c(paste0(recIdField,".", 1:2), "is_match", "Weight")], 
                file = csvPath, row.names = FALSE, col.names = FALSE, sep=",", quote = TRUE)
    
    t2 <- Sys.time()
    delta_t <- t2 - t1
    message("Wall clock running time ", delta_t, " ", units(delta_t), "\n")
    
    rm(rpairs, is_match, result, pred.match, pred.nonmatch, resultDf)
    gc()
  }
}
