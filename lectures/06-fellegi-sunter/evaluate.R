require(tidyverse)
require(RecordLinkage)
#source("evaluationMetrics.R")

evaluate <- function(filesDf, linkingFields, strLinkingFields, recIdField, 
                     entIdField, blockPasses, strDist, strCutoff, dataName, 
                     threshold, sampleSize) {
  if (length(blockPasses) != 2) stop("We only support exactly two blocking passes")
  
  # Prepare data frame containing all matching pairs in the population, 
  # based on ground truth.
  renameList <- list(id = recIdField, ent_id = entIdField)
  filesDf %>% select_(.dots = renameList) -> groundTruth
  full_join(groundTruth, groundTruth, by="ent_id", suffix=c("1", "2")) %>% 
    filter(id1 < id2) %>% # remove duplicate and "self" pairs
    select(id1, id2) %>%
    mutate(is_match = TRUE) -> groundTruth
  if (nrow(groundTruth) <= 0) stop("No matching pairs in the population")
  message("There are ", nrow(groundTruth), " matching pairs in the population")
  
  # String identifiers for the blocking passes
  blockStrings <- sapply(blockPasses, function(x) paste(x, collapse = "."))
  
  # Read CSV files containing candidate pairs + weights for each 
  # blocking pass
  pairsDfs <- lapply(blockStrings, function(bs) {
    csvPath <- paste0(dataName, "_candidates_", bs, ".csv")
    message("Reading candidate pairs for blocking pass \"", bs, "\" from ", csvPath)
    pairs <- read_csv(csvPath, col_names = FALSE, col_types = "ccld")
    colnames(pairs) <- c("id1", "id2", "is_match", bs)
    return(pairs)
  })
  
  # Merge candidate pairs for the blocking passes
  pairsMerged <- full_join(pairsDfs[[1]], pairsDfs[[2]])
  pairsMerged %>% mutate(aboveZero = rowSums(.[blockStrings], na.rm = TRUE) >= 0) -> pairsMerged
  numPairsMerged <- nrow(pairsMerged)
  message("There are ", numPairsMerged, " candidate pairs with score >= ", threshold, " across all passes")
  # Merge candidate pairs with ground truth matches
  pairsMerged <- full_join(pairsMerged, groundTruth)
  message("There are ", nrow(pairsMerged) - numPairsMerged, " matching pairs outside the blocks")
  
  # Free memory
  rm(pairsDfs, groundTruth)

  # Index pairs in disjunction of blocking passes
  message("Indexing record pairs...\n")
  rpairs <- RLBigDataDedup(filesDf, 
                           blockfld = blockPasses, 
                           identity = as.factor(filesDf[,entIdField]),
                           exclude = setdiff(colnames(filesDf), linkingFields), 
                           strcmp = strLinkingFields, 
                           strcmpfun = strDist)
  
  for (ss in sampleSize) {
    message("*** Evaluating for sample size ", 2*ss, " ***")
    
    # Sample pairs above threshold first
    rowIdsAbove <- which(pairsMerged$aboveZero)
    numPairsAbove <- length(rowIdsAbove)
    message("There are ", numPairsAbove, " pairs with score >= 0 across all passes")
    ssAbove <- min(ss, numPairsAbove)
    rowIds <- sample(rowIdsAbove, size=ssAbove)
    df1 <- pairsMerged[rowIds,-ncol(pairsMerged)]
    message("Sampled ", nrow(df1), " pairs with score >= 0 to include in the training set")
    
    # Free memory
    rm(rowIdsAbove, rowIds)
    
    # Function for querying a record pair in pairsMerged
    queryPairsMerged <- function(pairsMerged, id1, id2) {
      return (pairsMerged[pairsMerged$id1 == id1 & pairsMerged$id2 == id2,])
    }
    
    # Sample pairs below (rejection sampling)
    numPairs <- nrow(rpairs@pairs)
    message("There are ", numPairs, " total candidate pairs")
    rowIds <- sample.int(numPairs, size=min(10*ss, numPairs), replace = FALSE)
    ssBelow = 0
    i = 1
    df2 <- data.frame()
    message("Starting rejection sampling of pairs with score < 0. This can be slow.")
    while (ssBelow < ss & i <= length(rowIds)) {
      pair <- rpairs@pairs[rowIds[i],c("id1", "id2", "is_match")]
      query <- queryPairsMerged(pairsMerged, pair$id1, pair$id2)
      if (nrow(query) > 0 & query[1,"aboveZero"]) {
        print("SUCCESS")
      } else {
        ssBelow <- ssBelow + 1
        if (nrow(query) <= 0) {
          scores <- rep_len(NA, length(blockStrings))
          names(scores) <- blockStrings
        } else {
          print("SUCCESS")
          scores <- query[1,blockStrings]
        }
        df2 <- rbind(df2, c(pair, scores))
      }
      i <- i + 1
    }
    message("Sampled ", nrow(df2), " pairs with score < 0 to include in the training set")
    
    combined <- rbind(df1, df2)
    
    # Free memory
    rm(df1, df2, rowIds)
    
    # Get optimal thresholds for each block
    optThresholds <- sapply(blockStrings, function(bs) {
      message("Finding optimal threshold for scores in blocking pass \"", bs, "\"")
      candidateThresholds <- sort(unique(combined[[bs]]))
      message("Candidate thresholds are ", paste(candidateThresholds, collapse=","))
      if (length(candidateThresholds) == 0) {
        warning("No candidate thresholds. Using default 0.0")
        return(0.0)
      }
      optThreshold <- candidateThresholds[1]
      bestF1 <- 0
      for (t in candidateThresholds) {
        predictions <- replace_na(combined[[bs]] >= t, replace=FALSE)
        labels <- combined$is_match
        F1 <- calcF1(predictions, labels)
        message("  At threshold ", t, " F1 score is ", F1)
        if (F1 > bestF1) { 
          optThreshold <- t
          bestF1 <- F1
        }
      }
      message("Optimal threshold is ", optThreshold)
      optThreshold
    })
    
    # Replace scores for each blocking pass with predictions using optimal threshold
    pairsPredLabel <- pairsMerged %>% select(-aboveZero)
    for (bs in blockStrings) {
      pairsPredLabel[[bs]] <- replace_na(pairsPredLabel[[bs]] >= optThresholds[bs], FALSE)
    }
    pairsPredLabel %>% 
      select(-starts_with("aboveZero")) %>%
      mutate(final_pred = as.logical(rowSums(.[blockStrings]))) %>%
      select(id1, id2, is_match, final_pred) -> pairsPredLabel
    
    message("Precision is ", calcPrecision(pairsPredLabel$final_pred, pairsPredLabel$is_match))
    message("Recall is ", calcRecall(pairsPredLabel$final_pred, pairsPredLabel$is_match))
    message("F1 score is ", calcF1(pairsPredLabel$final_pred, pairsPredLabel$is_match), "\n")
    
    # Free memory
    rm(pairsPredLabel)
  }
}
