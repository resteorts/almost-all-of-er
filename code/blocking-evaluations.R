#' Perform evaluations (recall) for blocking.
#' 
#' @import RecordLinkage
#' @param blocking A list of the blocks
#' @param true_ids The true identifiers for comparisons
#' @param recall.only Flag that when true only prints the recall, otherwise
#' prints many evaluation metrics in a list
#' @return A vector of that returns the recall and the precision
#' @export
#' @examples
#' data("RLdata500")
#' klsh.blocks <- klsh(RLdata500, p=20, num.blocks=5, k=2)
#' confusion.from.blocking(klsh.blocks, identity.RLdata500)
#' confusion.from.blocking(klsh.blocks, identity.RLdata500, recall.only=TRUE)

confusion.from.blocking <- function(blocking, true_ids, recall.only=FALSE) {
	# convert blocking into a vector of labels
	nn <- sum(sapply(blocking,length))
	block.ids = rep(NA, nn)
	for(ii in 1:length(blocking))  block.ids[blocking[[ii]]] = ii
	# For each pair of records, check whether they are in the same block
	
    candidate.pairs = combn(length(block.ids), 2)
    same.block <- block.ids[candidate.pairs[1,]] == block.ids[candidate.pairs[2,]]
    same.truth <- true_ids[candidate.pairs[1,]] == true_ids[candidate.pairs[2,]]
	
	#same.block <- outer(block.ids,block.ids,"==")
	# For each pair of records, check whether the true ids match
	#same.truth <- outer(true_ids,true_ids,"==")
	# table same-block vs. same-truth
	confusion <- table(same.block,same.truth, dnn=c("same block?","actually same?"))
	# In the confusion matrix, rows refer to the blocks and columns refer to the truth
	
	false.positives <- confusion[2,1]
	false.negatives <- confusion[1,2]
	true.positives <- confusion[2,2]
	true.negatives <- confusion[1,1]
	recall <- true.positives/(false.negatives + true.positives)
	
	
	#true.positives <- confusion[2,2]
	#misses <- confusion[1,2]
	#recall <- true.positives/(true.positives+misses)
	#precision <- true.positives/(same.truth)
	if (recall.only) {
		return(recall)
	} else {
		return(return(list(confusion, 
              recall = true.positives/(false.negatives + true.positives),
              precision = true.positives/(true.positives + false.positives),
              fpr = false.positives/(false.positives + true.negatives),
              fnr = false.negatives/(false.negatives + true.positives),
              accuracy = (true.positives + true.negatives)/(true.positives + 	true.negatives + false.negatives + false.positives), 
 			        specificity = true.negatives/(true.negatives + false.positives))))
	}
}

#' Returns the block ids associated with a blocking method.
#' 
#' @import RecordLinkage
#' @param blocking A list of the blocks.
#' @return A list of the blocks ids that corresponds to each block 
#' @export
#' @examples
#' data("RLdata500")
#' klsh.blocks <- klsh(RLdata500, p=20, num.blocks=5, k=2)
#' block.ids.from.blocking(klsh.blocks)

block.ids.from.blocking <- function(blocking) {
	nn <- sum(sapply(blocking,length))
	block.ids = rep(NA, nn)
	for(ii in 1:length(blocking))block.ids[blocking[[ii]]] = ii
	return(block.ids)
}

#' Returns the reduction ratio associated with a blocking method
#' 
#' @import RecordLinkage
#' @import utils
#' @param block.labels A list of the blocks labels.
#' @return The reduction ratio
#' @export
#' @examples
#' data("RLdata500")
#' klsh.blocks <- klsh(RLdata500, p=20, num.blocks=5, k=2)
#' block.ids <- block.ids.from.blocking(klsh.blocks)
#' reduction.ratio(block.ids)

reduction.ratio <- function(block.labels) 1 - sum(choose(table(block.labels),2)) / choose(length(block.labels),2)

#' Returns the reduction ratio associated with a blocking method
#' 
#' @import RecordLinkage
#' @param blocking The actual blocks 
#' @return The reduction ratio
#' @export
#' @examples
#' data("RLdata500")
#' klsh.blocks <- klsh(RLdata500, p=20, num.blocks=5, k=2)
#' reduction.ratio.from.blocking(klsh.blocks)
reduction.ratio.from.blocking <- function(blocking) {
	reduction.ratio(block.ids.from.blocking(blocking))
}