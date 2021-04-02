## Code for creating LSH blocks

library(RecordLinkage)
library(dplyr)
library(blink)
library(knitr)
library(textreuse)
library(tokenizers)
library(devtools)
library(cora)
library(ggplot2)
library(igraph)
library(tidyverse)

b = 10
m = 100
minhash = minhash_generator(n = m, seed = 1234)

dat = read.csv("sv-mauricio.csv")
dat = dat %>%
  filter(!is.na(HandID)) 



docs <- apply(dat, 1, function(x) paste(x[-c(1, 2, 5:11)], collapse = " ")) # get strings
head(docs)
  
  
#docs <- apply(dat, 1, function(x) paste(x[-c(1, 2, 9)], collapse = " ")) # get strings


## TODO: I think the ID's need to be re-adjusted to align with the size of the new data set so that the last line does not
## throw an error. Bernardo: check this out and see if you agree. 

names(docs) <- dat$ID # add id as names in vector
corpus <- TextReuseCorpus(text = docs, # dataset
                          tokenizer = tokenize_character_shingles, n = 1, simplify = TRUE, # shingles
                          progress = FALSE, # quietly
                          keep_tokens = TRUE, # store shingles
                          minhash_func = minhash) # use minhash

buckets <- lsh(corpus, bands = b, progress = FALSE)
candidates <- lsh_candidates(buckets)
lsh_jaccard <- lsh_compare(candidates, corpus, 
                           jaccard_similarity, progress = FALSE)


qplot(lsh_jaccard$score)


g <- make_empty_graph(nrow(dat), directed = FALSE) # empty graph
g <- add_edges(g, is.vector((candidates[, 1:2])))
g <- set_vertex_attr(g, "id", value = dat$ID) # add id




clust <- components(g, "weak") # get clusters
blocks <- data.frame(id = V(g)$ID, # record id
                     block = clust$membership) # block number 
