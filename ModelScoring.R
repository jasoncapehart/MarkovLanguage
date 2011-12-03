a <- c(0.333, 1, .5, .5, 1 )
# Perplexity w/ cross entropy = 1.644
# Perplexity w/ entropy = 2.88

# Language Model Scoring Function
# This is a convenience wrapper for several scoring functions
# Input: prob.vec - a vector of probabilities
# Output: a list that includes measures for
#        out.of.vocab - out of vocabulary rate
#        perplexity
#        avg.log - average log-likelihood
#        avg.prob - average probability

lang.scoring <- function(prob.vec, ...) {
  oov <- out.of.vocab(prob.vec)
  perplexity.score <- perplexity(prob.vec, ...)
  avg.log.likelihood <- avg.log(prob.vec)
  avg.prob <- avg.prob(prob.vec)
  
  score.list <- list("out.of.vocab" = oov, "perplexity" = perplexity.score
                     , "avg.log.likelihood" = avg.log.likelihood
                     , "avg.prob" = avg.prob)
  return(score.list)
}


# Perplexity via Cross Entropy
# Input: prob.vec - a vector of probabilities from a language model
#        cross.entropy - whether perplexity should be calculated using cross.entropy or entroy
# Output: the perplexity score

perplexity <- function(prob.vec, cross.entropy = TRUE) {
  # Remove NAs
  prob <- na.omit(prob.vec)
  # Find the number of probabilities
  n <- length(prob)
  # Calculate Perplexity
  if (cross.entropy == TRUE) {
    cross.entropy <- -(1/n)*sum(log(prob, base = 2))
    perplexity <- 2^cross.entropy  
  } else {
    entropy <- -sum(prob*log(prob, base = 2))
    perplexity <- 2^entropy
  }
  return(perplexity)
}

## Average log-likelihood 
## Input: prob.vec - probabilities from the markov.object assigned to each transitions in test.vec 
## Output: Average log-likelihood

avg.log <- function(prob.vec){
  
	prob.vec <- replace(prob.vec,is.na(prob.vec),0)
	log <- prob.vec*log(prob.vec,base=2)
	sum.log <- sum(log,na.rm=T)
	avg.log <- sum.log / length(prob.vec)
	
	return("Average Log-likelihood"=avg.log)
}

## Out-of-Vocab rate
## Input: prob.vec 
## Output: The rate of out-of-vocabularies in test input 

out.of.vocab <- function(prob.vec){  
	return(sum(is.na(prob.vec))/length(prob.vec))
}

# Average probability
# Input: prob.vec
# Output: The average probability with NAs substituted with zeros

avg.prob <- function(prob.vec){
  
  prob <- replace(prob.vec,is.na(prob.vec),0)
	avg.prob <- mean(prob)
	
	return(avg.prob)
}