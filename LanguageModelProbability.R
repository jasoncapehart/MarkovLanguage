# Language Model Probability
# Input: input.vec - test data which a character vector with a sequence of states
#        markov.object - an object created by mm.generator()
# Output: a data frame with the language model's probability for each sequence in the input text
# Requires: tau, foreach
# Calls: ngram()

lang.model.prob <- function(input.vec, markov.object) {
  
  require(tau)
  # Check to make sure input.vec is a character vector
  stopifnot(is.character(input.vec))
  
  # Create the sequence of state histories and transitions to look up
  state.seq.df <- ngram(states.vec = input.vec, order = markov.object$order)
  # Find the name of the transition state
  trans.state.col.name <- paste("State", markov.object$order + 1, sep = '')
  
  # Backoff Strategy Note: Leave entire state.seq.df to use with lower order markov model
  # Cut the unnecessary data out of the state.seq.df
  prob.df <- data.frame("History" = state.seq.df$History
                        , "TransState" = state.seq.df[trans.state.col.name])
  colnames(prob.df)[which(colnames(prob.df) == trans.state.col.name)] <- "TransState" #Rename the final state column name
  rm(state.seq.df) # Remove the state.seq.df entirely
  
  # Merge the row and column numbers using the history.dim and trans.state.dim
  prob.df <- merge(x = prob.df, y = markov.object$history.dim
               , by.x = "History", by.y = "UniqueHistory", all.x = TRUE)
  prob.df <- merge(x = prob.df, y = markov.object$trans.states.dim
               , by.x = "TransState", by.y = "TransState", all.x = TRUE)
  
  # Find the probability that corresponds to each history in the input.vec
  prob.vec <- unlist(foreach(i=1:dim(prob.df)[1]) %do% markov.object$markov.matrix[prob.df[i, "RowNum"], prob.df[i, "ColNum"]])
  prob.df <- data.frame(prob.df, "Prob" = prob.vec)
  
  return(prob.df)
}

