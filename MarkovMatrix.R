# Markov Matrix Generator
# Jason Capehart
# Last Updated: 11.26.11

# Benchmarks
#    On Pentium i7 with 8 GB Ram with Windows 7 x64
#       (A) 1.54 min - 1st order markov matrix for Heart of Darkness Corpus
#       (B) 1.61 min - 2nd order markov matrix for Heart of Darkness Corpus

# TO DO: 
#    (1) Use the textcnt() function in the tau() package instead of ngram()
#    (2) Add require() to necessary functions
#    (3) Revist the matrix.insert() function
#    (4) Clean up code and make more concise, especially in actual.states()
#          (i) Refine terminology for "states", "histories", function names, etc.
#    (5) Revisit sparse matrices to create a matrix which enumerates all possible state histories in the rows

#-------------
# Common Preamble
#-------------

library(plyr)
library(foreach)

#----------------------------------------------
# Markov Matrix Generator
# Input: states.vec - a vector of observed states
#        order - the desired order of Markov Model (i.e. how many previous states the process depends on)
# Output: (1) A markov matrix for the input sequence
#         (2) A lookup table of possible states
#         (3) A lookup table of transition states
# Calls: actual.states(), ngram(), matrix.insert
# Requires: plyr and foreach
#------------------------------------------------

mm.generator <- function(states.vec, order) {
  
  # Get the observed frequencies from actual.states()
  actual.states.objects <- actual.states(states.vec, order)
  position.table <- actual.states.objects$position.table
  history.dim <- actual.states.objects$history.dim
  trans.states.dim <- actual.states.objects$trans.states.dim
  
  # Preallocate and empty matrix
  # Count the number of unique histories
  unique.histories <- length(unique(position.table$RowNum))
  # Count the number of unique states
  unique.count <-length(unique(states.vec))
  # Preallocate Step
  empty.mm <- matrix(0, nrow=unique.histories, ncol = unique.count)
  
  #Use "RowNum" and "ColNum" from position.table to populate the empty.mm
  freq.matrix <- matrix.insert(matrix.name = empty.mm, insert.row = position.table$RowNum
              , insert.col = position.table$ColNum, insert.val = position.table$Freq)
  
  mm <- as.matrix(aaply(.data=freq.matrix, .margins=1, .fun= function(x) if (sum(x) == 0) x else x/sum(x)))
  dimnames(mm) <- NULL

  # Return all markov matrix objects
  markov.objects <- list("markov.matrix" = mm
        , "history.dim" = history.dim
        , "trans.states.dim" = trans.states.dim)
  
  return(markov.objects)
}

#-------------------------------------------------
# Actual States
# Input: states.vec - a vector of observed states
#        order - the desired order of Markov Model
# Output: position.table - a data frame with each observed state history, the frequency of the observation, and the position the observation will enter in the markov matrix
# Calls: ngram()
#----------------------------------------------

actual.states <- function(states.vec, order) {
  # (1) Obse#rved States
  state.seq <- ngram(states.vec, order)
  state.seq$Seq <- as.character(state.seq$Seq) #Force to char
  # (2) Frequency of those States
  freq.table <- as.data.frame(table(state.seq$Seq))
  colnames(freq.table) <- c("Seq", "Freq")
  freq.table$Seq <- as.character(freq.table$Seq)
  
  #TODO: This join should be fixed so the de-duplication step is unecessary
  position.table <- merge(x=freq.table, y=state.seq, by="Seq")
  position.table <- unique(position.table) #Dedupe the table
  
  # Create a lookup table between the history and corresponding row
  history.row <- unique(position.table$History)
  history.row <- history.row[order(history.row)]
  history.dim <- data.frame("RowNum" = c(1:length(history.row)), "UniqueHistory" = history.row)
  
  # Create a lookup table between the transition state and corresponding column
  trans.state.col <- paste("State", order+1, sep='')
  trans.states <- unique(position.table[trans.state.col])
  trans.states <- trans.states[order(trans.states), 1]
  trans.states.dim <- data.frame("ColNum" = c(1:length(trans.states)), "TransState" = trans.states)
  
  # Row number lookup
  tmp <- merge(x = position.table, y = history.dim, by.x = "History", by.y = "UniqueHistory", all.x=TRUE)
  # Column number lookup
  lookup.state.name <- paste("State", order+1, sep="")
  tmp <- merge(x = tmp, y = trans.states.dim, by.x = lookup.state.name, by.y="TransState", all.x=TRUE)
  position.table <- tmp

  # Return history.dim, trans.states.dim, and the position.table
  actual.states.objects <- list("position.table" = position.table
      , "history.dim" = history.dim
      , "trans.states.dim" = trans.states.dim)
  
  return(actual.states.objects)
}

#---------------------------------------
# Ngram function
# Input: states.vec
#        order - size of n-gram is equal to Markov Model order plus 1
# Output: N gram data frame specifying the position of each state and the concatenated state sequence
# Required: foreach() package
# Called By: actual.states()
#---------------------------------------

ngram <- function(states.vec, order) {
  
  n <- order + 1 # the length of the n-gram is 1 greater than the order
  obs <- length(states.vec) # Number of observations
  # Preallocate matrix
  state.seq.matrix <- matrix(data=0, nrow=obs-order, ncol=n)
  colname.vec <- NULL # Initialize vector for column names
  
  # Set up each state sequence by manipulating the indices of the original "states.vec"
  for (i in 1:n) {
    state.i <- states.vec[i:(obs-(n-i))] # From first possible start point (i) to last possible end point obs-(n-i)
    state.seq.matrix[, i] <- state.i #Overwrite the appropriate column
    # Make a sequence of names for higher order Markov Models
    name.i <- paste("State", i, sep='')
    colname.vec <- c(colname.vec, name.i)
  }
  # Convert the matrix to a data frame to handle the strings
  state.seq.df <- as.data.frame(state.seq.matrix, stringsAsFactors = FALSE)
  colnames(state.seq.df) <- colname.vec # Attach the column names
  # Drop the original matrix from memory
  rm(state.seq.matrix)
  
  # Concatenate the word sequence into 1 column
  seq.cat <- unlist(foreach(i=1:dim(state.seq.df)[1]) %do% paste(paste(state.seq.df[i, ], sep =' '), collapse = ' '))
  history.cat <- unlist(foreach(i=1:dim(state.seq.df)[1]) %do% paste(paste(state.seq.df[i, 1:(dim(state.seq.df)[2]-1)], sep =' '), collapse = ' '))
  # Attach the column to the overall data frame
  state.seq.df <- data.frame(state.seq.df, "History" = history.cat, "Seq" = seq.cat)
  return(state.seq.df)
}

#-----------------------------------------------
# Matrix Insert Function
# Designed to be fast for sparse matrices by only visiting
#   those elements in the matrix which need to be overwritten
# Input: matrix.name - The matrix to have values inserted into
#        insert.row - A vector of row positions that are in the proper order for the correspoding columns
#        insert.col - A vector of column positions
#        insert.val - The value to insert i.e. matrix.name[insert.row, insert.col] <- insert.val
# Output: matrix with all values inserted
# TODO: (1) Is there a way to vectorize this without reading/writing the entire
#           matrix on each step, as I fear apply() would do?
#       (2) How much does the sequence matter? In other words for a large matrix
#           would it make sense to order the elements to make the distance between
#           one write and the next as small as possible? My guess is sorting the
#           list is more expensive than jumping to random positions on the matrix
#           but I don't know that for sure.
#----------------------------------------------------

matrix.insert <- function(matrix.name, insert.row, insert.col, insert.val) {
  total.writes <- length(insert.row)
  for (i in 1:total.writes) {
    matrix.name[insert.row[i], insert.col[i]] <- insert.val[i]
  }
  return(matrix.name)
}