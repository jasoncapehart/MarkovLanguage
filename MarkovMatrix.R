# Markov Language Model

# TODO: Use the textcnt() function in the tau() package instead of ngram()

# Markov Matrix Generator
# Input: states.vec - a vector of observed states
#        order - the desired order of Markov Model (i.e. how many previous states the process depends on)
# Output: (1) A markov matrix for the input sequence
#         (2) A lookup table of possible states
#         (3) A lookup table of transition states
# Required: possible.states(), actual.states()

mm.generator <- function(states.vec, order) {
  # Create an Ordered Unique state list
  unique.states <- unique(states.vec)
  unique.states <- unique.states[order(unique.states)]
  # Count the number of unique states
  unique.count <-length(unique.states)
    
  # Preallocate an empty matrix
  empty.mm <- matrix(0, nrow=unique.count^order, ncol = unique.count)
  
  # Get the observed frequencies from actual.states()
  position.table <- actual.states(states.vec, order)
    
  # Get the row and column information from possible.states()
  possible.results <- possible.states(states.vec, order) # To prevent running possible.states() twice for each part of the list returned
  possible.states.dim <- possible.results$possible.states.dim
  trans.states.dim <- possible.results$trans.states.dim
  rm(possible.results) # Clean up
  
  # Row number lookup
  tmp <- merge(x = position.table, y = possible.states.dim, by.x = "History", by.y = "Seq", all.x=TRUE)
  # Column number lookup
  lookup.state.name <- paste("State", order+1, sep="")
  tmp <- merge(x = tmp, y = trans.states.dim, by.x = lookup.state.name, by.y="Trans.State", all.x=TRUE)
  position.table <- tmp
  
  #Use "RowNum" and "ColNum" from position.table to populate the empty.mm
  freq.matrix <- matrix.insert(matrix.name = empty.mm, insert.row = position.table$RowNum
              , insert.col = position.table$ColNum, insert.val = position.table$Freq)
  
  mm <- as.matrix(aaply(.data=freq.matrix, .margins=1, .fun= function(x) if (sum(x) == 0) x else x/sum(x)))
  dimnames(mm) <- NULL

  markov.objects <- list("markov.matrix" = mm, "possible.states.dim" = possible.states.dim, "trans.states.dim" = trans.states.dim)
  
  return(markov.objects)
}

# Actual States
# Input: states.vec - a vector of observed states
#        order - the desired order of Markov Model
# Output: state.trans.freq - the frequency of all state transitions
# Calls: ngram()

actual.states <- function(states.vec, order) {
  # (1) Observed States
  state.seq <- ngram(states.vec, order)
  state.seq$Seq <- as.character(state.seq$Seq) #Force to char
  # (2) Frequency of those States
  freq.table <- as.data.frame(table(state.seq$Seq))
  colnames(freq.table) <- c("Seq", "Freq")
  freq.table$Seq <- as.character(freq.table$Seq)
  
  #TODO: This join should be fixed so the de-duplication step is unecessary
  position.table <- merge(x=freq.table, y=state.seq, by="Seq")
  position.table <- unique(position.table) #Dedupe the table

  return(position.table)
}


# Ngram function
# Input: states.vec
#        order - size of n-gram is equal to Markov Model order plus 1
# Output: N gram data frame specifying the position of each state and the concatenated state sequence
# Required: foreach() package
# Called By: actual.states()


# From ngram we need
#    (1) A column for the "history" all the preceeding states
#    (2) A column for the "sequence", the preceeding states and which state that history moved to

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

# Possible States
# Input: states.vec - a vector of observed states
#        order - the desired order of Markov Model
# Output: possible.states.dim - a data frame that shows what state(s) each matrix row number corresponds to
#         trans.states.dim - a data frame that shows which transition state each matrix column number corresponds to

possible.states <- function(states.vec, order) {
  # Create an Ordered Unique state list
  unique.states <- unique(states.vec)
  unique.states <- unique.states[order(unique.states)]
  # Count the number of unique states
  unique.count <-length(unique.states)
  
  # Each unique state is a potential transition state in the matrix
  trans.states.dim <- data.frame("ColNum" = c(1:unique.count), "Trans.State" = unique.states)
  trans.states.dim$Trans.State <- as.character(trans.states.dim$Trans.State)
  
  # Create a data frame that has a row for each possible state, in order
  state.matrix <- matrix(data=rep(unique.states, times = order), ncol = order, byrow=FALSE)
  state.df <- expand.grid(as.data.frame(state.matrix)) # Use expand.grid() to get all state combinations
  state.df <- as.data.frame(state.df, stringsAsFactors = FALSE) # Convert the states back to strings
  # Reverse the order of the columns from expand.grid() to make the state repitition chunks go from longest to shortest as you move from left to right on the table
  state.df <- state.df[, c(order:1)]
  
  if (order > 1) {
    state.df <- apply(state.df, MARGIN=2, FUN= as.character) # Make sure the data frame is composed of strings and not factors
    # Concatenate
    states.cat <- unlist(foreach(i=1:dim(state.df)[1]) %do% paste(paste(state.df[i,], sep =' '), collapse = ' '))
  } else {
    states.cat <- state.df
  }
  # Put together possible states dim
  possible.states.dim <- data.frame("RowNum" = c(1:(unique.count^order)), state.df, "Seq" = states.cat)
  possible.states.dim$Seq <- as.character(possible.states.dim$Seq)
  
  return(list("possible.states.dim" = possible.states.dim, "trans.states.dim" = trans.states.dim))
}

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

matrix.insert <- function(matrix.name, insert.row, insert.col, insert.val) {
  total.writes <- length(insert.row)
  for (i in 1:total.writes) {
    matrix.name[insert.row[i], insert.col[i]] <- insert.val[i]
  }
  return(matrix.name)
}


# Function creation above ....

#_______________________________________________________________________

# Example by hand below ...


#-------------
# Common Preamble
#-------------

library(plyr)
library(foreach)
test <- c("This", "is", "a", "character", "vector", "this", "is", "a", "test", ".")

# Create an Ordered Unique word list
unique.words <- unique(test)
unique.words <- unique.words[order(unique.words)]
# Count the number of unique words
unique.count <-length(unique.words)

#---------------
# 1st Order
#---------------

# Create the empty markov matrix
markov.matrix <- matrix(data=0, nrow = unique.count, ncol=unique.count)

# Define a lookup table for the position of each unique word in the matrix
unique.word.position <- data.frame("Word" = unique.words, "Position" = c(1:unique.count))

# Find the frequency of all bigrams
bigram <- data.frame("Word1" = test[1:(length(test)-1)]
                     , "Word2" = test[2:length(test)]
                     , "Bigram" = paste(test[1:(length(test)-1)], test[2:length(test)], sep = " ")
                     )

bigram.freq <- as.data.frame(table(bigram$Bigram), stringsAsFactors=FALSE)
colnames(bigram.freq) <- c("Bigram", "Freq")

#Add Frequency of Each Bigram
position.table <- merge(x=bigram, y=bigram.freq, by.x="Bigram", by.y="Bigram", all.x=TRUE)
# Add the Matrix Position of the 1st word
position.table <- merge(x=position.table, y=unique.word.position, by.x="Word1", by.y="Word", all.x=TRUE)
# Overwrite the default name given by merge() for the new column
colnames(position.table)[which(colnames(position.table) == "Position")] <- "Word1.Pos"
# Add the Matrix Position of the 2nd word
position.table <- merge(x=position.table, y=unique.word.position, by.x="Word2", by.y="Word", all.x=TRUE)
colnames(position.table)[which(colnames(position.table) == "Position")] <- "Word2.Pos"

# Fill in the frequency for each bigram
freq.matrix <- matrix.insert(matrix.name = markov.matrix, insert.row = position.table$Word1.Pos
              , insert.col = position.table$Word2.Pos, insert.val = position.table$Freq)

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

matrix.insert <- function(matrix.name, insert.row, insert.col, insert.val) {
  total.writes <- length(insert.row)
  for (i in 1:total.writes) {
    matrix.name[insert.row[i], insert.col[i]] <- insert.val[i]
  }
  return(matrix.name)
}

# Create the filled-in markov matrix using the frequency matrix
mm <- as.matrix(aaply(.data=freq.matrix, .margins=1, .fun= function(x) if (sum(x) == 0) x else x/sum(x)))
dimnames(mm) <- NULL


#-----------
# 2nd Order
#-----------

state1 <- unlist(llply(unique.words, function(x) rep(x, times = unique.count)))
state2 <- rep(unique.words, times = unique.count)

