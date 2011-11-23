# Markov Language Model

#-------------
# Common Preamble
#-------------

library(plyr)
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

