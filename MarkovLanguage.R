###############################
# Markov Language Model Project
###############################
# Lizzie Silver, Alexander Murray-Watters, Sean Su Han, Jason Capehart
# Last Updated: 12.5.11

# Function List
# 1) Text Processing
#   a) part.1()        -  a text processing function
#   b) htmlToText()    - an HTML parser for pseudo-Cosma
# 2) Markov Matrix
#   a) mm.generator()  - Generates a markov matrix  
#   b) actual.states() - Called by mm.generator(). Produces ngram frequencies and lookup tables
#   c) ngram()         - Called by actual.states(). Generates "ngrams" for a set of input states
#   d) matrix.insert() - Called by mm.generator(). Inserts values at specified elements of a matrix
# 3) Simulator
#   a) simulate.text()   - Generates simulated text from a Markov Model
#   b) set.precursor.1() - Called by simulate.text(). Chooses a starting word for a 1st order model
#   c) set.precursor.2() - Called by simulate.text(). Chooses a starting word for a 2nd order model
#   d) new.sentence()    - Called by simulate.text(). Simulates a sentence
#   e) next.word()       - Called by simulate.text(). Outputs the next state picked by the simulation
#   f) next.row.2()      - Called by new.sentence(). Calculates the new state history based on the previous history and the most recent state
#   g) curtail()         - Called by simulate.text(). Truncates a simulated sentence if a ".", "?", or "!" is reached
# 4) Model Scoring
#   a) lang.model.prob() - Data frame with relelvant information on test data: probabilities, etc.
#   b) lang.scoring()    - Convenience wrapper for language model scoring functions.
#   c) perplexity()      - Calculates the perplexity for a given sequence of states and Markov Model as input
#   d) out.of.vocab()    - The rate at which test words do not appear in the training corpus
#   e) avg.likelihood()  - Average log likelihood of transistion states according to markov matrix
#   f) avg.prob()        - Average probability of transition states as calculated by markov matrix
#   g) out.of.ngram()    - Calculates the out of n-gram Rate
# 5) Smoothing
#   a) laplace.smoother()  - Add 1 smoothing
#   b) gte.smoother()      - Good Turing Estimate Smoother
# 6) Cross Validation
#   a) seq.cross.val()  - Sequential cross validation

#-----------------
# Required Packages
#------------------
library(Rstem)
library(plyr)
library(tau)
library(foreach)


#_________________________
# (1) Text Processing
#_________________________
#_________________________
# (1) Text Processing
#_________________________
#--------------
# (1.a) part.1() - Text Processing Function
#---------------
# Function takes a text document 
# Commands used to construct sample html file for pseudo cosma.
# (Done from bash terminal)
# curl -O http://cscs.umich.edu/~crshalizi/weblog/[600-841].html  
# cat [7-8][0-3]*.html > blog.html
# Number of words in this file (after html stripped out) = 34532

# Function takes a text document 
part.1<-function(text="pg526.txt", stem=FALSE, cosma=FALSE){

  text<-readLines(text)
  if(cosma==TRUE){
		text<-paste("blog.html", collapse='\n')
		text<-htmlToText(text)		
		text<-gsub("Three-Toed Sloth", " ", x=text)
	}
	else{
		text<-paste(text[40:((length(text)-361))], collapse='\n') # removing parts of file which pertain to license. 40 and -361 correspond to the start/end parts of the HOD document. 
	}

	pat<-c("[[:alpha:]]*||\\.||\\!||\\?")

	m <- gregexpr(pat, text, ignore.case = TRUE)
	x <- regmatches(text, m)
	x <- do.call(c, x)

	m <- regexec(pat, x, ignore.case = TRUE)
	words <- regmatches(x, m)
	words[words==""]<-NULL # Removes empty strings

	words.vector<-unlist(words) # Just converting list to vector

	if(stem==TRUE){
		library(Rstem)
		final.words<-wordStem(words.vector, language="english") 
		return(final.words)
	}
	return(words.vector)
}

# Used for pseudo-cosma. Not written by group. Since was not required for assignment, code used from external source, provided it was cited, should be ok.

# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}


#_________________________
# (2) Markov Matrix
#_________________________

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

#----------------------------------------------
# (2.a) Markov Matrix Generator
# Input: states.vec - a vector of observed states
#        order - the desired order of Markov Model (i.e. how many previous states the process depends on)
# Output: (1) A markov matrix for the input sequence
#         (2) A lookup table of possible states
#         (3) A lookup table of transition states
#         (4) A reference for the probability of unobserved states
#         (5) A frequency matrix, if requested
# Calls: actual.states(), ngram(), matrix.insert()
# Requires: plyr and foreach
#------------------------------------------------

mm.generator <- function(states.vec, order, frequency.matrix = FALSE) {
  
  # Get the observed frequencies from actual.states()
  actual.states.objects <- actual.states(states.vec, order)
  position.table <- actual.states.objects$position.table
  history.dim <- actual.states.objects$history.dim
  trans.states.dim <- actual.states.objects$trans.states.dim
  
  # Preallocate and empty matrix
  # Count the number of unique histories
  unique.histories <- dim(history.dim)[1]
  # Count the number of unique states
  unique.count <- dim(trans.states.dim)[1]
  # Preallocate Step
  empty.mm <- matrix(0, nrow=unique.histories, ncol = unique.count)
  
  #Use "RowNum" and "ColNum" from position.table to populate the empty.mm
  freq.matrix <- matrix.insert(matrix.name = empty.mm, insert.row = position.table$RowNum
              , insert.col = position.table$ColNum, insert.val = position.table$Freq)
  
  row.totals <- apply(X=freq.matrix, MARGIN=1, FUN = sum)
  mm <- freq.matrix / row.totals
  dimnames(mm) <- NULL

  # Return all markov matrix objects
  if (frequency.matrix == FALSE) { 
    markov.objects <- list("markov.matrix" = mm
        , "history.dim" = history.dim
        , "trans.states.dim" = trans.states.dim
        , "order" = order
        , "unobserved.prob" = NA
        )
  } else {
    markov.objects <- list("markov.matrix" = mm
        , "history.dim" = history.dim
        , "trans.states.dim" = trans.states.dim
        , "order" = order
        , "unobserved.prob" = 0
        , "freq.matrix" = freq.matrix
        )
  }
  return(markov.objects)
}

#-------------------------------------------------
# (2.b) Actual States
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
# (2.c) Ngram function
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
# (2.d) Matrix Insert Function
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


#_________________________
# (3) Simulator
#_________________________  

# Markov Model Language Simulator
# Lizzie Silver
# Last updated: 11.29.2011

# to do:
# add more tests re: text.vector; precursor.word outside the model; full.sentences w/ small nwords
# Bug fix: if an element of nwords is 0, the mode outputs a two word vector. WHY?!

#----------------------------------------------
# 1st and 2nd order Markov Model Language Simulator: 
# simulate.text()
#
# Inputs:
# mm.object - a markov model produced by Jason Capehart's generator. It should be a list with the
#    following structure:
#    mm.object[[1]] - the transition matrix. For a 1st-order model each row corresponds to a single 
#      word observed in the training data. For a 2nd-order each row corresponds to an ordered pair 
#      of words.
#    mm.object[[2]] - a data frame for converting the matrix row indices to the corresponding words.
#      mm.object[[2]][,1] is the indices, mm.object[[2]][,2] is the words (or pairs of words in the
#      2nd order case). At the moment Jason's output has strings as factors, so these are coerced
#      to characters during simulation. Also note that the pairs of words are character vectors of 
#      length 1 (i.e. the words have been pasted together with a space between) rather than length 2
#    mm.object[[3]] - another data frame, for converting matrix column indices to words. In the 1st
#      order case this will be identical to mm.object[[2]], whereas in the second order case they 
#      differ: mm.object[[3]] contains the new word generated by the transition matrix, whereas 
#      mm.object[[2]] contains the two-word history used to generate it.
# order - the order of the model (i.e. how long the history is)
# nwords - the desired output vector length (if it's a vector, simulate.text returns a list of 
#    vectors of the desired lengths). 
#    NOTE: if full.sentences is TRUE, the actual output length may be shorter than nwords!
# precursor.word - the starting state for the simulation. It is NOT included in the output.
#    precursor.word is set to c("sea", ".") by default, so that the first word of the output vector  
#    will naturally be a sentence-starter word. Try setting it to a question mark or exclamation 
#    mark.
#    NOTE: precursor.word should be one of the states in the model. If it is outside the model, you 
#      MUST provide text.vector so that an alternative precursor can be chosen. If precursor.word 
#      is outside the model AND text.vector==NULL, simulate.text will stop.
#    In the 1st order case, if precursor.word has length > 1, simulate.text takes the last element.
#    In the 2nd order case, precursor.word may be either: 
#      (1) a character vector of length >= 2 - for example: precursor.word=c("Hello", "world") - or
#      (2) a character vector containing at least two words separated by a space = for example: 
#          precursor.word="Hello world" 
#      If precursor.word is longer, simulate.text takes the last two elements.
#      NOTE: If precursor.word only contains one word, another word will be chosen to precede it, 
#        sampled from all the instances of the given word in text.vector. 
#        For example: if the user inputs precursor.word=".", the actual precursor word might 
#        become "over ." or "end .", if the sequences ("over", ".") and ("end", ".") both appear 
#        in text.vector.
# If random.precursor==TRUE, the precursor word is chosen randomly from text.vector. This overrides
#    the value of precursor.word.
#    Note: just like precursor.word, this randomly chosen word will not appear in the output.
# text.vector - used for setting the precursor word. It is just a vector of the text used to train 
#    Jason's model, as produced by Alexander Murray-Watters' program. 
#    text.vector MUST be provided if: 
#    (a) random.precursor==TRUE; or
#    (b) the model is 2nd order, AND precursor.word has length 1; or
#    (c) precursor.word is not in the model.
# If full.sentences==TRUE, simulate.text truncates everything after the last period, question 
#    mark or excalamation mark in the output (if there is at least one period, question mark or 
#    exclamation mark).
# 
# Output:
# If nwords is a scalar, simulate.text outputs a character vector of length==nwords (or, if 
#    full.sentences==TRUE, shorter). 
# If nwords is a vector, simulate.text outputs a list of such character vectors.
#----------------------------------------------
simulate.text <- function(mm.object, order, nwords, precursor.word=c("sea","."), 
                            random.precursor=FALSE, text.vector=NULL, 
                            full.sentences=FALSE){
  x <- mm.object[[1]]
  row.dictionary <- mm.object[[2]]
  column.dictionary <- mm.object[[3]]

  # TESTS TESTS TESTS
  # Test: x should be a matrix; have non-negative entries
  stopifnot(is.matrix(x), all(x >= 0))
  # Test: rowsums of x should all equal 1
  K <- nrow(x) 
  stopifnot(all.equal(as.vector(rowSums(x)),rep(1,K))) 
  # Test: nwords should be a numeric vector of length 1 or more; non-negative entries; 
  #   integer entries; at least one non-zero entry
  stopifnot(is.numeric(nwords), length(nwords)>=1, sum(nwords>=0)==length(nwords), 
            sum(nwords==round(nwords))==length(nwords), sum(nwords)>=1)
  # END TESTS

  new.text <- list()

  # Creating each chunk of new text:
  for (i in 1:length(nwords)){     
    # Set the precursor word (depending on order):
    if (order==1){
      word.0 <- set.precursor.1(precursor.word, random.precursor, text.vector, row.dictionary)
    }
    if (order==2){
      word.0 <- set.precursor.2(precursor.word, random.precursor, text.vector, row.dictionary)
    }
    # Create a new chunk of text and add it to the list:
    sentence <- new.sentence(x, nwords=nwords[i], word.0, row.dictionary, column.dictionary, order)
    # If full.sentences==TRUE, cut off any half-sentences after the last ".", "?" or "!":
    if (full.sentences==TRUE){  
      sentence <- curtail(sentence)
    }
    new.text[[i]] <- sentence
  } 

  # if nwords is a scalar, simulate.1st.order returns a character vector, not a list:
  if (length(nwords)==1){  
    new.text <- new.text[[1]]
  }
  return(new.text)
}


#----------------------------------------------
# Set the precursor word for 1st order models
#----------------------------------------------
set.precursor.1 <- function(precursor.word, random.precursor, text.vector, row.dictionary){
    if (random.precursor==TRUE){    
      # Make sure you have some text to sample from:
      if(is.null(text.vector)==TRUE){stop("Error: Simulator cannot sample a random precursor.word 
    	unless you provide a value for text.vector.")}
      word.0 <- sample(text.vector, size=1)
    } else {
      # If the precursor is provided as a string of multiple words, split them up:
      precursor.vector <- unlist(strsplit(precursor.word, split=" "))
      # if the precursor has more than one word, take the last:
      word.0 <- precursor.vector[length(precursor.vector)] 
      # Test: check that word.0 is in the model:
      if (length(which(row.dictionary[,2]==word.0))==0) {
        warning("Specified precursor word is outside the model. 
                A different precursor will be chosen at random.")
        if(is.null(text.vector)==TRUE){stop("Error: Simulator cannot sample a random precursor.word 
  		  unless you provide a value for text.vector.")}
        word.0 <- sample(text.vector, size=1)
      }
    }
  return(word.0)
}

#----------------------------------------------
# Set the precursor word for 2nd order models
#----------------------------------------------
set.precursor.2 <- function(precursor.word, random.precursor, text.vector, row.dictionary){
  # Random first word:
  # Make sure you have some text to sample from:
  if (random.precursor==TRUE){    
  	if(is.null(text.vector)==TRUE){stop("Error: Simulator cannot sample a random precursor.word 
  		 unless you provide a value for text.vector.")}
    # Pick a random index from text.vector (excluding the first word):
    word.0.index <- sample(c(2:length(text.vector)), size=1)
    # Take the word at that index, and the one preceding it:
    word.0 <- paste(as.character(text.vector[word.0.index-1]), as.character(text.vector[word.0.index]), sep=" ")
  } else {

    # If precursor.word has at least two words, take the last two:
    precursor.vector <- unlist(strsplit(precursor.word, split=" "))
    if (length(precursor.vector)>=2){
      word.0 <- paste(precursor.vector[length(precursor.vector)-1], precursor.vector[length(precursor.vector)], sep=" ")
    } else {

      # If precursor.word only contains one word, we sample another from text.vector:
      if(is.null(text.vector)==TRUE){stop("Error: Simulator cannot sample a random precursor.word 
  		unless you provide a value for text.vector.")}
      # 1. Go to text.vector, remove the first word. 
      # 2. Sample an instance of precursor.word in text.vector. 
      # 3. Take the preceding word from that instance.
      instances <- which(text.vector==precursor.word)
      if(length(instances)==1){
        word.0.index <- instances
      } 
      if(length(instances)>1){
        word.0.index <- sample(instances, size=1)
      }
      if(length(instances)==0){
      	warning("Specified precursor word is outside the model! A different precursor will be chosen at random.")
        word.0.index <- sample(length(text.vector), size=1)
      }      
      # But just in case we end up with the very first word:
      if (word.0.index==1){word.0.index <- 2} 
      word.0 <- paste(as.character(text.vector[word.0.index-1]), as.character(text.vector[word.0.index]), sep=" ")
    }
  }
  # Test: check that word.0 is in the model:
  if (length(which(row.dictionary[,2]==word.0))==0) {
    warning("Specified precursor word is outside the model! A different precursor will be chosen at random.")
    if(is.null(text.vector)==TRUE){stop("Error: Simulator cannot sample a random precursor.word 
  	  unless you provide a value for text.vector.")}
    word.0.index <- sample(c(2:length(text.vector)), size=1)
    word.0 <- paste(as.character(text.vector[word.0.index]), as.character(text.vector[word.0.index+1]), sep=" ")
  }
  return(word.0)
}


#----------------------------------------------
# new.sentence
# Inputs: a precursor word (not included in output), a 1st or 2nd order markov transition matrix, 
# and a row and a column dictionary (data frames) linking rows & column indices in the matrix to 
# corresponding words
# Output: a character vector of simulated text.
#----------------------------------------------
new.sentence <- function(x, nwords, word.0, row.dictionary, column.dictionary, order){
  # Create an empty vector to hold the output:
  sentence <- vector(length=nwords)
  # Translate the precursor word into a row of the transition matrix:
  word.i <- which(row.dictionary[,2]==word.0)
  # Generate a vector of new words (actually just column indices) generated by the model
  for (i in 1:nwords){
    word.j <- next.word(word.i, x)
    sentence <- c(sentence, word.j)
    if (order==1){
      word.i <- word.j
    }
    if (order==2){
      word.i <- next.row.2(word.i, word.j, row.dictionary, column.dictionary)
    }
  }
  # Translate the column indices into the words they correspond to:
  sentence <- as.character(column.dictionary[sentence,2])
  return(sentence)
}


#----------------------------------------------
# Given a transition matrix and a row number (word.i), next.word returns the column number 
# after one transition. Works the same for both 1st and 2nd order models.
#----------------------------------------------
next.word <- function(word.i, x){
  K <- ncol(x) 
  word.j <- sample(1:K,size=1,prob=x[word.i,]) 
  return(word.j)
}

#----------------------------------------------
# next.row.2 outputs the next row index after one transition in second order models.
#----------------------------------------------
next.row.2 <- function(word.i, word.j, row.dictionary, column.dictionary){
  # Translate previous row number into character vector:
  word.i.char <- as.character(row.dictionary[word.i,2])
  # Extract the two individual words:
  word.i.components <- unlist(strsplit(word.i.char, split=" "))
  # Translate previous column number into character vector:
  word.j.char <- as.character(column.dictionary[word.j,2])
  # Take the second component of word.i and paste it onto word.j:
  next.row.string <- paste(word.i.components[2],word.j.char, sep=" ")
  # Find the corresponding row number:
  next.row <- which(row.dictionary[,2]==next.row.string)
  return(next.row)
}


#----------------------------------------------
# curtail takes a character vector and curtails it after the last period/question mark/exclamation
# mark (if it contains at least one period/question mark/exclamation mark).
#----------------------------------------------
curtail <- function(sentence){
  # if there is at least one period/question mark/exclamation mark,
  if ((sum(sentence=="." | sentence=="!" | sentence=="?")>0) 
      # and it is not already the last word,
      & (sentence[length(sentence)] != ".") 
      & (sentence[length(sentence)] != "!")
      & (sentence[length(sentence)] != "?")){ 
    # then the position of the last period/question mark/exclamation mark is the end of the vector
    end <- max(which(sentence == "."| sentence == "!" | sentence == "?")) 
    # and we cut off everything else
    sentence <- sentence[-c((end+1):length(sentence))] 
  }
  return(sentence)
}



#_________________________
# (4) Model Scoring
#_________________________  

#------------------------------------
# Language Model Probability
# Input: input.vec - test data which a character vector with a sequence of states
#        markov.object - an object created by mm.generator()
# Output: a data frame with the language model's probability for each sequence in the input text
# Requires: tau, foreach
# Calls: ngram(), mm.generator()
# TODO: Remove "unobserved.prob" step from language scoring functions
#       and insert it into lang.model.prob() instead. That will allow
#       an arbitrary penalty to be input without writing into the prob.df
#       multiple times. The sequential.cross.valdiation function will have to
#       be adjusted as well.
#------------------------------------

lang.model.prob <- function(input.vec, markov.object) {
  
  require(tau)
  # Check to make sure input.vec is a character vector
  stopifnot(is.character(input.vec))
  
  # (1) Base prob.df
  #---------------
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
  
  # (2) Test probabilities for Perplexity calculation
  #----------------------
  # Count unique words in the test set
  test.unigram <- textcnt(input.vec, split = NULL, method = "string", n = 1L, tolower = FALSE)
  # Construct a test set probability data frame
  test.prob.df <- data.frame("word" = names(test.unigram), "test.prob" = as.vector(test.unigram)/sum(test.unigram))
  # Merge the results to prob.df
  prob.df <- merge(x = prob.df, y = test.prob.df, by.x="TransState", by.y = "word", all.x=TRUE)
  
  # (3) Meta data on prob.df
  #-------------------
  # Calculate the Out of Vocabulary and Out of Ngram flags
  oo.vocab <- ifelse(is.na(prob.df$ColNum), yes = 1, no = 0) # Out of Vocabulary flag
  oo.ngram <- ifelse(is.na(prob.df$RowNum), yes = 1, no = 0) # Out of Ngram flag
  # Add the prob.vec, oo.vocab, and oo.ngram to prob.df
  prob.df <- data.frame(prob.df, "Prob" = prob.vec, "oo.vocab.flag" = oo.vocab, "oo.ngram.flag" = oo.ngram)
  
  # (4) Unobserved probability substitution
  #-------------------
  # Replace unobserved state histories with probability specified by markov object
  prob.df[which(is.na(prob.df$Prob) | prob.df$Prob == 0), "Prob"] <- markov.object$unobserved.prob
  
  return(prob.df)
}

#-------------------------------------------------
# Language Model Scoring Function
# This is a convenience wrapper for several scoring functions
# Input: prob.vec - a vector of probabilities
# Output: a data.frame that includes measures for
#        out.of.vocab - out of vocabulary rate
#        perplexity
#        avg.log - average log-likelihood
#        avg.prob - average probability
#-------------------------------------------------

lang.scoring <- function(prob.df, ...) {
  oov <- out.of.vocab(oo.vocab.flag = prob.df$oo.vocab.flag)
  oon <- out.of.ngram(oo.ngram.flag = prob.df$oo.ngram.flag)
  perplexity.score <- perplexity(prob.vec = prob.df$Prob, ...)
  avg.log.likelihood <- avg.log(prob.vec = prob.df$Prob)
  avg.prob <- avg.prob(prob.vec = prob.df$Prob)
  
  score.df <- data.frame("out.of.vocab" = oov
                     ,"out.of.ngram" = oon
                     , "perplexity" = perplexity.score
                     , "avg.log.likelihood" = avg.log.likelihood
                     , "avg.prob" = avg.prob)
  return(score.df)
}


#---------------------------------------------
# Perplexity
# Input: prob.vec - a vector of probabilities from a language model
#        cross.entropy - whether perplexity should be calculated using cross.entropy or entroy
# Output: the perplexity score
#----------------------------------------------

perplexity <- function(prob.vec, cross.entropy = TRUE) {
  
  # Find the number of probabilities
  n <- length(prob.vec)
  # Calculate Perplexity
  if (cross.entropy == TRUE) {
    perplexity <- 2^(-sum(1/length(prob.vec)*log(prob.vec, base = 2)))
  } else {
    entropy <- -sum(prob.vec*log(prob.vec, base = 2))
    perplexity <- 2^entropy
  }
  return(perplexity)
}

#------------------------------------
## Average log-likelihood 
## Input: prob.vec - probabilities from the markov.object assigned to each transitions in test.vec 
## Output: Average log-likelihood
#------------------------------------

avg.log <- function(prob.vec) {

	avg.log.likelihood <- 1/length(prob.vec)*sum(log(prob.vec, 2))
	
	return("AvgLogLikelihood"=avg.log.likelihood)
}

#-----------------------------
## Out-of-Vocab rate
## Input: oo.vocab.flag
## Output: The rate of out-of-vocabularies in test input
#------------------------------

out.of.vocab <- function(oo.vocab.flag){  
	return(sum(oo.vocab.flag)/length(oo.vocab.flag))
}

#-----------------------------
## Out-of-Ngram rate
## Input: oo.ngram.flag
## Output: The rate of out-of-vocabularies in test input
#------------------------------

out.of.ngram <- function(oo.ngram.flag){  
  return(sum(oo.ngram.flag)/length(oo.ngram.flag))
}


#---------------------------------
# Average probability
# Input: prob.vec
# Output: The average probability with NAs substituted with zeros
#----------------------------------

avg.prob <- function(prob.vec){
  
  #prob <- replace(prob.vec,is.na(prob.vec),0)
	avg.prob <- mean(prob.vec)
	
	return(avg.prob)
}

#____________________________________
# Smoothing
#______________________________________

#-------------------
# Smoother Wrapper Function
# Input: markov.object
#        type  - The desired type of smoothing
# Output: markov.object - A smoothed markov object
#-------------------

smoother <- function(markov.object, type = c("laplace", "good.turing")) {
  if (type == "laplace") {
    markov.object <- laplace.smoother(markov.object)
  }
  if (type == "good.turing") {
    markov.object <- gte.smoother(markov.object)
  }
  return(markov.object)
}

#-----------------------------------------------------
# Laplace Smoother (Add-1 Smoothing)
# Input: markov.object    - A markov object from the mm.generator() function
# Output: markov.object   - The same object as input, but with smoothed transition matrix
#         unobserved.prob - The probability assigned to all state histories with 0 observations
#------------------------------------------------------

laplace.smoother <- function(markov.object) {
  # Stop if frequency matrix does not exist in markov object
  stopifnot(markov.object$freq.matrix != NULL)
  
  # Add 1 to all observed histories
  markov.object$freq.matrix <- markov.object$freq.matrix + 1
  # Create a markov matrix
  row.totals <- apply(X=markov.object$freq.matrix, MARGIN=1, FUN = sum)
  mm <- markov.object$freq.matrix / row.totals
  dimnames(mm) <- NULL
  
  markov.object$markov.matrix <- mm
  
  # Calculate the probability of an unobserved history
  total.transition.states <- dim(markov.object$freq.matrix)[2]
  unobserved.prob <- 1 / total.transition.states
  markov.object$unobserved.prob <- unobserved.prob
  
  return(markov.object)
}

#----------------------------
# Good-Turing Method 
# Input: markov.object    - A markov object from the mm.generator() function
# Output: markov.object   - The same object as input, but with smoothed transition matrix
#         unobserved.prob - The probability assigned to all state histories with 0 observations
#--------------------------

gte.smoother <- function(markov.object){
  
	freq <- markov.object$freq.matrix
  
  # Calculate the probability of unobserved data
  total.observations <- sum(freq)
  unobserved.prob <- 1 / total.observations
  markov.object$unobserved.prob <- unobserved.prob
  
  # Good Turing Smoothing
	n <- max(freq)
	ns <- vector(length=n)
	for (i in 0:n){
	ns[i+1] <- sum(freq==i)
	}
	
	for(i in 0:n){
		freq[freq==i] <- (freq[freq==i]+1) * (ns[i+1]+1)/ns[i+1]
	}
	
  # Write the smoothed frequency matrix to the markov object
	markov.object$freq.matrix <- freq
	
  # Write the transition matrix to the markov object
	row.totals <- apply(X=freq, MARGIN=1, FUN = sum)
 	markov.object$markov.matrix <- freq/ row.totals
  
	return(markov.object)
}

#_________________________________
# Cross-Validation
#________________________________


#-----------------------------------------------------------
# Sequential Cross Validation for Language Modeling
# Cross validation that randomly selects a sequential set of training and test data
# Input: corpus  - a corpus in a character vector with 1 state per element
#        folds   - number of folds for cross validation
#        order   
#        frequency.matrix
#        smooth
#        unobserved.penalty.flag
# Output: Scores - language model scores for each run
# TODO: (1) As it stands this method creates 1 state history which is an error
#         any time the training set wraps around the end of the corpus
#--------------------------------------------------------------

seq.cross.val <- function(corpus, folds, order, smooth = FALSE, type = c("laplace", "good.turing")) {
  # Initialize data frame for scoring results
  test.score.df <- NULL
  
  for (i in 1:folds) {
  
    # Train and test set creation procedure
    #---------------------------------
    total.states <- length(corpus)
    train.size <- floor(total.states - (total.states/folds))
    # Select a start and end point
    start.point <- sample(x = 1:total.states, size = 1)
    end.point <- ifelse(start.point == 1
                        , yes = total.states - (total.states/folds)
                        , no = (start.point + train.size)%%total.states
                        ) # Handle case where 1 is the starting point
    # Construct the train and test set
    if ((start.point + train.size) > total.states) { # Test to see if train set wraps around the end of corpus
      train <- corpus[c(start.point:total.states, 1:(end.point-1))]
      test <- corpus[end.point:(start.point-1)]
    } else {
      train <- corpus[start.point:end.point]
      test <- corpus[c((end.point+1):total.states, 1:start.point)]
    }
    
    # Scoring
    #----------------------------
    # Make the markov model from training data
    train.markov.object <- mm.generator(states.vec = train, order, frequency.matrix = TRUE)
    # Implement a smoother if called for
    if (smooth == TRUE) {
      train.markov.object <- smoother(markov.object=train.markov.object, type)
    } else { train.markov.object$unobserved.prob <- 1 / sum(train.markov.object$freq.matrix)} # Use the good-turing unobserved data estimate for models with no smoothing
    # Find probabilities for the test set data based on the markov model
    test.prob <- lang.model.prob(input.vec = test, markov.object = train.markov.object)
    # Calculate scores
    test.score.i <- lang.scoring(prob.df = test.prob)
    # Store the results
    test.score.df <- rbind(test.score.df, test.score.i)
  }
  
  return(test.score.df)
}
