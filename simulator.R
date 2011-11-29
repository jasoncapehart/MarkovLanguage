# Markov Model Language Simulator
# Lizzie Silver
# Last updated: 11.26.2011

# to do:
# fix minor bug in set.precursor.2
# add more tests re: text.vector; precursor.word outside the model; full.sentences w/ small nwords

#----------------------------------------------
# 1st and 2nd order Markov Model Language Simulator: 
# simulate.text()
#
# Inputs:
# mm.object - a markov model produced by Jason Capehart's generator. It should be a list with the
#    following structure:
#    mm.object[[1]] - the transition matrix. For a 1st-order model it is square. For a 2nd-order 
#      model nrow >= ncol, with each row corresponding to a pair of words.
#    mm.object[[2]] - a data frame for converting the matrix row indices to the corresponding words.
#      mm.object[[2]][,1] is the indices, mm.object[[2]][,2] is the words (or pairs of words in the
#      2nd order case). At the moment Jason's output has strings as factors, so these are coerced
#      to characters during simulation. Also note that the pairs of words are character vectors of 
#      length 1 (i.e. the words have been pasted together with a space between) rather than length 2
#    mm.object[[3]] - another data frame, for converting matrix column indices to words. In the 1st
#      order case this will be identical to mm.object[[2]], whereas in the second order case they 
#      differ: mm.object[[2]] contains the preceding two words used to generate the following word,
#      which is in mm.objecct[[3]]
# order - the order of the model
# nwords - the desired output vector length (if it's a vector, simulate.text returns a list of 
#    vectors of the desired lengths). 
#    Note: if full.sentences is TRUE, the actual output length may be shorter than nwords!
# precursor.word - the starting state for the simulation. It is NOT included in the output.
#    precursor.word is set to a period by default, so that the first word of the output vector will  
#    naturally be a sentence-starter word. Try setting it to a question mark or exclamation mark.
#    In the 1st order case, if precursor.word has length > 1, simulate.text takes the last element.
#    In the 2nd order case, precursor.word should be a character vector of length 2. 
#      For example: precursor.word=c("Hello", "world")
#      If it is  longer, simulate.text takes the last two elements.
#      If it only has one element, another word will be chosen to precede it, sampled from all the 
#      instances of that word in text.vector. 
#        For example: if the user inputs precursor.word=".", the actual precursor word might 
#        become "over ." or "end .", if the sequences ("over", ".") and ("end", ".") both appear 
#        in text.vector.
#    precursor.word MUST be one of the states in the model (unless random.precursor==TRUE).
#      If it is outside the model, a warning will appear and a different precursor.word will be 
#      chosen at random from text.vector.
# If random.precursor==TRUE, the precursor word is chosen randomly from text.vector. This overrides
#    the value of precursor.word.
#    Note: just like precursor.word, this randomly chosen word will not appear in the output.
# text.vector - used for setting the precursor word. It is just a vector of the text used to train 
#    Jason's model, as produced by Alexander Murray-Watters' program. 
#    text.vector MUST be provided if: 
#    (a) random.precursor==TRUE; or
#    (b) the model is 2nd order, AND precursor.word has length 1, or
#    (c) precursor.word is not in the model.
# If full.sentences==TRUE, simulate.text truncates everything after the last period, question 
#    mark or bang in the output (if there is at least one period, question mark or bang).
# 
# Output:
# If nwords is a scalar, simulate.text outputs a character vector of length==nwords (or, if 
#    full.sentences==TRUE, shorter). 
# If nwords is a vector, simulate.text outputs a list of such character vectors.
#----------------------------------------------
simulate.text <- function(mm.object, order, nwords, precursor.word=".", 
                            random.precursor=FALSE, text.vector=NULL, 
                            full.sentences=FALSE){
  x <- mm.object[[1]]
  row.dictionary <- mm.object[[2]]
  column.dictionary <- mm.object[[3]]

  # TESTS TESTS TESTS --------------------------
  # Test: x should be a matrix; have non-negative entries
  stopifnot(is.matrix(x), all(x >= 0))
  # Test: rowsums of x should all equal 1
  K <- nrow(x) 
  stopifnot(all.equal(as.vector(rowSums(x)),rep(1,K))) 
  # Test: nwords should be a numeric vector of length 1 or more; non-negative entries; 
  #   integer entries; at least one non-zero entry
  stopifnot(is.numeric(nwords), length(nwords)>=1, sum(nwords>=0)==length(nwords), 
            sum(nwords==round(nwords))==length(nwords), sum(nwords)>=1)
  # Test: if random.precursor==TRUE, text.vector should be a character vector
  stopifnot((random.precursor==FALSE || is.character(text.vector)))
  # Test: if the model is 2nd-order but precursor.word has length 1, text.vector should be a 
  #   character vector
  stopifnot((length(precursor.word)>=2 || order==1 || is.character(text.vector)))
  ### Need to add test: precursor.word should appear in the word column of dictionary (or 
  #     if not, text.vector must be provided).
  # END TESTS ----------------------------------

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
      word.0 <- sample(text.vector, size=1)
    } else {
      # if the user specified a vector with more than one element, take the last:
      word.0 <- precursor.word[length(precursor.word)] 
      # Test: check that word.0 is in the model:
      if (length(which(row.dictionary[,2]==word.0))==0) {
        warning("Specified precursor word is outside the model. 
                A different precursor will be chosen at random.")
        word.0 <- sample(text.vector, size=1)
      }
    }
  return(word.0)
}

#----------------------------------------------
# Set the precursor word for 2nd order models
#----------------------------------------------
set.precursor.2 <- function(precursor.word, random.precursor, text.vector, row.dictionary){
  if (random.precursor==TRUE){    
    # Pick a random index from text.vector (excluding the first word):
    word.0.index <- sample(c(2:length(text.vector)), size=1)
    # Take the word at that index, and the one preceding it:
    word.0 <- paste(as.character(text.vector[word.0.index-1]), as.character(text.vector[word.0.index]), sep=" ")
  } else {
    # If precursor.word has at least two elements, take the last two:
    if (length(precursor.word)>=2){
      word.0 <- paste(precursor.word[length(precursor.word)-1], precursor.word[length(precursor.word)], sep=" ")
    } else {
      # If precursor.word only has one element: 
      # 1. Go to text.vector, remove the first word. 
      # 2. Sample an instance of precursor.word in text.vector. 
      # 3. Take the preceding word from that instance.
      instances <- which(text.vector[-1]==precursor.word)
      if(length(instances)==1){
        word.0.index <- instances
      } else {
        word.0.index <- sample(instances, size=1)
      }
      word.0 <- paste(as.character(text.vector[word.0.index]), as.character(text.vector[word.0.index + 1]), sep=" ")
 ### ^ need to add a fix for when the ONLY instance is the 1st word of text.vector

      # Test: check that word.0 is in the model:
      if (length(which(row.dictionary[,2]==word.0))==0) {
        warning("Specified precursor word is outside the model. 
                A different precursor will be chosen at random.")
        word.0.index <- sample(c(2:length(text.vector)), size=1)
        word.0 <- paste(as.character(text.vector[word.0.index-1]), as.character(text.vector[word.0.index]), sep=" ")
      }
    }
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
# curtail takes a character vector and curtails it after the last period/question mark/bang (if it 
# contains at least one period/question mark/bang).
#----------------------------------------------
curtail <- function(sentence){
  # if there is at least one period/question mark/bang,
  if ((sum(sentence=="." | sentence=="!" | sentence=="?")>0) 
      # and it is not already the last word,
      & (sentence[length(sentence)] != ".") 
      & (sentence[length(sentence)] != "!")
      & (sentence[length(sentence)] != "?")){ 
    # then the position of the last period/question mark/bang is the end of the vector
    end <- max(which(sentence == "."| sentence == "!" | sentence == "?")) 
    # and we cut off everything else
    sentence <- sentence[-c((end+1):length(sentence))] 
  }
  return(sentence)
}

#----------------------------------------------
# TESTS TESTS TESTS
# Test cases: 
# forest.test produces a deterministic matrix should output the sentence, 'More 
#   griping from the disenchanted forest.' over and over again. It contains a period so you
#   can try out full.sentences=TRUE and also the default precursor.word, "."
# rain.test is slightly longer and non-deterministic (the word "Don't", for example, is 
#   followed by several other words). However it contains no periods (as individual words) so 
#   you can't use the default precursor.word, and full.sentences must be FALSE
#----------------------------------------------
forest.test <- c("More", "griping", "from", "the", "disenchanted", "forest", ".", "More", "griping")
rain <- "Don't tell me not to live, Just sit and putter, Life's candy and the sun's A ball of butter. Don't bring around a cloud To rain on my parade! Don't tell me not to fly-- I've simply got to. If someone takes a spill, It's me and not you. Who told you you're allowed To rain on my parade! I'll march my band out, I'll beat my drum, And if I'm fanned out, Your turn at bat, sir. At least I didn't fake it. Hat, sir, I guess I didn't make it! But whether I'm the rose Of sheer perfection, Or freckle on the nose Of life's complexion, The cinder or the shiny apple of its eye, I gotta fly once, I gotta try once, Only can die once, right, sir? Ooh, life is juicy, Juicy, and you see I gotta have my bite, sir! Get ready for me, love, cause I'm a commer, I simply gotta march, My heart's a drummer. Don't bring around a cloud To rain on my parade! I'm gonna live and live now, Get what I want--I know how, One roll for the whole show bang, One throw, that bell will go clang, Eye on the target and wham One shot, one gun shot, and BAM Hey, Mister Armstein, Here I am! I'll march my band out, I will beat my drum, And if I'm fanned out, Your turn at bat, sir, At least I didn't fake it. Hat, sir, I guess I didn't make it. Get ready for me, love, 'cause I'm a commer, I simply gotta march, My heart's a drummer. Nobody, no, nobody Is gonna rain on my parade!"
rain.test <- unlist(strsplit(rain, split=" "))
forest.object.1 <- mm.generator(forest.test, 1)
forest.object.2 <- mm.generator(forest.test, 2)
rain.object.1 <- mm.generator(rain.test, 1)
rain.object.2 <- mm.generator(rain.test, 2)
# Try these test cases: (a) The deterministic model, forest:
simulate.text(forest.object.1, order=1, nwords=12, precursor.word=".", random.precursor=FALSE, 
              text.vector=NULL, full.sentences=FALSE)
simulate.text(forest.object.1, order=1, nwords=12, precursor.word=".", random.precursor=TRUE, 
              text.vector=forest.test, full.sentences=FALSE)
simulate.text(forest.object.1, order=1, nwords=c(5,10, 15), precursor.word=".", 
              random.precursor=TRUE, text.vector=forest.test, full.sentences=TRUE)
simulate.text(forest.object.2, order=2, nwords=12, precursor.word=c("from", "the"), 
              random.precursor=FALSE, text.vector=NULL, full.sentences=FALSE)
simulate.text(forest.object.2, order=2, nwords=12, precursor.word=".", random.precursor=TRUE, 
              text.vector=forest.test, full.sentences=FALSE)
simulate.text(forest.object.2, order=2, nwords=c(5,10, 15), precursor.word=".", 
              random.precursor=TRUE, text.vector=forest.test, full.sentences=TRUE)
# (b) The stochastic model, rain:
simulate.text(rain.object.1, order=1, nwords=c(12, 7, 5), precursor.word="Don't", random.precursor=FALSE, 
              text.vector=NULL, full.sentences=FALSE)
simulate.text(rain.object.1, order=1, nwords=12, precursor.word="Don't", random.precursor=TRUE, 
              text.vector=rain.test, full.sentences=FALSE)
simulate.text(rain.object.2, order=2, nwords=12, precursor.word=c("Don't","tell"), 
              random.precursor=FALSE, text.vector=NULL, full.sentences=FALSE)
simulate.text(rain.object.2, order=2, nwords=12, precursor.word="Don't", random.precursor=TRUE, 
              text.vector=rain.test, full.sentences=FALSE)
