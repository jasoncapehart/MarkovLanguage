## Sean Seung Su Han 
## Markov Language Model 
## Perplexity 
## Last Updated: 11/29/2011

#Generating Partial Markov Matrix 

#Inputs: states.vec - a vector of observed states 
#		 range - range of a vector to generate Markov Matrix 
#		 order - the desired order of markov matrix 
#Output: A markov matrix for input sequence 
#		 A lookup table of possible states
#		 A lookup talbe of transition states 
#Calls: part.1, mm.generator
#Requries: Rstem, plyr, tau, foreach

library(Rstem)
library(plyr)
library(tau)
library(foreach)

partial.mm <- function(states.vec,range=c(start,end),order){
	
	state.vec<-part.1(states.vec)
	
	#start point if start == 0, start = text[1]
	if (start==0){
		s.point <- 1
	} else {s.point <- round(length(state.vec)*(start/100)) }
	#end point if end = 100, end = length of state vector 
	if (end==100){
		e.point <- length(text)
	} else {e.point <- round(length(state.vec)*(end/100))}
	
	partial.state.vec <- state.vec[s.point:e.point]
	
	mm <- mm.generator(partial.state.vec,order)
	return(mm)
}


	
##Perplextiy 
# Call: partial.mm
# Inputs: states.vec, range
# Output: perplexity 
perplexity <- function(states.vec, range=c(start,end)) { 
	
	n <- length(states.vec)
	
	# Position of starting word 
	s <- round(n*(start/100))
	# Position of ending word 
	e <- round(n*(end/100))
	
	##Call. partial.mm and store as mat 
	mat <- partial.mm(states.vec,range)$markov.matrix
	hist.dim <- partial.mm(states.vec,range)$history.dim
	trans.dim <- partial.mm(states.vec,range)$trans.states.dim
	
	# Null vector which will store probabilities for each word from transition matrix 
	p <- vector()

	# Loop to store probabilities 
		for(i in 1:e) {
			p <- mat[match(states.vec[s+i],hist.dim), match(states.vec[s-1+i],trans.dim)]
		}
		
	#Smoothing 
	#	Replace Missing Values with small number, 1/50000
	p <-replace(p,is.na(p),1/50000)
	
	#Calculating Entropy
	entropy <- (-1)*sum(p*log(p,base=2))
	perplexity <- 2^entropy		
	
	return("perplexity"=perplexity)
	
}
