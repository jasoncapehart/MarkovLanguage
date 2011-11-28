# Alexander Murray-Watters
# amurrayw
# 36-350: Final Project - part 1

# Function takes a text document 
part.1<-function(text="pg526.txt"){

	library(Rstem)
	library(plyr)

	text<-readLines(text)

	text<-paste(text[40:((length(text)-361))], collapse='\n') # removing parts of file which pertain to license. 40 and -361 correspond to the start/end points of the license document. 


	pat<-c("[[:alpha:]]*||\\.")

	m <- gregexpr(pat, text, ignore.case = TRUE)
	x <- regmatches(text, m)
	x <- do.call(c, x)

	m <- regexec(pat, x, ignore.case = TRUE)
	words <- regmatches(x, m)

	words[words==""]<-NULL # Removes empty strings

	words.vector<-unlist(words) # Just converting list to vector

	# This is the stemming part of the code - not sure if this is what was wanted as output.	
	final.words<-wordStem(words.vector, language="english") 


	return(final.words)
}



