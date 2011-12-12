# Alexander Murray-Watters
# amurrayw
# 36-350: Final Project - part 1

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
