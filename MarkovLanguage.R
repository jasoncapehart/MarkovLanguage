# Markov Language Models

library(Rstem)
library(tau)

# Load the Heart of Darkness
hod <- readLines('http://www.gutenberg.org/cache/epub/526/pg526.txt')
hod <- tolower(hod)
hod <- gsub(pattern = '[!;.,?\"-]', replacement = '', x = hod)
hod <- strsplit(hod, ' ')
hod <- unlist(hod)

length(unique(hod))
# 6,874 unique words, numbers, and urls
hod.words <- unique(hod)
length(unique(wordStem(hod.words, "english")))
# Down to 5,290 unique words, numbers, and urls with the stemmer
hod.words <- unique(wordStem(hod.words, "english"))
length(as.vector(textcnt(hod.words, method="string", n = 2L)))
# 5306 unique bigrams
# 5315 unique trigrams <-- doesn't seem right'

example <- 'This; sentence, \"has ! commas -- and. Periods. And ? marks?'
nex <- gsub(pattern = '[!;.,?\"-]', replacement = '', x = example)
strsplit(nex, ' ')




simple <- 'This dog is a lab which is a mammal'
textcnt(simple, method = "string", n = 2L)
as.vector(textcnt(simple, method = "string", n = 2L))