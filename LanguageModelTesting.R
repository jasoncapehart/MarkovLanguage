# Markov Language Testing

hod <- part.1(text="http://www.gutenberg.org/cache/epub/526/pg526.txt")

# First Order Test
mm.object.1 <- mm.generator(states.vec = hod, order = 1)
simulate.text(mm.object.1, order = 1, nwords = 10)

# Second Order Test
mm.object.2 <- mm.generator(states.vec = hod, order = 2)
simulate.text(mm.object.2, order = 2, nwords = 10)


# Perplexity Test

perplexity(head(hod), range = c(10, 90), matrix = mm.object.1$markov.matrix
           , words = mm.object.1$history.dim[, 2])
