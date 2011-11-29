# Markov Language Testing

hod <- part.1(text="http://www.gutenberg.org/cache/epub/526/pg526.txt")

# First Order Test
start <- Sys.time()
mm.object.1 <- mm.generator(states.vec = hod, order = 1)
elapsed <- Sys.time() - start # 1.14 min with modification on mm step of mm.generator(), 2.78 min previously
simulate.text(mm.object.1, order = 1, nwords = 10)

# Second Order Test
start <- Sys.time()
mm.object.2 <- mm.generator(states.vec = hod, order = 2)
elapsed <- Sys.time() - start # 1.43 min with mm.generator() change, didn't work before
simulate.text(mm.object.2, order = 2, nwords = 10)

# Perplexity Test

perplexity(head(hod), range = c(10, 90), matrix = mm.object.1$markov.matrix
           , words = mm.object.1$history.dim[, 2])
