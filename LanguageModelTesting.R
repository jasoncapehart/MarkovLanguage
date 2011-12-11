# Markov Language Testing

hod <- part.1(text="http://www.gutenberg.org/cache/epub/526/pg526.txt")

# First Order Test
start <- Sys.time()
mm.object.1 <- mm.generator(states.vec = hod, order = 1, frequency.matrix = TRUE)
elapsed <- Sys.time() - start # 1.14 min with modification on mm step of mm.generator(), 2.78 min previously
simulate.text(mm.object.1, order = 1, nwords = 10)

# Second Order Test
start <- Sys.time()
mm.object.2 <- mm.generator(states.vec = hod, order = 2, frequency.matrix = TRUE)
elapsed <- Sys.time() - start # 1.43 min with mm.generator() change, didn't work before
simulate.text(mm.object.2, order = 2, nwords = 50, text.vector=hod)

# Scoring Functionality

check <- lang.model.prob(input.vec = test, markov.object=mm.object.1)
# First order: 2.2 seconds for 1,000 words
# Second order: 2.3 second for 1,000 words

lang.scoring(prob.df=check, cross.entropy = TRUE)
out.of.vocab(check$Prob)
avg.log(check$Prob)
perplexity(check$Prob)


# Smoothing Test

train <- c("This", "is", "a", "dog", "this", "is", "a", "cat", "which", "is", "a", "mammal")
test <- c("This", "is", "a", "bug", "this", "is", "a", "cat", "which", "is", "a", "mammal")

# Unsmoothed for comparsion
mm1 <- mm.generator(states.vec = train, order = 1, frequency.matrix = FALSE)
mm1.prob <- lang.model.prob(input.vec=test, markov.object = mm1)
lang.scoring(mm1.prob$Prob, oov.flag = mm1.prob$oov.flag)

# Laplace Smoothed
mm1 <- mm.generator(states.vec = train, order = 1, frequency.matrix = TRUE)
mm1.sm <- laplace.smoother(markov.object=mm1)
mm1.prob.sm <- lang.model.prob(input.vec=test, markov.object = mm1.sm)
lang.scoring(mm1.prob.sm$Prob, oov.flag = mm1.prob.sm$oov.flag)

# Good Turing Smoothed
mm1 <- mm.generator(states.vec = train, order = 1, frequency.matrix = TRUE)
mm1.gte <- gte.smoother(markov.object=mm1)
mm1.prob.gte <- lang.model.prob(input.vec=test, markov.object = mm1.gte)
lang.scoring(mm1.prob.gte$Prob, oov.flag = mm1.prob.gte$oov.flag)

# Smoothing Wrapper Test
mm1.smoother <- smoother(markov.object=mm1, type="good.turing")
mm1.smoother$markov.matrix