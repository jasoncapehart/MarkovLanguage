# Language Model Results

hod <- part.1(text="http://www.gutenberg.org/cache/epub/526/pg526.txt")

#------------
# 1st Order + 1st Order Smoothed
#------------

mm.1 <- seq.cross.val(corpus=hod, folds=5, order = 1, smooth=FALSE)
# 5.88 minutes

mm.1.laplace <- seq.cross.val(corpus=hod, folds=5, order = 1, smooth=TRUE, type="laplace")
# 5.86 minutes

#----------
# 2nd Order + 2nd Order Smoothed
#----------

mm.2 <- seq.cross.val(corpus=hod, folds=5, order = 2, smooth = FALSE)
# 7.92 minutes

mm.2.laplace <- seq.cross.val(corpus=hod, folds=5, order = 2, smooth=TRUE, type="laplace")
# 7.96 minutes

#----------
# 2nd Order + 2nd Order Smoothed
#----------
mm.3 <- seq.cross.val(corpus=hod, folds=5, order = 3, smooth = FALSE)
# 7.92 minutes



#---------
# Simulate text
#---------
sim.mm1 <- mm.generator(states.vec=hod, order = 1)
simulate.text(mm.object=sim.mm1, order=1, nwords=10)

sim.mm2 <- mm.generator(states.vec=hod, order = 2)
simulate.text(mm.object=sim.mm2, order=2, nwords=12)