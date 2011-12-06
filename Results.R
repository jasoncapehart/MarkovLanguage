# Language Model Results

hod <- part.1(text="http://www.gutenberg.org/cache/epub/526/pg526.txt")

#------------
# 1st Order + 1st Order Smoothed
#------------

mm.1 <- seq.cross.val(corpus=hod, folds=5, order = 1, smooth=FALSE)
# 5.88 minutes

mm.1.laplace <- seq.cross.val(corpus=hod, folds=5, order = 1, smooth=TRUE, type="laplace")
# 5.86 minutes

mm.1.gt <- seq.cross.val(corpus=hod, folds=5, order = 1, smooth=TRUE, type="good.turing")
# 34.1 minutes

#----------
# 2nd Order + 2nd Order Smoothed
#----------

mm.2 <- seq.cross.val(corpus=hod, folds=5, order = 2, smooth = FALSE)
# 7.92 minutes

mm.2.laplace <- seq.cross.val(corpus=hod, folds=5, order = 2, smooth=TRUE, type="laplace")
# 7.96 minutes
