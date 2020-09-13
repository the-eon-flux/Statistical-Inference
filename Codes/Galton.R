install.packages("UsingR")

library(UsingR)
data(galton)

head(galton)
  # A data frame with 928 observations on the following 2 variables.
  # child (The child's height)  & parent (The “midparent” height')

library(reshape2)
longGalton <- melt(galton, measure.vars = c("child", "parent"))

head(longGalton)
