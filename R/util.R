# Split A Vector Into Equally Populated Factors
#
# Assigns each member of a vector to a factor, based on the quantiles of the
# distribution, so that each factor is equal populated. Levels range from
# one to \code{nfactors}.
#
# @param x Numeric vector
# @param nfactors Number of factors required.
# @return factor-encoded vector specifying the level for each item in the
# vector.
equal_factors <- function(x, nfactors) {
  breaks <- quantile(x, probs = seq(0, 1, length.out = nfactors + 1))
  cuts <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  as.factor(cuts)
}
