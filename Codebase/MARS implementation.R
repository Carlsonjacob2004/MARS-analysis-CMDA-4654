library(earth)
library(dplyr)

fia <- read.csv("fia_model.csv")


seed(4654)
idx   <- sample(nrow(fia), 0.8 * nrow(fia))
train <- fia[idx, ]
test  <- fia[-idx, ]


# --- 4. Fit MARS ---
mars_gcv <- earth(
  tree_height ~ .,
  data    = train,
  degree  = 2,          # allow 2-way interactions
  pmethod = "backward", # prune by GCV (default)
  penalty = 3,          # GCV knot penalty d; earth uses 3 when degree > 1
  nk      = 50          # max terms in forward pass before pruning
)

summary(mars_gcv)
