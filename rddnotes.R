## Packages
library(rdd)
library(rdpower)
library(tidyverse)
library(tidymodels)

## Testing Package
RDestimate(dta$cvp_non_defense~f$c.80)

## Setting Seed
set.seed(12389)

## Simulating Data
x<-runif(1000,-1,1)
cov<-rnorm(1000)
y<-3+2*x+3*cov+10*(x>=0)+rnorm(1000)

## Function to Find Percentiles
percentile <- function(x) {
  
  unique(quantile(x, probs = c(seq(.01, 1, by = .01))))
  
}


## Saving Percentiles (Candidate Cut-points)

g <- percentile(df$age)

## Initializing Data Frame
f <- data.frame(df$wjtest01)

## Creates the Candidate (x-c)'s
for (i in 1:length(g)) {
  
  p <- paste0("X_", i)
  c <- assign(p, (x - g[i]))
  f <- data.frame(c, f)
  
}
r <- RDestimate(y~x, data=f)

## Drop the needed initialized column
f <- f %>%
  select(-X1)

## Testing Equivalencies
q <- x-g[98]
q == f$c.98

## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  models[[var]] = RDestimate(
    as.formula(paste0("y ~ ", var)),
    data = f
  )
}

## Selection Function
selection_criteria <- function(x) {
  d <- abs(mean(models[[x]]$est)/mean(models[[x]]$p))
  print(d)
}

## Testing
selection_criteria("c.49")

## Creating Cuts Name
cuts <- colnames(f)

## Finding Optimal Cut-point
s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)

max(s)

