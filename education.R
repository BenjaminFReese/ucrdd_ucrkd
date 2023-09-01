## Packages
library(rdd)
library(rdpower)
library(tidyverse)
library(tidymodels)
library(haven)

## Data
cvp <- read_dta("Datasets/CVP.dta")
elec <- read_dta("Datasets/HouseElectionResults.dta")

## Joining Data
dta <- cvp %>%
  left_join(elec, by = c("state", "dist", "cong"))

## Basic Plot
dta %>%
  filter(dta$cong == 111) %>%
  ggplot(aes(x = voteshare, y = cvp_agriculture)) +
  geom_point() +
  theme_bw()

## Setting Seed
set.seed(123889)

## Saving Percentiles (Candidate Cut-points)

g <- percentile(dta$voteshare)

## Initializing Data Frame
f <- data.frame(1)

## Creates the Candidate (x-c)'s
for (i in 1:length(g)) {
  
  p <- paste0("X_", i)
  c <- assign(p, (dta$voteshare - g[i]))
  f <- data.frame(c, f)
  
}

## Drop the needed initialized column
f <- f %>%
  select(-X1)

## Testing Equivalencies
q <- dta$voteshare-g[45]
q == f$c.45

## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c) %>%
  cbind(dta$cvp_agriculture)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  models[[var]] = RDestimate(
    as.formula(paste0("dta$cvp_agriculture ~ ", var)),
    data = f
  )
}

for (var in variables) {
  skip_to_next <- FALSE
  tryCatch(models[[var]] <- RDestimate(
    as.formula(paste0("dta$cvp_agriculture ~ ", var)),
    data = f
  ), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

## Testing
selection_criteria("c.43")

## Creating Cuts Name
cuts <- colnames(f)

## Finding Optimal Cut-point
s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)

s[s== max(s)]
df <- data.frame(s)

ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(title="Distribution of Selection Criteria", x = "Selection Criteria", ylab=NULL,
       subtitle = "Recovering Simulated Cut-point with No Uncertainity")
 