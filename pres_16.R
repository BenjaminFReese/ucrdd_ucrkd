## Packages
library(rdd)
library(rdpower)
library(rdrobust)
library(tidyverse)
library(tidymodels)
library(readr)
library(sf)

## Data
data <- read_sf("Datasets/election/election.shp")


## Basic Plot
data %>%
  ggplot(aes(x = RHI125214, y = pct_dem_16)) +
  geom_point() +
  theme_bw()

## Setting Seed
set.seed(123889)

## Saving Percentiles (Candidate Cut-points)

g <- percentile(data$RHI125214)

## Initializing Data Frame
f <- data.frame(1)

## Creates the Candidate (x-c)'s
for (i in 1:length(g)) {
  
  p <- paste0("X_", i)
  c <- assign(p, (data$RHI125214 - g[i]))
  f <- data.frame(c, f)
  
}

## Drop the needed initialized column
f <- f %>%
  select(-X1)

## Testing Equivalencies
q <- data$RHI125214-g[45]
q == f$c.45

## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c) %>%
  cbind(data$pct_dem_16)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  models[[var]] = RDestimate(
    as.formula(paste0("data$RHI125214 ~ ", var)),
    data = f
  )
}

for (var in variables) {
  skip_to_next <- FALSE
  tryCatch(models[[var]] <- RDestimate(
    as.formula(paste0("data$RHI125214 ~ ", var)),
    data = f
  ), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

## Testing
selection_criteria("c.2")


summary(RDestimate(`data$pct_dem_16`~ c.94, data=f))

x <- RDestimate(`data$pct_dem_16`~ c.94, data=f)

plot(x)

## Creating Cuts Name
cuts <- colnames(f)

## Finding Optimal Cut-point
s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)

s[s== max(s, na.rm=T)]
df <- data.frame(s)

ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(title="Distribution of Selection Criteria", x = "Selection Criteria", ylab=NULL,
       subtitle = "Recovering Simulated Cut-point with No Uncertainity")
