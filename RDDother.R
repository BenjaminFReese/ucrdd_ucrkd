## Packages
library(rdd)
library(rdpower)
library(tidyverse)
library(tidymodels)

## Setting Seed
set.seed(123889)

## Simulating Data
x<-runif(1000,-1,1)
cov<-rnorm(1000)
y<-3+2*x+3*cov+10*(x>=0)+rnorm(1000)

plot(x,y)

#define x-axis
x <- seq(-1, 1, length=100)

#calculate uniform distribution probabilities
uni_y <- dunif(x, min = -.8, max = .8)

#plot uniform distribution
p1 <- plot(x, uni_y, type = 'l', main = "Probability Distribution of Finding True Cut-point", lwd = 3,
     xlab = "Candidate Cut-points", ylab = "Probability Candidate Cut-point is True Cut-point",
      xaxt = "n")


min <- 0
max <- 1

# Specify x-values for qunif function
xpos <- seq(min, max , by = 0.02)                      

# supplying corresponding y coordinations
ypos <- qunif(xpos, min = .10, max = 1)       

# plotting the graph 
p2 <- plot(ypos, type = "l", main = "Cumulative Distribution of Finding True Cut-point", lwd = 3,
     xlab = "Candidate Cut-points", ylab = "Probability Candidate Cut-point is True Cut-point",
     xaxt = "n") 

## Function to Find Percentiles
percentile <- function(x) {
  
  unique(quantile(x, probs = c(seq(.01, 1, by = .01)), na.rm = T))
  
}


## Saving Percentiles (Candidate Cut-points)

g <- percentile(x)

## Initializing Data Frame
f <- data.frame(1)

## Creates the Candidate (x-c)'s
for (i in 1:length(g)) {
  
  p <- paste0("X_", i)
  c <- assign(p, (x - g[i]))
  f <- data.frame(c, f)
  
}

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

s[s== max(s)]
df <- data.frame(s)

ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(title="Distribution of Selection Criteria", x = "Selection Criteria", ylab=NULL,
       "Recovering Simulated Cut-point with No Uncertainity")

