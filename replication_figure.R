library(patchwork)

## Data
cvp <- read_dta("Datasets/CVP.dta")
elec <- read_dta("Datasets/HouseElectionResults.dta")

## Joining Data
dta <- cvp %>%
  left_join(elec, by = c("state", "dist", "cong"))

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


## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c) %>%
  cbind(dta$cvp_pubtrans)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  skip_to_next <- FALSE
  tryCatch(models[[var]] <- RDestimate(
    as.formula(paste0("dta$cvp_pubtrans ~ ", var)),
    data = f
  ), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

## Creating Cuts Name
cuts <- colnames(f)

## Finding Optimal Cut-point
s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)

s[s== max(s)]
df <- data.frame(s)

c1 <- ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(x = "Selection Criteria", ylab=NULL, title = "Transportation")

############################################################################################

## Data
cvp <- read_dta("Datasets/CVP.dta")
elec <- read_dta("Datasets/HouseElectionResults.dta")

## Joining Data
dta <- cvp %>%
  left_join(elec, by = c("state", "dist", "cong"))

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

## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c) %>%
  cbind(dta$cvp_defense)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  skip_to_next <- FALSE
  tryCatch(models[[var]] <- RDestimate(
    as.formula(paste0("dta$cvp_defense ~ ", var)),
    data = f
  ), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

## Creating Cuts Name
cuts <- colnames(f)

## Finding Optimal Cut-point
s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)

s[s== max(s)]
df <- data.frame(s)

c2 <- ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(x = "Selection Criteria", ylab=NULL, title = "Defense")

############################################################################################

## Data
cvp <- read_dta("Datasets/CVP.dta")
elec <- read_dta("Datasets/HouseElectionResults.dta")

## Joining Data
dta <- cvp %>%
  left_join(elec, by = c("state", "dist", "cong"))

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

## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c) %>%
  cbind(dta$cvp_welfare)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  skip_to_next <- FALSE
  tryCatch(models[[var]] <- RDestimate(
    as.formula(paste0("dta$cvp_welfare ~ ", var)),
    data = f
  ), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

## Creating Cuts Name
cuts <- colnames(f)

## Finding Optimal Cut-point
s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)

s[s== max(s)]
df <- data.frame(s)

c3 <- ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(x = "Selection Criteria", ylab=NULL, title = "Welfare")

############################################################################################

## Data
cvp <- read_dta("Datasets/CVP.dta")
elec <- read_dta("Datasets/HouseElectionResults.dta")

## Joining Data
dta <- cvp %>%
  left_join(elec, by = c("state", "dist", "cong"))

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

## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c) %>%
  cbind(dta$cvp_energy)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  skip_to_next <- FALSE
  tryCatch(models[[var]] <- RDestimate(
    as.formula(paste0("dta$cvp_energy ~ ", var)),
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

c4 <- ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(x = "Selection Criteria", ylab=NULL, title = "Energy")

############################################################################################

## Data
cvp <- read_dta("Datasets/CVP.dta")
elec <- read_dta("Datasets/HouseElectionResults.dta")

## Joining Data
dta <- cvp %>%
  left_join(elec, by = c("state", "dist", "cong"))

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

## Initializing Variables for local linear regression loop
models <- list()
f <- f %>%
  select(-c) %>%
  cbind(dta$cvp_agriculture)
variables <- setdiff(names(f), c("row_id"))

## Local Linear Regression Loop
for (var in variables) {
  skip_to_next <- FALSE
  tryCatch(models[[var]] <- RDestimate(
    as.formula(paste0("dta$cvp_agriculture ~ ", var)),
    data = f
  ), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

## Creating Cuts Name
cuts <- colnames(f)

## Finding Optimal Cut-point
s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)

s[s== max(s)]
df <- data.frame(s)

c5 <- ggplot(df, aes(x=s)) +
  geom_histogram() +
  theme_minimal() + 
  labs(x = "Selection Criteria", ylab=NULL, title="Agriculture")

(c1 + c2 + c3) /
  (c4 + c5) + 
  plot_annotation(title="Distribution of Selection Criteria By Policy Domain",
                  caption = "Data Source: Hall & Fowler (2016)")

