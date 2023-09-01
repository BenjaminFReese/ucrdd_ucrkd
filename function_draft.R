## Simulated Data Function

unknown_cut <- function(x, y) {
  
  g <- percentile(x)
  f <- data.frame(1)
  for (i in 1:length(g)) {
    
    p <- paste0("X_", i)
    c <- assign(p, (x - g[i]))
    f <- data.frame(c, f)
  }
    f <- f %>%
      select(-X1)
    models <- list()
    f <- f %>%
      select(-c)
    variables <- setdiff(names(f), c("row_id"))
  
  for (var in variables) {
    models[[var]] = RDestimate(
      as.formula(paste0("y ~ ", var)),
      data = f
    )
  }
    ## Creating Cuts Name
    cuts <- colnames(f)
    
    ## Finding Optimal Cut-point
    s <- sapply(cuts, selection_criteria, USE.NAMES = TRUE)
    
    s[s== max(s)]
}

p <- unknown_cut(x,y)
p
