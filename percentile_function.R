#########################################################################################################
#####################################       Percentile Function       ###################################
#########################################################################################################


## Function to Find Percentiles
percentile <- function(x) {
  
  unique(quantile(x, probs = c(seq(.01, 1, by = .01))))
  
}

# Test

## Test Data
x<-rnorm(1000,0, 3)

## With percentile()
percentile(x)

## With quantile()
quantile(x, probs = c(seq(.01, 1, by = .01)))


#########################################################################################################
#####################################       Percentile Function       ###################################
#########################################################################################################