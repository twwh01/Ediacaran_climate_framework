#' functions to extract values from base R cut() results

cut_borders <- function(x){
  .x = as.character(x)
  
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  first <- as.numeric(gsub(pattern,"\\2", x))
  last <- as.numeric(gsub(pattern,"\\3", x))
  
  borders <- data.frame(first, last)
  return(borders)
}

cut_mean <- function(x){
  .x = as.character(x)
  
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  first <- as.numeric(gsub(pattern,"\\2", x))
  last <- as.numeric(gsub(pattern,"\\3", x))
  
  mid <- mean(c(first, last))
  return(mid)
}