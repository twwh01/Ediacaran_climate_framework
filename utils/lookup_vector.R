## function to lookup values from a named vector

lookup_vector <- function(vec, value){
  looked_up <- unname(vec[value])
  return(looked_up)
}