## Coursera Data Science - R week 3 Programming Assignment by Chinmay Gabel (28/07/18)

## Below give are the description about the two functions 

## This function take a matrix as an input and has various sub functions like :
## 1.) set_matrix which is use to take a new matrix as an input.
## 2.) get_matrix which prints the value of the matrix
## 3.) set_inverse which is use to cache the value of the inverse by getting it from user
## 4.) get_inverse which is used to display the stored value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_matrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(mean) m <<- mean
  get_inverse <- function() m
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function is used to either return the cached inverse or if no cache value is present it calculates the inverse using the 'solve' command and returns it.

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data of the matrix inverse")
    return(m)
  }
  data <- x$get_matrix()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
