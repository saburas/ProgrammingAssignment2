## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function create a matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {

# Initialize matrix variable
inv_matrix = NULL

  set = function(a) {
  # Initialize matrix m variable
  m <<- a
  #Initialize inverse matrix variable
  inv_matrix <<- NULL
  }

  get = function() m
  set_inverse = function(inverse) inv_matrix <<- inverse
  get_inverse = function() inv_matrix
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)

}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'

  inv_matrix = m$get_inverse()

  if (!is.null(inv_matrix)){
    message("getting cached data")
    return(inv_matrix)
  }

  matrix.data = m$get()
  inv_matrix = solve(matrix.data, ...)
  m$set_inverse(inv_matrix)

  return(inv_matrix)
}
