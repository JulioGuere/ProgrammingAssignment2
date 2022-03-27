#TAREA JULIO GUERE
#Assignment: Caching the Inverse of a Matrix
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
#Write the following functions:
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#cachematrix.R

#Let's create the makeCacheMatrix function, this function creates a matrix, where the inverse of the matrix can be cached
#Create function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    #Gobal variable 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_Inverse <- function(inverse) inv <<- inverse
  get_Inverse <- function() inv
  list(set = set,
       get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}

## In the cacheSolve function, we are going to calculate the values ​​of the inverse matrix created in the makeCacheMatrix function (see lines above)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_Inverse()
  #If the inverse has already been computed and the matrix has not changed, then you should retrieve the inverse from the cache.
  if (!is.null(inv)) {
    message("Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set_Inverse(inv)
  inv
}

##Test the function akeCacheMatrix
#x <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

##Run function cacheSolve
#cacheSolve(x)
#x$get_Inverse()
