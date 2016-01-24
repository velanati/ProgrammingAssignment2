## It is good to cache time consuming computations results
##so that the results can be retrieved later instead of computing them again.
##For example, maxtrix inversion is usually costly, especially 
##when running inside of a loop. The following functions can compute
##and cache the inverse of a matrix.

## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## 2.cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

#### makeCacheMatrix return a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## This list is used as the input to cacheSolve function



makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##The following function calculates the inverse of the special "matrix"
##created with the above function. 
##However, it first checks to see if the invesrse has already been calculated. 
##If so, it gets the matrix invesrse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the matrix 
##and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache. 
    message("getting cached data")
    return(inv)
  }
  
  # calculates the inverse if not cached 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

