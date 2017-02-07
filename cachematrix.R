## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matx = matrix())
{
  inv_matx <- NULL # Intializing the inverse matrix with 'NULL'
  
  # Function to assign the values to the matrix
  setMatx <- function(mat_var)
  {
    matx <<- mat_var
    inv_matx <<- NULL # Caching the inverse matrix as 'NULL'
  }
  # Function to get the matrix
  getMatx <- function()
  {
    matx
  }
  # Function to set(assign) the inverse matrix to the cache vector
  setInverse <- function(imat)
  {
    inv_matx <<- imat
  }
  # Function to retrieve the inverse matrix
  getInverse <- function()
  {
    inv_matx
  }
  # Returning the special vector
  list(
    setMatx = setMatx,
    getMatx = getMatx,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(mtx, ...) {
  # Getting the inverse matrix (if cached)
  inverse <- mtx$getInverse()
  # If inverse matix is available then retrieve it from cache
  if (!is.null(inverse)) {
    message("Retrieving cached data...")
    return(inverse)
  }
  # Getting the matrix
  m <- mtx$getMatx()
  # Find the inverse matix
  m <- solve(m, ...)
  # Setting the inverse matrix
  mtx$setInverse(m)
  # returning inverse matrix
  m
}
