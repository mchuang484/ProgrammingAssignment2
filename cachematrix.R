## This two functions cache the inverse of a matrix

## makeCacheMatrix function creates special matrix which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value fo the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## setting matrix x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## retrieve matrix x
  get <- function() x
  ## set matrix m <- inverse of x
  setsolve <- function(solve) m <<- solve
  ## get matrix m
  getsolve <- function() m
  ## return list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function retrieve the inverse matrix m,  If m is not null, then return m
## If m is null then calculate m and store m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## retrieve inverse matrix of x and assign it to m
  m <- x$getsolve()
  ## if matrix is not null them return the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## retrieve matrix x
  data <- x$get()
  ## inverse matrix x and assign it to m
  m <- solve(data, ...) 
  ## Store m, inverse of x
  x$setsolve(m)
  m
}
