#The first function, makeCacheMatrix creates a special vector, which is
#really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the matrix
#get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inver <<- inverse
  get_inverse <- function() inver
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x,...) {
  ## It will give a matrix that is the inverse of 'x'
  inver <- x$get_inverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat,...)
  x$set_inverse(inver)
  inver
}
