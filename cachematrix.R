makeCacheMatrix <- function(x = matrix()) {
  solved_m <- NULL
  set <- function(y) { # initialization
    x <<- y # assign the input matrix to x
    solved_m <<- NULL # set the solved_m to a default value
  } 
  get <- function() x # print the input matrix
  setm <- function(inversed) solved_m <<- inversed # store the inversed matrix
  getm <- function() solved_m # print the inversed matrix
  return(list(set = set, get = get,
       setm = setm,
       getm = getm))
}

cacheSolve <- function(x, ...) { # x is the return value of makeCacheMatrix
  m <- x$getm() # get the input matrix
  
  # if one solution has existed, return it directly
  if(!is.null(m)) { 
    message("getting inversed matrix")
    return(m)
  }
  
  # otherwise, calculate it and store it
  data <- x$get()
  solved <- solve(data)
  x$setm(solved)
}


# an example
package_function <- makeCacheMatrix(x = matrix(c(1, 0, 0, 1), 2, 2))
res <- cacheSolve(x = package_function)
package_function$getm()
