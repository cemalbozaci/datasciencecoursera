

makeVector <- function(x = numeric()) {
  browser()
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  a <<- 10
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

myVector <- makeVector(1:10)
cachemean(myVector)


cachemean(makeVector(1:100))

makeCacheMatrix = function(x=matrix()) {
if(!is.matrix(x)){
message("please make sure your input in the form of matrix(c(2,3,1,4),nrow=2,ncol=2)")
}
i = NULL
assign_catched = function(catched) i <<- catched
get_catched = function() i 
get_data = function() x
list(assign_catched = assign_catched, get_catched = get_catched,
     get_data = get_data)
}

cacheSolve = function(x,...){

  i = x$get_catched()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get_data()
  i <- solve(data, ...)
  x$assign_catched(i)
  i
}

a=makeCacheMatrix(matrix(c(2,3,1,4),nrow=2,ncol=2))
cacheSolve(a)

a = matrix(c(2,3,1,4),nrow=2,ncol=2)
solve(a)
