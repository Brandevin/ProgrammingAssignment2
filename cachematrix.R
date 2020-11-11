## makeCacheMatrix returns a list of 4 functions that are used to help
## caching the data

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set <-function(y){
        x<<- y
        m<<- NULL
      }
      get<-function() x
      setinverse <- function(inverse) m<<-inverse
      getinverse <- function() m
      list(set=set,get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}


## Cache solve function returns the inverse of the matrix x
## If it was already calculated, returns the cached value
## If it is the first time called, calculates it by using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        print(data)
        m <- solve(data)
        x$setinverse(m)
        m
}

M<-matrix(rnorm(9),nrow=3,ncol=3)

matrix<-makeCacheMatrix(M)
cacheSolve(matrix)
