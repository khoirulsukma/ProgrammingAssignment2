#Lexical Scooping Assignment####################
setwd("E:/Kursus R/Coursera/week 3/ProgrammingAssignment2")

#I simply change the "mean" function to "solve" function
#I also change the setmean/getmean to setinverse/getinverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting inversed matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

#Testing with data
matrix1 <- matrix(rnorm(1:25), 5,5)
m1 <- makeCacheMatrix(matrix1)
cacheSolve(m1)