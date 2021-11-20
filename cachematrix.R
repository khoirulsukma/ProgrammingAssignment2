#Lexical Scooping Assignment####################
setwd("E:/Kursus R/Coursera/week 3/ProgrammingAssignment2")

#I simply change the "mean" function to "solve" function
#I also change the setmean/getmean to setinverse/getinverse

##this makecachematrix func makes a chaeable matrix which 
##can then be inputted into the other 
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL             ##this initializes the inverse as NULL
    set <- function(y) {  ##if user want to reset matrix,
        x <<- y           ##reassign "new" matrix to x
        m <<- NULL        ##and reinitialize m to NULL
    }
    get <- function() x   ##this is a func to get matrix x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m ##this func to obtain matrix inversion
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##chachesolve function is a function for getting and setting
##the inv. matrix value
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) { ##if user had calculated the same matrix befotr
        message("getting inversed matrix")
        return(m)     ##return the inverse of the value
    }
    data <- x$get()   ## get the uncalculated matrix
    m <- solve(data, ...) ##this calculate the inv matrx
    x$setinverse(m)   ##reassign inverse matrix
    m                 ## print the inverse matrix   
}

#Testing with data
matrix1 <- matrix(rnorm(1:25), 5,5)
m1 <- makeCacheMatrix(matrix1) ##store the chaeable matrix
cacheSolve(m1)