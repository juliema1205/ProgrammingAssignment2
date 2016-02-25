#############Create a special vector contains a list of functions: set, get, setmean, getmean##
makeVector <- function(x = numeric() ) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { x }
        setmean <- function(mean) m<<-mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean =getmean)
}

#########Caculate the mean of a vector or return a pre-caculated mean if any##
cachemean <- function(x, ...){
        m <- x$getmean()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
#a<-makeVector()
#c<-makeVector()
#a$set(c(1,2))
#c$set(c(3,4))
#a$get()
#c$get()
#cachemean(a)
#cachemean(c)

#############Create a special vector contains a list of functions: set, get, setInverse, getInverse##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() { x }
        setInverse <- function(solve) inv<<-solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse =getInverse)        
}

#########Caculate the inverse of a matrix or return a pre-caculated inverse if any##
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

#m1<-makeCacheMatrix()
#m2<-makeCacheMatrix()
#m1$set(matrix(1:4,2,2))
#m2$set(matrix(5:8,2,2))
#m1$get()
#m2$get()
#cacheSolve(m1)
#cacheSolve(m2)
