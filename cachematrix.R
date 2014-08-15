## R function that is able to cache potentially time-consuming computations like inverse of matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(mean) inv <<- mean
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
		message("calculating inverse")
        x$setinv(inv)
        inv
}

## test
c=rbind(c(1, -1/4), c(-1/4, 1)) 
inv = solve(c)

z <- makeCacheMatrix(c)
res <- cacheSolve(z)
res2 <- cacheSolve(z)
## cache message here

inv = res
## all true

all.equal(inv, res)
## TRUE