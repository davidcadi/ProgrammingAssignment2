#Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than computing it repeatedly (there are 
#also alternatives to matrix inversion that we will not discuss here). 

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(mxinverse) inv <<- mxinverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}


#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
    
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
     
}

# How to run
# > x = rbind(c(1, -0.5), c(-0.5, 1))
# > m = makeCacheMatrix(x)
# > m$get()
#       [,1] [,2]
# [1,]  1.0 -0.5
# [2,] -0.5  1.0

# No cache in the first run
# > cacheSolve(m)
#           [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333

# Retrieving from the cache in the second run
# > cacheSolve(m)
# getting cached data.
#           [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333
# > 
