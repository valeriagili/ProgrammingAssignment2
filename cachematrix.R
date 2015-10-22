## The following functions create a special object that store a matrix and cache its inverse

## makeCacheMatrix function takes a square matrix as imput and returns a list of functions which
#  - set the value of the matrix
#  - get the value of the matrix
#  - set the value of the inverse
#  - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) {  
                x <<- y      
                inv <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        # Return a list of getters and setters for the matrix and its inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve checks if an inverse for the given matrix is already cached. 
#  If so, it returns the cached value. If not it computes the inverse matrix 
#  and caches the result

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        if (!is.null(inv)) { 
                message("getting cached data")
                return(inv) 
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
