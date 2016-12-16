## makeCacheMatrix is a constructor function that creates functions to cache
## and recall the inverse of its argument *which must be a square matrix* 


makeCacheMatrix <- function(x = matrix()) {
        ## Create a variable which stores the cached version, and initalize to 
        ## Null
       m <- NULL
       ## Create the functions which will carry the data
       
       ## sets value of the matrix 
       set <- function(y) {
               x <<- y
               m <<- NULL
       } 
       
       ## recall the value
       get <- function() x
       
       ## Caches the inverse of the matrix
       cacheInverse <- function(solve){
               m <<- solve
       }
       ## recalls the inverse of the matrix
       getInverse <- function() m
       ## return list of the functions
       list(set = set, get = get, 
            cacheInverse = cacheInverse, 
            getInverse = getInverse)
        

}


## cacheSolve is a function that will first determine if the argument from
## makeCacheSolve has been cached, and if it has not, will create and return 
## the inverse matrix

cacheSolve <- function(x, ...) {
        ## see if inverse matrix is in cache and return
                m <- x$getInverse()
                if(!is.null(m)){
                        message("getting cached data")
                        return(m)
                }
                ## if no inverse matrix in cache, use solve to create one
                matrix <- x$get()
                m <- solve(matrix, ...)
                x$cacheInverse(m)
                ## return the inverse matrix
                m
        }

