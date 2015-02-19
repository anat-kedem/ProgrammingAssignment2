## The functions enable to calculate the inverse of a matrix and cache the inverse matrix.
## When the inverse of this matrix is requested again, instead of recompute it, 
## it is restored from the cache.


## The function makeCacheMatrix receive a matrix and return a list of functions to do:
## 1. Cache the recieved matrix (set)
## 2. Get the matrix from cache (get)
## 3. Cache the inverse matrix (setinv)
## 4. Get the inverse matrix from Cache (getinv)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                         ##when makeCacheMatrix is called by cacheSolve, 
                                          ##cacheSolve try at first to read m, which is why m should be set
        
        set <- function(y) {              ##when a new matrix passed to makeCacheMatrix
                x <<- y                   ##this internal function store the new matrix externally
                m <<- NULL                ##and clean any stored inverse matrix
        }
        
        get <- function() x               ##cacheSolve use this function to read the matrix from cache
        
        setinv <- function(inv) m <<- inv ##this internal function receive from cacheSolve 
                                          ##an inverse matrix and store it in the 
                                          ##external environment (in memory)
        
        getinv <- function() m            ##cacheSolve use this function to try to read a stored
                                          ##inverse matrix from cache
        
        list(set = set, get = get,        ##return a list of the internal functions of makeCacheMatrix
             setinv = setinv,
             getinv = getinv)
        
}


## The function cacheSolve check the cache for inverse matrix and return it if exist.
## If the cache is empty, compute the inverse matrix, store it in cache and return it.

cacheSolve <- function(x, ...) {
        m <- x$getinv()        ##try to read the inverse matrix from the cache
        if(!is.null(m)) {      ##if the inverse matrix already exist do: 
                message("getting cached inverse matrix") ##print a message
                return(m)      ##return the stored inverse matrix
        }
        data <- x$get()        ##read the matrix
        m <- solve(data, ...)  ##calculate the inverse of the matrix
        x$setinv(m)            ##and store the inverse matrix in the external environment
        m                      ##return the inverse matrix
}
